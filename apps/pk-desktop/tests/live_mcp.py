"""Exercise real stdio MCP -> loopback HTTP -> R. Not a Tauri/Mac GUI test."""
import argparse
import asyncio
import json
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import time
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def test(root, r):
    with tempfile.TemporaryDirectory(prefix="pk-live-") as tmp:
        project=Path(tmp)
        with socket.socket() as s:
            s.bind(('127.0.0.1',0));port=s.getsockname()[1]
        log=(project/'R.log').open('w')
        child=subprocess.Popen([r,'--vanilla','--slave','-f','apps/pk-desktop/tests/live_engine.R','--args',tmp,str(port)],cwd=root,stdout=log,stderr=log)
        try:
            runtime=project/'runtime.json'
            deadline=time.monotonic()+60
            while not runtime.exists():
                if child.poll() is not None or time.monotonic()>deadline:
                    log.flush();raise RuntimeError((project/'R.log').read_text())
                await asyncio.sleep(.1)
            params=StdioServerParameters(command=sys.executable,args=[str(root/'apps/pk-desktop/mcp_server.py'),'--runtime',str(runtime)])
            async with stdio_client(params) as (read,write):
                async with ClientSession(read,write) as session:
                    await session.initialize()
                    tools=(await session.list_tools()).tools
                    assert len(tools)==23, [t.name for t in tools]
                    async def call(name,arguments={}):
                        reply=await session.call_tool(name,arguments)
                        data=json.loads(next(c.text for c in reply.content if c.type=='text'))
                        assert data.get('ok'),data
                        return data['result']
                    status=await call('pk_get_status')
                    scope={'base_revision':status['revision'],'project_id':status['project_id']}
                    spec=await call('pk_get_spec')
                    changed=await call('pk_update_spec',dict(scope,section='conc_plot',values={'group_by':['ARM'],'facet_by':['SEX'],'error':'se'},actor='MCP test',reason='two variable stratification'))
                    scope['base_revision']=changed['spec']['revision']
                    run=await call('pk_run',scope)
                    result=await call('pk_get_result',{'run_id':run['run_id']})
                    assert result['analysis_hash']==run['analysis_hash']
                    replayed=await call('pk_replay_run',dict(scope,run_id=run['run_id']))
                    assert replayed['analysis_hash']==run['analysis_hash']
                    manifest=await call('pk_get_run_manifest',{'run_id':replayed['run_id']})
                    assert manifest['replay_of']==run['run_id']
                    meta=await call('pk_describe_data',{'source':'ADNCA'})
                    assert meta['rows']==750 and 'USUBJID' in meta['columns']
                    table=await call('pk_prepare_data',dict(scope,source='ADPP',steps=[{'op':'filter','column':'PARAMCD','operator':'eq','values':['CMAXSS']}],name='CMAX',actor='MCP test'))
                    assert table['counts']['result']==50
                    joined=await call('pk_join_data',dict(scope,left='ADNCA',right=table['id'],by=['STUDYID','USUBJID'],columns=['AVAL'],join='left',name='joined',actor='MCP test'))
                    assert joined['counts']['matched']==750 and joined['counts']['result']==750
                    assert len(await call('pk_list_tables'))==2
                    catalog=await call('pk_list_modules')
                    assert catalog['revision']==0
                    mod=await call('pk_add_module',dict(scope,modules_revision=0,module='pk_scatterplot',source=joined['id'],label='Live scatter',options={'x':'TIME_H','y':'AVAL'},actor='test',reason='live addition'))
                    assert mod['entry']['input_rows']==750
                    await call('pk_remove_module',dict(scope,modules_revision=1,id=mod['entry']['id'],actor='test',reason='remove'))
                    assert not (await call('pk_list_modules'))['entries'][0]['enabled']
                    local=project/'id.csv';local.write_text('ID,VALUE\n001,3.14\n')
                    imported=await call('pk_import_table',dict(scope,path=str(local),name='Imported IDs',actor='test'))
                    assert imported['counts']['result']==1
                    validated=await call('pk_validate_analysis_tables',dict(scope,adnca='ADNCA',adpp='ADPP',mapping={},time_unit='h'))
                    applied=await call('pk_apply_analysis_tables',dict(scope,validation_id=validated['validation_id'],actor='test',reason='validated table pipeline'))
                    assert applied['revision']==scope['base_revision']+1
                    print(json.dumps({'status':'passed','tools':len(tools),'strata':['ARM'],'facets':['SEX'],'interval':'se','replay':True,'joined_rows':750,'scope':'stdio/HTTP/R; no Mac GUI'},indent=2))
        finally:
            (project/'stop').touch()
            try: child.wait(timeout=10)
            except subprocess.TimeoutExpired: child.terminate();child.wait(timeout=10)
            log.close()

if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--r',required=True)
    args=parser.parse_args();asyncio.run(test(Path(__file__).resolve().parents[3],args.r))
