"""Run against a running empty synthetic NCA project; never use patient data."""
import asyncio, csv, json, math, os, shutil, subprocess, sys
from pathlib import Path
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client
import argparse
parser = argparse.ArgumentParser(description="Synthetic pre-NCA DM/VS live MCP acceptance")
parser.add_argument("--project", type=Path, required=True, help="Empty dedicated project with a running NCA app")
parser.add_argument("--output", type=Path, required=True, help="JSON evidence file")
args = parser.parse_args()
APP = Path(__file__).resolve().parents[1]
FIX = APP / "examples/nca_dose_body"
PROJECT = args.project.resolve()
OUT = args.output.resolve()
async def main():
 params=StdioServerParameters(command=sys.executable,args=[str(APP/'mcp_server.py'),'--mode','nca','--runtime',str(PROJECT/'.pkapp-runtime.json')])
 checks=[]
 async with stdio_client(params) as (read,write):
  async with ClientSession(read,write) as session:
   await session.initialize()
   async def call(name,args):
    answer=await session.call_tool(name,args)
    payload=answer.structuredContent or json.loads(answer.content[0].text)
    if answer.isError or not payload.get('ok'): raise RuntimeError(f'{name}: {payload}')
    return payload['result']
   status=await call('pk_get_status',{})
   await call('pk_nca_get_spec',{})
   assert status['contract_version']=='pkapp-nca-18'
   tools=await session.list_tools()
   assert len(tools.tools)==19
   assert not await call('pk_list_tables',{}),'Requires empty dedicated synthetic project'
   async def scope():
    s=await call('pk_get_status',{})
    return dict(project_id=s['project_id'],base_revision=s['revision'],actor='Synthetic acceptance')
   ids={}
   for role,file in [('source','synthetic.csv'),('dm','DM.csv'),('vs','VS.csv')]:
    assert not (PROJECT/file).exists(), 'Use an empty dedicated synthetic project'
    shutil.copyfile(FIX/file,PROJECT/file)
    ids[role]=(await call('pk_import_table',{**await scope(),'path':str(PROJECT/file),'name':'TEST ONLY '+role}))['id']
   spec=json.loads((FIX/'prepare_spec.json').read_text())
   prepared=await call('pk_prepare_nca_covariates',{**await scope(),**ids,'specification':spec,'name':'TEST ONLY pre-NCA DM VS','reason':'Synthetic pre-NCA validation'})
   checks.append('pre-NCA DMVS through stdio')
   nca=json.loads((FIX/'nca_spec.json').read_text()); nca['c0_method']='none'
   configured=await call('pk_nca_configure',{**await scope(),'source':prepared['id'],'specification':nca,'reason':'Synthetic analytic fixture'})
   result=await call('pk_nca_run',{**await scope(),'reason':'Synthetic derived parameter validation'})
   assert result['is_current']
   assert len(result['parameters'])==16
   assert all(not any(k in p for k in ('mean','min','max','sd','median')) for p in result['parameters'])
   checks.append('all 16 base and derived parameters counts only')
   saved=await call('pk_nca_save_result',{**await scope(),'result_id':result['result_id'],'name':'TEST ONLY dose/body NCA','reason':'Synthetic local checks and replay'})
   with (PROJECT/'prepared'/saved['id']/'result.csv').open() as stream:
    rows=list(csv.DictReader(stream))
   expected={'CMAX':10,'TMAX':0,'AUCLST':100*(1-math.exp(-4.8)),'LAMZ':.1,'LAMZHL':math.log(2)/.1,'AUCIFO':100,
     'CMAXD':.1,'AUCLSTD':1-math.exp(-4.8),'AUCIFOD':1,'CLFO':1,'VZFO':10,'CLFOWT':.02,'VZFOWT':.2,'CLFOBSA':.5,'VZFOBSA':5}
   for code,value in expected.items():
    match=[r for r in rows if r['PARAMCD']==code]
    assert len(match)==1,(code,len(match))
    assert math.isclose(float(match[0]['AVAL']),value,rel_tol=1e-10,abs_tol=1e-10),(code,match[0]['AVAL'])
   checks.append('15 independent closed-form reference values')
   rscript = shutil.which('Rscript')
   assert rscript, 'Rscript must be on PATH with the app dependencies installed'
   for table in [prepared['id'],saved['id']]:
    command = "source(commandArgs(TRUE)[1]); value <- pkapp_replay(commandArgs(TRUE)[2]); stopifnot(nrow(value$data)>0)"
    p=subprocess.run([rscript,'--vanilla','-e',command,str(APP/'pkapp.R'),str(PROJECT/'prepared'/table)],cwd=APP.parents[1],env=os.environ.copy(),capture_output=True,text=True)
    if p.returncode: raise RuntimeError('Replay failed: '+p.stderr[-2000:])
   checks.append('preparation and NCA frozen replay')
   evidence={'status':'passed','checks':checks,'contract':'pkapp-nca-18','tools':19,'app_version':status['app_version'],'table_ids':ids,'prepared_id':prepared['id'],'result_id':result['result_id'],'saved_id':saved['id'],'expected':expected}
   OUT.write_text(json.dumps(evidence,indent=2)+'\n')
   print(json.dumps({'status':'passed','checks':len(checks),'tools':19}))
asyncio.run(main())
