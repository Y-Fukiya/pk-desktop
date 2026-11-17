"""Synthetic-only live NCA and exploration MCP round trip in an empty project."""
import argparse, asyncio, csv, json, sys
from pathlib import Path
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def main():
    parser=argparse.ArgumentParser(description=__doc__); parser.add_argument("--project", required=True, help="empty dedicated test project"); options=parser.parse_args()
    project=Path(options.project).resolve()
    app=Path(__file__).resolve().parents[1]
    fixture=project/'synthetic_nca030.csv'
    with fixture.open('x',newline='') as f:
        writer=csv.writer(f);writer.writerow(['ID','TIME','CONC','UNIT','ARM','PERIOD','ANALYTE'])
        for subject,arm,peak in [('S01','A',8),('S02','A',10),('S03','A',12)]:
            for analyte, factor in [('parent',1),('metabolite',0.5)]:
                for t,conc in [(0,0),(1,peak),(2,peak/2),(4,peak/4),(8,peak/8)]:writer.writerow([subject,t,conc*factor,'mg/L',arm,'1',analyte])
    params=StdioServerParameters(command=sys.executable,args=[str(app/'mcp_server.py'),'--mode','nca','--runtime',str(project/'.pkapp-runtime.json')])
    async with stdio_client(params) as (read,write):
      async with ClientSession(read,write) as session:
        await session.initialize()
        async def call(name,args):
            response=await session.call_tool(name,args)
            payload=response.structuredContent or json.loads(response.content[0].text)
            if response.isError or not payload.get('ok'):raise RuntimeError((name,payload))
            return payload['result']
        status=await call('pk_get_status',{}); await call('pk_nca_get_spec',{})
        assert status['app_version']=='0.61.1-alpha' and status['contract_version']=='pkapp-nca-18'
        assert not await call('pk_list_tables',{})
        scope={'project_id':status['project_id'],'base_revision':status['revision'],'actor':'test'}
        table=await call('pk_import_table',{**scope,'path':str(fixture),'name':'TEST ONLY source'})
        prepared=await call('pk_prepare_data',{**scope,'source':table['id'],'name':'TEST ONLY numeric','steps':[{'op':'convert','column':'TIME','type':'numeric'},{'op':'convert','column':'CONC','type':'numeric'}]})
        state=await call('pk_nca_configure',{**scope,'reason':'synthetic test','source':prepared['id'],'specification':{'id':'ID','groups':['ARM','PERIOD','ANALYTE'],'group_roles':[{'column':'ARM','role':'treatment'},{'column':'PERIOD','role':'period'},{'column':'ANALYTE','role':'analyte'}],'period_order':{'column':'PERIOD','values':['1']},'time':'TIME','concentration':'CONC','unit_column':'UNIT','c0_method':'none'}})
        assert state['specification']['period_order']=={'column':'PERIOD','values':['1']}
        invalid_roles={'id':'ID','groups':['ARM','PERIOD','ANALYTE'],'group_roles':[{'column':'PERIOD','role':'period'},{'column':'PERIOD','role':'analyte'}],'time':'TIME','concentration':'CONC','unit_column':'UNIT','c0_method':'none'}
        refused=await session.call_tool('pk_nca_configure',{**scope,'reason':'duplicate roles','source':prepared['id'],'specification':invalid_roles,'base_revision':state['revision']})
        refused_body=refused.structuredContent or json.loads(refused.content[0].text)
        assert not refused_body['ok'] and '複数の役割' in refused_body['error']['message']
        invalid_roles['group_roles']=[{'column':'secret_missing_group','role':'period'}]
        refused=await session.call_tool('pk_nca_configure',{**scope,'reason':'invalid role','source':prepared['id'],'specification':invalid_roles,'base_revision':state['revision']})
        refused_body=refused.structuredContent or json.loads(refused.content[0].text)
        assert not refused_body['ok'] and '追加グループ列' in refused_body['error']['message'] and 'secret_missing_group' not in json.dumps(refused_body)
        scope['base_revision']=state['revision']
        result=await call('pk_nca_run',{**scope,'reason':'synthetic test'})
        assert result['counts']['curves']==6 and result['counts']['failed_curves']==0
        saved=await call('pk_nca_save_result',{**scope,'reason':'save test','name':'TEST ONLY result','result_id':result['result_id']})
        exploration=await call('pk_nca_explore_get',{})
        chosen=await call('pk_nca_explore_set',{**scope,'reason':'explore test','source':saved['id'],'view':'boxplot','parameter':'CMAX','unit':'mg/L','group':'ARM','explore_revision':exploration['explore_revision'],'explore_session':exploration['explore_session']})
        assert chosen['selection']=={'source':saved['id'],'view':'boxplot','parameter':'CMAX','unit':'mg/L','group':'ARM','unit_missing':False,'aggregate_over':[],'scale':'linear','summary_basis':'analysis_unit'}
        assert (await call('pk_nca_explore_get',{}))['selection']==chosen['selection']
        assert not any(k in json.dumps(chosen) for k in ['S01','S02','S03','AVAL'])
        assert not (await call('pk_nca_get_result',{'result_id':result['result_id'],'project_id':scope['project_id']}))['disclosure']['allowed']
        invalid=await session.call_tool('pk_nca_explore_set',{**scope,'reason':'bad unit test','source':saved['id'],'view':'boxplot','parameter':'CMAX','unit':'ng/mL','group':'ARM','explore_revision':chosen['explore_revision'],'explore_session':chosen['explore_session']})
        body=invalid.structuredContent or json.loads(invalid.content[0].text)
        assert not body['ok'] and (await call('pk_nca_explore_get',{}))['selection']==chosen['selection']
        pooled=await call('pk_nca_explore_set',{**scope,'reason':'explicit pooling test','source':saved['id'],'view':'summary','parameter':'CMAX','unit':'mg/L','group':'','aggregate_over':['PERIOD'],'scale':'log','explore_revision':chosen['explore_revision'],'explore_session':chosen['explore_session']})
        assert pooled['selection']['scale']=='log'
        assert pooled['selection']['aggregate_over'] == ['PERIOD']
        rejected=await session.call_tool('pk_nca_explore_set',{**scope,'reason':'reject pooling id','source':saved['id'],'view':'summary','parameter':'CMAX','unit':'mg/L','group':'','aggregate_over':['ID'],'explore_revision':pooled['explore_revision'],'explore_session':pooled['explore_session']})
        rejected_body=rejected.structuredContent or json.loads(rejected.content[0].text)
        assert not rejected_body['ok'] and '解析グループ列' in rejected_body['error']['message']
        blocked=await session.call_tool('pk_nca_explore_set',{**scope,'reason':'reject analyte pooling','source':saved['id'],'view':'summary','parameter':'CMAX','unit':'mg/L','group':'ARM','aggregate_over':['ANALYTE'],'explore_revision':pooled['explore_revision'],'explore_session':pooled['explore_session']})
        blocked_body=blocked.structuredContent or json.loads(blocked.content[0].text)
        assert not blocked_body['ok']
        contradiction=await session.call_tool('pk_nca_explore_set',{**scope,'reason':'reject contradictory selection','source':saved['id'],'view':'summary','parameter':'CMAX','unit':'mg/L','group':'PERIOD','aggregate_over':['PERIOD'],'explore_revision':pooled['explore_revision'],'explore_session':pooled['explore_session']})
        contradiction_body=contradiction.structuredContent or json.loads(contradiction.content[0].text)
        assert not contradiction_body['ok']
        subject_summary=await call('pk_nca_explore_set',{**scope,'reason':'subject-level test','source':saved['id'],'view':'summary','parameter':'CMAX','unit':'mg/L','group':'ARM','aggregate_over':['PERIOD'],'summary_basis':'subject_geomean','explore_revision':pooled['explore_revision'],'explore_session':pooled['explore_session']})
        assert subject_summary['selection']['summary_basis']=='subject_geomean'
        manifests=[p for p in project.rglob('manifest.json') if p.parent.name==saved['id']]
        assert len(manifests)==1
        origin=json.loads((manifests[0].parent/'role_provenance.json').read_text())
        assert origin['origin']['status']=='matched' and origin['origin']['actor']=='test'
        modules=await call('pk_nca_list_modules',{})
        scatter=next(x for x in modules['catalog'] if x['module']=='pk_scatterplot')
        if scatter['available']:
            added=await call('pk_nca_add_module',{**scope,'modules_revision':modules['revision'],'reason':'synthetic plot','source':prepared['id'],'module':'pk_scatterplot','label':'TEST ONLY concentration points','options':{'x':'TIME','y':'CONC','color':'ARM'}})
            assert added['entry']['enabled'] and added['entry']['input_rows']==30
            assert not (await call('pk_nca_get_result',{'result_id':result['result_id'],'project_id':scope['project_id']}))['disclosure']['allowed']
            print('PASS: teal module added:',added['entry']['id'])
        else:
            assert not any(x['available'] for x in modules['catalog'])
            print('PASS: optional teal modules reported unavailable')
        no_treatment_spec={'id':'ID','groups':['PERIOD','ANALYTE'],'group_roles':[{'column':'PERIOD','role':'period'},{'column':'ANALYTE','role':'analyte'}],'time':'TIME','concentration':'CONC','unit_column':'UNIT','c0_method':'none'}
        state2=await call('pk_nca_configure',{**scope,'reason':'test omitted treatment','source':prepared['id'],'specification':no_treatment_spec})
        scope['base_revision']=state2['revision']
        result2=await call('pk_nca_run',{**scope,'reason':'test missing treatment role'})
        saved2=await call('pk_nca_save_result',{**scope,'reason':'save test','name':'TEST ONLY no treatment','result_id':result2['result_id']})
        request={**scope,'reason':'test safety','source':saved2['id'],'view':'summary','parameter':'CMAX','unit':'mg/L','group':'','aggregate_over':['PERIOD'],'summary_basis':'subject_geomean','explore_revision':subject_summary['explore_revision'],'explore_session':subject_summary['explore_session']}
        no_role=await session.call_tool('pk_nca_explore_set',request)
        no_role_body=no_role.structuredContent or json.loads(no_role.content[0].text)
        assert not no_role_body['ok'] and '処置の役割' in no_role_body['error']['message']
        assert not any(x in json.dumps(no_role_body) for x in ['S01','S02','S03','parent','metabolite'])
        exception=await call('pk_nca_explore_set',{**request,'single_treatment_confirmed':True,'single_treatment_reason':'synthetic single-treatment test only'})
        assert exception['selection']['single_treatment_confirmed'] and exception['selection']['single_treatment_reason']=='synthetic single-treatment test only'
        false_role_spec={'id':'ID','groups':['PERIOD','ANALYTE'],'group_roles':[{'column':'PERIOD','role':'period'},{'column':'ANALYTE','role':'treatment'}],'time':'TIME','concentration':'CONC','unit_column':'UNIT','c0_method':'none'}
        state3=await call('pk_nca_configure',{**scope,'reason':'synthetic wrong role','source':prepared['id'],'specification':false_role_spec})
        scope['base_revision']=state3['revision']
        result3=await call('pk_nca_run',{**scope,'reason':'synthetic wrong role'})
        saved3=await call('pk_nca_save_result',{**scope,'reason':'save test','name':'TEST ONLY wrong role','result_id':result3['result_id']})
        unsafe={**scope,'reason':'conflicting treatments','source':saved3['id'],'view':'summary','parameter':'CMAX','unit':'mg/L','group':'','aggregate_over':['PERIOD'],'summary_basis':'subject_geomean','explore_revision':exception['explore_revision'],'explore_session':exception['explore_session']}
        refusal=await session.call_tool('pk_nca_explore_set',unsafe)
        refusal_body=refusal.structuredContent or json.loads(refusal.content[0].text)
        assert not refusal_body['ok'] and refusal_body['error']['code']=='E_VALIDATION' and '同一被験者・同一期' in refusal_body['error']['message']
        assert not any(x in json.dumps(refusal_body) for x in ['S01','S02','S03','parent','metabolite'])
        print('PASS: 0.46 stdio MCP import / NCA / conflicting treatment guard / single-treatment exception / disclosure')
asyncio.run(main())
