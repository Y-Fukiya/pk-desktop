"""Run a synthetic assembly, BLQ, terminal NCA and replay against an empty NCA project."""
import argparse
import asyncio
import csv
import json
import datetime
from importlib.metadata import version
from pathlib import Path
import subprocess
import sys
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def run(project):
    root = Path(__file__).resolve().parents[3]
    server = Path(__file__).resolve().parents[1] / 'mcp_server.py'
    rows = {
        'conc': [['ID','SAMPLE_ID','ANALYTE','CONC','BLQ','MDV','UNIT']] + [['SYNTHETIC',str(i),'DRUG',str(x),'N','N','ng/mL'] for i,x in enumerate([0,10,8,4,2,1,0.5])],
        'pc': [['ID','SAMPLE_ID','SAMPLE_DTM']] + [['SYNTHETIC',str(i),f'2025-01-{1+h//24:02d}T{h%24:02d}:00:00'] for i,h in enumerate([0,1,2,4,8,12,16])],
        'ex': [['ID','DOSE_ID','DOSE_DTM'],['SYNTHETIC','D1','2025-01-01T00:00:00']],
    }
    paths = []
    try:
        for name, data in rows.items():
            path = project / ('nca019_synthetic_' + name + '.csv')
            if path.exists():
                raise RuntimeError('Synthetic fixture already exists')
            with path.open('w', newline='') as out:
                csv.writer(out).writerows(data)
            paths.append(path)
        params = StdioServerParameters(command=sys.executable,args=[str(server),'--mode','nca','--runtime',str(project / '.pkapp-runtime.json')])
        async with stdio_client(params) as (read, write):
            async with ClientSession(read,write) as session:
                await session.initialize()
                async def call(name, args):
                    response = await session.call_tool(name,args)
                    payload = response.structuredContent or json.loads(response.content[0].text)
                    if response.isError or not payload.get('ok'):
                        raise RuntimeError(str(payload))
                    return payload['result']
                status = await call('pk_get_status',{})
                assert status['contract_version'] == 'pkapp-nca-18'
                assert not await call('pk_list_tables',{})
                scope = {'project_id':status['project_id'],'base_revision':status['revision'],'actor':'synthetic test'}
                ids = {}
                for path, name in zip(paths,rows):
                    ids[name] = (await call('pk_import_table',{**scope,'path':str(path),'name':'synthetic '+name}))['id']
                specification = {'version':1,'mapping':{'CONC':{'ID':'ID','SAMPLE_ID':'SAMPLE_ID','ANALYTE':'ANALYTE','CONC':'CONC','BLQ':'BLQ','MDV':'MDV','UNIT':'UNIT'},'PC':{'ID':'ID','SAMPLE_ID':'SAMPLE_ID','SAMPLE_DTM':'SAMPLE_DTM'},'EX':{'ID':'ID','DOSE_ID':'DOSE_ID','DOSE_DTM':'DOSE_DTM'}},'sample_keys':['ID','SAMPLE_ID'],'profile_keys':['ID','ANALYTE'],'dose_group_keys':['ID'],'dose_record_keys':['ID','DOSE_ID'],'observation_keys':['ID','SAMPLE_ID','ANALYTE'],'dose_reference':{'mode':'first'},'datetime':{'sample_type':'iso','dose_type':'iso','utc_offset':'+00:00'},'unmatched':'missing','missing_datetime':'missing','dose_history_confirmed':True}
                assembled = await call('pk_assemble_nca',{**scope,**ids,'specification':specification,'name':'synthetic assembly','reason':'integration test'})
                assert assembled['counts']['result'] == 7 and 'SYNTHETIC' not in json.dumps(assembled)
                scope['base_revision'] = assembled['revision']
                step = {'op':'blq_rules','mode':'positional','profile_columns':['ID','ANALYTE'],'concentration':'CONC','time':'NCA_TIME_H','blq_column':'BLQ','mdv_column':'MDV','zero_time':'pre','pre_rule':'zero','first_rule':'zero','middle_rule':'exclude','last_rule':'exclude','all_blq_rule':'exclude'}
                prepared = await call('pk_prepare_data',{**scope,'source':assembled['id'],'steps':[step],'name':'synthetic BLQ'})
                state = await call('pk_nca_configure',{**scope,'source':prepared['id'],'reason':'integration test','specification':{'id':'ID','groups':['ANALYTE'],'time':'NCA_TIME_H','concentration':'NCA_CONC','unit_column':'UNIT','blq_column':'NCA_BLQ','mdv_column':'NCA_MDV','blq':'as_recorded','c0_method':'none','terminal_method':'auto','dosing_context':'single','dose_history_confirmed':True}})
                scope['base_revision'] = state['revision']
                result = await call('pk_nca_run',{**scope,'reason':'integration test'})
                assert result['is_current'] and 'SYNTHETIC' not in json.dumps(result)
                assert all(not any(k in p for k in ('mean','min','max')) for p in result['parameters'])
                assert 'terminal_status' in result
                saved = await call('pk_nca_save_result',{**scope,'result_id':result['result_id'],'name':'synthetic result','reason':'integration test'})
                with (project/'prepared'/saved['id']/'result.csv').open() as f:
                    values = {r['PARAMCD']:r['AVAL'] for r in csv.DictReader(f)}
                assert all(values.get(k) not in (None,'','NA') for k in ('CMAX','AUCLST','LAMZ','LAMZHL','AUCIFO','AUCPEO'))
                replay = {}
                for label, table in zip(('assembly', 'blq', 'nca'), (assembled, prepared, saved)):
                    folder = project/'prepared'/table['id']
                    subprocess.run(['Rscript','-e','source("apps/pk-desktop/pkapp.R"); pkapp_replay(commandArgs(TRUE)[1])',str(folder)],cwd=root,check=True,stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                    replay[label] = 'passed'
                return {'contract_version': status['contract_version'], 'app_version': status['app_version'],
                        'assembly_counts': assembled['counts'], 'result_current': result['is_current'],
                        'disclosure': 'counts_only', 'terminal_status': result['terminal_status'],
                        'local_csv_required_parameters_present': sorted(('CMAX','AUCLST','LAMZ','LAMZHL','AUCIFO','AUCPEO')),
                        'replay': replay}
    finally:
        for path in paths:
            path.unlink(missing_ok=True)

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--project',type=Path,required=True)
parser.add_argument('--output',type=Path,required=True, help='New JSON evidence file')
args = parser.parse_args()
if args.output.exists():
    parser.error('--output must be a new file')
report = {'scope': 'synthetic stdio, loopback, saved CSV and replay',
          'timestamp_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
          'package_versions': {'python': sys.version.split()[0], 'mcp': version('mcp'), 'jsonschema': version('jsonschema')}}
try:
    r_versions = subprocess.run(['Rscript', '-e', 'cat(jsonlite::toJSON(list(R=as.character(getRversion()), PKNCA=as.character(packageVersion("PKNCA")), jsonlite=as.character(packageVersion("jsonlite")), shiny=as.character(packageVersion("shiny"))), auto_unbox=TRUE))'], capture_output=True, text=True, check=True)
    report['package_versions'].update(json.loads(r_versions.stdout))
    report.update(asyncio.run(run(args.project.resolve(strict=True))))
    report['status'] = 'passed'
except Exception as exc:
    report.update(status='failed', error_type=type(exc).__name__)
finally:
    args.output.write_text(json.dumps(report, indent=2) + '\n', encoding='utf-8')
print('Synthetic MCP integration:', report['status'])
sys.exit(0 if report['status'] == 'passed' else 1)
