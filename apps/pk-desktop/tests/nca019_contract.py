"""Assembly specification validation is safe and matches the published tool schema."""
import importlib.util
import json
from pathlib import Path
from jsonschema import Draft202012Validator

script = Path(__file__).resolve().parents[1] / 'mcp_server.py'
spec = importlib.util.spec_from_file_location('nca019_mcp', script)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
secret = 'PRIVATE_INPUT_VALUE_919'
valid = {'version': 1, 'mapping': {'CONC': {'ID':'ID','CONC':'CONC'}, 'PC': {'ID':'ID','SAMPLE_DTM':'DT'}, 'EX': {'ID':'ID','DOSE_ID':'DOSE','DOSE_DTM':'DT'}}, 'sample_keys':['ID'], 'profile_keys':['ID'], 'dose_group_keys':['ID'], 'dose_record_keys':['ID','DOSE_ID'], 'observation_keys':['ID'], 'dose_reference':{'mode':'first'}, 'datetime':{'sample_type':'iso','dose_type':'iso','utc_offset':'+00:00'}, 'unmatched':'missing', 'missing_datetime':'missing', 'dose_history_confirmed':True}
assert module.validate_assembly(valid) is None
invalid = [None, [], {**valid, 'version': 2}, {**valid, 'mapping': {'CONC': {'ID': secret}, 'PC': {}, 'EX': {}}}, {**valid, 'sample_keys':['ID','ID']}, {**valid, 'datetime': {**valid['datetime'], 'utc_offset': secret}}, {**valid, 'dose_reference': {'mode':'selected','predicate':{'op':'system','args':[{'value':secret}]}}}, {**valid, 'unexpected':secret}]
for value in invalid:
    result = module.validate_assembly(value)
    assert result and result['error']['code'] == 'E_VALIDATION'
    assert secret not in json.dumps(result)
server = module.create_nca_server(Path('/nonexistent'))
tools = server._tool_manager._tools
assert len(tools) == 19
schema = tools['pk_assemble_nca'].parameters
assert 'specification' in schema['properties']
assert not list(Draft202012Validator(schema).iter_errors({'conc':'a','pc':'b','ex':'c','specification':valid,'name':'x','project_id':'p','base_revision':1,'actor':'a','reason':'r'}))
print('NCA019 assembly schema: PASS')

import asyncio
import sys
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def stdio_validation():
    params = StdioServerParameters(command=sys.executable, args=[str(script), '--mode', 'nca', '--runtime', '/nonexistent'])
    async with stdio_client(params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            tools = {tool.name: tool for tool in (await session.list_tools()).tools}
            assert len(tools) == 19 and 'specification' in tools['pk_assemble_nca'].inputSchema['properties']
            for item in invalid:
                response = await session.call_tool('pk_assemble_nca', {'conc':'a','pc':'b','ex':'c','specification':item,'name':'x','project_id':'p','base_revision':1,'actor':'a','reason':'r'})
                payload = response.structuredContent or json.loads(response.content[0].text)
                assert payload['error']['code'] == 'E_VALIDATION'
                assert secret not in json.dumps(payload)

asyncio.run(stdio_validation())
print('NCA019 real stdio safe validation: PASS')
