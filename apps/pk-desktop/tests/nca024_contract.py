"""Strict pre-NCA schema and real stdio errors must not disclose input values."""
import asyncio
import copy
import importlib.util
import json
from pathlib import Path
import sys
from jsonschema import Draft202012Validator
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

app = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("nca024_mcp", app / "mcp_server.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
fixture = app / "examples/nca_dose_body"
valid = json.loads((fixture / "prepare_spec.json").read_text())
nca = json.loads((fixture / "nca_spec.json").read_text())
assert module.CONTRACT_VERSION == "pkapp-nca-18"
assert module.APP_VERSION == json.loads((app / "src-tauri/tauri.conf.json").read_text())["version"]
assert module.validate_covariates(valid) is None
assert module.validate_nca(nca) is None
secret = "PRIVATE_BODY_VALUE_024"
invalid = [None, [], {"version": 1}, {**valid, "unknown": secret}]
for path, value in [(('vs', 'baseline', 'confirmed'), False), (('vs', 'baseline', 'mode'), secret), (('profile', 'groups'), 'PERIOD')]:
    item = copy.deepcopy(valid)
    owner = item
    for key in path[:-1]:
        owner = owner[key]
    owner[path[-1]] = value
    invalid.append(item)
for item in invalid:
    assert module.validate_covariates(item)['error']['code'] == 'E_VALIDATION'
server = module.create_nca_server(Path('/nonexistent'))
tools = server._tool_manager._tools
assert len(tools) == 19
args = dict(source='table-' + 'a' * 24, vs='table-' + 'b' * 24, specification=valid, name='test', project_id='p', base_revision=1, actor='test', reason='test')
assert not list(Draft202012Validator(tools['pk_prepare_nca_covariates'].parameters).iter_errors(args))
for field, value in [('dose', {**nca['dose'], 'meaning': 'planned'}), ('derived_parameters', ['SYSTEMIC_CL']), ('body_size', {'reference_weight': 70})]:
    assert module.validate_nca({**nca, field: value})['error']['code'] == 'E_VALIDATION'

async def check():
    params = StdioServerParameters(command=sys.executable, args=[str(app/'mcp_server.py'), '--mode', 'nca', '--runtime', '/nonexistent'])
    async with stdio_client(params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            listed = {t.name: t for t in (await session.list_tools()).tools}
            assert len(listed) == 19 and 'pk_prepare_nca_covariates' in listed
            for item in invalid:
                response = await session.call_tool('pk_prepare_nca_covariates', {**args, 'specification': item})
                payload = response.structuredContent or json.loads(response.content[0].text)
                assert payload['error']['code'] == 'E_VALIDATION'
                assert secret not in json.dumps(payload)

asyncio.run(check())
print('NCA024 schema and real stdio safe validation passed')
