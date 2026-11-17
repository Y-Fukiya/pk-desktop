"""Specimen role is accepted at the public MCP validation boundary."""
import importlib.util
from pathlib import Path

path = Path(__file__).resolve().parents[1] / 'mcp_server.py'
module_spec = importlib.util.spec_from_file_location('pkapp_nca048_contract', path)
module = importlib.util.module_from_spec(module_spec)
module_spec.loader.exec_module(module)
spec = {
    'id': 'USUBJID', 'time': 'TIME', 'concentration': 'CONC',
    'groups': ['STUDYID', 'PPSPEC'],
    'group_roles': [{'column': 'STUDYID', 'role': 'study'}, {'column': 'PPSPEC', 'role': 'specimen'}],
}
assert module.validate_nca(spec) is None
spec['group_roles'][1]['role'] = 'invalid'
assert module.validate_nca(spec)['error']['code'] == 'E_VALIDATION'
print('Specimen role accepted; invalid role rejected: PASS')
