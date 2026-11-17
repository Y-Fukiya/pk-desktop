"""Conservative relocation audit, run again after staging and before signing.

Only Apple system libraries may resolve outside the candidate. This is a static
check, not a substitute for launching the moved app on a clean Mac.
"""
from __future__ import annotations
import argparse
import json
import platform
from pathlib import Path
import re
import subprocess

MAGIC = {bytes.fromhex(x) for x in (
    'cffaedfe', 'feedfacf', 'cefaedfe', 'feedface',
    'cafebabe', 'bebafeca', 'cafebabf', 'bfbafeca')}
LOADS = {'LC_LOAD_DYLIB', 'LC_LOAD_WEAK_DYLIB', 'LC_REEXPORT_DYLIB', 'LC_LOAD_UPWARD_DYLIB', 'LC_LAZY_LOAD_DYLIB'}

def is_macho(file):
    if not file.is_file() or file.is_symlink():
        return False
    with file.open('rb') as stream:
        magic = stream.read(4)
    if magic == bytes.fromhex('cafebabe'):
        return 'Mach-O' in command('file', '-b', str(file))
    return magic in MAGIC

def command(*args):
    return subprocess.check_output(args, text=True).strip()

def parse_loads(text):
    deps, rpaths, identity = [], [], []
    kind = None
    for line in text.splitlines():
        match = re.match(r'\s*cmd (LC_\w+)\s*$', line)
        if match:
            kind = match.group(1)
        match = re.match(r'\s*(?:name|path) (.+) \(offset \d+\)\s*$', line)
        if not match:
            continue
        value = match.group(1)
        target = deps if kind in LOADS else rpaths if kind == 'LC_RPATH' else identity if kind == 'LC_ID_DYLIB' else None
        if target is not None and value not in target:
            target.append(value)
    return {'dependencies': deps, 'rpaths': rpaths, 'identity': identity}

def inspect(file):
    result = parse_loads(command('otool', '-l', str(file)))
    result['architectures'] = command('lipo', '-archs', str(file)).split()
    result['executable'] = bool(re.search(r'\bEXECUTE\b', command('otool', '-hv', str(file))))
    return result

def system_path(path):
    normalized = str(Path(path).resolve())
    return normalized.startswith((
        '/usr/lib/',
        '/System/Library/',
        '/System/Volumes/Preboot/Cryptexes/OS/System/Library/',
    ))

def expand(value, loader, executable):
    for token, directory in [('@loader_path', loader.parent), ('@executable_path', executable.parent)]:
        if value == token or value.startswith(token + '/'):
            return (directory / value[len(token):].lstrip('/')).resolve()
    return Path(value).resolve() if value.startswith('/') else None

def audit(app, inspector=inspect):
    app = Path(app).resolve()
    problems = []
    records = {}
    for file in app.rglob('*'):
        if file.is_symlink() and (not file.exists() or not file.resolve().is_relative_to(app)):
            problems.append({'file': str(file.relative_to(app)), 'code': 'external_or_broken_symlink', 'value': str(file.readlink())})
        if is_macho(file):
            records[file.resolve()] = inspector(file)
    if not records:
        problems.append({'file': '.', 'code': 'no_macho_files'})
    main = (app / 'Contents/MacOS/pk-desktop').resolve()
    r_exec = (app / 'Contents/Resources/runtime/R.framework/Resources/bin/exec/R').resolve()
    for file, record in records.items():
        rel = str(file.relative_to(app))
        def problem(code, value):
            problems.append({'file': rel, 'code': code, 'value': value})
        if 'arm64' not in record.get('architectures', []):
            problem('missing_arm64', record.get('architectures', []))
        executable = file if record.get('executable') else r_exec if file.is_relative_to(app / 'Contents/Resources/runtime') else main
        # Audit the image and its host executable. Additional inherited paths are
        # deliberately not guessed: unresolved plug-in paths require review.
        paths = []
        owners = [(file, record)]
        if executable != file and executable in records:
            owners.append((executable, records[executable]))
        for owner, metadata in owners:
            for value in metadata['rpaths']:
                path = expand(value, owner, executable)
                if path is None or (not path.is_relative_to(app) and not system_path(path)):
                    problem('external_or_unknown_rpath', value)
                elif value.startswith('/') and path.is_relative_to(app):
                    problem('non_relocatable_rpath', value)
                else:
                    paths.append(path)
        for value in record['dependencies']:
            if value.startswith('/') and system_path(value):
                continue  # System dyld cache entries need not be present as files.
            if value.startswith('/'):
                problem('absolute_dependency', value)
                continue
            if value.startswith('@rpath/'):
                candidates = [(path / value[len('@rpath/'):]).resolve() for path in paths]
                target = next((p for p in candidates if p.exists() or system_path(p)), None)
            else:
                target = expand(value, file, executable)
            if target is None or (not target.exists() and not system_path(target)):
                problem('unresolved_dependency', value)
            elif not target.is_relative_to(app) and not system_path(target):
                problem('external_dependency', value)
            elif target.is_relative_to(app) and target not in records:
                problem('dependency_is_not_macho', value)
    return {'format': 'pkapp-macos-audit-1', 'status': 'passed' if not problems else 'failed',
            'native_binaries': len(records), 'unresolved': problems,
            'scope': 'Static arm64/loader-path/symlink audit; clean-Mac runtime acceptance is separate.'}

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--app', required=True)
    parser.add_argument('--output', required=True)
    args = parser.parse_args()
    if platform.system() != 'Darwin':
        raise SystemExit('Run the real Mach-O audit on macOS.')
    result = audit(args.app)
    Path(args.output).write_text(json.dumps(result, indent=2) + '\n')
    raise SystemExit(0 if result['status'] == 'passed' else 1)
