"""Stage a fresh macOS app with R. Requires macOS; produces an unsigned candidate.

External native dependencies are inventoried and rejected, never silently omitted.
Use --native-root for dependencies packaged separately under Resources/native.
"""
from __future__ import annotations
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
import subprocess
from audit_macos import audit, is_macho, parse_loads

def run(*args):
    return subprocess.check_output(args, text=True).strip()

SMOKE_FORMAT = "pk-desktop-bundle-smoke-1"
SMOKE_CHECKS = {"runtime_isolation", "source_api", "prepare", "join_replay", "analysis_run_replay", "sync_word", "cleanup"}


def file_sha256(path):
    value = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(chunk)
    return value.hexdigest()

def framework_copy_filter(framework):
    version = (framework / "Versions/Current").resolve().name
    runtime = Path("Versions") / version / "Resources"

    def ignored(directory, names):
        relative = Path(directory).relative_to(framework)
        entries = {name for name in names if name.endswith(".dSYM") or name == ".DS_Store"}
        if relative == Path("."):
            entries.update(name for name in names if name.endswith(".old"))
        if relative == Path("Versions"):
            entries.update(set(names) - {"Current", version})
        if relative == runtime:
            entries.add("fontconfig")
        if relative == runtime / "bin":
            entries.add("fc-cache")
        if relative == runtime / "library":
            for name in names:
                description = Path(directory) / name / "DESCRIPTION"
                if name == "tcltk" or not description.is_file() or not re.search(
                    r"^Priority: (base|recommended)\s*$", description.read_text(errors="replace"), re.M
                ):
                    entries.add(name)
        if relative == runtime / "modules":
            entries.update({"R_X11.so", "R_de.so"})
        if relative == runtime / "library/grDevices/libs":
            entries.add("cairo.so")
        return entries

    return ignored


def verify_smoke_receipt(resources, recorded=None):
    """Reject host-R runs, incomplete evidence and changed tested source files."""
    resources = Path(resources).resolve()
    report = resources / "bundle-smoke.json"
    value = json.loads(report.read_text())
    if not isinstance(value, dict) or (recorded is not None and not isinstance(recorded, dict)):
        raise ValueError("Malformed bundled R smoke receipt.")
    if recorded is not None and (recorded.get("status") != "passed" or
            recorded.get("report") != report.name or recorded.get("sha256") != file_sha256(report)):
        raise ValueError("Bundled R smoke receipt is missing, incomplete or changed.")
    checks = value.get("checks", {})
    if not isinstance(checks, dict) or not isinstance(value.get("host"), dict):
        raise ValueError("Malformed bundled R smoke checks or host information.")
    if value.get("format") != SMOKE_FORMAT or value.get("status") != "passed" or value.get("execution_scope") != "bundled_r":
        raise ValueError("A successful bundled-R smoke receipt is required; host-R tests do not qualify.")
    if value.get("host", {}).get("sysname") != "Darwin" or value.get("host", {}).get("machine") != "arm64":
        raise ValueError("The bundled R smoke must run on the target Apple Silicon Mac.")
    if not all(isinstance(checks.get(name), dict) and checks[name].get("status") == "passed" for name in SMOKE_CHECKS):
        raise ValueError("The bundled R smoke did not complete every required step.")
    isolation = checks["runtime_isolation"].get("evidence", {})
    if not isinstance(isolation, dict) or not isinstance(isolation.get("library_paths"), list):
        raise ValueError("Malformed runtime-isolation evidence.")
    app = resources.parent.parent
    paths = [isolation.get("r_home", "")] + isolation.get("library_paths", [])
    if isolation.get("verified") is not True or not isolation.get("library_paths") or not all(
            isinstance(path, str) and path and Path(path).resolve().is_relative_to(app) and Path(path).is_dir() for path in paths):
        raise ValueError("The smoke did not verify isolated bundled R and package paths.")
    if Path(value.get("source_root", "")).resolve() != resources / "source":
        raise ValueError("The smoke used another source tree.")
    files = value.get("files", {})
    required = {"source/apps/pk-desktop/R/engine.R", "source/apps/pk-desktop/release/smoke_bundle.R",
                "source/outputs/pk-review/bundles/reviewed-repeated/ADNCA.csv",
                "source/outputs/pk-review/bundles/reviewed-repeated/ADPP.csv", "runtime/bin/Rscript"}
    if not isinstance(files, dict) or not required.issubset(files):
        raise ValueError("The smoke lacks source/input/launcher fingerprints.")
    for name, digest in files.items():
        path = (resources / name).resolve()
        if Path(name).is_absolute() or not path.is_relative_to(resources) or not path.is_file() or not re.fullmatch(r"[a-f0-9]{64}", str(digest)) or file_sha256(path) != digest:
            raise ValueError("A tested file is missing or changed: " + name)
    return value


def validate_runtime(launcher, resources, env, inventory, smoke_timeout=300):
    """Run the three real subprocess gates in order; failure never promotes status."""
    if smoke_timeout <= 0:
        raise ValueError("smoke_timeout must be positive")
    resources = Path(resources)
    inventory_path = resources / "bundle-inventory.json"
    report = resources / "bundle-smoke.json"
    record = {"status": "not_run", "report": report.name, "log": "bundle-smoke.log"}
    inventory.update(status="needs_validation", runtime_isolation="not_run", dependency_check="not_run", runtime_smoke=record)
    def save():
        inventory_path.write_text(json.dumps(inventory, indent=2))
    save()
    step = "runtime_isolation"
    try:
        if report.exists():
            raise ValueError("Smoke evidence already exists; stage a fresh candidate.")
        with (resources / record["log"]).open("w") as log:
            def execute(command, timeout):
                log.write("\nRelease validation: " + step + "\n")
                log.flush()
                subprocess.run(command, cwd=resources, env=env, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=timeout)
            check = 'root <- normalizePath(Sys.getenv("PK_BUNDLE_ROOT")); stopifnot(startsWith(normalizePath(R.home()), paste0(root,"/"))); stopifnot(all(startsWith(normalizePath(.libPaths()),paste0(root,"/"))))'
            execute([str(launcher), "--vanilla", "-e", check], 30)
            inventory["runtime_isolation"] = "passed"
            step = "dependency_check"
            execute([str(launcher), "--vanilla", "source/apps/pk-desktop/install.R", "--check", "--with-parquet"], 120)
            inventory["dependency_check"] = "passed"
            step = "runtime_smoke"
            record["status"] = "running"
            save()
            execute([str(launcher), "--vanilla", "source/apps/pk-desktop/release/smoke_bundle.R",
                     "--source", "source", "--output", report.name, "--bundle-root", "../.."], smoke_timeout)
        verify_smoke_receipt(resources)
        record.update(status="passed", sha256=file_sha256(report))
        inventory.update(status="dependency_check_passed", loader_audit="passed")
        save()
    except (Exception, KeyboardInterrupt) as error:
        if step == "runtime_smoke":
            record["status"] = "failed"
        else:
            inventory[step] = "failed"
        inventory["failure"] = {"step": step, "message": str(error) or type(error).__name__}
        save()
        raise SystemExit("Bundled runtime validation failed at " + step + "; inspect bundle-inventory.json, bundle-smoke.log and bundle-smoke.json (if created).") from error

def stage(args):
    if platform.system() != "Darwin" or platform.machine() != "arm64":
        raise SystemExit("Build and stage on an Apple Silicon Mac.")
    source = Path(__file__).resolve().parents[3]
    built = Path(args.app).resolve()
    out = Path(args.output).resolve()
    if out.exists(): raise SystemExit("Output exists; choose a fresh candidate path.")
    if out.is_relative_to(source): raise SystemExit("Stage outside the source tree.")
    framework = Path(args.framework).resolve()
    library = Path(args.library).resolve()
    native_files = [Path(p).resolve(strict=True) for p in getattr(args, "native_file", ())]
    inputs = [built, framework, library] + [Path(p).resolve() for p in args.native_root] + native_files
    if any(out.is_relative_to(p) for p in inputs):
        raise SystemExit('Stage outside all input directories to avoid recursive copying.')
    if not (built / "Contents/MacOS/pk-desktop").is_file(): raise SystemExit("Fresh Tauri app required.")
    shutil.copytree(built, out, symlinks=True)
    resources = out / "Contents/Resources"
    resources.mkdir(parents=True, exist_ok=True)
    shutil.copytree(source, resources / "source", ignore=shutil.ignore_patterns(
        ".git", ".venv", ".runtime", "target", "outputs", "node_modules", "__pycache__", "*.pyc", "Rplots.pdf"), dirs_exist_ok=True)
    # Ship only the synthetic input bundle, never project histories/user data.
    outputs = resources / "source/outputs"
    if outputs.exists(): shutil.rmtree(outputs)
    shutil.copytree(source / "outputs/pk-review/bundles/reviewed-repeated", outputs / "pk-review/bundles/reviewed-repeated")
    runtime = resources / "runtime"
    runtime.mkdir(exist_ok=True)
    shutil.copytree(framework, runtime / "R.framework", symlinks=True, ignore=framework_copy_filter(framework))
    shutil.copytree(library, runtime / "library", symlinks=False)
    r_shell = runtime / "R.framework/Resources/bin/R"
    shell_text = r_shell.read_text()
    shell_text, changed = re.subn(r'^R_HOME_DIR=.*$',
        'R_HOME_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"',
        shell_text, count=1, flags=re.MULTILINE)
    if changed != 1: raise SystemExit("Unknown R launcher layout; inspect before relocating.")
    for var, subdir in [('R_SHARE_DIR','share'),('R_INCLUDE_DIR','include'),('R_DOC_DIR','doc')]:
        shell_text = re.sub(r'^' + var + r'=.*$', var + '="${R_HOME_DIR}/' + subdir + '"', shell_text, flags=re.MULTILINE)
    r_shell.write_text(shell_text)
    mappings = [(framework, runtime / "R.framework"), (library, runtime / "library")]
    for i, directory in enumerate(args.native_root):
        original = Path(directory).resolve()
        destination = resources / "native" / str(i)
        shutil.copytree(original, destination, symlinks=False)
        mappings.append((original, destination))
    if native_files:
        destination = resources / "native" / "libraries"
        destination.mkdir(parents=True)
        for original in native_files:
            copied = destination / original.name
            if not original.is_file() or copied.exists():
                raise SystemExit("Invalid or duplicate native file: " + str(original))
            shutil.copy2(original, copied)
            mappings.append((original, copied))
    def relocated(path):
        p = Path(path).resolve()
        for original, dest in mappings:
            if p.is_relative_to(original): return dest / p.relative_to(original)
        return None
    unresolved = []
    binaries = []
    for file in out.rglob("*"):
        if file.is_symlink():
            if not file.resolve().is_relative_to(out): unresolved.append({"file":str(file.relative_to(out)),"symlink":str(file.readlink())})
            continue
        if not is_macho(file): continue
        binaries.append(file)
        file.chmod(file.stat().st_mode | 0o200)
        loads = parse_loads(run("otool", "-l", str(file)))
        # Dependencies are rewritten relative to each loader, preserving spaces in paths.
        for dep in loads['dependencies']:
            if not dep.startswith("/") or dep.startswith(("/usr/lib/", "/System/Library/")): continue
            dest = relocated(dep)
            if dest is None or not dest.exists():
                unresolved.append({"file":str(file.relative_to(out)), "dependency":dep})
            else:
                replacement = "@loader_path/" + os.path.relpath(dest, file.parent)
                subprocess.run(["install_name_tool", "-change", dep, replacement, str(file)], check=True)
        for rpath in loads['rpaths']:
            if not rpath.startswith('/') or rpath.startswith(('/usr/lib/', '/System/Library/')): continue
            dest = relocated(rpath)
            if dest is None or not dest.exists():
                unresolved.append({'file': str(file.relative_to(out)), 'rpath': rpath})
            else:
                replacement = '@loader_path/' + os.path.relpath(dest, file.parent)
                subprocess.run(['install_name_tool', '-rpath', rpath, replacement, str(file)], check=True)
        if loads['identity']:
            subprocess.run(['install_name_tool', '-id', '@loader_path/' + file.name, str(file)], check=True)
        subprocess.run(['codesign', '--force', '--sign', '-', str(file)], check=True)
    inventory = {"status":"needs_validation", "unsigned":True,
        "r_framework_source":str(framework), "native_binaries":len(binaries), "unresolved":unresolved}
    (resources / "bundle-inventory.json").write_text(json.dumps(inventory, indent=2))
    if unresolved:
        raise SystemExit("Unresolved external dependencies/symlinks; see bundle-inventory.json. Candidate is not distributable.")
    checked = audit(out)
    (resources / 'loader-audit.json').write_text(json.dumps(checked, indent=2))
    if checked['status'] != 'passed':
        inventory['unresolved'] = checked['unresolved']
        (resources / 'bundle-inventory.json').write_text(json.dumps(inventory, indent=2))
        raise SystemExit('Static loader audit failed; inspect loader-audit.json before distributing.')
    launcher = runtime / "bin/Rscript"; launcher.parent.mkdir(exist_ok=True)
    launcher.write_text('''#!/bin/sh
set -eu
runtime_dir="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
unset R_LIBS R_ARCH R_PROFILE R_ENVIRON R_LD_LIBRARY_PATH R_JAVA_LD_LIBRARY_PATH
unset DYLD_LIBRARY_PATH DYLD_FALLBACK_LIBRARY_PATH DYLD_FRAMEWORK_PATH DYLD_FALLBACK_FRAMEWORK_PATH DYLD_INSERT_LIBRARIES
export R_HOME="$runtime_dir/R.framework/Resources"
export RHOME="$R_HOME"
export R_JAVA_LD_LIBRARY_PATH=
export CALLR_NO_TEMP_DLLS=true
export R_LIBS_SITE="$runtime_dir/library"
export R_LIBS_USER="$runtime_dir/library"
export R_PROFILE_USER=/dev/null
export R_ENVIRON_USER=/dev/null
exec "$R_HOME/bin/Rscript" "$@"
''')
    launcher.chmod(0o755)
    # Run with no external user R profile/library. A real clean-Mac check remains mandatory.
    env = dict(os.environ, PK_REVIEW_ROOT=str(resources / "source"), R_ENVIRON_USER="/dev/null", R_PROFILE_USER="/dev/null")
    for key in list(env):
        if key.startswith('DYLD_') or key in {'R_LIBS', 'R_HOME', 'RHOME'}:
            env.pop(key)
    env['PK_BUNDLE_ROOT'] = str(out)
    validate_runtime(launcher, resources, env, inventory, getattr(args, "smoke_timeout", 300))
    inventory['bytes'] = sum(p.stat().st_size for p in out.rglob('*') if p.is_file())
    (resources / "bundle-inventory.json").write_text(json.dumps(inventory, indent=2))
    print(out)

if __name__ == "__main__":
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument("--app", required=True)
    p.add_argument("--output", required=True)
    p.add_argument("--framework", default="/Library/Frameworks/R.framework")
    p.add_argument("--library", required=True, help="Self-contained, validated package library")
    p.add_argument("--native-root", action="append", default=[])
    p.add_argument("--native-file", action="append", default=[], help="Native library to copy and relocate")
    p.add_argument("--smoke-timeout", type=int, default=300, help="Maximum seconds for the real bundled-R workflow smoke (default: 300)")
    stage(p.parse_args())
