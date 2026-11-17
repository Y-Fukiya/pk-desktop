#!/usr/bin/env python3
"""Build a deterministic, explicitly scoped PK Desktop source distribution."""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import stat
import zipfile

DESKTOP = "apps/pk-desktop"
REVIEW = "apps/pk-review"
TREES = (
    ".github/workflows", f"{DESKTOP}/R", f"{DESKTOP}/ui", f"{DESKTOP}/src-tauri", f"{DESKTOP}/tests",
    f"{DESKTOP}/release", f"{DESKTOP}/examples", f"{DESKTOP}/integrations",
    f"{REVIEW}/R", f"{REVIEW}/integrations/tealflow",
    "outputs/pk-review/bundles/reviewed-repeated",
)
FILES = (
    "LICENSE", "tools/pk_review.py", "tools/make_adnca.R",
    *(f"{DESKTOP}/{name}" for name in (
        "pkapp.R", "launch.R", "install.R", "restore.R", "renv.lock", "diagnose.R",
        "check_macos.R", "check_macos.command", "launch_macos.command", "launch_nca_macos.command", "utf8_locale.sh",
        "PKDesktop.Rproj", "analysis-spec-m0.schema.json", "analysis-spec-v1.schema.json",
        "mcp_server.py", "mcp_config.py", "mcp_register.py", "nca_mcp_contract.json", "requirements-mcp.txt", "build_alpha.py", "RUNTIME_ENVIRONMENT.md")),
    *(f"{REVIEW}/{name}" for name in (
        "app.R", "bootstrap.R", "cli.R", "launch.R", "reproduce.R", "report.Rmd", "reference.docx",
        "mcp_server.py", "client_config.py", "requirements.txt", "start.command", "tests/test_analysis.R")),
)
EXCLUDED_PARTS = {".git", ".runtime", ".venv", "target", "node_modules", "__pycache__", ".pytest_cache"}
EXCLUDED_NAMES = {".DS_Store", "VERIFICATION.md", "ACCEPTANCE_MAC.md"}


def sha256(contents: bytes) -> str:
    return hashlib.sha256(contents).hexdigest()


def selected_files(source: Path) -> list[Path]:
    selected = set()
    for name in FILES:
        path = source / name
        if not path.is_file() or path.is_symlink():
            raise ValueError(f"Required regular file missing: {name}")
        selected.add(path)
    for name in TREES:
        folder = source / name
        if not folder.is_dir() or folder.is_symlink():
            raise ValueError(f"Required regular directory missing: {name}")
        for path in folder.rglob("*"):
            relative = path.relative_to(source)
            if EXCLUDED_PARTS.intersection(relative.parts) or path.name in EXCLUDED_NAMES:
                continue
            if path.is_symlink():
                raise ValueError(f"Selected source contains a symlink: {relative}")
            if path.is_file():
                selected.add(path)
    return sorted(selected, key=lambda p: p.relative_to(source).as_posix())


def check_versions(source: Path, expected: str) -> None:
    engine = (source / DESKTOP / "R/engine.R").read_text()
    engine_versions = re.findall(r'^desktop_version <- "([^"]+)"', engine, re.M)
    cargo = (source / DESKTOP / "src-tauri/Cargo.toml").read_text()
    cargo_versions = re.findall(r'^version = "([^"]+)"', cargo, re.M)
    cargo_lock = (source / DESKTOP / "src-tauri/Cargo.lock").read_text()
    lock_versions = re.findall(r'name = "pk-desktop"\nversion = "([^"]+)"', cargo_lock)
    tauri = json.loads((source / DESKTOP / "src-tauri/tauri.conf.json").read_text())["version"]
    contract = json.loads((source / DESKTOP / "nca_mcp_contract.json").read_text())["app_version"]
    if engine_versions != [expected] or cargo_versions != [expected] or lock_versions != [expected] or tauri != expected or contract != expected:
        raise ValueError(f"Application version metadata does not consistently equal {expected}")
    lock = json.loads((source / DESKTOP / "renv.lock").read_text())
    if lock["Packages"]["PKNCA"]["Version"] != "0.12.1":
        raise ValueError("The validated backend must be PKNCA 0.12.1")


def copy_regular(source: Path, destination: Path) -> None:
    if source.is_symlink() or not source.is_file():
        raise ValueError(f"Not a regular file: {source}")
    destination.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(source, destination)
    mode = 0o755 if source.suffix == ".command" or source.stat().st_mode & 0o111 else 0o644
    destination.chmod(mode)


def stage(source: Path, destination: Path, version: str, release_root: Path | None, includes: list[str]) -> dict:
    if destination.exists():
        raise ValueError(f"Destination already exists: {destination}")
    check_versions(source, version)
    files = selected_files(source)
    extras = []
    for name in includes:
        relative = Path(name)
        if release_root is None or relative.is_absolute() or ".." in relative.parts or len(relative.parts) != 1:
            raise ValueError("--include requires a single top-level release file and --release-root")
        if name in ("PACKAGE_CONTENTS.json", "source") or name in [p.name for p in extras]:
            raise ValueError(f"Reserved or duplicate include: {name}")
        path = release_root / relative
        if path.is_symlink() or not path.is_file():
            raise ValueError(f"Release include is not a regular file: {name}")
        extras.append(path)
    destination.mkdir(parents=True)
    for path in files:
        copy_regular(path, destination / "source" / path.relative_to(source))
    for path in extras:
        copy_regular(path, destination / path.name)
    records = []
    for path in sorted(p for p in destination.rglob("*") if p.is_file()):
        contents = path.read_bytes()
        records.append({"path": path.relative_to(destination).as_posix(), "bytes": len(contents),
                        "sha256": sha256(contents), "mode": oct(stat.S_IMODE(path.stat().st_mode))})
    manifest = {
        "format": "pk-desktop-source-package-1", "version": version,
        "scope": "Full R app, NCA, reports, source adapters, synthetic demo and focused application tests",
        "not_included": ["drug development library", "repository-wide harness", "installed runtimes",
                         "native binaries", "unrelated project changelogs and historical release notes"],
        "validation_scope": "Packaging structure and byte integrity only; scientific/UI acceptance is recorded separately",
        "files": records,
    }
    (destination / "PACKAGE_CONTENTS.json").write_text(json.dumps(manifest, indent=2, ensure_ascii=False) + "\n")
    (destination / "PACKAGE_CONTENTS.json").chmod(0o644)
    return manifest


def archive(destination: Path, output: Path) -> str:
    if output.exists():
        raise ValueError(f"Archive already exists: {output}")
    output.parent.mkdir(parents=True, exist_ok=True)
    paths = sorted(p for p in destination.rglob("*") if p.is_file())
    with zipfile.ZipFile(output, "x", compression=zipfile.ZIP_DEFLATED, compresslevel=9) as z:
        for path in paths:
            entry = zipfile.ZipInfo(f"{destination.name}/{path.relative_to(destination).as_posix()}", (2026, 1, 1, 0, 0, 0))
            entry.create_system = 3
            entry.compress_type = zipfile.ZIP_DEFLATED
            entry.external_attr = (stat.S_IFREG | stat.S_IMODE(path.stat().st_mode)) << 16
            z.writestr(entry, path.read_bytes(), compresslevel=9)
    with zipfile.ZipFile(output) as z:
        if z.testzip() is not None or len(z.infolist()) != len(paths):
            raise ValueError("ZIP verification failed")
        for path in paths:
            name = f"{destination.name}/{path.relative_to(destination).as_posix()}"
            if z.read(name) != path.read_bytes() or stat.S_IMODE(z.getinfo(name).external_attr >> 16) != stat.S_IMODE(path.stat().st_mode):
                raise ValueError(f"ZIP content/mode differs: {name}")
    return sha256(output.read_bytes())


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", required=True, type=Path)
    parser.add_argument("--destination", required=True, type=Path)
    parser.add_argument("--archive", required=True, type=Path)
    parser.add_argument("--version", required=True)
    parser.add_argument("--release-root", type=Path)
    parser.add_argument("--include", action="append", default=[])
    args = parser.parse_args()
    source = args.source.resolve(strict=True)
    destination = args.destination.resolve()
    output = args.archive.resolve()
    if output.exists() or output.is_relative_to(destination):
        parser.error("Archive must be a new file outside the staging directory")
    if destination.is_relative_to(source) or source.is_relative_to(destination):
        parser.error("Source and staging directory must not contain each other")
    manifest = stage(source, destination, args.version, args.release_root, args.include)
    digest = archive(destination, output)
    print(json.dumps({"archive": str(output), "sha256": digest, "payload_files": len(manifest["files"]), "manifest": str(destination / "PACKAGE_CONTENTS.json")}))


if __name__ == "__main__":
    main()
