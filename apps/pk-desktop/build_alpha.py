"""Assemble a workspace-dependent macOS alpha .app from a built Tauri binary."""
import argparse
import json
from pathlib import Path
import plistlib
import shutil

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--binary", required=True, type=Path)
parser.add_argument("--out", required=True, type=Path)
args = parser.parse_args()
if not args.binary.is_file():
    raise SystemExit("Build the Tauri binary first")
app = args.out / "PKDesktop-alpha.app"
if app.exists():
    raise SystemExit("Destination exists; choose a new output directory")
macos = app / "Contents/MacOS"
macos.mkdir(parents=True)
binary = macos / "pk-desktop"
shutil.copy2(args.binary, binary)
binary.chmod(0o755)
version = json.loads((Path(__file__).parent / "src-tauri/tauri.conf.json").read_text())["version"]
info = dict(CFBundleName="PK Desktop alpha", CFBundleDisplayName="PK Desktop alpha", CFBundleExecutable="pk-desktop",
            CFBundleIdentifier="org.pkdummy.pkdesktop", CFBundleVersion=version, CFBundleShortVersionString=version,
            CFBundlePackageType="APPL", LSMinimumSystemVersion="13.0", NSHighResolutionCapable=True)
with (app / "Contents/Info.plist").open("wb") as handle:
    plistlib.dump(info, handle)
print(app.resolve())
