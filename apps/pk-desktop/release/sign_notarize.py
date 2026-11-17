"""Sign nested Mach-O files, submit to Apple's notary service, staple and verify.
Credentials are referenced through a Keychain profile; never passed in source files.
"""
import argparse
import json
from pathlib import Path
import platform
import subprocess
import tempfile
from audit_macos import audit, is_macho
from stage_macos import verify_smoke_receipt

def main(args):
    if platform.system() != "Darwin": raise SystemExit("macOS is required.")
    app=Path(args.app).resolve()
    receipt=Path(args.receipt).resolve()
    if receipt.is_relative_to(app) or receipt.exists():
        raise SystemExit('Use a new receipt path outside the signed app.')
    inventory=json.loads((app/'Contents/Resources/bundle-inventory.json').read_text())
    if inventory['status']!='dependency_check_passed' or inventory['unresolved']:
        raise SystemExit('Stage and validate bundled dependencies first.')
    try:
        verify_smoke_receipt(app / 'Contents/Resources', inventory.get('runtime_smoke', {}))
    except (ValueError, OSError, TypeError, KeyError) as error:
        raise SystemExit('Stage and pass the bundled R workflow smoke before signing: ' + str(error)) from error
    checked = audit(app)
    if checked['status'] != 'passed':
        raise SystemExit('Current candidate fails the loader audit; stage again before signing.')
    if not args.identity.startswith('Developer ID Application:'):
        raise SystemExit('Use a Developer ID Application identity.')
    entitlements=Path(__file__).with_name('entitlements.plist')
    def sign(path):
        subprocess.run(['codesign','--force','--timestamp','--options','runtime','--entitlements',str(entitlements),'--sign',args.identity,str(path)],check=True)
    for file in sorted(app.rglob('*'),key=lambda p:len(p.parts),reverse=True):
        if is_macho(file): sign(file)
    for framework in sorted(app.rglob('*.framework'), key=lambda p:len(p.parts), reverse=True): sign(framework)
    sign(app)
    subprocess.run(['codesign','--verify','--deep','--strict',str(app)],check=True)
    with tempfile.TemporaryDirectory() as temp:
        archive=Path(temp)/'PKDesktop.zip'
        subprocess.run(['ditto','-c','-k','--keepParent',str(app),str(archive)],check=True)
        result=subprocess.check_output(['xcrun','notarytool','submit',str(archive),'--keychain-profile',args.keychain_profile,'--wait','--output-format','json'],text=True)
        report=json.loads(result)
        receipt.write_text(json.dumps(report,indent=2))
        if report.get('status')!='Accepted': raise SystemExit('Notarization not accepted; inspect receipt.')
    subprocess.run(['xcrun','stapler','staple',str(app)],check=True)
    subprocess.run(['xcrun','stapler','validate',str(app)],check=True)
    subprocess.run(['spctl','--assess','--type','execute','--verbose',str(app)],check=True)

if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--app',required=True);p.add_argument('--identity',required=True)
    p.add_argument('--keychain-profile',required=True);p.add_argument('--receipt',required=True)
    main(p.parse_args())
