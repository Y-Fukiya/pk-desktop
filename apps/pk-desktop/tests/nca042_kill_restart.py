"""Start a real project, SIGKILL its owner and reopen; never touches user data."""
import os
from pathlib import Path
import signal
import subprocess
import tempfile
import time

source = Path(__file__).resolve().parents[3]
with tempfile.TemporaryDirectory(prefix='pk042-recovery-') as tmp:
    folder = Path(tmp)
    project = folder / 'project'
    project.mkdir()
    ready = folder / 'ready'
    rfile = folder / 'owner.R'
    rfile.write_text('''args <- commandArgs(TRUE)
source("apps/pk-desktop/pkapp.R")
core <- environment(pkapp_open)
e <- core$desktop_nca_engine(args[1]); owner <- core$desktop_acquire_project(args[1])
runtime <- core$desktop_r_runtime(e, 45811, 45812, "nca"); core$desktop_write_r_runtime(e, runtime)
writeLines("ready", args[2]); Sys.sleep(120)
''')
    process = subprocess.Popen(['Rscript', '--vanilla', str(rfile), str(project), str(ready)], cwd=source, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    try:
        deadline = time.monotonic() + 45
        while not ready.exists() and process.poll() is None and time.monotonic() < deadline:
            time.sleep(0.1)
        if not ready.exists():
            process.kill(); output = process.communicate(timeout=5)[0].decode(errors='replace')
            raise RuntimeError('Owner did not start: ' + output)
        process.kill(); process.wait(timeout=5)
        reopen = folder / 'reopen.R'
        reopen.write_text('''args <- commandArgs(TRUE); source("apps/pk-desktop/pkapp.R"); core <- environment(pkapp_open)
e <- core$desktop_nca_engine(args[1]); owner <- core$desktop_acquire_project(args[1])
runtime <- core$desktop_r_runtime(e, 45813, 45814, "nca"); core$desktop_write_r_runtime(e, runtime)
core$desktop_remove_r_runtime(e, runtime); core$desktop_release_project(owner); cat("SIGKILL restart: PASS\\n")
''')
        subprocess.run(['Rscript','--vanilla',str(reopen),str(project)],cwd=source,check=True,timeout=45)
    finally:
        if process.poll() is None:
            process.kill(); process.wait()
