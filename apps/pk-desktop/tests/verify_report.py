"""Independently compare every displayed ADPP statistic in the sample Word to its frozen CSV."""
import argparse
import csv
import hashlib
import json
import math
from pathlib import Path
import xml.etree.ElementTree as ET
import zipfile

root = Path(__file__).resolve().parents[3]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--batch-result", type=Path, help="JSON stdout saved from launch.R --mode batch")
args = parser.parse_args()
if args.batch_result:
    result = json.loads(args.batch_result.read_text())
    validation = {"project": result["project"], "run": result["run_id"], "job": result["report"]}
else:
    validation = json.loads((root / "outputs/pk-desktop/report-validation-20261105/report_validation.json").read_text())
job = validation["job"]
run = Path(validation["project"]) / "runs" / validation["run"]
spec = json.loads((run / "spec.json").read_text())
with (run / "tables/parameter_summary.csv").open(newline="") as f:
    rows = {(r["PARAMCD"], r["AVALU"], r[".GROUP"]): r for r in csv.DictReader(f)}
metrics = {"N (nonmissing)": "n", "Missing": "missing", "N positive": "n_positive", "Arithmetic mean": "mean",
           "Sample SD": "sd", "Arithmetic CV (%)": "cv_percent", "Geometric mean": "geometric_mean",
           "Geometric CV (%)": "geometric_cv_percent", "Median": "median", "Minimum": "min",
           "Maximum": "max", "5th percentile": "p05", "95th percentile": "p95"}
ns = {"w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main"}
text = lambda e: "".join(t.text or "" for t in e.findall(".//w:t", ns))
checked = set()
with zipfile.ZipFile(job["artifacts"]["docx"]) as archive:
    doc = ET.fromstring(archive.read("word/document.xml"))
    previous = ""
    for element in doc.find("w:body", ns):
        if element.tag.endswith("}p"):
            previous = text(element)
        elif element.tag.endswith("}tbl"):
            table = [[text(cell) for cell in row.findall("w:tc", ns)] for row in element.findall("w:tr", ns)]
            if table[0][0] != "Statistic":
                continue
            code, unit = previous.split(" | ", 1)
            groups = [g.removeprefix("Group: ") for g in table[0][1:]]
            for record in table[1:]:
                metric = metrics[record[0]]
                for group, cell in zip(groups, record[1:], strict=True):
                    raw = rows[code, unit, group][metric]
                    key = (code, unit, group, metric)
                    assert key not in checked
                    if not raw:
                        assert cell == "NE", (key, cell)
                    else:
                        value = float(raw)
                        # Python's independent significant-digit formatting; counts must be exact.
                        expected = value if metric in ("n", "missing", "n_positive") else float(f"{value:.{spec['report']['digits']}g}")
                        assert math.isclose(float(cell), expected, rel_tol=1e-13, abs_tol=0), (key, cell, expected)
                    checked.add(key)
    image_hash = hashlib.sha256((run / "plots/concentration.png").read_bytes()).hexdigest()
    assert image_hash in {hashlib.sha256(archive.read(n)).hexdigest() for n in archive.namelist() if n.startswith("word/media/")}
assert len(checked) == len(rows) * len(metrics)
report = Path(job["artifacts"]["directory"])
manifest = json.loads((report / "REPORT_MANIFEST.json").read_text())
for name, expected in manifest["outputs"].items():
    assert hashlib.sha256((report / name).read_bytes()).hexdigest() == expected, name
assert manifest["analysis_hash"] == job["analysis_hash"]
print(f"Independent Word verification: PASS ({len(checked)} ADPP cells, frozen PNG and {len(manifest['outputs'])} artifact hashes)")
