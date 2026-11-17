"""Independent Python arithmetic against the real harness run's unrounded R CSV."""
import csv
import json
import math
from pathlib import Path
import statistics

root = Path(__file__).resolve().parents[3]
check = json.loads((root / "outputs/pk-desktop/validation-20261104/validation.json").read_text())
project = Path(check["project"])
spec = json.loads((project / "runs" / check["api_run"] / "spec.json").read_text())
groups = {}
with (Path(check["input_bundle"]) / "ADPP.csv").open(newline="") as handle:
    for row in csv.DictReader(handle):
        if row["PARAMCD"] not in spec["param_summary"]["paramcd"] or row.get("ANL01FL", "Y") != "Y":
            continue
        groups.setdefault((row["SEX"], row["PARAMCD"], row["AVALU"]), []).append(float(row["AVAL"]) if row["AVAL"] else None)
cells = 0
with (project / "runs" / check["api_run"] / "tables/parameter_summary.csv").open(newline="") as handle:
    for row in csv.DictReader(handle):
        raw = groups[(row[".GROUP"], row["PARAMCD"], row["AVALU"])]
        x = [v for v in raw if v is not None]
        pos = [v for v in x if v > 0]
        expected = dict(n=len(x), missing=len(raw)-len(x), n_positive=len(pos), mean=statistics.mean(x),
            sd=statistics.stdev(x), cv_percent=100*statistics.stdev(x)/statistics.mean(x),
            median=statistics.median(x), min=min(x), max=max(x),
            geometric_mean=math.exp(statistics.mean(map(math.log,pos))),
            geometric_cv_percent=100*math.sqrt(math.expm1(statistics.variance(map(math.log,pos)))))
        for key, value in expected.items():
            assert math.isclose(float(row[key]), value, rel_tol=1e-12, abs_tol=1e-14), (row["PARAMCD"], key, row[key], value)
            cells += 1
print(f"Independent Python statistics: PASS ({cells} unrounded numeric cells, rtol=1e-12, atol=1e-14)")
