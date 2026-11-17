"""PK review bundles and reproducible views shared by the CLI and MCP server.

ADNCA contains concentrations. ADPP contains precomputed PK parameters.
The upstream legacy generator is isolated in a temporary directory; it never
writes a parameter dataset named ADNCA into a review bundle.
"""
from __future__ import annotations

import argparse
import contextlib
import csv
import errno
from collections import Counter
from datetime import datetime, timezone
from decimal import Decimal, InvalidOperation
import getpass
import hashlib
import io
import json
import math
import os
import re
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
from typing import Any
import uuid
import urllib.request
import yaml

ROOT = Path(__file__).resolve().parents[1]
APP = ROOT / "apps" / "pk-review"
GROUPS = ["ARM", "SEX", "DOSE_MG", "ROUTE"]


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read_csv(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    return _parse_csv(path.read_bytes(), path.name)


def _parse_csv(contents: bytes, name: str) -> tuple[list[str], list[dict[str, str]]]:
    with io.StringIO(contents.decode("utf-8-sig"), newline="") as handle:
        validate_csv_quotes(handle.read())
        handle.seek(0)
        reader = csv.reader(handle, strict=True)
        columns = next(reader, [])
        if not columns or any(not c.strip() or c != c.strip() for c in columns) or len(set(columns)) != len(columns):
            raise ValueError(f"Invalid or duplicate CSV header: {name}")
        rows = []
        for values in reader:
            if len(values) != len(columns):
                raise ValueError(f"CSV field count differs at record ending on line {reader.line_num}: {name}")
            rows.append(dict(zip(columns, values)))
        return columns, rows


class _InputSnapshot:
    """Parse and fingerprint the same bytes, including repeated auxiliary reads."""

    def __init__(self):
        self.contents: dict[Path, bytes] = {}

    def read_bytes(self, path: Path) -> bytes:
        if path not in self.contents:
            self.contents[path] = path.read_bytes()
        return self.contents[path]

    def exists(self, path: Path) -> bool:
        return path in self.contents or path.exists()

    def read_csv(self, path: Path) -> tuple[list[str], list[dict[str, str]]]:
        return _parse_csv(self.read_bytes(path), path.name)

    def manifest(self) -> list[dict[str, str]]:
        return [{"path": str(path), "sha256": hashlib.sha256(contents).hexdigest()}
                for path, contents in self.contents.items()]


def validate_csv_quotes(text: str) -> None:
    """Require quotes to enclose whole fields before either CSV reader sees them."""
    state = "start"
    for char in text:
        if state == "quoted":
            if char == '"':
                state = "closed"
        elif state == "closed":
            if char == '"':
                state = "quoted"
            elif char in ",\r\n":
                state = "start"
            else:
                raise ValueError("Invalid CSV quoting: characters after a closing quote")
        elif char == '"':
            if state != "start":
                raise ValueError("Invalid CSV quoting: quote inside an unquoted field")
            state = "quoted"
        elif char in ",\r\n":
            state = "start"
        else:
            state = "bare"
    if state == "quoted":
        raise ValueError("Invalid CSV quoting: unterminated quoted field")


def write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError(f"No records for {path.name}")
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def write_json(path: Path, value: Any) -> None:
    with tempfile.NamedTemporaryFile("w", dir=path.parent, delete=False, encoding="utf-8") as handle:
        temp = Path(handle.name)
        json.dump(value, handle, ensure_ascii=False, indent=2, allow_nan=False)
        handle.write("\n")
    try:
        os.replace(temp, path)
    finally:
        temp.unlink(missing_ok=True)


@contextlib.contextmanager
def view_lock(path: Path):
    lock = Path(str(path) + ".lock")
    if lock.is_dir():
        raise ValueError("Legacy directory lock found. Stop all old review processes before removing this directory: " + str(lock))
    # Keep the file: unlinking it would let another process lock a different inode.
    with lock.open("a+b") as handle:
        if os.name == "nt":
            import msvcrt
            if handle.tell() == 0:
                handle.write(b"\0")
                handle.flush()
            handle.seek(0)
            acquire = lambda: msvcrt.locking(handle.fileno(), msvcrt.LK_NBLCK, 1)
            release = lambda: msvcrt.locking(handle.fileno(), msvcrt.LK_UNLCK, 1)
        else:
            import fcntl
            acquire = lambda: fcntl.flock(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
            release = lambda: fcntl.flock(handle.fileno(), fcntl.LOCK_UN)
        try:
            acquire()
        except OSError as exc:
            if exc.errno in {errno.EACCES, errno.EAGAIN}:
                raise ValueError("View is being changed. Reload and retry.") from exc
            raise
        try:
            yield
        finally:
            release()


def r_environment() -> dict[str, str]:
    env = os.environ.copy()
    lib = ROOT / ".runtime" / "R-library"
    if lib.is_dir():
        env["R_LIBS_USER"] = str(lib) + (os.pathsep + env["R_LIBS_USER"] if env.get("R_LIBS_USER") else "")
    env.setdefault("R_CACHE_ROOTPATH", str(ROOT / ".runtime" / "r-cache"))
    env["PK_REVIEW_ROOT"] = str(ROOT)
    env["PK_REVIEW_PYTHON"] = sys.executable
    return env


def run_r(arguments: list[str], timeout: int = 180) -> str:
    result = subprocess.run(
        [os.environ.get("RSCRIPT", "Rscript"), *arguments], cwd=ROOT,
        env=r_environment(), capture_output=True, text=True, timeout=timeout,
    )
    if result.returncode:
        raise ValueError((result.stderr + "\n" + result.stdout)[-8000:])
    return result.stdout


def checked_path(path: str | Path, root: Path, *, exists: bool = True) -> Path:
    candidate = Path(path)
    resolved = (root / candidate).resolve() if not candidate.is_absolute() else candidate.resolve()
    if not resolved.is_relative_to(root.resolve()):
        raise ValueError(f"Path must be inside the workspace: {root}")
    if exists and not resolved.exists():
        raise ValueError(f"Path does not exist: {resolved}")
    return resolved


def dose_key(value: str, name: str = "DOSE_MG") -> str:
    """Canonical decimal dose identity without rounding its coefficient."""
    token = str(value).strip(" \t\r\n")
    if not re.fullmatch(r"[+]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?", token):
        raise ValueError(f"Invalid nonnegative decimal {name}: {value!r}")
    number = Decimal(token)
    if not math.isfinite(float(number)) or (number != 0 and float(number) == 0):
        raise ValueError(f"{name} is outside the supported numeric range")
    if number == 0:
        return "0"
    _, coefficient, exponent = number.as_tuple()
    digits = "".join(map(str, coefficient))
    significant = digits.rstrip("0")
    power = exponent + len(digits) - len(significant)
    position = len(significant) + power
    if 0 < position <= 21:
        return (significant + "0" * power if power >= 0 else
                significant[:position] + "." + significant[position:])
    if -5 <= position <= 0:
        return "0." + "0" * -position + significant
    mantissa = significant[0] + ("." + significant[1:] if len(significant) > 1 else "")
    return f"{mantissa}e{position - 1}"


def _attribute_missing(value: str) -> bool:
    return value.strip(" \t\r\n") in {"", "NA"}


def _attribute_key(value: str, name: str):
    return dose_key(value, name) if name in {"DOSE_MG", "AGE", "WT"} else value


def _subject_covariates(rows: list[dict[str, str]]) -> dict[str, dict[str, str]]:
    by_subject: dict[str, dict[str, str]] = {}
    for row in rows:
        subject = row.get("USUBJID", "")
        if not subject:
            raise ValueError("USUBJID must not be blank")
        current = by_subject.setdefault(subject, {})
        for name in ["STUDYID", *GROUPS, "AGE", "WT"]:
            value = row.get(name, "")
            if _attribute_missing(value):
                continue
            identity = _attribute_key(value, name)
            previous = _attribute_key(current[name], name) if name in current else None
            if name in current and previous != identity:
                raise ValueError(f"{name} changes within {subject}; split this run into distinct profiles")
            current.setdefault(name, value)
    for row in rows:
        for name in by_subject[row["USUBJID"]]:
            if _attribute_missing(row.get(name, "")):
                raise ValueError(f"Partially missing subject attribute {name} for {row['USUBJID']}")
    return by_subject


def flag_value(value: str, name: str) -> bool:
    token = str(value).strip(" \t\r\n").upper()
    if token in {"Y", "TRUE"}:
        return True
    if token in {"N", "FALSE"}:
        return False
    try:
        if not re.fullmatch(r"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:E[+-]?[0-9]+)?", token):
            raise InvalidOperation
        number = Decimal(token)
        if number.is_finite() and number in {0, 1}:
            return bool(number)
    except InvalidOperation:
        pass
    raise ValueError(f"Missing or invalid {name} flag: {value!r}")


def normalise_flags(rows: list[dict[str, str]]) -> None:
    for row in rows:
        for name in ["MDV", "BLQ", "SSNCAFL", "TROUGHFL", "ANL01FL"]:
            if name in row:
                yes = flag_value(row[name], name)
                row[name] = ("1" if yes else "0") if name in {"MDV", "BLQ"} else ("Y" if yes else "N")


def _add_repeated_labels(rows: list[dict[str, str]], source: Path, snapshot: _InputSnapshot | None = None) -> list[Path]:
    snapshot = snapshot or _InputSnapshot()
    used = []
    time_map: dict[tuple[str, float], dict[str, str]] = {}
    for filename, flag, time in [("NCA_SS_INPUT.csv", "SSNCAFL", "ABS_TIME_H"), ("TROUGH_INPUT.csv", "TROUGHFL", "TIME_H")]:
        path = source / filename
        if not snapshot.exists(path):
            continue
        used.append(path)
        _, records = snapshot.read_csv(path)
        normalise_flags(records)
        concentration_keys = Counter((row["USUBJID"], float(row["TIME_H"])) for row in rows)
        seen = set()
        for record in records:
            key = (record["USUBJID"], float(record[time]))
            if key in seen:
                raise ValueError(f"Duplicate time in {filename}: {key}")
            seen.add(key)
            labels = time_map.setdefault(key, {})
            if flag not in record:
                raise ValueError(f"{filename} requires {flag}")
            if record[flag] == "Y" and concentration_keys[key] != 1:
                raise ValueError(f"Selected {flag} record in {filename} must match exactly one concentration record: {key}")
            labels[flag] = record[flag]
            if flag == "SSNCAFL" and record[flag] == "Y":
                labels["TAD_H"] = record["TAD_H"]
    if used:
        for row in rows:
            labels = time_map.get((row["USUBJID"], float(row["TIME_H"])), {})
            for key, value in labels.items():
                if row.get(key, ""):
                    equal = Decimal(row[key]) == Decimal(value) if key == "TAD_H" else row[key] == value
                    if not equal:
                        raise ValueError(f"Conflicting {key} between ADNCA and auxiliary input")
            row.setdefault("TAD_H", "")
            row.setdefault("SSNCAFL", "N")
            row.setdefault("TROUGHFL", "N")
            row.update(labels)
    return used


def collect_quality_checks(source: Path, snapshot: _InputSnapshot | None = None) -> list[dict[str, str]]:
    snapshot = snapshot or _InputSnapshot()
    path = source / "TROUGH_SUMMARY.csv"
    if not snapshot.exists(path):
        return []
    _, records = snapshot.read_csv(path)
    specification = source / "REPEATED_NCA_MANIFEST.yml"
    rule = "Upstream reported verdict; rule and threshold not provided"
    if snapshot.exists(specification):
        spec = yaml.safe_load(snapshot.read_bytes(specification))
        if not isinstance(spec, dict) or not isinstance(spec.get("settings", {}), dict):
            raise ValueError("Invalid repeated NCA specification")
        threshold = spec.get("settings", {}).get("stable_rel_threshold")
        if threshold is not None:
            rule = f"Harness IPRED check: abs(C144-C120)/C144 <= {threshold}; clinical steady state is not established by this flag alone"
    checks = []
    for record in records:
        if "PRED_TROUGH_STABLE_QC" not in record:
            continue
        status = record["PRED_TROUGH_STABLE_QC"].strip().upper()
        if status not in {"PASS", "FAIL"}:
            raise ValueError("Invalid PRED_TROUGH_STABLE_QC")
        checks.append({"USUBJID": record["USUBJID"], "check": "PRED_TROUGH_STABLE_QC", "status": status,
                       "relative_change": record.get("PRED_TROUGH_REL_CHANGE_120_144", ""),
                       "rule": rule})
    return checks


def label_generated_auc(parameters: list[dict[str, str]], rows: list[dict[str, str]]) -> list[str]:
    """Preserve legacy values but name their actual integration interval explicitly."""
    observed: dict[str, list[float]] = {}
    positive: dict[str, list[float]] = {}
    for row in rows:
        if row.get("MDV", "0") == "0" and row.get("BLQ", "0") == "0":
            observed.setdefault(row["USUBJID"], []).append(float(row["TIME_H"]))
            if float(row["AVAL"]) > 0:
                positive.setdefault(row["USUBJID"], []).append(float(row["TIME_H"]))
    renamed = set()
    for record in parameters:
        if record.get("SOURCE_MODE") != "single":
            continue
        times = observed[record["USUBJID"]]
        start, end = min(times), max(times)
        # Compare original observation times, not the legacy CSV's rounded TLAST.
        if max(positive.get(record["USUBJID"], [end])) != end:
            raise ValueError("Automatic single-dose NCA has included terminal zero concentrations: "
                             "the integration endpoint differs from TLAST. Provide ADPP calculated "
                             "under an explicit terminal-zero policy; no bundle was saved.")
        if start < 0:
            raise ValueError("Automatic single-dose NCA requires nonnegative times")
        record["NCA_START_H"], record["NCA_END_H"] = str(start), str(end)
        if start != 0 and record["PARAMCD"] in {"AUC0TL", "AUC0INF"}:
            record["PARAMCD"] = {"AUC0TL": "AUCFSTL", "AUC0INF": "AUCFSTIF"}[record["PARAMCD"]]
            record["PARAM"] = (f"AUC from first included time ({start:g} h) to " +
                               (f"last included time ({end:g} h)" if record["PARAMCD"] == "AUCFSTL" else "infinity (terminal extrapolation)"))
            renamed.add(record["USUBJID"])
    return ([f"AUC starts after time zero for {len(renamed)} subjects. AUCFSTL/AUCFSTIF preserve the actual interval; no baseline was imputed."] if renamed else [])


def prepare_bundle(source: Path, destination: Path) -> dict[str, Any]:
    """Import canonical data, or adapt a harness analysis_inputs directory."""
    source = source.resolve()
    if (source / "analysis_inputs").is_dir():
        source /= "analysis_inputs"
    if destination.exists():
        raise ValueError("Destination already exists; use a new bundle directory")
    concentration = source / "ADNCA.csv"
    if not concentration.exists():
        concentration = source / "ADPC.csv"
    if not concentration.exists():
        raise ValueError("Provide ADNCA.csv concentrations or harness ADPC.csv")
    snapshot = _InputSnapshot()
    columns, rows = snapshot.read_csv(concentration)
    if not {"USUBJID", "TIME_H", "AVAL", "AVALU"}.issubset(columns):
        raise ValueError("ADNCA must be concentration records with USUBJID, TIME_H, AVAL, AVALU. PK parameters belong in ADPP.csv.")
    normalise_flags(rows)
    covariance = _subject_covariates(rows)
    _add_repeated_labels(rows, source, snapshot)
    warnings = []
    quality_checks = collect_quality_checks(source, snapshot)
    qc_subjects = [record["USUBJID"] for record in quality_checks]
    if len(qc_subjects) != len(set(qc_subjects)) or not set(qc_subjects).issubset(covariance):
        raise ValueError("QC subjects must be unique and present in ADNCA")
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=".pk-bundle-", dir=destination.parent) as temporary:
        stage = Path(temporary)
        parameter = source / "ADPP.csv"
        if snapshot.exists(parameter):
            _, parameters = snapshot.read_csv(parameter)
            normalise_flags(parameters)
            _subject_covariates(parameters)
            method = "Imported ADPP; no PK recalculation"
        else:
            if concentration.name != "ADPC.csv":
                raise ValueError("Canonical ADNCA requires an accompanying ADPP.csv; no automatic PK recalculation")
            labelled_repeated = any(row.get(flag) == "Y" for row in rows for flag in ("SSNCAFL", "TROUGHFL"))
            summaries_available = all(snapshot.exists(source / name) for name in ("NCA_SS_SUMMARY.csv", "TROUGH_SUMMARY.csv"))
            if labelled_repeated and not summaries_available:
                raise ValueError("Repeated-dose artifacts are incomplete for labelled concentrations. "
                                 "Automatic single-dose NCA is not permitted with SSNCAFL=Y or TROUGHFL=Y; "
                                 "provide ADPP or complete repeated-dose auxiliary inputs.")
            units = {row["AVALU"] for row in rows}
            if len(units) != 1 or not next(iter(units)).strip():
                raise ValueError("Automatic NCA requires a single concentration unit; explicit conversion is required")
            # Only calculation observations go to the legacy calculator/temporary plots.
            # All original rows remain in the review ADNCA, including excluded missing values.
            validated = stage / "validated-inputs"
            validated.mkdir()
            included = [row for row in rows if row.get("MDV", "0") == "0" and row.get("BLQ", "0") == "0"]
            legacy_inputs = {"ADPC.csv": included}
            for filename in ["NCA_SS_INPUT.csv", "TROUGH_INPUT.csv", "NCA_SS_SUMMARY.csv", "TROUGH_SUMMARY.csv"]:
                p = source / filename
                if snapshot.exists(p):
                    _, records = snapshot.read_csv(p)
                    normalise_flags(records)
                    for record in records:
                        if record.get("CONC_UNIT") and record["CONC_UNIT"] not in units:
                            raise ValueError(f"Concentration unit differs in {filename}")
                        if record.get("AUC_UNIT") and record["AUC_UNIT"].replace("*h/", "/") not in units:
                            raise ValueError(f"AUC unit differs from the concentration unit in {filename}")
                    legacy_inputs[filename] = records
            for filename, records in legacy_inputs.items():
                if any(not record.get("USUBJID", "").strip() for record in records):
                    raise ValueError(f"{filename} requires nonblank USUBJID values")
            # Type inference in the legacy reader must never change an opaque ID.
            subjects = sorted({record["USUBJID"] for records in legacy_inputs.values() for record in records})
            aliases = {subject: f"PKREVIEW_SUBJECT_{i:012d}" for i, subject in enumerate(subjects)}
            original_ids = {alias: subject for subject, alias in aliases.items()}
            for filename, records in legacy_inputs.items():
                write_csv(validated / filename, [dict(record, USUBJID=aliases[record["USUBJID"]]) for record in records])
            legacy = stage / "legacy"
            run_r([str(APP / "cli.R"), "validate-legacy-times", str(validated / "ADPC.csv")])
            run_r([str(ROOT / "tools/make_adnca.R"), "--analysis-dir", str(validated), "--out-dir", str(legacy)])
            _, parameters = read_csv(legacy / "ADNCA.csv")
            for record in parameters:
                subject = original_ids.get(record["USUBJID"])
                if subject is None or subject not in covariance:
                    raise ValueError("Automatic NCA returned a subject absent from ADNCA")
                record["USUBJID"] = subject
            warnings = label_generated_auc(parameters, rows)
            method = "Legacy NCA values preserved; ADPP names and NCA_START_H/NCA_END_H identify the actual interval"
            shutil.rmtree(legacy)
            shutil.rmtree(validated)
        cov_columns = [name for name in ["STUDYID", *GROUPS, "AGE", "WT"] if any(name in cov for cov in covariance.values())]
        for record in parameters:
            cov = covariance.get(record.get("USUBJID", ""))
            if cov is None:
                raise ValueError("ADPP contains a subject absent from ADNCA")
            for name in cov_columns:
                value = cov.get(name, "")
                if name in record and _attribute_missing(record[name]) and value:
                    raise ValueError(f"Partially missing subject attribute {name} in ADPP")
                if record.get(name) and value and str(record[name]) != str(value):
                    if _attribute_key(record[name], name) != _attribute_key(value, name):
                        raise ValueError(f"Conflicting {name} between ADNCA and ADPP")
                record.setdefault(name, value)
        write_csv(stage / "ADNCA.csv", rows)
        write_csv(stage / "ADPP.csv", parameters)
        for manifest in [source / "MANIFEST.yml", source.parent / "MANIFEST.yml", source / "REPEATED_NCA_MANIFEST.yml"]:
            if snapshot.exists(manifest):
                snapshot.read_bytes(manifest)
        manifest = {
            "version": 2, "run_id": destination.name,
            "datasets": {"ADNCA": "Concentration records", "ADPP": "Precomputed PK parameter records"},
            "parameter_source": method,
            "warnings": warnings, "quality_checks": quality_checks,
            "import_code_hashes": {str(p.relative_to(ROOT)): sha256(p) for p in [ROOT / "tools/pk_review.py", ROOT / "tools/make_adnca.R"]},
            "source_files": snapshot.manifest(),
            "files": {name: sha256(stage / name) for name in ["ADNCA.csv", "ADPP.csv"]},
        }
        write_json(stage / "BUNDLE.json", manifest)
        run_r([str(APP / "cli.R"), "inspect", str(stage)])
        os.rename(stage, destination)
    return inspect_bundle(destination)


def inspect_bundle(bundle: Path) -> dict[str, Any]:
    return json.loads(run_r([str(APP / "cli.R"), "inspect", str(bundle)]))


def default_config(bundle: Path, title: str | None = None) -> dict[str, Any]:
    info = inspect_bundle(bundle)
    preferred = ["AUC0TL", "AUCFSTL", "CMAX", "TMAX", "AUCTAUSS", "CMAXSS", "TMAXSS"]
    eligible = info["eligible_parameters"]
    if not eligible:
        raise ValueError("No eligible ADPP parameters remain after ANL01FL exclusion")
    selected = [p for p in preferred if p in eligible] or eligible[:3]
    return {
        "version": 1, "revision": 1, "title": title or "PK simulation review",
        "bundle": str(bundle.resolve()), "input_hashes": info["input_hashes"],
        "profile": "all", "x": "TIME_H", "scale": "linear",
        "statistic": "mean", "interval": "sd", "show_individual": True,
        "group_by": "NONE", "subjects": [], "parameters": selected,
        "include_mdv": False, "blq": "exclude", "digits": 3, "include_listings": False,
    }


def create_view(bundle: Path, path: Path, title: str | None = None) -> dict[str, Any]:
    if path.exists():
        raise ValueError("View exists; use update_view with its revision")
    path.parent.mkdir(parents=True, exist_ok=True)
    config = default_config(bundle, title)
    config["audit"] = [audit_event(None, config, getpass.getuser(), "Create view", "create")]
    with tempfile.TemporaryDirectory(prefix=".pk-config-", dir=path.parent) as tmp:
        candidate = Path(tmp) / "view.json"
        write_json(candidate, config)
        run_r([str(APP / "cli.R"), "validate", str(candidate)])
    with view_lock(path):
        if path.exists():
            raise ValueError("View already exists")
        write_json(path, config)
    return {"config_path": str(path), "config": config}


def read_view(path: Path) -> dict[str, Any]:
    config = json.loads(path.read_text(encoding="utf-8"))
    history = config.get("audit", [])
    previous = None
    for event in history:
        if not all(event.get(k) for k in ["actor", "reason", "at_utc", "channel"]):
            raise ValueError("Incomplete view change history")
        if previous is not None and (event["before"] != previous or event["after"]["revision"] != previous["revision"] + 1):
            raise ValueError("Inconsistent view change history")
        previous = event["after"]
    if history and previous != view_base(config):
        raise ValueError("Settings do not match the recorded change history")
    return config


def view_base(config: dict[str, Any]) -> dict[str, Any]:
    return {key: value for key, value in config.items() if key != "audit"}


def audit_event(before, after, actor: str, reason: str, channel: str) -> dict[str, Any]:
    if not isinstance(actor, str) or not actor.strip() or not isinstance(reason, str) or not reason.strip():
        raise ValueError("A declared actor and change reason are required")
    if len(actor) > 120 or len(reason) > 1000:
        raise ValueError("Actor or reason is too long")
    return {"at_utc": datetime.now(timezone.utc).isoformat(), "actor": actor.strip(),
            "reason": reason.strip(), "channel": channel,
            "before": view_base(before) if before is not None else None, "after": view_base(after)}


def update_view(path: Path, changes: dict[str, Any], expected_revision: int, *, actor: str, reason: str, channel: str = "cli") -> dict[str, Any]:
    editable = {"title", "profile", "x", "scale", "statistic", "interval", "show_individual", "group_by", "subjects", "parameters", "include_mdv", "blq", "digits", "include_listings"}
    unknown = set(changes) - editable
    if unknown:
        raise ValueError(f"Unknown or immutable settings: {sorted(unknown)}")
    with view_lock(path):
        config = read_view(path)
        if config["revision"] != expected_revision:
            raise ValueError("View changed since it was read. Read the current revision and retry.")
        before = view_base(config)
        history = config.get("audit") or [audit_event(None, before, actor, "Legacy baseline; earlier history unavailable", "baseline")]
        config.update(changes)
        config["revision"] += 1
        config["audit"] = [*history, audit_event(before, config, actor, reason, channel)]
        with tempfile.TemporaryDirectory(prefix=".pk-config-", dir=path.parent) as tmp:
            candidate = Path(tmp) / "view.json"
            write_json(candidate, config)
            run_r([str(APP / "cli.R"), "validate", str(candidate)])
        write_json(path, config)
    return {"config_path": str(path), "config": config}


def render_report(path: Path, destination: Path, formats: list[str], expected_revision: int) -> dict[str, Any]:
    if not formats or not set(formats).issubset({"html", "docx"}):
        raise ValueError("Report formats must be html and/or docx")
    if destination.exists():
        raise ValueError("Report destination exists; use a new directory")
    with view_lock(path):
        config = read_view(path)
        if config["revision"] != expected_revision:
            raise ValueError("Requested report revision no longer matches; preview again")
    with tempfile.TemporaryDirectory(prefix="pk-report-view-") as tmp:
        snapshot = Path(tmp) / "view.json"
        write_json(snapshot, config)
        run_r([str(APP / "cli.R"), "report", str(snapshot), str(destination), ",".join(formats)], timeout=300)
    manifest = json.loads((destination / "REPORT_MANIFEST.json").read_text())
    return {"directory": str(destination), "files": [str(p) for p in sorted(destination.rglob("*")) if p.is_file()],
            "report_id": manifest["report_id"], "revision": config["revision"],
            "config_sha256": manifest["config_sha256"], "input_hashes": manifest["input_hashes"], "counts": manifest["counts"]}


def preview_view(path: Path) -> dict[str, Any]:
    return json.loads(run_r([str(APP / "cli.R"), "preview", str(path)]))


def launch_app(path: Path, port: int = 3876) -> dict[str, Any]:
    if not 1024 <= port <= 65535:
        raise ValueError("Port must be between 1024 and 65535")
    # Validate before spawning so an invalid view never produces a success URL.
    preview_view(path)
    logs = path.parent / "app-logs"
    logs.mkdir(exist_ok=True)
    logfile = logs / f"shiny-{uuid.uuid4().hex[:8]}.log"
    env = r_environment()
    env.update(PK_REVIEW_CONFIG=str(path.resolve()), PK_REVIEW_PORT=str(port))
    with logfile.open("w") as handle:
        process = subprocess.Popen([os.environ.get("RSCRIPT", "Rscript"), str(APP / "launch.R")], cwd=ROOT, env=env, stdout=handle, stderr=subprocess.STDOUT, start_new_session=True)
    import time
    deadline = time.monotonic() + 25
    while time.monotonic() < deadline:
        if process.poll() is not None:
            raise ValueError(logfile.read_text(encoding="utf-8")[-4000:])
        if f"Listening on http://127.0.0.1:{port}" in logfile.read_text(encoding="utf-8"):
            try:
                with urllib.request.urlopen(f"http://127.0.0.1:{port}", timeout=1) as response:
                    if response.status == 200 and process.poll() is None:
                        return {"url": f"http://127.0.0.1:{port}", "pid": process.pid, "log": str(logfile)}
            except OSError:
                pass
        time.sleep(0.2)
    process.terminate()
    raise ValueError(f"Shiny startup timed out; see {logfile}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    prepare = commands.add_parser("prepare")
    prepare.add_argument("source", type=Path)
    prepare.add_argument("destination", type=Path)
    view = commands.add_parser("create-view")
    view.add_argument("bundle", type=Path)
    view.add_argument("path", type=Path)
    view.add_argument("--title")
    update = commands.add_parser("update")
    update.add_argument("path", type=Path)
    update.add_argument("--request", required=True, type=Path)
    for name in ["preview", "inspect", "launch"]:
        cmd = commands.add_parser(name)
        cmd.add_argument("path", type=Path)
        if name == "launch":
            cmd.add_argument("--port", type=int, default=3876)
    report = commands.add_parser("report")
    report.add_argument("path", type=Path)
    report.add_argument("destination", type=Path)
    report.add_argument("--formats", default="html,docx")
    report.add_argument("--expected-revision", type=int, required=True)
    args = parser.parse_args()
    if args.command == "prepare":
        result = prepare_bundle(args.source, args.destination.resolve())
    elif args.command == "create-view":
        result = create_view(args.bundle, args.path.resolve(), args.title)
    elif args.command == "update":
        request = json.loads(args.request.read_text(encoding="utf-8"))
        result = update_view(args.path, request["changes"], request["expected_revision"],
                             actor=request["actor"], reason=request["reason"], channel=request.get("channel", "cli"))
    elif args.command == "preview":
        result = preview_view(args.path)
    elif args.command == "inspect":
        result = inspect_bundle(args.path)
    elif args.command == "launch":
        result = launch_app(args.path, args.port)
    else:
        result = render_report(args.path, args.destination.resolve(), args.formats.split(","), args.expected_revision)
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
