"""PK Desktop MCP: stdio -> authenticated loopback R engine; no analysis state."""
from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import stat
import time
import copy
from typing import Any, Literal
import urllib.error
import urllib.request

from mcp.server.fastmcp import FastMCP
from jsonschema import Draft202012Validator

CONTRACT = json.loads(Path(__file__).with_name("nca_mcp_contract.json").read_text(encoding="utf-8"))
CONTRACT_VERSION = CONTRACT["contract_version"]
APP_VERSION = CONTRACT["app_version"]
PREPARATION_SCHEMA = CONTRACT["preparation_steps"]
ASSEMBLY_SCHEMA = CONTRACT["assembly_specification"]
ANNOTATION_SCHEMA = CONTRACT["annotation_specification"]
COVARIATE_SCHEMA = CONTRACT["covariate_specification"]
NCA_SCHEMA = CONTRACT["nca_specification"]

def validate_nca(specification: Any) -> dict | None:
    try:
        problem = next(Draft202012Validator(NCA_SCHEMA).iter_errors(specification), None)
        if problem is not None:
            if "group_roles" in problem.absolute_path:
                return validation_failure("グループ列の役割は、列名と許可された役割名を持つ配列で指定してください。")
            return validation_failure("Invalid NCA specification; use the published fields and enum values.")
        roles = specification.get("group_roles", [])
        groups = specification.get("groups", []) or []
        if isinstance(groups, str):
            groups = [groups]
        if len({entry["column"] for entry in roles}) != len(roles):
            return validation_failure("同じグループ列に複数の役割は指定できません。")
        if any(entry["column"] not in groups for entry in roles):
            return validation_failure("役割は追加グループ列のみに指定してください。")
    except (TypeError, ValueError):
        return validation_failure("Invalid NCA specification; use the published schema.")
    return None


def validate_covariates(specification: Any) -> dict | None:
    try:
        if next(Draft202012Validator(COVARIATE_SCHEMA).iter_errors(specification), None) is not None or not any(k in specification for k in ("dm", "vs")):
            return validation_failure("Covariate input requires profile keys, dm and/or vs, explicit selection and confirmed baseline.")
    except (RecursionError, TypeError, ValueError):
        return validation_failure("Invalid covariate input specification; use the published schema.")
    return None


def validate_annotation(specification: Any) -> dict | None:
    try:
        if next(Draft202012Validator(ANNOTATION_SCHEMA).iter_errors(specification), None) is not None or not any(k in specification for k in ("dm", "vs")):
            return validation_failure("Annotation specification requires version 1 and dm and/or vs with explicit keys, columns and VS selection.")
    except (RecursionError, TypeError, ValueError):
        return validation_failure("Invalid annotation specification; use the published schema.")
    return None

def validate_assembly(specification: Any) -> dict | None:
    try:
        if next(Draft202012Validator(ASSEMBLY_SCHEMA).iter_errors(specification), None) is not None:
            return validation_failure("Invalid assembly specification; check the published schema and required mappings, keys, policies, and clock.")
    except (RecursionError, TypeError, ValueError):
        return validation_failure("Invalid assembly specification; check the published schema and required mappings, keys, policies, and clock.")
    return None

def validation_failure(message: str) -> dict:
    return {"ok": False, "error": {"code": "E_VALIDATION", "message": message}}

def validate_preparation(steps: Any) -> dict | None:
    if not isinstance(steps, list) or len(steps) > 100:
        return validation_failure("steps must be an array with at most 100 operations.")
    definitions = PREPARATION_SCHEMA["$defs"]
    for step in steps:
        if not isinstance(step, dict) or not isinstance(step.get("op"), str) or step.get("op") not in definitions or step.get("op") == "predicate":
            return validation_failure("step.op must be filter, select, rename, convert, sort, distinct, where, elapsed_time, or blq_rules.")
        operation = step["op"]
        schema = {**definitions[operation], "$defs": definitions}
        try:
            error = next(Draft202012Validator(schema).iter_errors(step), None)
        except (RecursionError, TypeError, ValueError):
            return validation_failure(operation + ": arguments are invalid or too deeply nested; see the tool schema.")
        if error is not None:
            if operation == "filter":
                return validation_failure("filter: use column, operator (eq/ne/in/ge/le/gt/lt/missing/present), and values as a non-empty array. Comparisons other than in require one value; missing/present use no values.")
            fields = ", ".join(definitions[operation]["properties"])
            return validation_failure(operation + ": invalid arguments; use the published types and required fields: " + fields + ".")
    return None

def initialized_client(mcp: FastMCP) -> dict:
    try:
        info = mcp.get_context().session.client_params.clientInfo
        name, version = str(info.name), str(info.version)
    except (AttributeError, ValueError, LookupError):
        name, version = "unknown", "unknown"
    return {"name": name[:200], "version": version[:120], "source": "mcp_initialization_self_reported"}


class EngineClient:
    def __init__(self, runtime: Path, mode: str = "summary"):
        self.runtime = runtime
        self.mode = mode
        self.client_info_provider = None

    def call(self, operation: str, arguments: dict[str, Any] | None = None) -> dict:
        if self.mode == "nca" and operation == "nca_configure":
            invalid = validate_nca((arguments or {}).get("specification"))
            if invalid is not None:
                return invalid
        if self.mode == "nca" and operation == "prepare":
            invalid = validate_preparation((arguments or {}).get("steps"))
            if invalid is not None:
                return invalid
        if self.mode == "nca" and operation == "prepare_nca_covariates":
            invalid = validate_covariates((arguments or {}).get("specification"))
            if invalid is not None:
                return invalid
        if self.mode == "nca" and operation == "annotate_nca":
            invalid = validate_annotation((arguments or {}).get("specification"))
            if invalid is not None:
                return invalid
        if self.mode == "nca" and operation == "assemble_nca":
            invalid = validate_assembly((arguments or {}).get("specification"))
            if invalid is not None:
                return invalid
        try:
            info = self.runtime.stat()
            if info.st_uid != os.getuid() or stat.S_IMODE(info.st_mode) & 0o077:
                return {"ok": False, "error": {"code": "E_AUTH", "message": "Runtime file must be owned by this user with mode 0600"}}
            runtime = json.loads(self.runtime.read_text(encoding="utf-8"))
            if self.mode == "nca":
                project = runtime.get("project")
                if not isinstance(project, str) or Path(project).resolve(strict=True) != self.runtime.parent.resolve(strict=True):
                    return {"ok": False, "error": {"code": "E_PROJECT_CONFLICT", "message": "Runtime belongs to another project."}}
            port = runtime["api_port"]
            if type(port) is not int or not 1024 <= port <= 65535:
                raise ValueError("Invalid port")
            token = runtime["token"]
            if not isinstance(token, str) or len(token) != 64:
                raise ValueError("Invalid runtime token")
            # Ignore system proxies. Never forward the local token through a proxy.
            opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
            health = urllib.request.Request(f"http://127.0.0.1:{port}/health", headers={"Authorization": f"Bearer {token}"})
            with opener.open(health, timeout=2) as response:
                ready = json.load(response)
            if ready.get("instance_id") != runtime.get("instance_id") or ready.get("pid") != runtime.get("pid"):
                raise ValueError("Stale runtime instance")
            if self.mode == "nca" and ready.get("project") != runtime.get("project"):
                return {"ok": False, "error": {"code": "E_PROJECT_CONFLICT", "message": "Running owner belongs to another project."}}
            if ready.get("mode", "summary") != self.mode or runtime.get("mode", "summary") != self.mode:
                return {"ok": False, "error": {"code": "E_MODE", "message": "Runtime mode does not match MCP server mode."}}
            headers = {"Authorization": f"Bearer {token}", "Content-Type": "application/json"}
            payload = dict(arguments or {})
            if self.mode == "nca":
                if ready.get("contract_version") != CONTRACT_VERSION or ready.get("app_version") != APP_VERSION:
                    return {"ok": False, "error": {"code": "E_VERSION", "message": "MCP adapter and app versions differ. Update the registration to this app release and reconnect."}}
                headers["X-PKApp-Contract"] = CONTRACT_VERSION
                payload["_mcp_client"] = self.client_info_provider() if self.client_info_provider else {"name": "unknown", "version": "unknown", "source": "mcp_initialization_self_reported"}
            request = urllib.request.Request(f"http://127.0.0.1:{port}/v1/{operation}",
                data=json.dumps(payload).encode("utf-8"), method="POST", headers=headers)
            try:
                with opener.open(request, timeout=120) as response:
                    return json.load(response)
            except urllib.error.HTTPError as exc:
                return json.load(exc)
        except (OSError, ValueError, KeyError, urllib.error.URLError):
            return {"ok": False, "error": {"code": "E_NOT_RUNNING", "message": "Open PK Desktop and start the project first."}}


def create_server(runtime: Path, mode: str = "summary") -> FastMCP:
    if mode == "nca":
        return create_nca_server(runtime)
    client = EngineClient(runtime)
    mcp = FastMCP("pkapp", instructions=(
        "This desktop alpha analyses synthetic ADNCA concentrations and precomputed ADPP parameters. "
        "Call pk_get_status and pk_get_spec before updates or runs; pass the exact project_id and revision. "
        "Never infer an AUC code: inspect available PARAMCDs. ADPP is not recalculated. "
        "Updates require an actor and reason. Conflicts require rereading and reconciling user intent, not blind retries. "
        "The app accepts a preloaded CSV bundle. To export Word, choose a completed run_id and call pk_export_report; "
        "poll pk_get_job until succeeded, failed or cancelled. Changing live settings does not change a frozen report. "
        "The export returns local artifact paths and hashes, never individual records. Older runs without frozen figures require a new run. "
        "Results contain aggregate tables only; do not attempt to retrieve individual records. "
        "Use pk_list_modules then pk_add_module to add a supported module to the running UI without restart. "
        "TealFlowMCP may help select a module; only catalog identifiers and structured options are accepted, never arbitrary R. "
        "Prepared tables can be mapped with pk_validate_analysis_tables, then explicitly applied with pk_apply_analysis_tables."
    ))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_get_status() -> dict:
        """Get active project, revision, available parameters, grouping variables and profiles."""
        return client.call("status")

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_get_spec() -> dict:
        """Read canonical AnalysisSpec and its analysis hash from the running R owner."""
        return client.call("spec")

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_update_spec(section: Literal["population", "conc_plot", "param_summary", "report"],
                       values: dict[str, Any], base_revision: int, project_id: str, actor: str, reason: str) -> dict:
        """Change one section. For example conc_plot.group_by=['SEX']; invalid settings leave no change."""
        return client.call("update", dict(section=section, values=values, base_revision=base_revision,
                          project_id=project_id, actor=actor, reason=reason))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_run(base_revision: int, project_id: str) -> dict:
        """Freeze this revision and input snapshot, calculate summaries and publish an immutable run."""
        return client.call("run", dict(base_revision=base_revision, project_id=project_id))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_replay_run(run_id: str, base_revision: int, project_id: str) -> dict:
        """Replay a frozen Run and compare aggregate CSVs without replacing live settings."""
        return client.call("replay_run", dict(run_id=run_id, base_revision=base_revision, project_id=project_id))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_get_run_manifest(run_id: str) -> dict:
        """Read the verified Run manifest, checksums and environment metadata."""
        return client.call("run_manifest", dict(run_id=run_id))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_get_result(run_id: str) -> dict:
        """Verify recorded artifact checksums and return the aggregate results of a completed run."""
        return client.call("result", dict(run_id=run_id))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_list_runs() -> dict:
        """List completed runs and their revisions, channels and analysis hashes."""
        return client.call("runs")

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_export_report(run_id: str, project_id: str, actor: str, reason: str) -> dict:
        """Queue Word generation from a completed immutable Run. Poll pk_get_job for status and verified artifact paths."""
        return client.call("export_report", dict(run_id=run_id, project_id=project_id, actor=actor, reason=reason))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_get_job(job_id: str) -> dict:
        """Get report job status; verify output checksums before returning successful artifacts. No file contents are returned."""
        return client.call("job", dict(job_id=job_id))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_list_jobs() -> dict:
        """List report job metadata, including terminal errors and artifact locations. Use pk_get_job to verify artifacts."""
        return client.call("jobs")

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_cancel_job(job_id: str, project_id: str) -> dict:
        """Cancel a queued or running report. A succeeded report is retained. Cancelling workers cannot publish artifacts."""
        return client.call("cancel_job", dict(job_id=job_id, project_id=project_id))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_describe_data(source: str) -> dict:
        """Columns, types, row and missing counts only. Source: ADNCA, ADPP or saved table ID."""
        return client.call("describe", dict(source=source))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_list_tables() -> dict:
        """List persisted preparation/join metadata and checksums, without individual records."""
        return client.call("tables")

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_prepare_data(source: str, steps: list[dict[str, Any]], name: str,
                        base_revision: int, project_id: str, actor: str) -> dict:
        """Save a reproducible table. Operations: filter(column,operator,values), select(columns),
        rename(column,name), convert(column,type), sort(columns,decreasing), distinct(columns),
        where(predicate). Predicate nodes: {column:'SEX'}, {value:['F']},
        {op:'%in%',args:[{column:'SEX'},{value:['F']}]}; combine with &, |, !, is.na,
        ==, !=, >=, <=, >, <. No arbitrary code. IDs remain character."""
        return client.call("prepare", dict(source=source, steps=steps, name=name,
                          base_revision=base_revision, project_id=project_id, actor=actor))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_join_data(left: str, right: str, by: list[str], columns: list[str],
                     join: Literal["left", "inner"], name: str,
                     base_revision: int, project_id: str, actor: str) -> dict:
        """Join built-in or saved tables. Right keys must be unique; prefilter ADPP first.
        Returns counts/hashes only. Does not change the active analysis dataset."""
        return client.call("join", dict(left=left, right=right, by=by, columns=columns,
                          join=join, name=name, base_revision=base_revision, project_id=project_id, actor=actor))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_load_data(bundle: str, base_revision: int, project_id: str, actor: str, reason: str) -> dict:
        """Validate and snapshot ADNCA.csv/ADPP.csv in a project-local bundle. Resets view conditions;
        preserves previous Runs. External paths and escaping input symlinks are rejected."""
        return client.call("load_data", dict(bundle=bundle, base_revision=base_revision,
                          project_id=project_id, actor=actor, reason=reason))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_list_modules() -> dict:
        """Read supported module catalog, availability, options, module-list revision and saved definitions.
        Built-in scatterplot/distribution/table work without extra packages. tm_data_table requires
        teal.modules.general. Exploratory modules are separate from validated PK Runs and Word reports."""
        return client.call("modules")

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_add_module(module: str, source: str, label: str, options: dict[str, Any],
                      base_revision: int, modules_revision: int, project_id: str, actor: str, reason: str) -> dict:
        """Validate and add a catalog module to the running app's Exploratory Modules tab immediately.
        Snapshots the chosen ADNCA/ADPP/saved table; later data changes do not replace that snapshot.
        Call pk_list_modules first. pk_scatterplot options: x,y,optional color; pk_distribution: x,optional bins.
        Does not run/install arbitrary generated R. Missing package or invalid options make no registration."""
        return client.call("add_module", dict(module=module, source=source, label=label, options=options,
            base_revision=base_revision, modules_revision=modules_revision, project_id=project_id, actor=actor, reason=reason))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_remove_module(id: str, base_revision: int, modules_revision: int, project_id: str, actor: str, reason: str) -> dict:
        """Disable an exploratory module in the live UI; retain its definition, snapshot and audit metadata."""
        return client.call("remove_module", dict(id=id, base_revision=base_revision, modules_revision=modules_revision,
            project_id=project_id, actor=actor, reason=reason))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_import_table(path: str, name: str, base_revision: int, project_id: str, actor: str) -> dict:
        """Import project-local CSV/XPT/SAS7BDAT/RDS/Parquet into a saved work table. Preserve IDs; no individual records returned."""
        return client.call("import_table", dict(path=path, name=name, base_revision=base_revision, project_id=project_id, actor=actor))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_validate_analysis_tables(adnca: str, adpp: str, mapping: dict[str, dict[str, str]],
                                    time_unit: Literal["h", "min", "day"], base_revision: int, project_id: str) -> dict:
        """Stage and validate built-in or saved table IDs for analysis, without changing active analysis inputs.
        mapping={'ADNCA':{'destination_column':'source_column'},'ADPP':{}}; {} keeps existing column names.
        Returns validation_id and QC metadata. Applying later resets population/plot settings; explain that effect."""
        return client.call("validate_tables", dict(adnca=adnca, adpp=adpp, mapping=mapping, time_unit=time_unit,
            base_revision=base_revision, project_id=project_id))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_apply_analysis_tables(validation_id: str, base_revision: int, project_id: str, actor: str, reason: str) -> dict:
        """Apply the exact validated snapshot; reject stale revision or changed input files.
        Replaces active analysis inputs and resets view conditions, preserving existing Runs and reports."""
        return client.call("apply_tables", dict(validation_id=validation_id, base_revision=base_revision,
            project_id=project_id, actor=actor, reason=reason))

    return mcp


def create_nca_server(runtime: Path) -> FastMCP:
    client = EngineClient(runtime, "nca")
    mcp = FastMCP("pkapp-nca", instructions=(
        "Call pk_get_status and pk_nca_get_spec first. Specify project_id and exact base_revision. "
        "Import only project-local files, prepare tables, then configure a whole specification. "
        "Explicit c0_method is required: none, zero or predose. Prepared NCA_CONC uses protected "
        "NCA_BLQ/NCA_MDV and as_recorded; change BLQ rules in preparation. Conflicts require rereading "
        "and reconciling, never blind retries. Results are session-scoped. By default only counts and statuses are returned. "
        "Direct numeric summaries require explicit local UI consent for that result; small groups can reveal individual values. "
        "This is not a confidentiality or anonymization boundary: filter counts and QC outcomes can also reveal or infer values without consent. "
        "A condition change or failed run makes a result unavailable as current. actor is self-declared, not an authenticated identity. "
        "The GUI can inspect shared conditions."
    ))
    client.client_info_provider = lambda: initialized_client(mcp)

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_get_status() -> dict:
        """Get NCA project identity, revision, mode and capabilities."""
        return client.call("status")

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_nca_list_modules() -> dict:
        """List supported safe exploratory modules and their installation status; returns no data."""
        return client.call("modules")

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_nca_add_module(module: Literal["pk_scatterplot", "pk_distribution", "pk_data_table", "tm_data_table"],
                          source: str, label: str, options: dict[str, Any], base_revision: int,
                          modules_revision: int, project_id: str, actor: str, reason: str) -> dict:
        """Mount a pre-registered teal-based module on a saved work-table snapshot in the running app.
        pk_scatterplot requires numeric x,y and optional color; pk_distribution needs numeric x and
        optional bins; table modules accept no options. Teal and teal.data must be installed; tm_data_table
        also requires teal.modules.general. Call pk_nca_list_modules first. No arbitrary R is evaluated,
        no PK numbers are returned, and the NCA numeric-disclosure grant is not changed. Exploratory
        charts are not PK Run or Word-report output; select an appropriate parameter and unit beforehand."""
        return client.call("add_module", dict(module=module, source=source, label=label, options=options,
            base_revision=base_revision, modules_revision=modules_revision, project_id=project_id, actor=actor, reason=reason))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_nca_remove_module(id: str, base_revision: int, modules_revision: int, project_id: str, actor: str, reason: str) -> dict:
        """Disable a registered exploratory module; retain its snapshot and history."""
        return client.call("remove_module", dict(id=id, base_revision=base_revision, modules_revision=modules_revision,
            project_id=project_id, actor=actor, reason=reason))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_nca_explore_get() -> dict:
        """Read the NCA exploration selection and allowed views; returns no PK values."""
        return client.call("explore_get")

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_nca_explore_set(source: str, view: Literal["summary", "boxplot", "listing", "qc"],
                           parameter: str, unit: str, group: str, base_revision: int,
                           project_id: str, actor: str, reason: str, explore_revision: int, explore_session: str, unit_missing: bool = False, aggregate_over: list[str] | None = None, scale: Literal["linear", "log"] = "linear", summary_basis: Literal["analysis_unit", "subject_geomean"] = "analysis_unit", single_treatment_confirmed: bool = False, single_treatment_reason: str = "") -> dict:
        """Select a saved NCA result and configure a predefined local exploration view.
        Call status, spec and describe_data first. parameter and unit must identify one existing
        combination; group is an existing column name or empty for all. This selects the local UI
        view without returning values or executing arbitrary R; the user opens the exploration tab.
        Frozen NCA results remain unchanged and AI numeric disclosure is not enabled. Pooling periods
        requires a recorded treatment group. If none exists, only set single_treatment_confirmed
        after the user explicitly confirms every period has the same treatment, and provide their
        reason in single_treatment_reason; this is a self-reported exception, not verified identity."""
        return client.call("explore_set", dict(source=source, view=view, parameter=parameter, unit=unit,
            group=group, base_revision=base_revision, project_id=project_id, actor=actor, reason=reason,
            explore_revision=explore_revision, explore_session=explore_session, unit_missing=unit_missing, aggregate_over=aggregate_over, scale=scale, summary_basis=summary_basis, single_treatment_confirmed=single_treatment_confirmed, single_treatment_reason=single_treatment_reason))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_describe_data(source: str) -> dict:
        """Get saved table column names, types, row counts and missing counts; no records."""
        return client.call("describe", {"source": source})

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_list_tables() -> dict:
        """List saved work-table metadata and checksums."""
        return client.call("tables")

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_import_table(path: str, name: str, base_revision: int, project_id: str, actor: str) -> dict:
        """Import CSV, XPT, SAS7BDAT, RDS or Parquet from inside the active project; paths and symlinks cannot escape it."""
        return client.call("import_table", dict(path=path, name=name, base_revision=base_revision, project_id=project_id, actor=actor))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_prepare_data(source: str, steps: Any, name: str, base_revision: int, project_id: str, actor: str) -> dict:
        """Save a reproducible preparation. Structured ops: filter, select, rename, convert, sort,
        distinct, where, elapsed_time, blq_rules. Example elapsed_time: {"op":"elapsed_time",
        "sample_column":"SAMPLE_DT", "dose_column":"DOSE_DT", "utc_offset":"+09:00", "output":"NCA_TIME_H"}.
        Positional BLQ example: {"op":"blq_rules", "mode":"positional", "profile_columns":["ID","PERIOD"],
        "concentration":"CONC", "blq_column":"BLQ", "time":"NCA_TIME_H", "mdv_column":"MDV",
        "zero_time":"pre", "pre_rule":"zero", "first_rule":"zero", "middle_rule":"exclude",
        "last_rule":"exclude", "all_blq_rule":"exclude", "lloq_source":"column", "lloq_column":"LLOQ"}.
        Choose rules from keep, zero, half_lloq, exclude according to the analysis plan, not this example.
        flag_convention is strict or y_blank; profile_columns must exactly match NCA id plus groups.
        LLOQ must have concentration units. Use pk_describe_data first.
        No arbitrary code or individual records are returned."""
        return client.call("prepare", dict(source=source, steps=steps, name=name, base_revision=base_revision, project_id=project_id, actor=actor))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_join_data(left: str, right: str, by: list[str], columns: list[str], join: Literal["left", "inner"], name: str,
                     base_revision: int, project_id: str, actor: str) -> dict:
        """Join saved tables with unique right keys. Returns metadata, no records."""
        return client.call("join", dict(left=left, right=right, by=by, columns=columns, join=join, name=name,
                          base_revision=base_revision, project_id=project_id, actor=actor))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_assemble_nca(conc: str, pc: str, ex: str, specification: Any, name: str,
                        project_id: str, base_revision: int, actor: str, reason: str) -> dict:
        """Assemble saved concentration, PC, and EX tables using explicit sample keys, profile keys,
        dose reference, actual clock and dose-history policy. Saves frozen sources and lineage.
        Returns a manifest and aggregate counts, never source rows."""
        return client.call("assemble_nca", dict(conc=conc, pc=pc, ex=ex, specification=specification,
                           name=name, project_id=project_id, base_revision=base_revision, actor=actor, reason=reason))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_prepare_nca_covariates(source: str, specification: Any, name: str, project_id: str,
                                  base_revision: int, actor: str, reason: str, dm: str | None = None, vs: str | None = None) -> dict:
        """Prepare saved concentration input with optional DM/VS before NCA. Explicit profile keys,
        VS baseline scope/confirmation and measurement roles are required. Never selects latest records.
        Duplicate selected keys stop; unmatched rows remain. Saves sources, selected rows and units.
        Use the returned table as NCA input and explicitly configure dose/body_size/derived_parameters.
        Returns metadata/counts only, never selected measurements or PK values."""
        arguments = dict(source=source, specification=specification, name=name, project_id=project_id,
                         base_revision=base_revision, actor=actor, reason=reason)
        if dm is not None:
            arguments["dm"] = dm
        if vs is not None:
            arguments["vs"] = vs
        return client.call("prepare_nca_covariates", arguments)

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_annotate_nca(result_source: str, specification: Any, name: str, project_id: str,
                        base_revision: int, actor: str, reason: str, dm: str | None = None, vs: str | None = None) -> dict:
        """Append optional recorded DM/VS fields to a saved NCA result without changing PK values.
        Map NCA output keys to DM/VS source keys. Select VS tests and explicit filter or all_unique policy.
        Duplicate selected keys stop; missing information keeps PK rows. No BMI/BSA derivation or unit conversion.
        Frozen inputs and row/time/unit provenance are saved locally. Return manifest/counts only."""
        arguments = dict(result_source=result_source, specification=specification, name=name,
                         project_id=project_id, base_revision=base_revision, actor=actor, reason=reason)
        if dm is not None:
            arguments["dm"] = dm
        if vs is not None:
            arguments["vs"] = vs
        return client.call("annotate_nca", arguments)

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_nca_get_spec() -> dict:
        """Read shared source table, complete normalized conditions, revision and current result status."""
        return client.call("nca_get_spec")

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_nca_configure(source: str, specification: dict[str, Any], base_revision: int, project_id: str, actor: str, reason: str) -> dict:
        """Replace the entire NCA specification on a saved source table. Required fields include id, time,
        concentration and explicit c0_method. groups is an array of additional profile columns.
        Optional fields: time_unit (h|min|day), unit_column or concentration_unit, blq_column,
        mdv_column, blq (exclude|zero|as_recorded), flag_convention (strict|y_blank), scope (all|ss),
        time_origin, start, end (null for last observation), auc_method (linear|lin up/log down),
        c0_method (none|zero|predose), predose_window_h, all_blq_policy (missing|zero), include_aucproc.
        QC fields: predose_qc_threshold_pct and predose_qc_window_h; both must be positive when enabled.
        QC LLOQ supplement: predose_qc_lloq_source (column|constant), predose_qc_lloq_column or
        predose_qc_lloq_value. Supplement only fills eligible missing LLOQ records.
        Terminal fields: terminal_method (none|auto|manual), dosing_context (single|isolated_single_period|repeated_final|ongoing),
        dose_history_confirmed, washout_attestation (unconfirmed|protocol_confirmed), infusion_end_h, terminal_start_h, terminal_end_h, terminal_min_r_squared,
        terminal_min_span_ratio, aucinf_max_extrap_pct and terminal_qc_action (flag|withhold).
        isolated_single_period requires complete version-2 prior/next dose times and counts per profile,
        time_unit h and time_origin 0, including raw tables. Prior doses require protocol_confirmed
        and original-evidence predose QC pass/pass_blq. All scoped original non-MDV sampling must
        precede any next dose, including BLQ observations after a manual terminal endpoint.
        Attestation is not a BE conclusion or an automatic exclusion rule.
        Manual bounds and infusion end use elapsed hours; terminal analysis requires confirmed dose history.
        Prepared NCA_CONC requires blq_column=NCA_BLQ, mdv_column=NCA_MDV, blq=as_recorded.
        Optional derived_parameters are CMAXD,AUCLSTD,AUCIFOD,CLFO,VZFO,CLFOWT,VZFOWT,CLFOBSA,VZFOBSA.
        They require dose amount/unit/route columns, explicit actual_administered meaning, basis
        absolute|per_kg|per_m2 and reference selected_dose|declared_profile. selected_dose binds
        preserved EX source columns. Body-size bindings require intact prepare_nca_covariates roles.
        body_size.bsa.mode recorded uses selected BSA; mosteller explicitly derives from selected
        height and weight. No assumed body size or bioavailability. CL/V are EV apparent observed
        single-dose/isolated-period outputs only. Canonical dose mg, concentration mg/L, output L/h or L.
        Check columns and units first; invalid changes leave accepted conditions unchanged."""
        return client.call("nca_configure", dict(source=source, specification=specification, base_revision=base_revision,
                          project_id=project_id, actor=actor, reason=reason))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_nca_run(base_revision: int, project_id: str, actor: str, reason: str) -> dict:
        """Run accepted conditions while the local UI stays responsive. Return counts/statuses by default;
        numeric statistics require a human to allow this exact result in the local UI. actor is self-declared."""
        answer = client.call("nca_run", dict(base_revision=base_revision, project_id=project_id, actor=actor, reason=reason))
        job = answer.get("result", {})
        if not answer.get("ok") or not job.get("job_id"):
            return answer
        deadline = time.monotonic() + 300
        while time.monotonic() < deadline:
            time.sleep(0.1)
            answer = client.call("nca_get_job", {"project_id": project_id, "job_id": job["job_id"]})
            if not answer.get("ok"):
                return answer
            current = answer.get("result", {})
            if current.get("status") == "succeeded":
                return {"ok": True, "result": current["result"]}
        return {"ok": False, "error": {"code": "E_TIMEOUT", "message": "Calculation is still running. Check job status in pk_nca_get_spec before retrying."}}

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_nca_get_result(result_id: str, project_id: str) -> dict:
        """Inspect frozen result counts, status and freshness. Numeric statistics are withheld until
        a human allows this exact result in the local UI; authorized small groups may reveal individual values."""
        return client.call("nca_get_result", dict(result_id=result_id, project_id=project_id))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_nca_save_result(result_id: str, name: str, base_revision: int, project_id: str, actor: str, reason: str) -> dict:
        """Save the exact current frozen result and trace as a local work table without recalculating."""
        return client.call("nca_save_result", dict(result_id=result_id, name=name, base_revision=base_revision,
                          project_id=project_id, actor=actor, reason=reason))

    for tool in mcp._tool_manager._tools.values():
        tool.fn_metadata.arg_model.model_config["hide_input_in_errors"] = True
        tool.fn_metadata.arg_model.model_rebuild(force=True)
    schema = mcp._tool_manager._tools["pk_prepare_data"].parameters
    schema["$defs"] = copy.deepcopy(PREPARATION_SCHEMA["$defs"])
    schema["properties"]["steps"] = {k: copy.deepcopy(v) for k, v in PREPARATION_SCHEMA.items() if k not in ("$schema", "$defs")}
    assembly = mcp._tool_manager._tools["pk_assemble_nca"].parameters
    assembly["$defs"] = copy.deepcopy(ASSEMBLY_SCHEMA["$defs"])
    assembly["properties"]["specification"] = {k: copy.deepcopy(v) for k, v in ASSEMBLY_SCHEMA.items() if k not in ("$schema", "$defs")}
    annotation = mcp._tool_manager._tools["pk_annotate_nca"].parameters
    annotation["properties"]["specification"] = {k: copy.deepcopy(v) for k, v in ANNOTATION_SCHEMA.items() if k != "$schema"}
    covariate = mcp._tool_manager._tools["pk_prepare_nca_covariates"].parameters
    covariate["properties"]["specification"] = {k: copy.deepcopy(v) for k, v in COVARIATE_SCHEMA.items() if k != "$schema"}
    nca = mcp._tool_manager._tools["pk_nca_configure"].parameters
    nca["properties"]["specification"] = {k: copy.deepcopy(v) for k, v in NCA_SCHEMA.items() if k != "$schema"}
    return mcp


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runtime", type=Path, default=Path.home() / "Library/Application Support/org.pkdummy.pkdesktop/runtime.json")
    parser.add_argument("--mode", choices=("summary", "nca"), default="summary")
    args = parser.parse_args()
    create_server(args.runtime, args.mode).run(transport="stdio")
