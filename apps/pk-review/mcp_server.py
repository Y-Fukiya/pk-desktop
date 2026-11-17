"""Local stdio MCP server for PK review, using the official MCP Python SDK."""
from __future__ import annotations

import argparse
from pathlib import Path
import re
import sys
from typing import Literal

from mcp.server.fastmcp import FastMCP
from pydantic import BaseModel, ConfigDict

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from tools import pk_review as review


class ViewChanges(BaseModel):
    model_config = ConfigDict(extra="forbid")
    title: str | None = None
    profile: Literal["all", "steady_state", "trough"] | None = None
    x: Literal["TIME_H", "TAD_H"] | None = None
    scale: Literal["linear", "log10"] | None = None
    statistic: Literal["mean", "median", "geometric_mean"] | None = None
    interval: Literal["none", "sd", "p05_p95"] | None = None
    show_individual: bool | None = None
    group_by: Literal["NONE", "ARM", "SEX", "DOSE_MG", "ROUTE"] | None = None
    subjects: list[str] | None = None
    parameters: list[str] | None = None
    include_mdv: bool | None = None
    blq: Literal["exclude", "as_recorded"] | None = None
    digits: int | None = None
    include_listings: bool | None = None


def create_server(workspace: Path) -> FastMCP:
    workspace = workspace.resolve()
    output = workspace / "outputs" / "pk-review"
    mcp = FastMCP(
        "pkdummy-review",
        instructions=("ADNCA.csv contains concentration records. ADPP.csv contains precomputed PK parameters. "
                      "Inspect the run, prepare a bundle, create a view, then read and update its revision. "
                      "Use only settings accepted by the tools. ADPP is never recalculated by visualization filters. "
                      "Plots reload saved settings; the editor requires explicit reload after external changes. "
                      "Existing files are not overwritten by prepare/create/export tools. "
                      "Use pk_preview to verify counts and warnings; pass its revision to export. "
                      "Updates require a self-declared actor and a concrete change reason. "
                      "Inspect exact available parameter codes; do not guess which AUC an ambiguous request means. "
                      "A steady_state view selects the labelled final dose interval; QC PASS is not proof of clinical steady state. "
                      "Successful export is not human approval."),
    )

    def name(value: str) -> str:
        if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_-]{0,79}", value):
            raise ValueError("Use a name of 1-80 letters, digits, underscores or hyphens")
        return value

    def path(value: str) -> Path:
        return review.checked_path(value, workspace)

    def view_path(value: str) -> Path:
        p = path(value)
        cfg = review.read_view(p)
        bundle = review.checked_path(cfg["bundle"], workspace)
        for filename in ["ADNCA.csv", "ADPP.csv", "BUNDLE.json"]:
            if (bundle / filename).exists():
                review.checked_path(bundle / filename, workspace)
        return p

    def new_path(relative: str) -> Path:
        return review.checked_path(output / relative, workspace, exists=False)

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_list_runs() -> dict:
        """List harness analysis_inputs directories and prepared review bundles."""
        runs = [str(p.parent.relative_to(workspace)) for p in workspace.glob("outputs/**/analysis_inputs/ADPC.csv")]
        bundles = [str(p.parent.relative_to(workspace)) for p in workspace.glob("outputs/pk-review/bundles/*/BUNDLE.json")]
        return {"runs": sorted(runs), "bundles": sorted(bundles)}

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_prepare_bundle(source: str, bundle_name: str) -> dict:
        """Prepare ADNCA concentrations and ADPP parameters from a harness run or canonical pair of CSVs."""
        p = path(source)
        for filename in ["ADNCA.csv", "ADPC.csv", "ADPP.csv", "NCA_SS_INPUT.csv", "TROUGH_INPUT.csv", "NCA_SS_SUMMARY.csv", "TROUGH_SUMMARY.csv"]:
            for candidate in [p / filename, p / "analysis_inputs" / filename]:
                if candidate.exists():
                    review.checked_path(candidate, workspace)
        return review.prepare_bundle(p, new_path(f"bundles/{name(bundle_name)}"))

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_inspect_bundle(bundle: str) -> dict:
        """Return available subjects, PK parameters, profiles and grouping variables."""
        p = path(bundle)
        for filename in ["ADNCA.csv", "ADPP.csv"]:
            review.checked_path(p / filename, workspace)
        return review.inspect_bundle(p)

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_create_view(bundle: str, view_name: str, title: str = "PK simulation review") -> dict:
        """Create a persistent view with shared settings for teal and reports."""
        p = path(bundle)
        for filename in ["ADNCA.csv", "ADPP.csv"]:
            review.checked_path(p / filename, workspace)
        return review.create_view(p, new_path(f"views/{name(view_name)}.json"), title)

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_read_view(view: str) -> dict:
        """Read settings and the revision needed for an update."""
        p = view_path(view)
        return {"config_path": str(p), "config": review.read_view(p)}

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_update_view(view: str, expected_revision: int, changes: ViewChanges, actor: str, reason: str) -> dict:
        """Update settings atomically. TAD_H requires steady_state; SD requires mean. Empty subjects means all."""
        return review.update_view(view_path(view), changes.model_dump(exclude_unset=True), expected_revision,
                                  actor=actor, reason=reason, channel="mcp")

    @mcp.tool(annotations={"readOnlyHint": True})
    def pk_preview(view: str) -> dict:
        """Return selected record counts, exclusions and summary tables without recalculating PK parameters."""
        return review.preview_view(view_path(view))

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_export_report(view: str, report_name: str, expected_revision: int, formats: list[Literal["html", "docx"]] = ["html", "docx"]) -> dict:
        """Export a new report directory with complete CSVs, figures, settings and checksums."""
        return review.render_report(view_path(view), new_path(f"reports/{name(report_name)}"), formats, expected_revision)

    @mcp.tool(annotations={"readOnlyHint": False, "destructiveHint": False})
    def pk_launch_app(view: str, port: int = 3876) -> dict:
        """Start a local teal app on 127.0.0.1 and return its URL and process ID."""
        return review.launch_app(view_path(view), port)

    return mcp


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workspace", type=Path, default=ROOT)
    args = parser.parse_args()
    create_server(args.workspace).run(transport="stdio")
