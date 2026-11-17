# Standalone NCA MCP

Start the standalone application with `Rscript apps/pk-desktop/launch.R --mode nca --project /absolute/project --api-port 3880`. Use distinct UI and API ports. Generate a client configuration without modifying global settings:

```sh
/path/to/venv/bin/python apps/pk-desktop/mcp_config.py --project /absolute/project --python /path/to/venv/bin/python --format codex
```

Use `--format json` for `mcpServers` clients. The configuration contains paths, never the bearer token. Restart the MCP client after registering it. The active NCA process owns the private runtime and project; the server rejects a summary-mode runtime. Table imports must be inside that project, including symlink targets.

Call `pk_get_status`, then import or prepare a table. `pk_nca_configure` replaces the entire specification and requires an explicit `c0_method` (`none`, `zero`, or `predose`); call `pk_nca_run` with the returned revision. Prepared BLQ fields remain protected. A revision conflict requires reading conditions again and reconciling the change.

The ordinary standalone `NCAを実行` button publishes the GUI conditions and runs the same R owner as MCP. File uploads become immutable input work tables; unchanged uploads reuse the accepted snapshot. After an MCP change, use `共有条件・結果を画面に読み込む` to replace the local form draft with the accepted source and complete conditions. Matching results appear in the ordinary individual result tables and concentration plots. Editing the draft, changing sources, a shared revision change, or a failed run makes outdated results unavailable for saving. A stale screen cannot publish its draft until it loads the new shared state. Summary mode keeps its previous workflow.

Result IDs last for the owner process. Inspection returns counts and statuses by default. Numeric summaries are withheld even for a single filtered profile. A human can allow numeric summaries for one exact current result in the local UI and revoke that permission. A new run, configuration update, or process restart expires consent. When allowed, min, max and single-profile means can reveal individual PK values; consent is not anonymization. Save only a current result. A failed calculation suppresses the current result until a new run succeeds; reaccepting conditions does not restore an older result.

This controls direct summary fields, not confidentiality: even without consent, adaptive filters and their returned counts, or QC outcomes, can reveal/infer values. For example, filtering a saved NCA result by AVAL thresholds gives a counting oracle. The previous claim that individual PK values cannot reach AI is withdrawn. Use only data permitted for the connected AI service. Separate filesystem/browser access is outside this API control.

Accepted conditions, before/after changes, reasons, result hashes and save provenance are recorded. actor is a self-declared label, not authenticated identity. The R process observes its OS account and PID; MCP initialization client name/version are separately labelled self-reported metadata. The state checksum detects accidental edits; it is not an electronic signature. Runtime path, project, instance, process, mode and owner-only permissions are checked before dispatch. The disclosure gate applies to this NCA MCP result API only. Local files or other tools available to an AI are outside this gate. Counts, statuses and metadata can themselves be sensitive. The adapter and R API enforce contract pkapp-nca-2 and compatible application version.

The release verifier uses a Python environment with `mcp` and `tomli` installed (Python 3.11+ provides `tomllib`). `tests/nca017_mcp_contract.py` exercises real stdio initialization without a listener. `nca017_pipeline.R` exercises the authenticated HTTP handler in-process, not a TCP connection. `nca017_ui.R` checks Shiny server state and generated HTML, not a live browser.

For real end-to-end acceptance, launch NCA on an empty dedicated test project, then run:

```sh
python apps/pk-desktop/tests/live_nca_mcp.py --project /absolute/test-project --output-json /absolute/new-result.json
```

The test imports the artificial actual-time example, prepares elapsed time and BLQ, checks that numeric values are withheld, tests invalid and valid filters, checks pass_blq, and saves the frozen result. The test then reads its synthetic saved CSV locally to verify CMAX=8, TMAX=1 and AUCLST=31.5. It refuses a project containing work tables. Four `TEST ONLY NCA018` tables remain for inspection; close NCA and remove the dedicated test project afterwards if unnecessary. Do not use a production project.

Preparation operation schemas are published in `nca_mcp_contract.json`. Filter uses `values` as an array, for example `{"op":"filter","column":"ID","operator":"eq","values":["S03"]}`. Syntax and validation failures return fixed `E_VALIDATION` guidance without echoing data values. Unexpected internal exceptions remain masked.

NCA runs use a separate R worker so the owner can serve GUI and status updates. The MCP adapter waits by polling the internal job endpoint. Client cancellation or timeout does not imply that the owner stopped its job; inspect current status before retrying. Do not start a second calculation while a job is running.
