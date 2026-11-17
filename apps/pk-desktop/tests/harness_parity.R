Sys.setenv(PK_REVIEW_ROOT = normalizePath(getwd()))
source("apps/pk-desktop/R/engine.R")
source("apps/pk-desktop/R/api.R")
project <- file.path(pk_root, "outputs", "pk-desktop", "validation-20261104")
engine <- desktop_new(project, file.path(pk_root, "outputs/pk-review/bundles/reviewed-repeated"))
apply <- function(channel) desktop_apply(engine, "conc_plot", list(group_by = list("SEX")), engine$spec$revision,
  engine$spec$project_id, channel, "M0 validation", "Compare identical UI and MCP conditions")
apply("ui")
ui <- desktop_run(engine, engine$spec$revision, engine$spec$project_id, "ui")
apply("mcp")
mcp <- desktop_dispatch(engine, "run", list(base_revision = engine$spec$revision, project_id = engine$spec$project_id))
stopifnot(identical(ui$analysis_hash, mcp$analysis_hash))
for (name in c("concentration_summary.csv", "parameter_summary.csv")) stopifnot(identical(
  pk_hash(file.path(project, "runs", ui$run_id, "tables", name)), pk_hash(file.path(project, "runs", mcp$run_id, "tables", name))))
desktop_atomic(list(project = project, ui_run = ui$run_id, api_run = mcp$run_id, analysis_hash = ui$analysis_hash,
  input_bundle = engine$spec$data$bundle), file.path(project, "validation.json"))
cat("Harness parity PASS:", ui$counts$subjects, "subjects;", ui$counts$concentration_rows, "concentration rows;", ui$counts$parameter_rows, "parameter rows\n")
