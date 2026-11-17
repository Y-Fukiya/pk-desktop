# Reproducible 50-subject example, using the same dispatcher as the MCP adapter.
Sys.setenv(PK_REVIEW_ROOT = normalizePath(getwd()))
source("apps/pk-desktop/R/engine.R")
source("apps/pk-desktop/R/api.R")
local({
  engine <- desktop_new(file.path(pk_root, "outputs/pk-desktop/report-validation-20261105"),
    file.path(pk_root, "outputs/pk-review/bundles/reviewed-repeated"))
  on.exit(desktop_jobs_close(engine), add = TRUE)
  desktop_apply(engine, "conc_plot", list(group_by = list("SEX"), profile = "steady_state", x = "TAD_H"), engine$spec$revision, engine$spec$project_id,
    "ui", "Local reviewer", "Example concentration and parameter summaries by sex")
  desktop_apply(engine, "report", list(title = "PK simulation review — 50 subjects", digits = 3L),
    engine$spec$revision, engine$spec$project_id, "ui", "Local reviewer", "Sample Word report")
  run <- desktop_dispatch(engine, "run", list(base_revision = engine$spec$revision, project_id = engine$spec$project_id))
  started <- Sys.time()
  job <- desktop_dispatch(engine, "export_report", list(run_id = run$run_id, project_id = engine$spec$project_id,
    actor = "Validation script", reason = "Verify frozen Run to Word with the real 50-subject harness bundle"))
  repeat {
    job <- desktop_dispatch(engine, "job", list(job_id = job$job_id))
    if (job$status %in% desktop_job_terminal) break
    stopifnot(as.numeric(difftime(Sys.time(), started, units = "secs")) < 60)
    Sys.sleep(.1)
  }
  stopifnot(identical(job$status, "succeeded"))
  desktop_atomic(list(project = engine$project, run = run$run_id, job = job,
    subjects = run$counts$subjects, elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs"))),
    file.path(engine$project, "report_validation.json"))
  cat("Word report PASS:", run$counts$subjects, "subjects;", job$artifacts$docx, "\n")
})
