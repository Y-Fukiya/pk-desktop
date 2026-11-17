# Run from the repository root with Rscript --vanilla apps/pk-desktop/examples/batch.R
source("apps/pk-desktop/pkapp.R")
local({
  p <- pkapp_open("outputs/pk-desktop/r-example")
  on.exit(pkapp_close(p), add = TRUE)
  print(pkapp_status(p))
  current <- pkapp_spec(p)
  pkapp_update(p, "conc_plot", list(group_by = "SEX", profile = "steady_state", x = "TAD_H"),
    base_revision = current$revision, reason = "Compare the labelled final dose interval by sex")
  available <- pkapp_status(p)$parameters
  selected <- intersect(c("CMAXSS", "AUCTAUSS", "TMAXSS"), unlist(available))
  stopifnot(length(selected) > 0)
  pkapp_update(p, "param_summary", list(paramcd = selected), base_revision = pkapp_spec(p)$revision,
    reason = "Summarise available steady-state ADPP parameters")
  run <- pkapp_run(p)
  print(run$parameter_summary)
  report <- pkapp_report(p, run$run_id, reason = "Review the frozen R analysis")
  cat("Word:", report$artifacts$docx, "\n")
})
