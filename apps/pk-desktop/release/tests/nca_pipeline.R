# Run from the source root. Uses temporary projects and the shipped examples.
source("apps/pk-desktop/pkapp.R")
core <- environment(pkapp_open)
for (kind in c("actual_time", "positional_blq")) {
  data <- pkapp_read_table(paste0("apps/pk-desktop/examples/nca_", kind, "_example.csv"))
  original <- data
  steps <- list()
  if (kind == "actual_time") steps <- list(list(op = "elapsed_time", sample_column = "SAMPLE_DTM", dose_column = "DOSE_DTM", utc_offset = "+09:00"))
  steps <- c(steps, list(list(op = "blq_rules", mode = "positional", profile_columns = c("ID", "ANALYTE", "PERIOD"),
    concentration = "CONC", time = if (kind == "actual_time") "NCA_TIME_H" else "TIME",
    blq_column = "BLQ", mdv_column = "MDV", flag_convention = if (kind == "actual_time") "y_blank" else "strict",
    zero_time = "pre", pre_rule = "zero", first_rule = "zero", middle_rule = "exclude",
    last_rule = if (kind == "actual_time") "exclude" else "half_lloq", lloq_source = "column", lloq_column = "LLOQ")))
  project <- tempfile("nca-pipeline-"); dir.create(project)
  engine <- core$desktop_nca_engine(project)
  prepared <- core$desktop_table_save(engine, kind, list(source = data), list(kind = "prepare", steps = steps))
  table <- pkapp_replay(core$desktop_table_path(engine, prepared$id))$data
  stopifnot(identical(table[names(original)], original))
  specification <- list(id = "ID", groups = c("ANALYTE", "PERIOD"),
    time = if (kind == "actual_time") "NCA_TIME_H" else "TIME", concentration = "NCA_CONC",
    unit_column = "UNIT", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV", blq = "as_recorded",
    c0_method = if (kind == "actual_time") "zero" else "none", auc_method = "linear")
  saved <- core$desktop_table_save(engine, paste(kind, "NCA"), list(source = table), list(kind = "nca", specification = specification))
  folder <- core$desktop_table_path(engine, saved$id)
  result <- pkapp_replay(folder)
  expected <- if (kind == "actual_time") c(8, 1, 31.5) else c(8, 1, 30)
  stopifnot(isTRUE(all.equal(result$data$AVAL, expected, tolerance = 1e-12)))
  stopifnot(all(result$data$ID == "001"), identical(result$data, core$desktop_table_get(engine, saved$id)))
  exported <- pkapp_read_table(file.path(folder, "result.csv"))
  stopifnot(isTRUE(all.equal(as.numeric(exported$AVAL), expected, tolerance = 1e-12)), all(exported$ID == "001"))
  stopifnot(file.exists(file.path(folder, "row_handling.csv")), file.exists(file.path(folder, "trace.json")))
  cat(sprintf("PASS %s: import -> preparation -> save/replay -> NCA -> save/replay -> CSV; CMAX=%g TMAX=%g AUCLST=%g\n", kind, expected[1], expected[2], expected[3]))
  unlink(project, recursive = TRUE)
}
