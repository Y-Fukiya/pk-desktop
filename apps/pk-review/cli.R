args <- commandArgs(trailingOnly = TRUE)
source(file.path(Sys.getenv("PK_REVIEW_ROOT", getwd()), "apps", "pk-review", "bootstrap.R"))
pk_assert(length(args) >= 2, "Usage: cli.R inspect BUNDLE | validate/preview CONFIG | report CONFIG OUTDIR FORMATS")
if (args[1] == "inspect") {
  result <- pk_inspect(args[2])
} else if (args[1] == "validate-legacy-times") {
  result <- pk_validate_legacy_times(args[2])
} else {
  config <- pk_read_config(args[2])
  data <- pk_load_bundle(config$bundle)
  analysis <- pk_analyse(data, config)
  if (args[1] %in% c("validate", "preview")) {
    result <- list(config = config, counts = analysis$counts,
                   concentration_summary = analysis$concentration_summary,
                   parameter_summary = analysis$parameter_summary, notes = analysis$notes,
                   warnings = analysis$warnings, quality_checks = analysis$quality_checks)
  } else if (args[1] == "report") {
    pk_assert(length(args) == 4, "report requires CONFIG OUTDIR FORMATS")
    result <- pk_write_report(analysis, data, args[3], strsplit(args[4], ",", fixed = TRUE)[[1]])
  } else stop("Unknown command", call. = FALSE)
}
cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null", digits = I(17)), "\n")
