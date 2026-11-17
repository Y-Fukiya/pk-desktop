pk_write_csv <- function(data, path) {
  numeric <- vapply(data, is.numeric, logical(1))
  quoted <- which(vapply(data, function(x) is.character(x) || is.factor(x), logical(1)))
  # Keep machine-readable doubles independent of the display's significant digits.
  data[numeric] <- lapply(data[numeric], function(x) {
    ifelse(is.na(x), NA_character_, sprintf("%.17g", x))
  })
  utils::write.table(data, path, sep = ",", quote = quoted, qmethod = "double", row.names = FALSE, na = "")
}

pk_write_report <- function(a, data, destination, formats = c("html", "docx")) {
  pk_assert(length(formats) > 0 && all(formats %in% c("html", "docx")), "Supported formats are html and docx")
  pk_assert(!dir.exists(destination) && !file.exists(destination), "Report destination already exists")
  pk_assert(rmarkdown::pandoc_available(), "Pandoc is required for report generation")
  pk_assert(identical(unname(pk_code_hashes), lapply(pk_code_files, pk_hash)), "Code changed during this session; restart before exporting")
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(pattern = ".pk-report-", tmpdir = dirname(destination))
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  cfg <- a$config
  cohort <- sort(unique(c(data$ADNCA$USUBJID, data$ADPP$USUBJID)))
  if (length(cfg$subjects)) cohort <- sort(unlist(cfg$subjects))
  exports <- list(ADNCA_selected = a$concentrations, ADPP_selected = a$parameters,
                  concentration_summary = a$concentration_summary, parameter_summary = a$parameter_summary,
                  quality_checks = a$quality_checks, selected_subjects = data.frame(USUBJID = cohort))
  for (name in names(exports)) pk_write_csv(exports[[name]], file.path(stage, paste0(name, ".csv")))
  jsonlite::write_json(cfg, file.path(stage, "view.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null")
  jsonlite::write_json(cfg$audit %||% list(), file.path(stage, "change_history.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null")
  jsonlite::write_json(a$counts, file.path(stage, "counts.json"), auto_unbox = TRUE, pretty = TRUE)
  metadata <- list(report_id = paste0("pk-", substr(digest::digest(list(Sys.time(), Sys.getpid(), tempfile()), algo = "sha256"), 1, 20)),
                   created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   run_id = data$manifest$run_id %||% basename(cfg$bundle), study = pk_identity_label(data, "STUDYID", cohort),
                   analyte = pk_identity_label(data, "ANALYTE", cohort),
                   cohort = if (length(cohort) <= 10) paste(cohort, collapse = ", ") else paste(length(cohort), "subjects; complete IDs in selected_subjects.csv"),
                   cohort_sha256 = pk_hash(file.path(stage, "selected_subjects.csv")),
                   config_sha256 = pk_hash(file.path(stage, "view.json")), input_hashes = data$hashes,
                   input_sha256 = digest::digest(paste(names(data$hashes), unlist(data$hashes), collapse = "\n"), algo = "sha256", serialize = FALSE),
                   warnings = a$warnings,
                   history_status = if (length(cfg$audit)) "Recorded locally; actor identities are self-declared, not authenticated signatures" else "Prior change history unavailable")
  jsonlite::write_json(metadata, file.path(stage, "report_identity.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
  methods <- unique(a$parameters[intersect(c("PARAMCD", "NCA_METHOD", "NCA_START_H", "NCA_END_H", "SOURCE_MODE"), names(a$parameters))])
  if (!"NCA_METHOD" %in% names(methods)) methods$NCA_METHOD <- "Not provided"
  methods$NCA_METHOD[is.na(methods$NCA_METHOD) | !nzchar(methods$NCA_METHOD)] <- "Not provided"
  saveRDS(list(concentrations = pk_display_table(a$concentration_summary, cfg$digits),
               parameters = pk_display_table(a$parameter_summary, cfg$digits, TRUE), methods = methods,
               quality_checks = a$quality_checks,
               concentration_listing = a$concentrations[c("USUBJID", ".TIME", "AVAL", "AVALU", ".GROUP")],
               parameter_listing = a$parameters[c("USUBJID", "PARAMCD", "AVAL", "AVALU", ".GROUP")]), file.path(stage, "tables.rds"))
  ggplot2::ggsave(file.path(stage, "concentrations.png"), pk_plot_concentration(a), width = 7, height = 4.2, dpi = 180)
  n_panels <- nrow(unique(a$parameters[c("PARAMCD", "AVALU")]))
  ggplot2::ggsave(file.path(stage, "parameters.png"), pk_plot_parameters(a), width = 7, height = max(3, ceiling(n_panels / 3) * 2.4), dpi = 180)
  for (file in c("report.Rmd", "reference.docx", "reproduce.R")) {
    pk_assert(file.copy(file.path(pk_app, file), file.path(stage, file)), paste("Cannot copy", file))
  }
  dir.create(file.path(stage, "input"))
  for (file in c("ADNCA.csv", "ADPP.csv", "BUNDLE.json")) {
    original <- file.path(cfg$bundle, file)
    if (file.exists(original)) pk_assert(file.copy(original, file.path(stage, "input", file)), paste("Cannot copy", file))
  }
  for (file in names(data$hashes)) pk_assert(identical(pk_hash(file.path(stage, "input", file)), data$hashes[[file]]), "Input changed while exporting")
  for (file in names(pk_code_hashes)) {
    target <- file.path(stage, "source", file)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    pk_assert(file.copy(file.path(pk_root, file), target) && identical(pk_hash(target), pk_code_hashes[[file]]), "Code changed while exporting")
  }
  previous <- getwd()
  setwd(stage)
  on.exit(setwd(previous), add = TRUE)
  for (format in unique(formats)) {
    output <- if (format == "html") "html_document" else "word_document"
    rmarkdown::render("report.Rmd", output_format = output, output_file = paste0("report.", format),
                      envir = new.env(parent = globalenv()), quiet = TRUE)
  }
  setwd(previous)
  packages <- sort(loadedNamespaces())
  environment <- list(R = as.character(getRversion()), platform = R.version$platform,
                       packages = stats::setNames(lapply(packages, function(p) as.character(utils::packageVersion(p))), packages),
                       pandoc = as.character(rmarkdown::pandoc_version()))
  jsonlite::write_json(environment, file.path(stage, "environment.json"), auto_unbox = TRUE, pretty = TRUE)
  writeLines(capture.output(sessionInfo()), file.path(stage, "sessionInfo.txt"))
  manifest <- c(metadata, list(version = 2, purpose = "PK synthetic data review", view_revision = cfg$revision,
                   data_roles = list(ADNCA = "Concentration records", ADPP = "Precomputed PK parameter records"),
                   source_manifest = data$manifest, counts = a$counts, notes = as.list(a$notes),
                   code_hashes = pk_code_hashes, software = environment, outputs = list()))
  for (file in list.files(stage, recursive = TRUE)) manifest$outputs[[file]] <- pk_hash(file.path(stage, file))
  jsonlite::write_json(manifest, file.path(stage, "REPORT_MANIFEST.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
  pk_assert(file.rename(stage, destination), "Could not move completed report into destination")
  list(directory = normalizePath(destination), files = as.list(list.files(destination, recursive = TRUE)))
}

`%||%` <- function(x, fallback) if (is.null(x)) fallback else x
