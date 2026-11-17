# The renderer consumes only files already frozen in a completed Run.
desktop_snapshot_report <- function(analysis, data, stage) {
  dir.create(file.path(stage, "plots"))
  ggplot2::ggsave(file.path(stage, "plots", "concentration.png"), pk_plot_concentration(analysis),
    device = ragg::agg_png, width = 7, height = 4.2, units = "in", dpi = 180, bg = "white")
  methods <- unique(analysis$parameters[intersect(c("PARAMCD", "AVALU", "NCA_METHOD", "NCA_START_H", "NCA_END_H", "SOURCE_MODE"),
    names(analysis$parameters))])
  if (!"NCA_METHOD" %in% names(methods)) methods$NCA_METHOD <- "Not provided"
  methods$NCA_METHOD[is.na(methods$NCA_METHOD) | !nzchar(methods$NCA_METHOD)] <- "Not provided"
  cohort <- unique(c(analysis$concentrations$USUBJID, analysis$parameters$USUBJID))
  desktop_atomic(list(format = "pkapp-report-context-1", counts = analysis$counts,
    study = pk_identity_label(data, "STUDYID", cohort), analyte = pk_identity_label(data, "ANALYTE", cohort),
    methods = methods, warnings = as.list(analysis$warnings), notes = as.list(analysis$notes)),
    file.path(stage, "report_context.json"))
}

desktop_report_number <- function(x, digits) vapply(x, function(value) {
  if (is.na(value)) "NE" else format(signif(value, digits), scientific = FALSE, trim = TRUE)
}, character(1))

desktop_report_table <- function(doc, data) {
  if (!nrow(data)) return(officer::body_add_par(doc, "No eligible records."))
  tab <- flextable::flextable(data)
  tab <- flextable::theme_booktabs(tab)
  tab <- flextable::font(tab, fontname = "Arial", part = "all")
  tab <- flextable::fontsize(tab, size = 9, part = "all")
  tab <- flextable::bg(tab, bg = "#173F49", part = "header")
  tab <- flextable::color(tab, color = "white", part = "header")
  tab <- flextable::padding(tab, padding = 4, part = "all")
  tab <- flextable::autofit(tab)
  tab <- flextable::fit_to_width(tab, max_width = 6.8)
  tab <- flextable::set_table_properties(tab, layout = "autofit", width = 1,
    opts_word = list(split = FALSE, keep_with_next = FALSE))
  tab <- flextable::paginate(tab, init = FALSE, hdr_ftr = TRUE)
  flextable::body_add_flextable(doc, tab)
}

desktop_parameter_panels <- function(ps, digits, group_by = character()) {
  metrics <- c(n = "N (nonmissing)", missing = "Missing", n_positive = "N positive",
    mean = "Arithmetic mean", sd = "Sample SD", cv_percent = "Arithmetic CV (%)",
    geometric_mean = "Geometric mean", geometric_cv_percent = "Geometric CV (%)",
    median = "Median", min = "Minimum", max = "Maximum", p05 = "5th percentile", p95 = "95th percentile")
  panels <- list()
  for (code in unique(ps$PARAMCD)) for (unit in unique(ps$AVALU[ps$PARAMCD == code])) {
    rows <- which(ps$PARAMCD == code & ps$AVALU == unit)
    # Keep every group; partition wide tables, never truncate or pool units.
    chunks <- split(rows, ceiling(seq_along(rows) / 3))
    for (chunk in chunks) {
      tab <- data.frame(Statistic = unname(metrics), check.names = FALSE)
      for (i in seq_along(chunk)) {
        values <- unlist(ps[chunk[i], names(metrics)], use.names = FALSE)
        cells <- desktop_report_number(values, digits)
        cells[1:3] <- as.character(values[1:3])
        tab[[paste0("Group: ", pk_group_labels(ps$.GROUP[chunk[i]], group_by))]] <- cells
      }
      panels[[length(panels) + 1L]] <- list(title = paste(code, "|", unit), table = tab)
    }
  }
  panels
}

desktop_render_report <- function(run_folder, candidate, job) {
  manifest <- desktop_verify_run(run_folder, job$run_manifest_sha256, report = TRUE)
  desktop_check(!dir.exists(candidate), "E_IO", "Report staging directory already exists")
  dir.create(candidate, recursive = TRUE)
  complete <- FALSE
  on.exit(if (!complete) unlink(candidate, recursive = TRUE), add = TRUE)
  # Work from a verified copy, with no dependency on a live project's settings or CSV inputs.
  frozen <- file.path(candidate, "run", manifest$run_id)
  dir.create(frozen, recursive = TRUE)
  for (name in c(names(manifest$outputs), "manifest.json")) {
    target <- file.path(frozen, name)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    desktop_check(file.copy(file.path(run_folder, name), target), "E_IO", "Cannot copy Run artifacts")
  }
  desktop_verify_run(frozen, job$run_manifest_sha256, report = TRUE)
  spec <- desktop_read(file.path(frozen, "spec.json"))
  context <- jsonlite::fromJSON(file.path(frozen, "report_context.json"))
  desktop_check(identical(context$format, "pkapp-report-context-1"), "E_VALIDATION", "Unsupported report context")
  # Declare identifier columns as character: numeric-looking groups must retain their labels.
  read_summary <- function(name) {
    path <- file.path(frozen, "tables", name)
    columns <- names(utils::read.csv(path, nrows = 0, check.names = FALSE))
    labels <- intersect(c(".GROUP", ".FACET", "AVALU", "PARAMCD"), columns)
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, na.strings = "",
      colClasses = stats::setNames(rep("character", length(labels)), labels))
  }
  ps <- read_summary("parameter_summary.csv")
  cs <- read_summary("concentration_summary.csv")
  digits <- spec$report$digits
  group <- if (length(spec$conc_plot$group_by)) paste(unlist(spec$conc_plot$group_by), collapse = " × ") else "Overall"
  facet <- if (length(spec$conc_plot$facet_by)) paste(unlist(spec$conc_plot$facet_by), collapse = " × ") else "None"
  doc <- officer::read_docx()
  section <- officer::prop_section(page_size = officer::page_size(width = 8.27, height = 11.69),
    page_margins = officer::page_mar(top = .7, bottom = .7, left = .7, right = .7),
    header_default = officer::block_list(officer::fpar(officer::ftext("PK DESKTOP  /  SYNTHETIC DATA REVIEW", officer::fp_text(font.size = 9, color = "#173F49")))),
    footer_default = officer::block_list(officer::fpar(officer::ftext(paste(manifest$run_id, "· Page "), officer::fp_text(font.size = 8)), officer::run_word_field("PAGE"))))
  doc <- officer::body_set_default_section(doc, section)
  doc <- officer::set_doc_properties(doc, title = spec$report$title, creator = "PK Desktop",
    subject = paste("Synthetic PK review", manifest$run_id))
  doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(spec$report$title,
    officer::fp_text(font.size = 24, bold = TRUE, color = "#173F49"))))
  doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Synthetic PK data · Exploratory review",
    officer::fp_text(font.size = 12, color = "#52747A"))))
  doc <- officer::body_add_par(doc, "ADNCA contains concentrations. ADPP contains precomputed PK parameters; this report does not recompute NCA.")
  doc <- officer::body_add_par(doc, "Run and analysis conditions", style = "heading 1")
  summary <- data.frame(Item = c("Run", "Revision", "Study", "Analyte", "Run created (UTC)", "Report created (UTC)",
    "Subjects selected", "Concentration / parameter subjects", "Concentration / parameter rows", "Grouping", "Concentration panels", "Concentration profile",
    "Time axis", "Concentration scale", "Statistic / interval", "BLQ", "MDV", "Individual overlay", "Display precision"),
    Value = c(manifest$run_id, spec$revision, context$study, context$analyte, manifest$created_at, desktop_time(),
      context$counts$subjects, paste(context$counts$concentration_subjects, context$counts$parameter_subjects, sep = " / "),
      paste(context$counts$concentration_rows, context$counts$parameter_rows, sep = " / "), group, facet, spec$conc_plot$profile,
      paste(spec$conc_plot$x, "(h)"), spec$conc_plot$scale, paste(spec$conc_plot$statistic, spec$conc_plot$error, sep = " / "),
      spec$conc_plot$blq, if (spec$conc_plot$include_mdv) "Included" else "Excluded", if (spec$conc_plot$show_individual) "Shown" else "Hidden",
      paste(digits, "significant digits; counts are exact")), check.names = FALSE)
  doc <- desktop_report_table(doc, summary)
  doc <- officer::body_add_par(doc, "This is an unsigned local review artifact. Recorded actors are self-declared; no regulatory review or electronic signature is implied.")
  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Concentration profile", style = "heading 1")
  doc <- officer::body_add_img(doc, file.path(frozen, "plots", "concentration.png"), width = 6.8, height = 4.08)
  doc <- officer::body_add_par(doc, paste("Frozen Run figure. Grouping:", group, "| BLQ:", spec$conc_plot$blq))
  doc <- officer::body_add_par(doc, "Methods and data handling", style = "heading 1")
  for (note in unlist(context$notes)) doc <- officer::body_add_par(doc, note)
  doc <- officer::body_add_par(doc, "NE means not estimable or no eligible value. Rounded values in Word are for display only; all unrounded aggregate statistics are preserved in the accompanying CSV files.")
  for (panel in desktop_parameter_panels(ps, digits, unlist(spec$conc_plot$group_by))) {
    doc <- officer::body_add_break(doc)
    doc <- officer::body_add_par(doc, "ADPP parameter summary", style = "heading 1")
    doc <- officer::body_add_par(doc, panel$title, style = "heading 2")
    doc <- desktop_report_table(doc, panel$table)
    doc <- officer::body_add_par(doc, paste("Grouping:", group, ". Geometric statistics use positive values only. Percentile bounds are descriptive, not confidence limits."))
  }
  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Upstream methods and QC", style = "heading 1")
  doc <- desktop_report_table(doc, context$methods)
  doc <- officer::body_add_par(doc, "Counts after applying the selected subject population", style = "heading 2")
  doc <- desktop_report_table(doc, data.frame(Count = names(context$counts), Value = unlist(context$counts), row.names = NULL))
  doc <- officer::body_add_par(doc, "Excluded concentration rows are counted within the selected profile. mdv_rows and blq_rows describe the configured exclusions and can overlap; do not add them. Missing values remain records but are excluded from n.")
  warnings <- unlist(context$warnings)
  if (!length(warnings)) warnings <- "No upstream warnings recorded. This does not establish independent validation of the inputs."
  for (warning in warnings) doc <- officer::body_add_par(doc, warning)
  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Concentration summary appendix", style = "heading 1")
  doc <- officer::body_add_par(doc, paste("Center:", spec$conc_plot$statistic, "| Interval:", spec$conc_plot$error,
    "| Time:", spec$conc_plot$x, "(h). This table shows plot values. The CSV preserves all computed statistics."))
  if (!".FACET" %in% names(cs)) cs$.FACET <- "All panels"
  for (unit in unique(cs$AVALU)) for (g in unique(cs$.GROUP[cs$AVALU == unit])) for (panel in unique(cs$.FACET[cs$AVALU == unit & cs$.GROUP == g])) {
    s <- cs[cs$AVALU == unit & cs$.GROUP == g & cs$.FACET == panel, , drop = FALSE]
    tab <- data.frame(`Time (h)` = sprintf("%.17g", s$.TIME), N = s$n, Missing = s$missing,
      `N positive` = s$n_positive, Center = desktop_report_number(s$center, digits),
      Lower = desktop_report_number(s$lower, digits), Upper = desktop_report_number(s$upper, digits), check.names = FALSE)
    doc <- officer::body_add_par(doc, paste("Group:", g, "|", unit, "| Panel:", panel), style = "heading 2")
    doc <- desktop_report_table(doc, tab)
  }
  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Provenance", style = "heading 1")
  for (line in c(paste("Run:", manifest$run_id), paste("Analysis SHA-256:", manifest$analysis_hash),
    paste("Run manifest SHA-256:", job$run_manifest_sha256), paste("Spec file SHA-256:", manifest$spec_file_sha256),
    paste("Report job:", job$job_id), paste("Requested by:", job$triggered_by, "/", job$actor), paste("Reason:", job$reason),
    paste("Analysis R / platform:", manifest$environment$R, "/", manifest$environment$platform),
    paste("Report renderer:", desktop_version, "| R", getRversion()),
    paste("Input", names(manifest$input_hashes), "SHA-256:", unlist(manifest$input_hashes)))) doc <- officer::body_add_par(doc, line)
  doc <- officer::body_add_par(doc, "The report directory contains the frozen Run, aggregate CSV tables, figure, full QC notices, environment record and checksum manifests. Input hashes identify the project snapshot; raw individual records are not bundled with this report.")
  print(doc, target = file.path(candidate, "report.docx"))
  desktop_verify_run(frozen, job$run_manifest_sha256, report = TRUE)
  desktop_verify_run(run_folder, job$run_manifest_sha256, report = TRUE)
  desktop_require_code()
  writeLines(capture.output(sessionInfo()), file.path(candidate, "report_session_info.txt"))
  outputs <- list.files(candidate, recursive = TRUE)
  report_manifest <- list(format = "pkapp-word-report-1", job_id = job$job_id, run_id = manifest$run_id,
    created_at = desktop_time(), revision = manifest$revision, analysis_hash = manifest$analysis_hash,
    source_run_manifest_sha256 = job$run_manifest_sha256,
    request = job[c("project_id", "triggered_by", "actor", "reason", "requested_at", "execution_mode")],
    renderer = list(version = desktop_version, code_hashes = c(pk_code_hashes, desktop_code_hashes),
      packages = stats::setNames(lapply(sort(loadedNamespaces()), function(p) as.character(utils::packageVersion(p))), sort(loadedNamespaces()))),
    outputs = stats::setNames(lapply(file.path(candidate, outputs), pk_hash), outputs))
  desktop_atomic(report_manifest, file.path(candidate, "REPORT_MANIFEST.json"))
  complete <- TRUE
  list(report_manifest_sha256 = pk_hash(file.path(candidate, "REPORT_MANIFEST.json")))
}
