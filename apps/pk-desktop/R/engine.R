# A single R owner of settings, shared by teal and the HTTP adapter.
source(file.path(Sys.getenv("PK_REVIEW_ROOT", getwd()), "apps", "pk-review", "bootstrap.R"), local = TRUE)
desktop_code_files <- c(list.files(file.path(pk_root, "apps", "pk-desktop", "R"), full.names = TRUE),
                        file.path(pk_root, "apps", "pk-desktop", c("analysis-spec-m0.schema.json", "analysis-spec-v1.schema.json", "nca_mcp_contract.json", "renv.lock")))
desktop_code_hashes <- stats::setNames(lapply(desktop_code_files, pk_hash), substring(desktop_code_files, nchar(pk_root) + 2L))
desktop_version <- "0.61.1-alpha"
desktop_lock_hash <- pk_hash(file.path(pk_root, "apps/pk-desktop/renv.lock"))

desktop_error <- function(code, message) {
  stop(structure(list(message = message, call = NULL, code = code), class = c("pkapp_error", "error", "condition")))
}
desktop_check <- function(ok, code, message) if (!isTRUE(ok)) desktop_error(code, message)
desktop_time <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
desktop_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null", na = "null")
desktop_read <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
desktop_sort <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) {
      sort_names <- enc2utf8(names(x)); Encoding(sort_names) <- "UTF-8"
      x <- x[order(sort_names, method = "radix")]
    }
    x <- lapply(x, desktop_sort)
  }
  x
}
desktop_hash <- function(x) digest::digest(desktop_json(desktop_sort(x)), algo = "sha256", serialize = FALSE)
desktop_require_code <- function() {
  desktop_check(identical(unname(desktop_code_hashes), lapply(desktop_code_files, pk_hash)) &&
    identical(unname(pk_code_hashes), lapply(pk_code_files, pk_hash)), "E_CODE_CHANGED", "Code changed; restart before running")
}
desktop_atomic <- function(value, path) {
  tmp <- tempfile(".commit-", dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  writeLines(desktop_json(value), tmp, useBytes = TRUE)
  Sys.chmod(tmp, "0600")
  desktop_check(file.rename(tmp, path), "E_IO", "Could not commit project state")
}
desktop_analysis_hash <- function(spec) {
  # Paths, revisions, actor and timestamps are audit information, not analysis conditions.
  desktop_hash(list(spec_version = spec$spec_version, inputs = spec$data$hashes,
                    population = spec$population, conc_plot = spec$conc_plot,
                    param_summary = spec$param_summary, report = spec$report))
}
desktop_config <- function(spec) {
  p <- spec$conc_plot
  list(version = 2L, revision = spec$revision, title = spec$report$title,
       bundle = spec$data$bundle, input_hashes = spec$data$hashes,
       profile = p$profile, x = p$x, scale = p$scale, statistic = p$statistic,
       interval = p$error, show_individual = p$show_individual,
       group_by = if (length(p$group_by)) unlist(p$group_by, use.names = FALSE) else "NONE",
       facet_by = if (length(p$facet_by)) unlist(p$facet_by, use.names = FALSE) else character(),
       subjects = spec$population$subjects, parameters = spec$param_summary$paramcd,
       include_mdv = p$include_mdv, blq = p$blq, digits = spec$report$digits,
       include_listings = FALSE)
}
desktop_new <- function(project, source_bundle, channel = "ui", actor = "local user") {
  desktop_check(channel %in% c("ui", "mcp", "r") && is.character(actor) && length(actor) == 1 &&
    nzchar(trimws(actor)) && nchar(actor) <= 120, "E_VALIDATION", "Valid creation channel and actor are required")
  dir.create(project, recursive = TRUE, showWarnings = FALSE)
  project <- normalizePath(project, mustWork = TRUE)
  input <- pk_load_bundle(source_bundle) # Preserve the existing strict CSV/data/QC checks.
  destination <- file.path(project, "data", desktop_hash(input$hashes))
  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
    for (f in names(input$hashes)) {
      desktop_check(file.copy(file.path(source_bundle, f), file.path(destination, f)), "E_IO", "Cannot snapshot input")
    }
  }
  frozen <- pk_load_bundle(destination)
  desktop_check(identical(input$hashes, frozen$hashes), "E_INPUT_CHANGED", "Input changed during import")
  info <- pk_inspect(destination)
  info$group_columns <- intersect(pk_strata_columns(frozen$ADNCA), pk_strata_columns(frozen$ADPP))
  info$facet_columns <- pk_strata_columns(frozen$ADNCA)
  preferred <- intersect(c("CMAXSS", "AUCTAUSS", "TMAXSS", "CMAX", "AUC0TL", "TMAX"), info$eligible_parameters)
  selected <- if (length(preferred)) preferred else head(info$eligible_parameters, 3)
  desktop_check(length(selected) > 0, "E_VALIDATION", "No eligible ADPP parameter")
  engine <- new.env(parent = emptyenv())
  engine$project <- project
  engine$data <- frozen
  engine$info <- info
  engine$signal <- shiny::reactiveVal(0L)
  engine$data_signal <- shiny::reactiveVal(0L)
  engine$runs_signal <- shiny::reactiveVal(0L)
  engine$last_result <- NULL
  engine$schema <- jsonvalidate::json_validator(file.path(pk_root, "apps", "pk-desktop", "analysis-spec-v1.schema.json"), engine = "ajv")
  state_path <- file.path(project, "state.json")
  if (file.exists(state_path)) {
    state <- desktop_read(state_path)
    engine$spec <- state$spec
    engine$audit <- state$audit
    desktop_check(identical(engine$spec$data$hashes, frozen$hashes), "E_INPUT_CHANGED", "This project belongs to different input data; create a new project")
    desktop_check(identical(engine$spec$data$bundle, destination), "E_VALIDATION", "Project data location changed")
    desktop_check(length(engine$audit) > 0 && identical(tail(engine$audit, 1)[[1]]$after, engine$spec), "E_VALIDATION", "Audit and saved settings disagree")
  } else {
    engine$spec <- list(spec_version = "1.3-alpha", project_id = desktop_hash(list(project, desktop_time(), tempfile())),
      revision = 1L, updated_by = channel, updated_at = desktop_time(),
      data = list(bundle = destination, hashes = frozen$hashes), population = list(subjects = list()),
      conc_plot = list(profile = "all", x = "TIME_H", scale = "linear", statistic = "mean", error = "none",
                       group_by = list(), facet_by = list(), show_individual = FALSE, include_mdv = FALSE, blq = "exclude"),
      param_summary = list(paramcd = as.list(selected)), report = list(title = "PK simulation review", digits = 3L))
    engine$audit <- list(list(at = desktop_time(), channel = channel, actor = actor, reason = "Create project", before = NULL, after = engine$spec))
  }
  desktop_validate(engine, engine$spec)
  desktop_atomic(list(spec = engine$spec, audit = engine$audit), state_path)
  engine$signal(engine$spec$revision)
  desktop_jobs_init(engine)
  desktop_tables_init(engine)
  desktop_modules_init(engine)
  engine
}
desktop_validate <- function(engine, spec) {
  desktop_check(engine$schema(desktop_json(spec)), "E_VALIDATION", "AnalysisSpec does not match the JSON schema")
  tryCatch(pk_validate_config(desktop_config(spec), engine$data), error = function(e) desktop_error("E_VALIDATION", conditionMessage(e)))
  invisible(spec)
}
desktop_revision <- function(engine, revision, project_id) {
  if (identical(engine$mode, "nca")) return(desktop_nca_revision(engine, revision, project_id))
  desktop_check(identical(project_id, engine$spec$project_id), "E_PROJECT_CONFLICT", "Active project changed; read status again")
  desktop_check(is.numeric(revision) && length(revision) == 1 && identical(as.numeric(revision), as.numeric(engine$spec$revision)),
                "E_REVISION_CONFLICT", "Settings changed; fetch the current spec before retrying")
}
desktop_apply <- function(engine, section, values, base_revision, project_id, channel, actor, reason) {
  desktop_revision(engine, base_revision, project_id)
  desktop_check(channel %in% c("ui", "mcp", "r"), "E_VALIDATION", "Invalid channel")
  desktop_check(is.character(actor) && length(actor) == 1 && nzchar(trimws(actor)) && nchar(actor) <= 120 &&
    is.character(reason) && length(reason) == 1 && nzchar(trimws(reason)) && nchar(reason) <= 1000, "E_VALIDATION", "Actor and concrete change reason are required")
  desktop_check(section %in% c("population", "conc_plot", "param_summary", "report"), "E_VALIDATION", "Unknown editable section")
  desktop_check(is.list(values) && length(values) > 0 && !is.null(names(values)) && !anyDuplicated(names(values)), "E_VALIDATION", "Changes must be an object")
  next_spec <- engine$spec
  next_spec$spec_version <- "1.3-alpha"
  if (is.null(next_spec$conc_plot$facet_by)) next_spec$conc_plot$facet_by <- list()
  for (key in names(values)) next_spec[[section]][key] <- values[key]
  next_spec$revision <- next_spec$revision + 1L
  next_spec$updated_by <- channel
  next_spec$updated_at <- desktop_time()
  desktop_validate(engine, next_spec)
  audit <- c(engine$audit, list(list(at = next_spec$updated_at, channel = channel, actor = actor, reason = reason,
                                   before = engine$spec, after = next_spec)))
  # One atomic envelope commits spec and history together; memory changes only after success.
  desktop_atomic(list(spec = next_spec, audit = audit), file.path(engine$project, "state.json"))
  engine$spec <- next_spec
  engine$audit <- audit
  engine$signal(next_spec$revision)
  list(spec = next_spec, analysis_hash = desktop_analysis_hash(next_spec))
}
desktop_run <- function(engine, base_revision, project_id, channel, snapshot = NULL, replay = NULL) {
  desktop_revision(engine, base_revision, project_id)
  desktop_check(channel %in% c("ui", "mcp", "r"), "E_VALIDATION", "Invalid run channel")
  desktop_require_code()
  spec <- snapshot %||% engine$spec
  current <- pk_load_bundle(spec$data$bundle)
  desktop_check(identical(current$hashes, spec$data$hashes), "E_INPUT_CHANGED", "Input snapshot changed")
  result <- pk_analyse(current, desktop_config(spec))
  runs <- file.path(engine$project, "runs")
  dir.create(runs, showWarnings = FALSE)
  run_id <- paste0("run-", format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-", substr(desktop_hash(list(tempfile(), desktop_time())), 1, 12))
  stage <- tempfile(".run-", runs)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  dir.create(file.path(stage, "tables"))
  desktop_atomic(spec, file.path(stage, "spec.json"))
  pk_write_csv(result$concentration_summary, file.path(stage, "tables", "concentration_summary.csv"))
  pk_write_csv(result$parameter_summary, file.path(stage, "tables", "parameter_summary.csv"))
  # MCP's result has aggregate values only. Detailed upstream messages remain in the local UI.
  public <- list(run_id = run_id, revision = spec$revision, analysis_hash = desktop_analysis_hash(spec),
                 counts = result$counts, concentration_summary = result$concentration_summary,
                 parameter_summary = result$parameter_summary,
                 warnings = as.list(if (length(result$warnings)) "Upstream QC notices are present; inspect the local application." else character()),
                 notes = as.list(result$notes))
  desktop_atomic(public, file.path(stage, "result.json"))
  # Capture the plot and local QC now; report workers never reanalyse current data.
  desktop_snapshot_report(result, current, stage)
  writeLines(capture.output(sessionInfo()), file.path(stage, "session_info.txt"))
  outputs <- list.files(stage, recursive = TRUE)
  manifest <- list(format = "pkapp-run-2", run_id = run_id, created_at = desktop_time(), triggered_by = channel,
                   revision = spec$revision, analysis_hash = public$analysis_hash,
                   spec_file_sha256 = pk_hash(file.path(stage, "spec.json")), input_hashes = current$hashes,
                   environment = list(R = as.character(getRversion()), platform = R.version$platform,
                     locale = Sys.getlocale(), app_version = desktop_version, lockfile_sha256 = desktop_lock_hash, code_hashes = c(pk_code_hashes, desktop_code_hashes),
                     packages = stats::setNames(lapply(sort(loadedNamespaces()), function(p) as.character(utils::packageVersion(p))), sort(loadedNamespaces()))),
                   outputs = stats::setNames(lapply(file.path(stage, outputs), pk_hash), outputs))
  if (!is.null(replay)) {
    desktop_verify_run(replay$folder, replay$manifest_hash)
    for (file in c("tables/concentration_summary.csv", "tables/parameter_summary.csv"))
      desktop_check(identical(pk_hash(file.path(stage, file)), pk_hash(file.path(replay$folder, file))),
        "E_REPLAY_DIFFERENCE", paste("Replayed table differs:", file))
    desktop_check(identical(public$analysis_hash, replay$analysis_hash), "E_REPLAY_DIFFERENCE", "Analysis hash differs")
    manifest$replay_of <- basename(replay$folder)
    manifest$replay_manifest_sha256 <- replay$manifest_hash
  }
  desktop_atomic(manifest, file.path(stage, "manifest.json"))
  desktop_check(file.rename(stage, file.path(runs, run_id)), "E_IO", "Cannot publish run")
  engine$last_result <- result
  engine$runs_signal(shiny::isolate(engine$runs_signal()) + 1L)
  public
}
desktop_run_folder <- function(engine, run_id) {
  desktop_check(is.character(run_id) && length(run_id) == 1 && grepl("^run-[0-9]{8}T[0-9]{6}-[a-f0-9]{12}$", run_id), "E_VALIDATION", "Invalid run ID")
  file.path(engine$project, "runs", run_id)
}
desktop_verify_run <- function(folder, expected_hash = NULL, report = FALSE) {
  desktop_check(file.exists(file.path(folder, "manifest.json")), "E_VALIDATION", "Unknown completed run")
  if (!is.null(expected_hash)) desktop_check(identical(pk_hash(file.path(folder, "manifest.json")), expected_hash),
    "E_INPUT_CHANGED", "Run manifest changed")
  manifest <- desktop_read(file.path(folder, "manifest.json"))
  required <- c("spec.json", "result.json", "tables/concentration_summary.csv", "tables/parameter_summary.csv", "session_info.txt")
  extra <- c("report_context.json", "plots/concentration.png")
  desktop_check(manifest$format %in% c("pkapp-m0-1", "pkapp-run-2") && identical(manifest$run_id, basename(folder)),
    "E_VALIDATION", "Unsupported run manifest")
  desktop_check(all(required %in% names(manifest$outputs)) && all(names(manifest$outputs) %in% c(required, extra)),
    "E_VALIDATION", "Incomplete run manifest")
  if (report) desktop_check(all(extra %in% names(manifest$outputs)), "E_REPORT_UNAVAILABLE",
    "This older run has no frozen report artifacts; save a new run to enable Word reporting")
  desktop_verify_files(folder, manifest$outputs)
  manifest
}
desktop_verify_files <- function(folder, hashes) {
  desktop_check(is.list(hashes) && length(hashes) > 0 && !is.null(names(hashes)) && !anyDuplicated(names(hashes)),
    "E_VALIDATION", "Invalid artifact manifest")
  for (file in names(hashes)) desktop_check(!grepl("(^/|\\.\\.|\\\\)", file) && file.exists(file.path(folder, file)) &&
    is.character(hashes[[file]]) && length(hashes[[file]]) == 1 && grepl("^[a-f0-9]{64}$", hashes[[file]]) &&
    identical(pk_hash(file.path(folder, file)), hashes[[file]]), "E_INPUT_CHANGED", "Artifact checksum changed")
  invisible(TRUE)
}
desktop_result <- function(engine, run_id) {
  folder <- desktop_run_folder(engine, run_id)
  desktop_verify_run(folder)
  desktop_read(file.path(folder, "result.json"))
}
desktop_replay_run <- function(engine, run_id, base_revision, project_id, channel) {
  desktop_revision(engine, base_revision, project_id)
  folder <- desktop_run_folder(engine, run_id)
  manifest <- desktop_verify_run(folder)
  desktop_check(identical(manifest$environment$code_hashes, c(pk_code_hashes, desktop_code_hashes)),
    "E_CODE_CHANGED", "Saved Run uses different code; open its original release to reproduce it")
  spec <- desktop_read(file.path(folder, "spec.json"))
  desktop_check(identical(spec$project_id, project_id) && engine$schema(desktop_json(spec)) &&
    identical(pk_hash(file.path(folder, "spec.json")), manifest$spec_file_sha256) &&
    identical(spec$data$hashes, manifest$input_hashes), "E_INPUT_CHANGED", "Saved Run specification differs")
  desktop_allowed_bundle(engine, spec$data$bundle)
  desktop_run(engine, base_revision, project_id, channel, snapshot = spec,
    replay = list(folder = folder, manifest_hash = pk_hash(file.path(folder, "manifest.json")), analysis_hash = manifest$analysis_hash))
}
source(file.path(pk_root, "apps", "pk-desktop", "R", "report.R"), local = TRUE)
source(file.path(pk_root, "apps", "pk-desktop", "R", "jobs.R"), local = TRUE)
source(file.path(pk_root, "apps", "pk-desktop", "R", "session.R"), local = TRUE)
desktop_runs <- function(engine) {
  paths <- list.files(file.path(engine$project, "runs"), pattern = "^run-", full.names = TRUE)
  lapply(rev(sort(paths)), function(p) {
    m <- desktop_read(file.path(p, "manifest.json"))
    m[c("run_id", "revision", "created_at", "triggered_by", "analysis_hash")]
  })
}
for (file in c("dependencies.R", "diagnostics.R", "i18n.R", "ui_helpers.R", "workbench_styles.R", "pk_preprocessing.R", "preparation.R", "pk_assembly.R", "assembly_context.R", "merge.R", "grid.R", "import.R", "data_api.R", "workbench.R", "handling.R", "module_registry.R", "nca_terminal.R", "nca_analysis.R", "nca_adpp.R", "nca_owner.R", "nca_jobs.R", "assembly_ui.R", "nca_annotation.R", "nca_covariates.R", "nca_derived.R", "annotation_ui.R", "nca_derived_ui.R", "nca_explore.R", "nca.R")) {
  source(file.path(pk_root, "apps", "pk-desktop", "R", file), local = TRUE)
}
