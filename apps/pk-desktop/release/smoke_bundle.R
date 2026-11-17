# R-only release smoke. GUI, Tauri shutdown, workers, signing and another Mac
# remain separate acceptance gates. Run only against synthetic shipped fixtures.
(function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 4L || length(args) %% 2L || any(!args[seq.int(1L, length(args), by = 2L)] %in% c("--source", "--output", "--bundle-root"))) {
    stop("Usage: smoke_bundle.R --source SOURCE --output NEW_JSON [--bundle-root APP]", call. = FALSE)
  }
  options <- stats::setNames(as.list(args[seq.int(2L, length(args), by = 2L)]), args[seq.int(1L, length(args), by = 2L)])
  if (anyDuplicated(names(options)) || is.null(options[["--source"]]) || is.null(options[["--output"]])) stop("Use each option once; --source and --output are required.", call. = FALSE)
  source_root <- normalizePath(options[["--source"]], mustWork = TRUE)
  output <- options[["--output"]]
  if (file.exists(output)) stop("Choose a new JSON receipt path; existing evidence is not overwritten.", call. = FALSE)
  if (!dir.exists(dirname(output))) stop("JSON receipt directory must exist.", call. = FALSE)
  output <- file.path(normalizePath(dirname(output), mustWork = TRUE), basename(output))
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite must be installed before the smoke test.", call. = FALSE)
  now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  bundle <- options[["--bundle-root"]]
  receipt <- list(format = "pk-desktop-bundle-smoke-1", status = "running", started_at = now(),
    execution_scope = if (is.null(bundle)) "host_r_only" else "bundled_r", source_root = source_root,
    host = as.list(Sys.info()[c("sysname", "release", "machine")]), R = as.character(getRversion()),
    checks = list(), files = list(), not_tested = as.list(c("Mac GUI", "Tauri shutdown", "asynchronous Word worker", "another Mac", "codesign", "notarization")))
  write_receipt <- function() {
    temporary <- tempfile(".smoke-", dirname(output))
    on.exit(unlink(temporary), add = TRUE)
    writeLines(jsonlite::toJSON(receipt, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null"), temporary, useBytes = TRUE)
    if (!file.rename(temporary, output)) stop("Cannot publish smoke JSON receipt.", call. = FALSE)
  }
  current <- NULL
  step <- function(name, expr) {
    current <<- name
    receipt$checks[[name]] <<- list(status = "running", started_at = now())
    write_receipt()
    evidence <- force(expr)
    receipt$checks[[name]] <<- list(status = "passed", evidence = evidence, finished_at = now())
    write_receipt()
    message("Bundle R smoke: ", name, " passed")
    evidence
  }
  api <- new.env(parent = globalenv())
  project <- NULL
  scratch <- tempfile("pk-bundle-smoke-")
  on.exit({
    if (!is.null(project) && !isTRUE(project$closed)) try(api$pkapp_close(project), silent = TRUE)
    unlink(scratch, recursive = TRUE)
  }, add = TRUE)
  status <- tryCatch({
    step("runtime_isolation", {
      home <- normalizePath(R.home(), mustWork = TRUE)
      libraries <- normalizePath(.libPaths(), mustWork = TRUE)
      if (!is.null(bundle)) {
        bundle <- normalizePath(bundle, mustWork = TRUE)
        stopifnot(startsWith(source_root, paste0(bundle, "/")), startsWith(home, paste0(bundle, "/")),
          length(libraries) > 0L, all(startsWith(libraries, paste0(bundle, "/"))))
      }
      list(verified = !is.null(bundle), bundle_root = bundle, r_home = home, library_paths = as.list(libraries))
    })
    step("source_api", {
      source(file.path(source_root, "apps/pk-desktop/pkapp.R"), local = api)
      core <- environment(api$pkapp_open)
      receipt$app_version <- core$desktop_version
      hashes <- c(core$pk_code_hashes, core$desktop_code_hashes)
      smoke <- "apps/pk-desktop/release/smoke_bundle.R"
      hashes[[smoke]] <- core$pk_hash(file.path(source_root, smoke))
      receipt$files <- stats::setNames(hashes, paste0("source/", names(hashes)))
      if (!is.null(bundle)) receipt$files[["runtime/bin/Rscript"]] <- core$pk_hash(file.path(bundle, "Contents/Resources/runtime/bin/Rscript"))
      dir.create(scratch)
      project <- api$pkapp_open(file.path(scratch, "project"))
      source_bundle <- file.path(source_root, "outputs/pk-review/bundles/reviewed-repeated")
      input_concentration <- api$pkapp_read_table(file.path(source_bundle, "ADNCA.csv"))
      input_parameters <- api$pkapp_read_table(file.path(source_bundle, "ADPP.csv"))
      concentration <- project$engine$data$ADNCA
      parameters <- project$engine$data$ADPP
      stopifnot(identical(concentration$USUBJID, input_concentration$USUBJID), identical(parameters$USUBJID, input_parameters$USUBJID))
      for (name in c("ADNCA.csv", "ADPP.csv", "BUNDLE.json")) {
        path <- file.path(source_bundle, name)
        if (file.exists(path)) receipt$files[[paste0("source/outputs/pk-review/bundles/reviewed-repeated/", name)]] <- core$pk_hash(path)
      }
      stopifnot(nrow(concentration) > 0L, nrow(parameters) > 0L)
      list(ADNCA_rows = nrow(concentration), ADPP_rows = nrow(parameters), app_version = core$desktop_version)
    })
    step("prepare", {
      steps <- list(list(op = "filter", column = "SEX", operator = "eq", values = list("F")))
      saved <- api$pkapp_save_preparation(project, concentration, steps, "Release smoke: female subjects")
      prepared <- api$pkapp_table(project, saved$id)
      expected <- concentration[!is.na(concentration$SEX) & concentration$SEX == "F", , drop = FALSE]
      rownames(expected) <- NULL
      stopifnot(identical(prepared, expected), nrow(prepared) > 0L, nrow(prepared) < nrow(concentration))
      replay <- api$pkapp_replay(file.path(project$owner$project, "prepared", saved$id))
      stopifnot(identical(replay$data, prepared))
      list(input_rows = nrow(concentration), result_rows = nrow(prepared), exact_replay = TRUE, ids_preserved = identical(prepared$USUBJID, expected$USUBJID))
    })
    step("join_replay", {
      right <- api$pkapp_prepare(parameters, list(list(op = "filter", column = "PARAMCD", operator = "eq", values = list("CMAXSS"))))$data
      keys <- c("STUDYID", "USUBJID")
      added <- c("PARAMCD", "AVAL", "AVALU")
      joined <- api$pkapp_merge(prepared, right, keys, added, right_name = "ADPP")
      stopifnot(identical(joined$data[names(prepared)], prepared), joined$counts[["matched"]] == nrow(prepared), joined$counts[["unmatched"]] == 0L)
      saved_join <- core$desktop_table_save(project$engine, "Release smoke: joined", list(left = prepared, right = right),
        list(kind = "join", by = as.list(keys), columns = as.list(added), join = "left", right_name = "ADPP"), "r", "release smoke")
      joined_folder <- file.path(project$owner$project, "prepared", saved_join$id)
      stopifnot(identical(api$pkapp_replay(joined_folder)$data, joined$data), identical(api$pkapp_table(project, saved_join$id), joined$data))
      csv_path <- file.path(joined_folder, "result.csv")
      roundtrip <- api$pkapp_read_table(csv_path)
      for (name in names(joined$data)) {
        if (is.numeric(joined$data[[name]])) roundtrip[[name]] <- core$pk_number(roundtrip[[name]], name, allow_na = TRUE)
        if (is.integer(joined$data[[name]])) roundtrip[[name]] <- as.integer(roundtrip[[name]])
        if (is.logical(joined$data[[name]])) roundtrip[[name]] <- as.logical(roundtrip[[name]])
      }
      stopifnot(identical(roundtrip, joined$data))
      list(counts = as.list(joined$counts), result_columns = ncol(joined$data), exact_replay = TRUE, exact_csv_roundtrip = TRUE, csv_sha256 = core$pk_hash(csv_path))
    })
    step("analysis_run_replay", {
      # Apply the prepared/joined concentration table explicitly. ADPP keeps all
      # original parameter values for the same subjects; no PK values are computed here.
      subjects <- unique(joined$data$USUBJID)
      report_parameters <- api$pkapp_prepare(parameters, list(list(op = "filter", column = "USUBJID", operator = "in", values = as.list(subjects))))$data
      parameter_csv <- file.path(scratch, "report-ADPP.csv")
      core$pk_write_csv(report_parameters, parameter_csv)
      imported <- api$pkapp_import_bundle(csv_path, parameter_csv, file.path(project$owner$project, "smoke-analysis-input"))
      api$pkapp_load_data(project, imported, api$pkapp_spec(project)$revision, "Release smoke prepared input")
      run <- api$pkapp_run(project)
      before <- api$pkapp_spec(project)
      replayed <- api$pkapp_replay_run(project, run$run_id)
      stopifnot(identical(api$pkapp_spec(project), before), identical(run$analysis_hash, replayed$analysis_hash))
      first <- api$pkapp_run_manifest(project, run$run_id)
      second <- api$pkapp_run_manifest(project, replayed$run_id)
      for (file in c("tables/concentration_summary.csv", "tables/parameter_summary.csv")) stopifnot(identical(first$outputs[[file]], second$outputs[[file]]))
      list(ADNCA_rows = nrow(project$engine$data$ADNCA), ADPP_rows = nrow(project$engine$data$ADPP), analysis_hash = run$analysis_hash, exact_summary_csv_replay = TRUE)
    })
    step("sync_word", {
      job <- api$pkapp_report_sync(project, run$run_id, "Bundled runtime release smoke", actor = "release smoke")
      checked <- api$pkapp_job(project, job$job_id)
      stopifnot(identical(checked$status, "succeeded"), identical(checked$execution_mode, "in_process"),
        identical(checked$run_id, run$run_id), identical(core$pk_hash(checked$artifacts$docx), checked$artifacts$docx_sha256))
      entries <- utils::unzip(checked$artifacts$docx, list = TRUE)
      stopifnot("word/document.xml" %in% entries$Name, any(grepl("^word/media/", entries$Name) & entries$Length > 0), file.info(checked$artifacts$docx)$size > 0)
      list(execution_mode = checked$execution_mode, frozen_run = checked$run_id, docx_bytes = unname(file.info(checked$artifacts$docx)$size), docx_sha256 = checked$artifacts$docx_sha256, document_and_embedded_image_present = TRUE)
    })
    step("cleanup", {
      owned <- project$owner$project
      api$pkapp_close(project)
      stopifnot(isTRUE(project$closed), !dir.exists(file.path(owned, ".pkapp-owner")))
      unlink(scratch, recursive = TRUE)
      stopifnot(!dir.exists(scratch))
      list(temporary_project_removed = TRUE, project_ownership_released = TRUE)
    })
    receipt$status <- "passed"
    0L
  }, error = function(e) {
    receipt$status <<- "failed"
    receipt$error <<- list(step = current, message = conditionMessage(e))
    if (!is.null(current)) receipt$checks[[current]] <<- list(status = "failed", error = conditionMessage(e), finished_at = now())
    message("Bundle R smoke failed: ", conditionMessage(e))
    1L
  })
  receipt$finished_at <- now()
  write_receipt()
  # Cleanup handlers execute before the process returns its exit code.
  status
})() -> smoke_exit_status
quit(save = "no", status = smoke_exit_status, runLast = FALSE)
