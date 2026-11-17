# Run with Rscript, or source(file.choose()) from any working directory.
# No installation, existing-project changes, GUI acceptance, or signing occurs here.
(function() {
  frames <- vapply(sys.frames(), function(frame) {
    if (is.character(frame$ofile) && length(frame$ofile) == 1L) frame$ofile else ""
  }, character(1))
  sourced <- any(nzchar(frames))
  cli <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  this_file <- if (sourced) tail(frames[nzchar(frames)], 1L) else if (length(cli) == 1L) cli else stop("check_macos.R の場所を特定できません。")
  this_file <- normalizePath(this_file, mustWork = TRUE)
  desktop_dir <- dirname(this_file)
  root <- normalizePath(file.path(desktop_dir, "../.."), mustWork = TRUE)
  old_libraries <- .libPaths()
  on.exit(.libPaths(old_libraries), add = TRUE)
  local_library <- file.path(root, ".runtime/R-library")
  if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
  now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  runtime <- function() list(version = R.version.string, platform = R.version$platform,
    os = unname(Sys.info()["sysname"]), release = unname(Sys.info()["release"]), architecture = unname(Sys.info()["machine"]))
  # Keep a usable failure report even when jsonlite itself is missing.
  json <- function(value) {
    quote_json <- function(text) {
      code <- utf8ToInt(enc2utf8(text))
      paste0('"', paste(vapply(code, function(x) {
        if (x == 34L) '\\"' else if (x == 92L) '\\\\' else if (x < 32L) sprintf("\\u%04x", x) else intToUtf8(x)
      }, character(1)), collapse = ""), '"')
    }
    if (is.null(value)) return("null")
    if (is.data.frame(value)) value <- lapply(seq_len(nrow(value)), function(i) as.list(value[i, , drop = FALSE]))
    if (is.list(value) || length(value) != 1L || !is.null(names(value))) {
      named <- !is.null(names(value)) && all(nzchar(names(value)))
      items <- vapply(seq_along(value), function(i) paste0(if (named) paste0(quote_json(names(value)[i]), ":") else "", json(value[[i]])), character(1))
      return(paste0(if (named) "{" else "[", paste(items, collapse = ","), if (named) "}" else "]"))
    }
    if (is.na(value)) return("null")
    if (is.logical(value)) return(if (value) "true" else "false")
    if (is.numeric(value)) return(if (is.finite(value)) format(value, digits = 16L, scientific = FALSE, trim = TRUE) else "null")
    quote_json(as.character(value))
  }
  write_json <- function(value, path) {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::write_json(value, path, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
    } else writeLines(json(value), path, useBytes = TRUE)
  }
  checksum <- function(path) {
    if (!file.exists(path) || !requireNamespace("digest", quietly = TRUE)) return(NA_character_)
    digest::digest(file = path, algo = "sha256")
  }
  version <- function() {
    lines <- readLines(file.path(desktop_dir, "R/engine.R"), warn = FALSE)
    line <- grep('^desktop_version <- "', lines, value = TRUE)
    if (length(line) != 1L) return(NA_character_)
    sub('^desktop_version <- "([^"]+)".*$', '\\1', line)
  }
  baseline <- c(grid = 6L, workbench = 5L, import_mapping_ui = 7L, app_shell = 3L,
    preparation = 7L, merge = 6L, preparation_ui = 4L, merge_ui = 4L,
    synchronization = 1L, extensions = 5L, engine = 7L, report_sync = 4L, handling = 5L, module_registry = 3L, pknca = 2L,
    nca = 7L, nca_ui = 7L, pk_preprocessing = 6L, nca_scientific = 10L, nca_review = 3L, nca011_core = 9L, nca011_preparation = 4L, nca011_ui = 5L, nca012_core = 12L, nca012_preparation = 8L, nca012_ui = 4L, nca013_serialization = 27L, nca013_core = 11L, nca013_flow = 9L, nca013_ui = 10L, nca014_diagnostics = 11L, nca014_ui = 4L)
  phases <- c(list(diagnostic = list(id = "diagnostic", file = "apps/pk-desktop/check_macos.R", groups = 0L, worker = TRUE)),
    lapply(names(baseline), function(id) list(id = id, file = paste0("apps/pk-desktop/tests/", id, ".R"), groups = unname(baseline[id]), worker = FALSE)),
    list(report_workers = list(id = "report_workers", file = "apps/pk-desktop/tests/reports.R", groups = 7L, worker = TRUE)))
  names(phases) <- vapply(phases, `[[`, character(1), "id")
  manual <- list(mac_gui = list(status = "pending", scope = "Mac画面で取込→前処理→結合→CSVを確認"),
    tauri_shutdown = list(status = "pending", scope = "Tauri終了時に所有するRが停止することを確認"),
    clean_mac = list(status = "pending", scope = "R未導入の別Macで配布版を確認"),
    signing_notarization = list(status = "pending", scope = "署名・公証と配布版の検証"),
    codex_live_connection = list(status = "pending", scope = "起動中アプリへのCodex実機接続を確認"))
  find_rscript <- function() {
    explicit <- Sys.getenv("PK_RSCRIPT", "")
    candidates <- if (nzchar(explicit)) explicit else c(file.path(R.home("bin"), "Rscript"),
      file.path(R.home("bin"), "Rscript.exe"), unname(Sys.which("Rscript")))
    for (candidate in candidates[nzchar(candidates)]) {
      if (!grepl("/|\\\\", candidate)) candidate <- unname(Sys.which(candidate))
      if (nzchar(candidate) && file.exists(candidate) && !isTRUE(file.info(candidate)$isdir) && file.access(candidate, 1L) == 0L)
        return(normalizePath(candidate, mustWork = TRUE))
    }
    stop("Rscriptが見つかりません。PK_RSCRIPTに実行ファイルのフルパスを指定してください。", call. = FALSE)
  }
  # Each child owns only its temporary test projects. Existing applications are untouched.
  child_phase <- function(id, receipt_path) {
    phase <- phases[[id]]
    if (is.null(phase)) stop("Unknown phase: ", id)
    setwd(root)
    result <- list(id = id, status = "running", started_at = now(), runtime = runtime(),
      source_sha256 = checksum(file.path(root, phase$file)), groups_expected = phase$groups,
      groups_run = 0L, groups_passed = 0L, groups_skipped = 0L, groups_failed = 0L, warnings = 0L, tests = list())
    caught <- NULL
    tryCatch({
      if (identical(id, "diagnostic")) {
        scope <- new.env(parent = globalenv())
        source(file.path(desktop_dir, "pkapp.R"), local = scope)
        dependencies <- scope$pkapp_dependencies()
        core <- environment(scope$pkapp_open)
        dependencies <- rbind(dependencies, core$desktop_dependency_status(c("testthat", "xml2", "withr", "nanoparquet", "bit64")))
        print(dependencies, row.names = FALSE)
        worker <- scope$pkapp_worker_check()
        result$details <- list(dependencies = dependencies, worker = worker)
        write_json(result$details, file.path(dirname(receipt_path), "diagnostic.json"))
        optional <- dependencies$package %in% c("nanoparquet", "bit64")
        if (!all(dependencies$available[!optional]) || !isTRUE(worker$available)) stop("依存パッケージまたは通常ワーカーの診断に失敗しました。詳細はdiagnostic.jsonを確認してください。")
        if (!all(dependencies$available[optional])) {
          result$status <- "incomplete"
          stop("Parquet試験用のnanoparquetまたはbit64がないため、完全な確認は未完了です。自動インストールは行いません。")
        }
      } else {
        if (!requireNamespace("testthat", quietly = TRUE)) stop("testthatがありません。依存パッケージを別途準備してください。")
        reporter <- testthat::ListReporter$new()
        testthat::with_reporter(reporter, {
          reporter$start_file(phase$file)
          source(file.path(root, phase$file), local = new.env(parent = globalenv()))
          reporter$end_file()
        })
        results <- reporter$get_results()
        result$tests <- lapply(results, function(test) {
          failed <- vapply(test$results, function(x) inherits(x, "expectation_failure") || inherits(x, "expectation_error"), logical(1))
          skipped <- vapply(test$results, inherits, logical(1), "expectation_skip")
          warnings <- vapply(test$results, inherits, logical(1), "expectation_warning")
          state <- if (any(failed)) "failed" else if (any(skipped)) "skipped" else "passed"
          messages <- vapply(test$results[failed | skipped | warnings], conditionMessage, character(1))
          cat(sprintf("[%s] %s\n", state, test$test))
          if (length(messages)) cat(paste(messages, collapse = "\n"), "\n")
          list(name = test$test, status = state, warnings = sum(warnings), messages = as.list(messages))
        })
        states <- vapply(result$tests, `[[`, character(1), "status")
        result$groups_run <- length(states)
        result$groups_passed <- sum(states == "passed")
        result$groups_skipped <- sum(states == "skipped")
        result$groups_failed <- sum(states == "failed")
        result$warnings <- sum(vapply(result$tests, `[[`, numeric(1), "warnings"))
        if (result$groups_failed > 0L) stop("テストの失敗があります。")
        if (result$groups_skipped > 0L || result$groups_run != phase$groups) {
          result$status <- "incomplete"
          stop("未実行・スキップ、または予定と異なるテスト数があります。")
        }
      }
      result$status <- "passed"
    }, error = function(e) { caught <<- conditionMessage(e) }, interrupt = function(e) { caught <<- "中断されました。"; result$status <<- "interrupted" })
    if (!is.null(caught)) {
      if (identical(result$status, "running")) result$status <- "failed"
      result$error <- caught
      cat("ERROR: ", caught, "\n", sep = "")
    }
    result$finished_at <- now()
    saveRDS(result, receipt_path)
    write_json(result, sub("\\.rds$", ".json", receipt_path))
    if (identical(result$status, "passed")) 0L else if (identical(result$status, "interrupted")) 130L else 1L
  }
  run_phase <- function(phase, rscript, output_dir, timeout = 600L, execute = system2) {
    logfile <- file.path(output_dir, paste0(phase$id, ".log"))
    receipt <- file.path(output_dir, paste0(phase$id, ".rds"))
    record <- list(id = phase$id, status = "running", file = phase$file, groups_expected = phase$groups,
      source_sha256 = checksum(file.path(root, phase$file)), log = basename(logfile), started_at = now())
    interrupted <- FALSE
    error <- NULL
    status <- tryCatch(execute(rscript, args = c("--vanilla", shQuote(this_file), "--phase", shQuote(phase$id), "--phase-result", shQuote(receipt)),
      stdout = logfile, stderr = logfile, timeout = timeout),
      error = function(e) { error <<- conditionMessage(e); 1L },
      interrupt = function(e) { interrupted <<- TRUE; 130L })
    record$exit_code <- as.integer(status)
    record$finished_at <- now()
    if (interrupted) record$status <- "interrupted" else {
      child <- if (file.exists(receipt)) tryCatch(readRDS(receipt), error = function(e) NULL) else NULL
      verified <- is.list(child) && identical(child$id, phase$id) && identical(child$source_sha256, record$source_sha256) &&
        child$status %in% c("passed", "failed", "incomplete", "interrupted")
      if (verified) {
        record$detail <- child
        record$status <- if (status == 0L && identical(child$status, "passed")) "passed" else if (identical(child$status, "passed")) "failed" else child$status
      } else {
        record$status <- "failed"
        record$error <- "完了記録がないか、実行元が一致しません。ログを確認してください。"
      }
      if (!is.null(error)) record$error <- error
      if (identical(as.integer(status), 124L)) record$error <- "工程が制限時間を超えました。"
    }
    record
  }
  run <- function(selected = names(phases), output_dir = NULL, timeout = 600L, rscript = find_rscript(), execute = system2) {
    if (!length(selected) || anyDuplicated(selected) || !all(selected %in% names(phases))) stop("--phases に既知の工程を重複なしで指定してください。", call. = FALSE)
    if (length(timeout) != 1L || is.na(timeout) || !is.finite(timeout) || timeout < 1L || timeout > 3600L) stop("--timeout は1〜3600秒です。", call. = FALSE)
    if (is.null(output_dir)) {
      destination <- file.path(root, "outputs/pk-desktop")
      dir.create(destination, recursive = TRUE, showWarnings = FALSE)
      output_dir <- tempfile("mac-check-", tmpdir = destination)
    }
    if (file.exists(output_dir) || !dir.create(output_dir, recursive = TRUE)) stop("結果の保存先には新しいフォルダを指定してください。", call. = FALSE)
    output_dir <- normalizePath(output_dir, mustWork = TRUE)
    source_files <- unique(c("apps/pk-desktop/check_macos.R", "apps/pk-desktop/check_macos.command", "apps/pk-desktop/launch_macos.command", "apps/pk-desktop/pkapp.R",
      paste0("apps/pk-desktop/R/", list.files(file.path(desktop_dir, "R"), pattern = "\\.R$")), vapply(phases, `[[`, character(1), "file")))
    hashes <- stats::setNames(lapply(file.path(root, source_files), checksum), source_files)
    result <- list(format = "pk-desktop-macos-check-1", app_version = version(), started_at = now(),
      controller_runtime = runtime(), rscript = rscript, source_root = root, output_dir = output_dir,
      baseline_files_expected = length(baseline), baseline_groups_expected = sum(baseline),
      source_hash_algorithm = "sha256", source_hashes = hashes,
      test_source_hashes = hashes[grepl("/tests/", names(hashes), fixed = TRUE)],
      phases = lapply(phases, function(phase) list(id = phase$id, file = phase$file, groups_expected = phase$groups,
        status = "not_run", reason = if (phase$id %in% selected) "未実行" else "今回の対象外")),
      automated_R = list(status = "running", complete = FALSE), manual_acceptance = manual)
    persist <- function() write_json(result, file.path(output_dir, "result.json"))
    persist()
    cat("PK Desktop ", result$app_version, " / ", R.version.string, "\n結果の保存先: ", output_dir, "\n", sep = "")
    cat("R処理を新しいテスト用プロジェクトで確認します。Mac実画面などの受入試験は別です。\n")
    for (i in seq_along(selected)) {
      id <- selected[[i]]
      cat(sprintf("[%d/%d] %s ...\n", i, length(selected), id)); flush.console()
      record <- run_phase(phases[[id]], rscript, output_dir, as.integer(timeout), execute)
      result$phases[[id]] <- record
      cat(sprintf("  %s (exit %s) / %s\n", record$status, record$exit_code, record$log)); flush.console()
      persist()
      if (identical(record$status, "interrupted")) break
    }
    final_hashes <- stats::setNames(lapply(file.path(root, source_files), checksum), source_files)
    result$source_unchanged <- identical(hashes, final_hashes) && !anyNA(unlist(hashes))
    statuses <- vapply(result$phases, `[[`, character(1), "status")
    failed <- sum(statuses %in% c("failed", "interrupted"))
    incomplete <- sum(statuses %in% c("incomplete", "not_run"))
    complete <- failed == 0L && incomplete == 0L && result$source_unchanged
    baseline_total <- function(field) sum(vapply(result$phases[names(baseline)], function(x) {
      if (is.null(x$detail[[field]])) 0L else x$detail[[field]]
    }, numeric(1)))
    result$automated_R <- list(status = if (failed || !result$source_unchanged) "failed" else if (incomplete) "partial" else "passed",
      complete = complete, phases_passed = sum(statuses == "passed"), phases_failed = failed,
      phases_incomplete = sum(statuses == "incomplete"), phases_not_run = sum(statuses == "not_run"),
      baseline_files_passed = sum(statuses[names(baseline)] == "passed"),
      baseline_groups_run = baseline_total("groups_run"), baseline_groups_passed = baseline_total("groups_passed"),
      baseline_groups_failed = baseline_total("groups_failed"), baseline_groups_skipped = baseline_total("groups_skipped"))
    result$finished_at <- now(); persist()
    cat("失敗した工程: ", failed, " / 未完了・対象外: ", incomplete, "\n", sep = "")
    if (!result$source_unchanged) cat("実行元の変更またはSHA256の未取得があるため、完了扱いにはしていません。\n")
    cat("R処理の判定: ", result$automated_R$status, "\n結果: ", file.path(output_dir, "result.json"), "\n", sep = "")
    cat("Mac画面操作、Tauri終了、別Mac、署名・公証、Codex実機接続は未確認です。\n")
    invisible(result)
  }
  parse_args <- function(args) {
    options <- list()
    while (length(args)) {
      flag <- args[[1]]
      if (flag %in% c("--help", "--skip-workers")) {
        if (!is.null(options[[substring(flag, 3L)]])) stop("重複したオプション: ", flag, call. = FALSE)
        options[[substring(flag, 3L)]] <- TRUE; args <- args[-1L]; next
      }
      if (!flag %in% c("--output-dir", "--phases", "--timeout", "--phase", "--phase-result") || length(args) < 2L || !is.null(options[[substring(flag, 3L)]])) stop("不明・重複・未完了のオプション: ", flag, call. = FALSE)
      options[[substring(flag, 3L)]] <- args[[2]]; args <- args[-c(1L, 2L)]
    }
    options
  }
  if (identical(getOption("pkdesktop.check_macos.autorun"), FALSE)) return(invisible(list(run = run, run_phase = run_phase, phases = phases, parse_args = parse_args, json = json)))
  options <- parse_args(if (sourced) character() else commandArgs(trailingOnly = TRUE))
  if (isTRUE(options[["help"]])) {
    cat("source(file.choose()) または Rscript --vanilla check_macos.R\n",
      "  --output-dir NEW_DIR  結果を保存する新しいフォルダ\n  --phases ID,ID       一部のみ実行（全工程の完了にはなりません）\n",
      "  --skip-workers       通常ワーカー診断・帳票を除外（部分確認）\n  --timeout SECONDS    1工程の制限時間、既定600秒\n",
      "工程: ", paste(names(phases), collapse = ","), "\n", sep = "")
    return(invisible(NULL))
  }
  if (!is.null(options[["phase"]])) {
    if (sourced || is.null(options[["phase-result"]])) stop("子工程の記録先がありません。", call. = FALSE)
    quit(save = "no", status = child_phase(options[["phase"]], options[["phase-result"]]))
  }
  selected <- if (is.null(options[["phases"]])) names(phases) else strsplit(options[["phases"]], ",", fixed = TRUE)[[1]]
  if (!all(selected %in% names(phases))) stop("不明な工程: ", paste(setdiff(selected, names(phases)), collapse = ", "), call. = FALSE)
  if (isTRUE(options[["skip-workers"]])) selected <- selected[!vapply(phases[selected], `[[`, logical(1), "worker")]
  answer <- run(selected, options[["output-dir"]], if (is.null(options[["timeout"]])) 600L else suppressWarnings(as.numeric(options[["timeout"]])))
  if (!sourced) quit(save = "no", status = if (identical(answer$automated_R$status, "failed") || answer$automated_R$phases_incomplete > 0L) 1L else 0L)
  invisible(answer)
})()
