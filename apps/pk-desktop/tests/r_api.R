root <- normalizePath(getwd())
api <- new.env(parent = globalenv())
before <- ls(globalenv(), all.names = TRUE)
source("apps/pk-desktop/pkapp.R", local = api)
stopifnot(identical(ls(globalenv(), all.names = TRUE), c(before, "before")[order(c(before, "before"))]))
library(testthat)
local({
  scratch <- tempfile("pkapp-r-api-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  bundle <- file.path(scratch, "input")
  dir.create(bundle)
  write.csv(data.frame(USUBJID = rep(c("S01", "S02"), each = 3), TIME_H = rep(0:2, 2),
    AVAL = c(0, 2, 1, 0, 4, 2), AVALU = "ng/mL", BLQ = rep(c(1, 0, 0), 2), SEX = rep(c("F", "M"), each = 3)),
    file.path(bundle, "ADNCA.csv"), row.names = FALSE)
  write.csv(data.frame(USUBJID = rep(c("S01", "S02"), each = 2), PARAMCD = rep(c("CMAX", "AUC0TL"), 2),
    AVAL = c(2, 2.5, 4, 5), AVALU = rep(c("ng/mL", "ng*h/mL"), 2), SEX = rep(c("F", "M"), each = 2)),
    file.path(bundle, "ADPP.csv"), row.names = FALSE)
  p <- api$pkapp_open(file.path(scratch, "project"), bundle)
  on.exit(api$pkapp_close(p), add = TRUE)
  core <- environment(api$pkapp_open)
  test_that("R loading is independent of working directory and does not expose engine internals", {
    old <- getwd()
    setwd(scratch)
    other <- new.env(parent = globalenv())
    source(file.path(root, "apps/pk-desktop/pkapp.R"), local = other)
    setwd(old)
    expect_identical(other$pkapp_root(), root)
    expect_true(all(startsWith(ls(other), "pkapp_")))
    expect_false(exists("pk_root", globalenv(), inherits = FALSE))
  })
  test_that("R and Tauri use the same project ownership guard", {
    expect_identical(api$pkapp_spec(p)$updated_by, "r")
    expect_identical(p$engine$audit[[1]]$channel, "r")
    expect_error(api$pkapp_open(p$owner$project, bundle), class = "pkapp_error")
    expect_error(core$desktop_acquire_project(p$owner$project), class = "pkapp_error")
    failed <- file.path(scratch, "failed-import")
    expect_error(api$pkapp_open(failed, file.path(scratch, "absent-bundle")))
    expect_false(dir.exists(file.path(failed, ".pkapp-owner")))
    python <- unname(Sys.which("python3"))
    expect_true(nzchar(python))
    config <- api$pkapp_mcp_config(p, python = python)
    python <- file.path(normalizePath(dirname(python)), basename(python))
    expect_match(config, paste0('command = "', python, '"'), fixed = TRUE)
    expect_match(config, "--runtime", fixed = TRUE)
    expect_match(config, api$pkapp_runtime_path(p), fixed = TRUE)
  })
  test_that("R vector updates preserve optimistic concurrency and validation", {
    spec <- api$pkapp_spec(p)
    api$pkapp_update(p, "conc_plot", list(group_by = "SEX"), spec$revision, reason = "Group in R")
    expect_identical(api$pkapp_spec(p)$updated_by, "r")
    expect_identical(api$pkapp_spec(p)$conc_plot$group_by, list("SEX"))
    expect_error(api$pkapp_update(p, "conc_plot", list(group_by = character()), spec$revision, reason = "Stale R selection"), class = "pkapp_error")
    now <- api$pkapp_spec(p)
    expect_error(api$pkapp_update(p, "param_summary", list(paramcd = "ABSENT"), now$revision, reason = "Invalid parameter"), class = "pkapp_error")
    expect_identical(api$pkapp_spec(p), now)
  })
  test_that("R, UI and MCP paths produce identical analysis hashes and unrounded CSVs", {
    r <- api$pkapp_run(p)
    core$desktop_apply(p$engine, "conc_plot", list(group_by = list("SEX")), p$engine$spec$revision,
      p$engine$spec$project_id, "ui", "test", "Same settings in UI")
    ui <- core$desktop_run(p$engine, p$engine$spec$revision, p$engine$spec$project_id, "ui")
    core$desktop_dispatch(p$engine, "update", list(section = "conc_plot", values = list(group_by = list("SEX")),
      base_revision = p$engine$spec$revision, project_id = p$engine$spec$project_id, actor = "test", reason = "Same settings in MCP"))
    mcp <- core$desktop_dispatch(p$engine, "run", list(base_revision = p$engine$spec$revision, project_id = p$engine$spec$project_id))
    expect_identical(r$analysis_hash, ui$analysis_hash)
    expect_identical(r$analysis_hash, mcp$analysis_hash)
    for (table in c("concentration_summary.csv", "parameter_summary.csv")) {
      hashes <- vapply(c(r$run_id, ui$run_id, mcp$run_id), function(id) core$pk_hash(file.path(p$owner$project, "runs", id, "tables", table)), character(1))
      expect_length(unique(hashes), 1)
    }
    expect_s3_class(api$pkapp_result(p, r$run_id)$parameter_summary, "data.frame")
    expect_true(inherits(api$pkapp_plot(p), "ggplot"))
    expect_equal(length(api$pkapp_runs(p)), 3)
  })
  test_that("R alone creates Word and records the R channel; pending waits can be cancelled", {
    run_id <- api$pkapp_runs(p)[[1]]$run_id
    report <- api$pkapp_report(p, run_id, reason = "Word from R")
    expect_identical(report$status, "succeeded")
    expect_identical(report$triggered_by, "r")
    expect_true(file.exists(report$artifacts$docx))
    n <- length(api$pkapp_jobs(p))
    expect_error(api$pkapp_report(p, run_id, reason = "Invalid wait", timeout = -1), class = "pkapp_error")
    expect_length(api$pkapp_jobs(p), n)
    pending <- api$pkapp_report(p, run_id, reason = "R cancellation", wait = FALSE)
    expect_error(api$pkapp_wait(p, pending$job_id, timeout = 0), "Wait ended")
    api$pkapp_cancel(p, pending$job_id)
    expect_identical(api$pkapp_wait(p, pending$job_id)$status, "cancelled")
  })
  test_that("R GUI startup uses the common app and removes its runtime and listener after return", {
    gui <- file.path(scratch, "gui")
    calls <- new.env(parent = emptyenv())
    calls$stopped <- FALSE
    core$desktop_r_launch(gui, bundle, 3877, 3878, FALSE,
      start_api = function(host, port, app) {
        expect_identical(host, "127.0.0.1")
        calls$app <- app
        "owned-listener"
      },
      stop_api = function(listener) { expect_identical(listener, "owned-listener"); calls$stopped <- TRUE },
      run_app = function(appDir, host, port, launch.browser, display.mode) {
        expect_s3_class(appDir, "shiny.appobj")
        runtime_path <- api$pkapp_runtime_path(gui)
        runtime <- core$desktop_read(runtime_path)
        expect_identical(as.integer(file.info(runtime_path)$mode) %% 512L, 384L) # 0600
        expect_equal(nchar(runtime$token), 64)
        response <- calls$app$call(list(HTTP_AUTHORIZATION = paste("Bearer", runtime$token),
          REQUEST_METHOD = "GET", PATH_INFO = "/health"))
        expect_equal(response$status, 200L)
        expect_true(is.function(launch.browser))
      })
    expect_true(calls$stopped)
    expect_false(file.exists(api$pkapp_runtime_path(gui)))
    expect_false(dir.exists(file.path(gui, ".pkapp-owner")))
  })
  test_that("failed GUI startup cannot leave a runtime or owner lock", {
    gui <- file.path(scratch, "bind-failure")
    expect_error(core$desktop_r_launch(gui, bundle, 3877, 3878, FALSE,
      start_api = function(...) stop("Controlled bind failure")), "Controlled bind failure")
    expect_false(dir.exists(file.path(gui, ".pkapp-owner")))
    expect_false(file.exists(api$pkapp_runtime_path(gui)))
    stopped <- FALSE
    expect_error(core$desktop_r_launch(gui, bundle, 3877, 3878, FALSE,
      start_api = function(...) "owned-listener", stop_api = function(...) stopped <<- TRUE,
      run_app = function(...) stop("Controlled GUI failure")), "Controlled GUI failure")
    expect_true(stopped)
    expect_false(dir.exists(file.path(gui, ".pkapp-owner")))
    expect_false(file.exists(api$pkapp_runtime_path(gui)))
  })
  test_that("closed handles cannot be reused and projects can be reopened", {
    api$pkapp_close(p)
    api$pkapp_close(p)
    expect_error(api$pkapp_run(p), class = "pkapp_error")
    again <- api$pkapp_open(p$owner$project, bundle)
    expect_identical(api$pkapp_spec(again), p$engine$spec)
    api$pkapp_close(again)
    expect_false(dir.exists(file.path(p$owner$project, ".pkapp-owner")))
  })
})
cat("Direct R API tests completed\n")
