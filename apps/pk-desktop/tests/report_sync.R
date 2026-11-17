source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
base <- tempfile("pk-sync-"); dir.create(base)
p <- pkapp_open(file.path(base, "project"))
run <- pkapp_run(p)
before <- pkapp_run_manifest(p, run$run_id)

test_that("explicit blocking reporting works without loading callr and preserves the frozen Run", {
  loaded_before <- "callr" %in% loadedNamespaces()
  pkapp_update(p, "report", list(title = "Different future title"), pkapp_spec(p)$revision, "Future title")
  live <- pkapp_spec(p)
  job <- pkapp_report_sync(p, run$run_id, "Verify explicit synchronous rendering")
  expect_identical(job$status, "succeeded")
  expect_identical(job$execution_mode, "in_process")
  expect_identical("callr" %in% loadedNamespaces(), loaded_before)
  expect_identical(pkapp_spec(p), live)
  expect_identical(pkapp_run_manifest(p, run$run_id), before)
  expect_identical(pkapp_job(p, job$job_id)$artifacts$docx_sha256, core$pk_hash(job$artifacts$docx))
  manifest <- core$desktop_read(job$artifacts$manifest)
  expect_identical(manifest$request$execution_mode, "in_process")
  expect_identical(manifest$request$triggered_by, "r")
  xml_dir <- tempfile("xml-"); utils::unzip(job$artifacts$docx, exdir = xml_dir)
  xml <- xml2::read_xml(file.path(xml_dir, "word/document.xml"))
  text <- paste(xml2::xml_text(xml2::xml_find_all(xml, "//w:t", xml2::xml_ns(xml))), collapse = " ")
  expect_false(grepl("Different future title", text, fixed = TRUE))
  unlink(xml_dir, recursive = TRUE)
})

test_that("synchronous export refuses pending work without changing the queue", {
  job <- core$desktop_report_job(p$engine, run$run_id, p$engine$spec$project_id, "r", "test", "Queued job")
  before_jobs <- p$engine$jobs
  expect_error(pkapp_report_sync(p, run$run_id, "Must not jump queue"), class = "pkapp_error")
  expect_identical(p$engine$jobs, before_jobs)
  core$desktop_job_finish(p$engine, job, "cancelled")
})

test_that("renderer failure and interrupt clean partial artifacts and remain visible in history", {
  original <- core$desktop_render_report
  on.exit(assign("desktop_render_report", original, envir = core), add = TRUE)
  for (interrupted in c(FALSE, TRUE)) {
    assign("desktop_render_report", function(run_folder, candidate, job) {
      dir.create(candidate); writeLines("partial", file.path(candidate, "report.docx"))
      if (interrupted) stop(structure(list(message = "test interrupt", call = NULL), class = c("interrupt", "condition")))
      stop("test rendering failure")
    }, envir = core)
    if (interrupted) {
      caught <- tryCatch(pkapp_report_sync(p, run$run_id, "Interrupt cleanup"), interrupt = function(e) TRUE)
      expect_true(caught)
    } else expect_error(pkapp_report_sync(p, run$run_id, "Error cleanup"), class = "pkapp_error")
    job <- tail(p$engine$jobs, 1)[[1]]
    expect_identical(job$status, if (interrupted) "cancelled" else "failed")
    expect_false(dir.exists(file.path(p$engine$project, "reports", job$job_id)))
    expect_false(dir.exists(file.path(core$desktop_job_path(p$engine, job$job_id), "candidate")))
  }
})

test_that("completed-worker publication shares checksum verification and rejects changed candidates", {
  for (tamper in c(FALSE, TRUE)) {
    job <- core$desktop_report_job(p$engine, run$run_id, p$engine$spec$project_id, "r", "test", "Worker publication check")
    job$status <- "running"; core$desktop_job_save(p$engine, job)
    candidate <- file.path(core$desktop_job_path(p$engine, job$job_id), "candidate")
    answer <- core$desktop_render_report(core$desktop_run_folder(p$engine, run$run_id), candidate, job)
    if (tamper) cat("changed", file = file.path(candidate, "report.docx"), append = TRUE)
    # Controlled finished handle tests publication; it does not assert OS child support.
    p$engine$worker <- list(job_id = job$job_id, started = Sys.time(),
      process = list(is_alive = function() FALSE, get_result = function() answer))
    core$desktop_jobs_poll(p$engine)
    result <- p$engine$jobs[[job$job_id]]
    expect_identical(result$status, if (tamper) "failed" else "succeeded")
    expect_null(p$engine$worker)
    expect_identical(dir.exists(file.path(p$engine$project, "reports", job$job_id)), !tamper)
  }
})
pkapp_close(p)
unlink(base, recursive = TRUE)
cat("Synchronous report tests completed\n")
