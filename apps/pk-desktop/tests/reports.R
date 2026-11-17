Sys.setenv(PK_REVIEW_ROOT = normalizePath(getwd()))
source("apps/pk-desktop/R/engine.R")
source("apps/pk-desktop/R/api.R")
source("apps/pk-desktop/R/app.R")
library(testthat)
local({
  scratch <- tempfile("pkapp-reports-")
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
  engine <- desktop_new(file.path(scratch, "project"), bundle)
  on.exit(desktop_jobs_close(engine), add = TRUE)
  project_id <- engine$spec$project_id
  desktop_apply(engine, "report", list(title = "Frozen PK review"), engine$spec$revision, project_id, "ui", "test", "Report fixture")
  run <- desktop_run(engine, engine$spec$revision, project_id, "ui")
  run_path <- desktop_run_folder(engine, run$run_id)
  original_manifest <- pk_hash(file.path(run_path, "manifest.json"))
  first <- second <- NULL
  await <- function(job_id) {
    deadline <- Sys.time() + 45
    repeat {
      job <- desktop_get_job(engine, job_id)
      if (job$status %in% desktop_job_terminal) return(job)
      if (Sys.time() > deadline) stop("Report test timed out")
      Sys.sleep(.05)
    }
  }
  xml_doc <- function(job) {
    target <- tempfile("docx-", scratch)
    dir.create(target)
    utils::unzip(job$artifacts$docx, exdir = target)
    list(xml = xml2::read_xml(file.path(target, "word", "document.xml")), path = target)
  }
  tables <- function(doc) lapply(xml2::xml_find_all(doc$xml, "//w:tbl"), function(tab) xml2::xml_text(xml2::xml_find_all(tab, ".//w:t")))
  test_that("Word workers use a fixed Run while live settings change and the queue remains bounded", {
    first <<- desktop_dispatch(engine, "export_report", list(run_id = run$run_id, project_id = project_id, actor = "Codex", reason = "Frozen export"))
    expect_identical(first$status, "running")
    desktop_apply(engine, "report", list(title = "NEW TITLE FOR FUTURE RUNS", digits = 1L), engine$spec$revision, project_id, "mcp", "Codex", "Change future display")
    expect_identical(desktop_dispatch(engine, "spec")$spec$report$title, "NEW TITLE FOR FUTURE RUNS")
    second <<- desktop_export_report(engine, run$run_id, project_id, "ui", "test", "Same immutable Run")
    queued <- desktop_export_report(engine, run$run_id, project_id, "ui", "test", "Cancel queued export")
    expect_identical(queued$status, "queued")
    engine$queue_limit <- 3L
    expect_error(desktop_export_report(engine, run$run_id, project_id, "mcp", "test", "Queue full"), "At most eight")
    cancelled <- desktop_cancel_job(engine, queued$job_id, project_id)
    expect_identical(cancelled$status, "cancelled")
    expect_false(dir.exists(file.path(engine$project, "reports", queued$job_id)))
    engine$queue_limit <- 8L
    first <<- await(first$job_id)
    second <<- await(second$job_id)
    expect_identical(first$status, "succeeded", info = paste(readLines(file.path(desktop_job_path(engine, first$job_id), "worker.log")), collapse = "\n"))
    expect_identical(second$status, "succeeded", info = paste(readLines(file.path(desktop_job_path(engine, second$job_id), "worker.log")), collapse = "\n"))
    expect_identical(first$analysis_hash, run$analysis_hash)
    expect_identical(first$revision, run$revision)
    expect_identical(pk_hash(file.path(run_path, "manifest.json")), original_manifest)
  })
  test_that("Word contains original title, independent numeric values and the byte-identical frozen figure", {
    a <- xml_doc(first)
    b <- xml_doc(second)
    text <- paste(xml2::xml_text(xml2::xml_find_all(a$xml, "//w:t")), collapse = " ")
    expect_match(text, "Frozen PK review", fixed = TRUE)
    expect_false(grepl("NEW TITLE FOR FUTURE RUNS", text, fixed = TRUE))
    expect_match(text, run$analysis_hash, fixed = TRUE)
    expect_match(text, "3.75", fixed = TRUE) # mean(2.5, 5), not the new one-digit display
    expect_match(text, "1.41", fixed = TRUE) # sample SD of c(2, 4), three significant digits
    expect_match(text, "2.83", fixed = TRUE) # geometric mean of c(2, 4)
    # Only the first metadata table contains the intentionally different report creation time.
    expect_identical(tables(a)[-1], tables(b)[-1])
    media <- list.files(file.path(a$path, "word", "media"), full.names = TRUE)
    expect_true(pk_hash(file.path(run_path, "plots", "concentration.png")) %in% vapply(media, pk_hash, character(1)))
    expect_identical(pk_hash(first$artifacts$docx), first$artifacts$docx_sha256)
    expect_identical(desktop_get_job(engine, first$job_id)$status, "succeeded")
    # Cancelling a completed job never removes its report.
    expect_identical(desktop_cancel_job(engine, first$job_id, project_id)$status, "succeeded")
  })
  test_that("running cancellation and worker errors cannot publish a successful report", {
    running <- desktop_export_report(engine, run$run_id, project_id, "ui", "test", "Cancel running export")
    desktop_cancel_job(engine, running$job_id, project_id)
    cancelled <- await(running$job_id)
    expect_identical(cancelled$status, "cancelled")
    expect_false(dir.exists(file.path(engine$project, "reports", running$job_id)))
    # Exercise the normal process-error branch with a controlled failing worker.
    engine$closing <- TRUE
    failed <- engine$jobs[[running$job_id]]
    failed$status <- "running"
    desktop_job_save(engine, failed)
    engine$worker <- list(job_id = failed$job_id, started = Sys.time(), process = callr::r_bg(function() stop("Controlled worker failure"),
      user_profile = FALSE, system_profile = FALSE, supervise = TRUE, cleanup_tree = FALSE))
    answer <- await(failed$job_id)
    expect_identical(answer$status, "failed")
    expect_identical(answer$error$code, "E_REPORT_FAILED")
    expect_false("artifacts" %in% names(answer))
    engine$closing <- FALSE
    timed <- desktop_export_report(engine, run$run_id, project_id, "ui", "test", "Timeout handling")
    engine$job_timeout <- 0
    timed <- await(timed$job_id)
    expect_identical(timed$status, "failed")
    expect_identical(timed$error$code, "E_TIMEOUT")
    expect_false(dir.exists(file.path(engine$project, "reports", timed$job_id)))
    engine$job_timeout <- 300
  })
  test_that("restart resolves interrupted jobs and shutdown terminates owned workers", {
    job <- desktop_export_report(engine, run$run_id, project_id, "ui", "test", "Owner shutdown")
    process <- engine$worker$process
    desktop_jobs_close(engine)
    expect_false(process$is_alive())
    expect_identical(engine$jobs[[job$job_id]]$status, "cancelled")
    stale <- engine$jobs[[job$job_id]]
    stale$status <- "running"
    desktop_job_save(engine, stale)
    restored <- desktop_new(engine$project, bundle)
    expect_identical(restored$jobs[[job$job_id]]$status, "failed")
    expect_identical(restored$jobs[[job$job_id]]$error$code, "E_INTERRUPTED")
    expect_identical(desktop_get_job(restored, first$job_id)$status, "succeeded")
  })
  test_that("the actual teal module can enqueue the selected immutable Run", {
    engine$closing <- FALSE
    shiny::testServer(desktop_module_server, args = list(engine = engine), {
      session$flushReact()
      session$setInputs(report_run = run$run_id, report_reason = "GUI report test", export_report = 1)
      session$flushReact()
      expect_match(output$notice, "Word作成を受け付けました")
      expect_match(output$job_status, "running")
    })
    desktop_jobs_close(engine)
  })
  test_that("older M0 Runs remain readable but cannot silently use current plots for reports", {
    legacy_engine <- desktop_new(file.path(scratch, "legacy-project"), bundle)
    legacy <- desktop_run(legacy_engine, legacy_engine$spec$revision, legacy_engine$spec$project_id, "ui")
    folder <- desktop_run_folder(legacy_engine, legacy$run_id)
    manifest <- desktop_read(file.path(folder, "manifest.json"))
    manifest$format <- "pkapp-m0-1"
    manifest$outputs[c("report_context.json", "plots/concentration.png")] <- NULL
    unlink(file.path(folder, c("report_context.json", "plots")), recursive = TRUE)
    desktop_atomic(manifest, file.path(folder, "manifest.json"))
    expect_identical(desktop_result(legacy_engine, legacy$run_id)$analysis_hash, legacy$analysis_hash)
    expect_error(desktop_export_report(legacy_engine, legacy$run_id, legacy_engine$spec$project_id, "ui", "test", "Old run"),
      "older run has no frozen report artifacts")
    expect_length(legacy_engine$jobs, 0)
  })
  test_that("a changed completed Word file is rejected by the job result reader", {
    path <- first$artifacts$docx
    original <- readBin(path, "raw", n = file.info(path)$size)
    writeBin(c(original, charToRaw("changed")), path)
    expect_error(desktop_get_job(engine, first$job_id), "Artifact checksum changed")
    writeBin(original, path)
  })
})
cat("PK Desktop report tests completed\n")
