# One bounded FIFO report queue per project. Only this R owner publishes results.
desktop_job_terminal <- c("succeeded", "failed", "cancelled")
desktop_job_path <- function(engine, job_id) {
  desktop_check(is.character(job_id) && length(job_id) == 1 && grepl("^job-[0-9]{8}T[0-9]{6}-[a-f0-9]{12}$", job_id),
    "E_VALIDATION", "Invalid job ID")
  file.path(engine$project, "jobs", job_id)
}
desktop_job_save <- function(engine, job) {
  desktop_atomic(job, file.path(desktop_job_path(engine, job$job_id), "job.json"))
  engine$jobs[[job$job_id]] <- job
  invisible(job)
}
desktop_jobs_init <- function(engine) {
  engine$jobs <- list()
  engine$worker <- NULL
  engine$closing <- FALSE
  engine$queue_limit <- 8L
  engine$job_timeout <- 300
  paths <- list.files(file.path(engine$project, "jobs"), pattern = "^job-", full.names = TRUE)
  for (path in paths) {
    job <- desktop_read(file.path(path, "job.json"))
    desktop_check(identical(job$project_id, engine$spec$project_id) && identical(basename(path), job$job_id),
      "E_VALIDATION", "Job belongs to a different project")
    if (!job$status %in% desktop_job_terminal) {
      # Never adopt or signal a saved PID. The former owner's worker is supervised.
      job$status <- "failed"
      job$error <- list(code = "E_INTERRUPTED", message = "Previous application session ended before report completion")
      job$finished_at <- desktop_time()
      unlink(file.path(path, "candidate"), recursive = TRUE)
      # A crash between directory rename and state commit must not leave an apparent success.
      unlink(file.path(engine$project, "reports", job$job_id), recursive = TRUE)
      desktop_job_save(engine, job)
    } else engine$jobs[[job$job_id]] <- job
  }
  invisible(engine)
}
desktop_job_public <- function(job) {
  # Explicit projection prevents worker logs or detailed QC notices from reaching MCP.
  fields <- c("job_id", "project_id", "run_id", "revision", "analysis_hash", "status", "requested_at", "started_at", "finished_at",
    "triggered_by", "actor", "reason", "execution_mode", "error", "artifacts")
  job[intersect(fields, names(job))]
}
desktop_job_finish <- function(engine, job, status, error = NULL) {
  job$status <- status
  job$finished_at <- desktop_time()
  job$error <- error
  if (status != "succeeded") {
    unlink(file.path(desktop_job_path(engine, job$job_id), "candidate"), recursive = TRUE)
    unlink(file.path(engine$project, "reports", job$job_id), recursive = TRUE)
    job$artifacts <- NULL
  }
  desktop_job_save(engine, job)
}
desktop_job_stop <- function(engine, job, final_status, error = NULL) {
  job$status <- "cancelling"
  job$stop_status <- final_status
  job$stop_error <- error
  desktop_job_save(engine, job)
  # The fixed officer renderer runs in one R child and starts no external commands.
  # Terminate only the owned handle; do not enumerate unrelated system processes.
  try(engine$worker$process$kill(), silent = TRUE)
  invisible(job)
}
desktop_report_publish <- function(engine, job, answer) {
  candidate <- file.path(desktop_job_path(engine, job$job_id), "candidate")
  receipt <- file.path(candidate, "REPORT_MANIFEST.json")
  desktop_check(identical(pk_hash(receipt), answer$report_manifest_sha256), "E_INPUT_CHANGED", "Report receipt changed")
  m <- desktop_read(receipt)
  desktop_check(identical(m$job_id, job$job_id) && identical(m$source_run_manifest_sha256, job$run_manifest_sha256),
    "E_INPUT_CHANGED", "Report belongs to a different job")
  desktop_verify_files(candidate, m$outputs)
  desktop_verify_run(desktop_run_folder(engine, job$run_id), job$run_manifest_sha256, report = TRUE)
  destination <- file.path(engine$project, "reports", job$job_id)
  dir.create(dirname(destination), showWarnings = FALSE)
  desktop_check(!dir.exists(destination) && file.rename(candidate, destination), "E_IO", "Cannot publish completed report")
  job$artifacts <- list(directory = destination, docx = file.path(destination, "report.docx"),
    manifest = file.path(destination, "REPORT_MANIFEST.json"), docx_sha256 = m$outputs[["report.docx"]],
    manifest_sha256 = answer$report_manifest_sha256)
  desktop_job_finish(engine, job, "succeeded")
}
desktop_jobs_poll <- function(engine) {
  if (!is.null(engine$worker)) {
    worker <- engine$worker
    job <- engine$jobs[[worker$job_id]]
    if (job$status == "running" && as.numeric(difftime(Sys.time(), worker$started, units = "secs")) > engine$job_timeout) {
      desktop_job_stop(engine, job, "failed", list(code = "E_TIMEOUT", message = "Report generation exceeded 300 seconds"))
      job <- engine$jobs[[worker$job_id]]
    }
    if (!worker$process$is_alive()) {
      if (job$status == "cancelling") {
        desktop_job_finish(engine, job, job$stop_status, job$stop_error)
      } else {
        tryCatch({
          answer <- worker$process$get_result()
          desktop_report_publish(engine, job, answer)
        }, error = function(e) {
          message("Report job ", job$job_id, " failed: ", conditionMessage(e))
          desktop_job_finish(engine, job, "failed", list(code = "E_REPORT_FAILED", message = "Report failed; inspect the local worker log"))
        })
      }
      engine$worker <- NULL
    }
  }
  queued <- names(Filter(function(j) identical(j$status, "queued"), engine$jobs))
  if (is.null(engine$worker) && length(queued) && !engine$closing) {
    job <- engine$jobs[[queued[[1]]]]
    job$status <- "running"
    job$started_at <- desktop_time()
    desktop_job_save(engine, job)
    tryCatch({
      desktop_require_code()
      path <- desktop_job_path(engine, job$job_id)
      process <- callr::r_bg(function(root, run_folder, candidate, job, hashes) {
        Sys.setenv(PK_REVIEW_ROOT = root)
        source(file.path(root, "apps", "pk-desktop", "R", "engine.R"))
        desktop_check(identical(c(pk_code_hashes, desktop_code_hashes), hashes), "E_CODE_CHANGED", "Renderer changed before worker startup")
        desktop_render_report(run_folder, candidate, job)
      }, args = list(root = pk_root, run_folder = desktop_run_folder(engine, job$run_id), candidate = file.path(path, "candidate"),
        job = job, hashes = c(pk_code_hashes, desktop_code_hashes)), libpath = .libPaths(),
        stdout = file.path(path, "worker.log"), stderr = "2>&1", user_profile = FALSE, system_profile = FALSE,
        supervise = TRUE, cleanup_tree = FALSE)
      engine$worker <- list(job_id = job$job_id, process = process, started = Sys.time())
    }, error = function(e) {
      message("Report worker startup failed: ", conditionMessage(e))
      writeLines(c("Report worker startup failed", conditionMessage(e), capture.output(sessionInfo())),
        file.path(desktop_job_path(engine, job$job_id), "worker.log"))
      desktop_job_finish(engine, job, "failed", list(code = "E_WORKER_START", message = "Report worker could not start; inspect the local R log"))
    })
  }
  invisible(NULL)
}
desktop_report_job <- function(engine, run_id, project_id, channel, actor, reason, mode = "worker") {
  desktop_check(identical(project_id, engine$spec$project_id), "E_PROJECT_CONFLICT", "Active project changed")
  desktop_check(!engine$closing && channel %in% c("ui", "mcp", "r"), "E_VALIDATION", "Application is closing or channel is invalid")
  desktop_check(is.character(actor) && length(actor) == 1 && nzchar(trimws(actor)) && nchar(actor) <= 120 &&
    is.character(reason) && length(reason) == 1 && nzchar(trimws(reason)) && nchar(reason) <= 1000,
    "E_VALIDATION", "Actor and concrete report reason are required")
  desktop_require_code()
  folder <- desktop_run_folder(engine, run_id)
  m <- desktop_verify_run(folder, report = TRUE)
  desktop_jobs_poll(engine)
  desktop_check(sum(vapply(engine$jobs, function(j) !j$status %in% desktop_job_terminal, logical(1))) < engine$queue_limit,
    "E_QUEUE_FULL", "At most eight report jobs may be pending; wait or cancel a job")
  job_id <- paste0("job-", format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-", substr(desktop_hash(list(tempfile(), desktop_time())), 1, 12))
  dir.create(desktop_job_path(engine, job_id), recursive = TRUE)
  job <- list(job_id = job_id, project_id = project_id, run_id = run_id, revision = m$revision, analysis_hash = m$analysis_hash,
    run_manifest_sha256 = pk_hash(file.path(folder, "manifest.json")), status = "queued", requested_at = desktop_time(),
    triggered_by = channel, actor = actor, reason = reason, execution_mode = mode)
  desktop_job_save(engine, job)
  job
}
desktop_export_report <- function(engine, run_id, project_id, channel, actor, reason) {
  job <- desktop_report_job(engine, run_id, project_id, channel, actor, reason)
  desktop_jobs_poll(engine)
  desktop_job_public(engine$jobs[[job$job_id]])
}
# Explicit R-only blocking operation. Never an automatic fallback in a Shiny callback.
desktop_export_report_sync <- function(engine, run_id, project_id, actor, reason) {
  desktop_check(is.null(engine$worker) && !any(vapply(engine$jobs,
    function(j) !j$status %in% desktop_job_terminal, logical(1))), "E_WORKER_BUSY",
    "Finish or cancel pending jobs before synchronous reporting")
  job <- desktop_report_job(engine, run_id, project_id, "r", actor, reason, "in_process")
  job$status <- "running"
  job$started_at <- desktop_time()
  desktop_job_save(engine, job)
  on.exit({
    if (!engine$jobs[[job$job_id]]$status %in% desktop_job_terminal)
      desktop_job_finish(engine, job, "cancelled", list(code = "E_INTERRUPTED", message = "Synchronous report interrupted"))
  }, add = TRUE)
  tryCatch({
    answer <- desktop_render_report(desktop_run_folder(engine, run_id),
      file.path(desktop_job_path(engine, job$job_id), "candidate"), job)
    desktop_report_publish(engine, job, answer)
  }, interrupt = function(e) {
    desktop_job_finish(engine, job, "cancelled", list(code = "E_INTERRUPTED", message = "Synchronous report interrupted"))
    stop(e)
  }, error = function(e) {
    writeLines(c(conditionMessage(e), capture.output(sessionInfo())),
      file.path(desktop_job_path(engine, job$job_id), "worker.log"))
    desktop_job_finish(engine, job, "failed", list(code = "E_REPORT_FAILED", message = "Report failed; inspect the local worker log"))
  })
  desktop_job_public(engine$jobs[[job$job_id]])
}
desktop_get_job <- function(engine, job_id) {
  desktop_job_path(engine, job_id)
  desktop_jobs_poll(engine)
  job <- engine$jobs[[job_id]]
  desktop_check(!is.null(job), "E_VALIDATION", "Unknown report job")
  if (job$status == "succeeded") {
    desktop_check(identical(pk_hash(job$artifacts$manifest), job$artifacts$manifest_sha256), "E_INPUT_CHANGED", "Report manifest changed")
    desktop_verify_files(job$artifacts$directory, desktop_read(job$artifacts$manifest)$outputs)
  }
  desktop_job_public(job)
}
desktop_list_jobs <- function(engine) {
  desktop_jobs_poll(engine)
  lapply(rev(engine$jobs), desktop_job_public)
}
desktop_cancel_job <- function(engine, job_id, project_id) {
  desktop_check(identical(project_id, engine$spec$project_id), "E_PROJECT_CONFLICT", "Active project changed")
  desktop_job_path(engine, job_id)
  job <- engine$jobs[[job_id]]
  desktop_check(!is.null(job), "E_VALIDATION", "Unknown report job")
  if (job$status == "queued") desktop_job_finish(engine, job, "cancelled")
  if (job$status == "running") desktop_job_stop(engine, job, "cancelled")
  desktop_jobs_poll(engine)
  desktop_job_public(engine$jobs[[job_id]])
}
desktop_jobs_close <- function(engine) {
  engine$closing <- TRUE
  for (job in engine$jobs) if (job$status == "queued") desktop_job_finish(engine, job, "cancelled")
  if (!is.null(engine$worker)) {
    job <- engine$jobs[[engine$worker$job_id]]
    desktop_job_stop(engine, job, "cancelled")
    engine$worker$process$wait(timeout = 5000)
    desktop_jobs_poll(engine)
  }
  invisible(NULL)
}
