desktop_nca_job_update <- function(engine, job) {
  engine$nca_jobs[[job$job_id]] <- job
  engine$nca_job <- job
  desktop_nca_notify(engine)
  invisible(job)
}
desktop_nca_finish_job <- function(engine, status, error = NULL, result_id = NULL) {
  worker <- engine$nca_worker
  if (is.null(worker)) return(invisible(NULL))
  job <- engine$nca_jobs[[worker$job_id]]
  job$status <- status
  job$error <- error
  job$result_id <- result_id
  job$finished_at <- desktop_time()
  if (status != "succeeded") {
    engine$nca_failed <- TRUE
    engine$nca_disclosure <- NULL
    tryCatch(desktop_nca_record(engine, "run_failed", worker$snapshot$args, worker$snapshot$channel,
      list(job_id = job$job_id, error_code = error$code %||% "E_NCA_FAILED")), error = function(e) message("Could not record failed NCA job"))
  }
  engine$nca_worker <- NULL
  if (identical(status, "failed")) {
    unlink(file.path(worker$path, "input.rds"))
    log <- file.path(worker$path, "worker.log")
    if (file.exists(log)) Sys.chmod(log, "0600")
    desktop_atomic(list(job_id = job$job_id, status = status, error = error), file.path(worker$path, "status.json"))
  } else unlink(worker$path, recursive = TRUE)
  desktop_nca_job_update(engine, job)
}
desktop_nca_poll_job <- function(engine, job_id = NULL) {
  worker <- engine$nca_worker
  if (is.null(worker) || is.null(worker$process) || (!is.null(job_id) && !identical(worker$job_id, job_id))) return(invisible(NULL))
  if (worker$process$is_alive()) {
    if (as.numeric(difftime(Sys.time(), worker$started, units = "secs")) <= engine$nca_job_timeout) return(invisible(NULL))
    try(worker$process$kill(), silent = TRUE)
    worker$process$wait(timeout = 1000)
    if (worker$process$is_alive()) return(invisible(NULL))
    desktop_nca_finish_job(engine, "failed", list(code = "E_TIMEOUT", message = "NCA exceeded its execution time limit"))
    return(invisible(NULL))
  }
  tryCatch({
    answer <- worker$process$get_result()
    desktop_check(is.list(answer) && isTRUE(answer$ok) && identical(answer$input_sha256, worker$input_sha256),
      "E_NCA_FAILED", "NCA failed; inspect the input and conditions in the local UI")
    result <- desktop_nca_publish(engine, worker$snapshot, answer$value)
    desktop_nca_finish_job(engine, "succeeded", result_id = result$result_id)
  }, error = function(e) {
    try(cat(conditionMessage(e), "\n", file = file.path(worker$path, "worker.log"), append = TRUE), silent = TRUE)
    safe <- if (inherits(e, "pkapp_error") && e$code %in% c("E_CODE_CHANGED", "E_INPUT_CHANGED", "E_REVISION_CONFLICT", "E_PROJECT_CONFLICT"))
      list(code = e$code, message = conditionMessage(e)) else
      list(code = "E_NCA_FAILED", message = "NCA failed; inspect the input and conditions in the local UI")
    desktop_nca_finish_job(engine, "failed", safe)
  })
  invisible(NULL)
}
desktop_nca_start_job <- function(engine, args, channel = "mcp") {
  desktop_nca_idle(engine)
  desktop_nca_revision(engine, args$base_revision, args$project_id)
  desktop_check(requireNamespace("callr", quietly = TRUE), "E_VALIDATION", "The callr package is required for background NCA")
  snapshot <- desktop_nca_begin_run(engine, args, channel)
  id <- paste0("nca-job-", substr(desktop_hash(list(tempfile(), desktop_time())), 1, 24))
  root <- file.path(engine$project, ".nca-jobs")
  dir.create(root, showWarnings = FALSE, mode = "0700")
  path <- file.path(root, id)
  desktop_check(dir.create(path, mode = "0700"), "E_IO", "Could not create NCA job workspace")
  started <- FALSE
  on.exit(if (!started) unlink(path, recursive = TRUE), add = TRUE)
  input <- file.path(path, "input.rds")
  saveRDS(list(data = snapshot$data, specification = snapshot$state$specification), input, version = 3)
  Sys.chmod(input, "0600")
  input_sha256 <- pk_hash(input)
  desktop_nca_record(engine, "run_start", args, channel, list(job_id = id, input_spec_hash = snapshot$hash))
  job <- list(job_id = id, status = "running", channel = channel, declared_actor = args$actor,
    started_at = desktop_time(), finished_at = NULL, result_id = NULL, error = NULL)
  desktop_nca_job_update(engine, job)
  engine$nca_worker <- list(job_id = id, path = path, snapshot = snapshot,
    input_sha256 = input_sha256, started = Sys.time(), process = NULL)
  process <- tryCatch(callr::r_bg(function(root, input, input_sha256, hashes) {
    source(file.path(root, "apps", "pk-desktop", "pkapp.R"))
    core <- environment(pkapp_nca)
    if (!identical(c(core$pk_code_hashes, core$desktop_code_hashes), hashes) ||
        !identical(core$pk_hash(input), input_sha256)) return(list(ok = FALSE))
    args <- readRDS(input)
    value <- pkapp_nca(args$data, args$specification)
    list(ok = TRUE, input_sha256 = input_sha256, value = value)
  }, args = list(root = pk_root, input = input, input_sha256 = input_sha256,
    hashes = c(pk_code_hashes, desktop_code_hashes)), libpath = .libPaths(),
    stdout = file.path(path, "worker.log"), stderr = "2>&1", user_profile = FALSE,
    system_profile = FALSE, supervise = TRUE, cleanup_tree = FALSE), error = function(e) NULL)
  if (is.null(process)) {
    desktop_nca_finish_job(engine, "failed", list(code = "E_NCA_FAILED", message = "Could not start the NCA worker"))
    desktop_error("E_NCA_FAILED", "Could not start the NCA worker")
  }
  engine$nca_worker$process <- process
  started <- TRUE
  tick <- function() {
    if (isTRUE(engine$nca_closing) || is.null(engine$nca_worker) || !identical(engine$nca_worker$job_id, id)) return(invisible(NULL))
    desktop_nca_poll_job(engine, id)
    if (!is.null(engine$nca_worker) && identical(engine$nca_worker$job_id, id)) later::later(tick, .1)
    invisible(NULL)
  }
  later::later(tick, .1)
  job
}
desktop_nca_get_job <- function(engine, job_id, project_id) {
  desktop_check(identical(project_id, engine$nca$project_id), "E_PROJECT_CONFLICT", "Active project changed; read status again")
  valid <- is.character(job_id) && length(job_id) == 1L && !is.na(job_id) && grepl("^nca-job-[a-f0-9]{24}$", job_id)
  desktop_check(valid && !is.null(engine$nca_jobs[[job_id]]), "E_NOT_FOUND", "Unknown job_id")
  desktop_nca_poll_job(engine, job_id)
  job <- engine$nca_jobs[[job_id]]
  if (job$status %in% c("failed", "cancelled")) desktop_error(job$error$code, job$error$message)
  if (identical(job$status, "succeeded")) job$result <- desktop_nca_public(engine, job$result_id, project_id)
  job
}
desktop_nca_jobs_close <- function(engine) {
  engine$nca_closing <- TRUE
  engine$nca_disclosure <- NULL
  worker <- engine$nca_worker
  if (!is.null(worker)) {
    if (!is.null(worker$process)) {
      try(worker$process$kill(), silent = TRUE)
      worker$process$wait(timeout = 5000)
      desktop_check(!worker$process$is_alive(), "E_BUSY", "NCA worker has not stopped; keep the project owner active")
    }
    desktop_nca_finish_job(engine, "cancelled", list(code = "E_CLOSED", message = "The NCA owner closed before completion"))
  }
  invisible(NULL)
}
