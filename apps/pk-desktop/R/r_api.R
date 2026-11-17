# Direct R API. These functions close over a private copy of the common engine functions.
pkapp_root <- function() pk_root
pkapp_worker_check <- function(timeout = 15) desktop_worker_check(timeout)
pkapp_dependencies <- function() {
  packages <- c("digest", "jsonlite", "jsonvalidate", "shiny", "ggplot2", "ragg", "callr", "officer", "flextable",
    "httpuv", "later", "teal", "teal.data", "teal.transform", "datamods", "PKNCA", "openssl", "ps", "haven", "zip")
  desktop_dependency_status(packages)
}
desktop_r_finalize <- function(handle) try(pkapp_close(handle), silent = TRUE)
desktop_r_engine <- function(project) {
  desktop_check(inherits(project, "pkapp_project") && is.environment(project) && !isTRUE(project$closed) &&
    identical(project$owner$pid, Sys.getpid()), "E_CLOSED", "Use an open pkapp_project owned by this R session")
  desktop_check(file.exists(file.path(project$owner$lock, "pid")) &&
    identical(readLines(file.path(project$owner$lock, "pid"), n = 1), as.character(project$owner$pid)),
    "E_PROJECT_CONFLICT", "Project ownership changed; close this handle")
  project$engine
}
pkapp_open <- function(project, bundle = file.path(pk_root, "outputs/pk-review/bundles/reviewed-repeated")) {
  if (missing(bundle) && file.exists(file.path(project, "state.json"))) bundle <- desktop_read(file.path(project, "state.json"))$spec$data$bundle
  owner <- desktop_acquire_project(project)
  opened <- FALSE
  on.exit(if (!opened) desktop_release_project(owner), add = TRUE)
  engine <- desktop_new(owner$project, normalizePath(bundle, mustWork = TRUE), channel = "r", actor = "R user")
  p <- new.env(parent = emptyenv())
  class(p) <- c("pkapp_project", "environment")
  p$owner <- owner
  p$engine <- engine
  p$closed <- FALSE
  reg.finalizer(p, desktop_r_finalize, onexit = TRUE)
  opened <- TRUE
  p
}
pkapp_close <- function(project) {
  desktop_check(inherits(project, "pkapp_project") && is.environment(project), "E_VALIDATION", "Expected a pkapp_project")
  if (isTRUE(project$closed)) return(invisible(NULL))
  desktop_check(identical(project$owner$pid, Sys.getpid()), "E_CLOSED", "Only the owning R session may close this project")
  desktop_jobs_close(project$engine)
  desktop_check(is.null(project$engine$worker), "E_WORKER_BUSY", "Worker has not stopped; retry close before releasing the project")
  desktop_release_project(project$owner)
  project$closed <- TRUE
  invisible(NULL)
}
pkapp_status <- function(project) desktop_dispatch(desktop_r_engine(project), "status")
pkapp_spec <- function(project) desktop_r_engine(project)$spec
pkapp_update <- function(project, section, values, base_revision, reason, actor = "R user") {
  engine <- desktop_r_engine(project)
  desktop_check(is.list(values), "E_VALIDATION", "values must be a named list")
  # R character vectors become JSON arrays, including length-one selections.
  arrays <- switch(section, population = "subjects", conc_plot = c("group_by", "facet_by"), param_summary = "paramcd", NULL)
  for (array in intersect(arrays, names(values))) if (is.character(values[[array]])) values[[array]] <- as.list(unname(values[[array]]))
  desktop_apply(engine, section, values, base_revision, engine$spec$project_id, "r", actor, reason)
}
pkapp_run <- function(project, base_revision = pkapp_spec(project)$revision) {
  engine <- desktop_r_engine(project)
  desktop_run(engine, base_revision, engine$spec$project_id, "r")
}
pkapp_runs <- function(project) desktop_runs(desktop_r_engine(project))
pkapp_run_manifest <- function(project, run_id) desktop_verify_run(desktop_run_folder(desktop_r_engine(project), run_id))
pkapp_replay_run <- function(project, run_id, base_revision = pkapp_spec(project)$revision) {
  engine <- desktop_r_engine(project)
  desktop_replay_run(engine, run_id, base_revision, engine$spec$project_id, "r")
}
pkapp_result <- function(project, run_id) {
  value <- desktop_result(desktop_r_engine(project), run_id)
  jsonlite::fromJSON(desktop_json(value))
}
pkapp_plot <- function(project, type = c("concentration", "parameters")) {
  engine <- desktop_r_engine(project)
  type <- match.arg(type)
  analysis <- pk_analyse(engine$data, desktop_config(engine$spec))
  if (type == "concentration") pk_plot_concentration(analysis) else pk_plot_parameters(analysis)
}
pkapp_job <- function(project, job_id) desktop_get_job(desktop_r_engine(project), job_id)
pkapp_jobs <- function(project) desktop_list_jobs(desktop_r_engine(project))
pkapp_cancel <- function(project, job_id) {
  engine <- desktop_r_engine(project)
  desktop_cancel_job(engine, job_id, engine$spec$project_id)
}
pkapp_wait <- function(project, job_id, timeout = 360, poll_interval = .1) {
  desktop_check(is.numeric(timeout) && length(timeout) == 1 && is.finite(timeout) && timeout >= 0 &&
    is.numeric(poll_interval) && length(poll_interval) == 1 && is.finite(poll_interval) && poll_interval > 0 && poll_interval <= 1,
    "E_VALIDATION", "timeout must be nonnegative; poll_interval must be in (0, 1] seconds")
  deadline <- Sys.time() + timeout
  repeat {
    job <- pkapp_job(project, job_id)
    if (job$status %in% desktop_job_terminal) return(job)
    desktop_check(Sys.time() < deadline, "E_WAIT_TIMEOUT", paste("Wait ended; job remains pending:", job_id, "Use pkapp_job or pkapp_cancel."))
    Sys.sleep(poll_interval)
  }
}
pkapp_report <- function(project, run_id, reason, actor = "R user", wait = TRUE, timeout = 360) {
  desktop_check(is.logical(wait) && length(wait) == 1 && !is.na(wait), "E_VALIDATION", "wait must be TRUE or FALSE")
  desktop_check(is.numeric(timeout) && length(timeout) == 1 && is.finite(timeout) && timeout >= 0,
    "E_VALIDATION", "timeout must be nonnegative")
  engine <- desktop_r_engine(project)
  job <- desktop_export_report(engine, run_id, engine$spec$project_id, "r", actor, reason)
  if (!wait) return(job)
  job <- pkapp_wait(project, job$job_id, timeout = timeout)
  desktop_check(identical(job$status, "succeeded"), job$error$code %||% "E_REPORT_CANCELLED",
    paste("Report did not complete:", job$job_id, job$error$message %||% job$status))
  job
}
pkapp_report_sync <- function(project, run_id, reason, actor = "R user") {
  engine <- desktop_r_engine(project)
  job <- desktop_export_report_sync(engine, run_id, engine$spec$project_id, actor, reason)
  desktop_check(identical(job$status, "succeeded"), job$error$code %||% "E_REPORT_CANCELLED",
    paste("Synchronous report did not complete:", job$job_id, job$error$message %||% job$status))
  job
}
pkapp_runtime_path <- function(project) {
  path <- if (inherits(project, "pkapp_project")) desktop_r_engine(project)$project else normalizePath(project, mustWork = TRUE)
  desktop_r_runtime_path(path)
}
pkapp_mcp_config <- function(project, python = file.path(pk_root, ".venv/bin/python"), tealflow_python = NULL) {
  # JSON strings are also valid TOML basic strings/arrays; paths stay correctly escaped.
  quote_toml <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
  python <- path.expand(python)
  desktop_check(file.exists(python) && !isTRUE(file.info(python)$isdir), "E_VALIDATION", "Select an existing Python executable")
  # Preserve the venv executable symlink: resolving its final component loses the venv.
  python <- file.path(normalizePath(dirname(python), mustWork = TRUE), basename(python))
  config <- paste0("[mcp_servers.pkapp]\ncommand = ", quote_toml(python),
    "\nargs = ", quote_toml(c(file.path(pk_root, "apps/pk-desktop/mcp_server.py"), "--runtime", pkapp_runtime_path(project))), "\n")
  if (!is.null(tealflow_python)) {
    tealflow_python <- path.expand(tealflow_python)
    desktop_check(file.exists(tealflow_python) && !isTRUE(file.info(tealflow_python)$isdir), "E_VALIDATION", "Select the TealFlowMCP Python executable")
    tealflow_python <- file.path(normalizePath(dirname(tealflow_python), mustWork = TRUE), basename(tealflow_python))
    config <- paste0(config, "\n[mcp_servers.tealflow-mcp]\ncommand = ", quote_toml(tealflow_python),
      "\nargs = ", quote_toml(c(file.path(pk_root, "apps/pk-review/integrations/tealflow/bridge.py"), "serve")),
      "\n[mcp_servers.tealflow-mcp.env]\nPK_REVIEW_ROOT = ", quote_toml(pk_root),
      "\nR_LIBS_USER = ", quote_toml(file.path(pk_root, ".runtime/R-library")), "\n")
  }
  config
}
desktop_r_launch <- function(project, bundle, port, api_port, launch_browser,
    start_api = httpuv::startServer, stop_api = httpuv::stopServer, run_app = shiny::runApp) {
  valid_port <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 1024 && x <= 65535 && x == floor(x)
  desktop_check(valid_port(port) && valid_port(api_port) && port != api_port, "E_VALIDATION", "Use distinct UI/API ports between 1024 and 65535")
  desktop_check(is.logical(launch_browser) && length(launch_browser) == 1 && !is.na(launch_browser), "E_VALIDATION", "launch_browser must be TRUE or FALSE")
  p <- pkapp_open(project, bundle)
  on.exit(pkapp_close(p), add = TRUE)
  engine <- desktop_r_engine(p)
  runtime <- desktop_r_runtime(engine, api_port, port)
  runtime_written <- FALSE
  listener <- NULL
  timer <- new.env(parent = emptyenv())
  timer$active <- TRUE
  on.exit({
    timer$active <- FALSE
    if (!is.null(listener)) stop_api(listener)
    if (runtime_written) desktop_remove_r_runtime(engine, runtime)
  }, add = TRUE, after = FALSE)
  # No runtime is advertised unless the API successfully starts; failure releases the project.
  listener <- start_api("127.0.0.1", api_port, desktop_http_app(engine, runtime))
  desktop_write_r_runtime(engine, runtime)
  runtime_written <- TRUE
  tick <- function() {
    if (!timer$active) return(invisible(NULL))
    tryCatch(desktop_jobs_poll(engine), error = function(e) message("Report queue: ", conditionMessage(e)))
    later::later(tick, .5)
  }
  later::later(tick, .5)
  url <- paste0("http://127.0.0.1:", port, "/?token=", runtime$token)
  message("PK Desktop R: stop with Esc / Ctrl+C. Runtime: ", desktop_r_runtime_path(engine$project))
  open_browser <- function(base_url) {
    if (launch_browser) utils::browseURL(url) else message("Open this local session URL: ", url)
  }
  run_app(desktop_app(engine, runtime$token), host = "127.0.0.1", port = port,
    launch.browser = open_browser, display.mode = "normal")
  invisible(NULL)
}
pkapp_launch <- function(project = file.path(pk_root, "outputs/pk-desktop/r-project"),
    bundle = file.path(pk_root, "outputs/pk-review/bundles/reviewed-repeated"), port = 3877L, api_port = 3878L, launch_browser = TRUE) {
  if (missing(bundle) && file.exists(file.path(project, "state.json"))) bundle <- desktop_read(file.path(project, "state.json"))$spec$data$bundle
  desktop_r_launch(project, bundle, port, api_port, launch_browser)
}
