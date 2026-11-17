desktop_worker_check <- function(timeout = 15) {
  desktop_check(is.numeric(timeout) && length(timeout) == 1 && is.finite(timeout) && timeout > 0 && timeout <= 60,
    "E_VALIDATION", "timeout must be in (0, 60] seconds")
  info <- list(checked_at = desktop_time(), app_version = desktop_version,
    R = as.character(getRversion()), platform = R.version$platform,
    pid = Sys.getpid(), proc_self = if (Sys.info()[["sysname"]] == "Linux") Sys.readlink("/proc/self") else NULL,
    available = FALSE, code = NULL, message = NULL)
  if (!is.null(info$proc_self) && !is.na(info$proc_self) && nzchar(info$proc_self) &&
      !identical(basename(info$proc_self), as.character(info$pid))) {
    info$code <- "E_PROCESS_NAMESPACE"
    info$message <- "R PID and /proc/self differ; processx/ps cannot reliably inspect this environment. Use a normal host session or explicit pkapp_report_sync()."
    return(info)
  }
  process <- NULL
  on.exit(if (!is.null(process)) try(process$kill(), silent = TRUE), add = TRUE)
  tryCatch({
    process <- callr::r_bg(function() list(value = 42L, pid = Sys.getpid(), R = as.character(getRversion())),
      libpath = .libPaths(), user_profile = FALSE, system_profile = FALSE, supervise = TRUE, cleanup_tree = FALSE)
    process$wait(timeout = timeout * 1000)
    desktop_check(!process$is_alive(), "E_WORKER_TIMEOUT", "Worker capability check timed out")
    answer <- process$get_result()
    desktop_check(identical(answer$value, 42L) && identical(answer$R, info$R), "E_WORKER_CHECK", "Unexpected worker result")
    info$available <- TRUE
    info$child_pid <- answer$pid
    info$message <- "Supervised R child returned successfully. Word rendering and application shutdown require separate acceptance checks."
    info
  }, error = function(e) {
    info$code <- e$code %||% "E_WORKER_START"
    info$message <- conditionMessage(e)
    info
  })
}
