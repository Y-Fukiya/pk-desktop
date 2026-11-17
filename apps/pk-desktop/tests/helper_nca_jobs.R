nca_wait_for_job <- function(engine, session) {
  deadline <- Sys.time() + 30
  repeat {
    state <- core$desktop_nca_get(engine)
    if (is.null(state$job) || state$job$status != "running") break
    core$desktop_nca_poll_job(engine, state$job$job_id)
    later::run_now(.02)
    session$flushReact()
    if (Sys.time() > deadline) stop("NCA worker did not finish")
    Sys.sleep(.02)
  }
  session$flushReact()
}
