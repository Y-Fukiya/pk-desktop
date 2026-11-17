root <- Sys.getenv("PK_REVIEW_ROOT")
source(file.path(root, "apps", "pk-desktop", "R", "engine.R"))
source(file.path(root, "apps", "pk-desktop", "R", "api.R"))
source(file.path(root, "apps", "pk-desktop", "R", "app.R"))
main <- function() {
  runtime <- desktop_read(Sys.getenv("PKAPP_RUNTIME"))
  owner <- desktop_acquire_project(runtime$project)
  on.exit(desktop_release_project(owner), add = TRUE)
  saved <- file.path(runtime$project, "state.json")
  bundle <- if (file.exists(saved)) desktop_read(saved)$spec$data$bundle else runtime$bundle
  engine <- desktop_new(runtime$project, bundle)
  on.exit(desktop_jobs_close(engine), add = TRUE, after = FALSE)
  api <- httpuv::startServer("127.0.0.1", runtime$api_port, desktop_http_app(engine, runtime))
  on.exit(httpuv::stopServer(api), add = TRUE, after = FALSE)
  parent_alive <- function() {
    if (!isTRUE(tools::pskill(runtime$parent_pid, signal = 0L))) { shiny::stopApp(); return() }
    desktop_jobs_poll(engine)
    later::later(parent_alive, .5)
  }
  later::later(parent_alive, 1)
  shiny::runApp(desktop_app(engine, runtime$token), host = "127.0.0.1", port = runtime$ui_port, launch.browser = FALSE)
}
main()
