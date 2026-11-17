args <- commandArgs(trailingOnly = TRUE)
source("apps/pk-desktop/pkapp.R")
core <- environment(pkapp_open)
p <- pkapp_open(args[[1]])
engine <- p$engine
runtime <- list(api_port = as.integer(args[[2]]), ui_port = as.integer(args[[2]]) + 1L,
  token = paste(rep("a", 64), collapse = ""), instance_id = "live-test", pid = Sys.getpid())
core$desktop_atomic(runtime, file.path(args[[1]], "runtime.json"))
api <- httpuv::startServer("127.0.0.1", runtime$api_port, core$desktop_http_app(engine, runtime))
tryCatch({
  while (!file.exists(file.path(args[[1]], "stop"))) {
    httpuv::service(50)
    core$desktop_jobs_poll(engine)
  }
}, finally = {httpuv::stopServer(api); pkapp_close(p)})
