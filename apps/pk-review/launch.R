root <- Sys.getenv("PK_REVIEW_ROOT", getwd())
source(file.path(root, "apps", "pk-review", "bootstrap.R"))
options(shiny.launch.browser = FALSE)
shiny::runApp(file.path(pk_app, "app.R"), host = "127.0.0.1", port = as.integer(Sys.getenv("PK_REVIEW_PORT", "3876")))
