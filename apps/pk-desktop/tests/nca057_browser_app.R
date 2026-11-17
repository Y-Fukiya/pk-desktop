# Isolated synthetic UI fixture. No user data, project credentials, or TCP bridge.
root <- normalizePath(Sys.getenv("PK_BROWSER_SOURCE", unset = "."), mustWork = TRUE)
project <- normalizePath(Sys.getenv("PK_BROWSER_PROJECT"), mustWork = TRUE)
port <- as.integer(Sys.getenv("PK_BROWSER_PORT"))
stopifnot(is.finite(port), port > 1024, port < 65536)
source(file.path(root, "apps/pk-desktop/pkapp.R"))
core <- environment(pkapp_open)
engine <- core$desktop_nca_engine(project)
data <- data.frame(ID = "未指定", STUDYID = "全体", PERIOD = "1",
  TIME = c(0, 60, 120, 1440, 1500), CONC = c(0, 10, 6, 1, 0.5), UNIT = "mg/L")
source_id <- core$desktop_table_save(engine, "Synthetic language fixture", list(source = data), list(kind = "prepare", steps = list()))$id
ui <- bslib::page_fluid(core$desktop_workbench_styles(), core$desktop_i18n_controls(),
  shiny::tags$script(shiny::HTML(paste0("window.PK_TEST_SOURCE=", jsonlite::toJSON(source_id, auto_unbox = TRUE), ";"))),
  core$desktop_nca_ui("nca"))
server <- function(input, output, session) core$desktop_nca_server("nca", engine)
shiny::runApp(shiny::shinyApp(ui, server), host = "127.0.0.1", port = port, launch.browser = FALSE, quiet = TRUE)
