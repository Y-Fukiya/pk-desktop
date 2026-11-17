root <- normalizePath(Sys.getenv("PK_BROWSER_SOURCE", unset = "."), mustWork = TRUE)
project <- normalizePath(Sys.getenv("PK_BROWSER_PROJECT"), mustWork = TRUE)
port <- as.integer(Sys.getenv("PK_BROWSER_PORT"))
stopifnot(is.finite(port), port > 1024, port < 65536)
source(file.path(root, "apps/pk-desktop/pkapp.R"))
core <- environment(pkapp_open)
engine <- core$desktop_nca_engine(project)
data <- do.call(rbind, lapply(seq_len(8), function(i) {
  n <- if (i <= 6) 6 else 3
  data.frame(ID = sprintf("S%02d", i), TIME = c(0, 1, 2, 4, 8, 12)[seq_len(n)],
    CONC = c(0, 10, 6, 4, 1, .6)[seq_len(n)], UNIT = "mg/L")
}))
source_id <- core$desktop_table_save(engine, "Synthetic warning fixture", list(source = data), list(kind = "prepare", steps = list()))$id
ui <- bslib::page_fluid(core$desktop_workbench_styles(), core$desktop_i18n_controls(),
  shiny::tags$script(shiny::HTML(paste0("window.PK_TEST_SOURCE=", jsonlite::toJSON(source_id, auto_unbox = TRUE), ";"))), core$desktop_nca_ui("nca"))
server <- function(input, output, session) core$desktop_nca_server("nca", engine)
shiny::runApp(shiny::shinyApp(ui, server), host = "127.0.0.1", port = port, launch.browser = FALSE, quiet = TRUE)
