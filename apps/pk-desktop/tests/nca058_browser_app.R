root <- normalizePath(Sys.getenv("PK_BROWSER_SOURCE", unset = "."), mustWork = TRUE)
project <- normalizePath(Sys.getenv("PK_BROWSER_PROJECT"), mustWork = TRUE)
port <- as.integer(Sys.getenv("PK_BROWSER_PORT"))
stopifnot(is.finite(port), port > 1024, port < 65536)
source(file.path(root, "apps/pk-desktop/pkapp.R"))
core <- environment(pkapp_open)
engine <- core$desktop_nca_engine(project)
data <- data.frame(ID = "001", STUDYID = "TEST", TIME = c(0, .5, 1, 7, 8), CONC = c(0, 2, 10, 3, 2), UNIT = "mg/L")
source_id <- core$desktop_table_save(engine, "Synthetic boundary fixture", list(source = data), list(kind = "prepare", steps = list()))$id
raw <- data.frame(ID = "001", TIME = c(5, 6, 7, 8), CONC = c(0, 10, 3, 1), BLQ = 0, UNIT = "mg/L")
step <- list(op = "blq_rules", concentration = "CONC", blq_column = "BLQ", time = "TIME", mode = "positional",
  profile_columns = "ID", time_origin = 5, zero_time = "post", pre_rule = "zero", first_rule = "zero",
  middle_rule = "half_lloq", last_rule = "exclude", all_blq_rule = "zero", lloq_value = .1)
prepared <- pkapp_prepare(raw, list(step))$data
prepared_id <- core$desktop_table_save(engine, "Synthetic BLQ fixture", list(source = prepared), list(kind = "prepare", steps = list()))$id
ui <- bslib::page_fluid(core$desktop_workbench_styles(), core$desktop_i18n_controls(),
  shiny::tags$script(shiny::HTML(paste0("window.PK_TEST_SOURCE=", jsonlite::toJSON(source_id, auto_unbox = TRUE), ";window.PK_PREPARED_SOURCE=", jsonlite::toJSON(prepared_id, auto_unbox = TRUE), ";"))),
  core$desktop_nca_ui("nca"))
server <- function(input, output, session) core$desktop_nca_server("nca", engine)
shiny::runApp(shiny::shinyApp(ui, server), host = "127.0.0.1", port = port, launch.browser = FALSE, quiet = TRUE)
