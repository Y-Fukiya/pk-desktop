source(file.path(Sys.getenv("PK_REVIEW_ROOT", getwd()), "apps", "pk-review", "bootstrap.R"))
source(file.path(pk_app, "R", "modules.R"))
path <- Sys.getenv("PK_REVIEW_CONFIG")
pk_assert(nzchar(path), "Set PK_REVIEW_CONFIG to a view JSON, or run python -m tools.pk_review launch VIEW")
path <- normalizePath(path, mustWork = TRUE)
config <- pk_read_config(path)
bundle <- pk_load_bundle(config$bundle)
pk_validate_config(config, bundle)
data <- teal.data::teal_data(ADNCA = bundle$ADNCA, ADPP = bundle$ADPP)
app <- teal::init(
  data = data,
  modules = teal::modules(pk_settings_module(path, bundle), pk_concentration_module(path, bundle),
                          pk_parameters_module(path, bundle), pk_report_module(path, bundle)),
  filter = teal::teal_slices(include_varnames = list(ADNCA = character(), ADPP = character()), allow_add = FALSE)
)
app <- teal::modify_title(app, "PK Review")
app <- teal::modify_header(app, shiny::tagList(
  shiny::tags$style(".teal-module {padding: 16px;} h3 {color:#163e50;} .shiny-input-container {margin-bottom:18px;}"),
  shiny::h2("PK Review"), shiny::p("pkdummy-harness  ·  ADNCA 濃度  /  ADPP パラメータ")
))
shiny::shinyApp(app$ui, app$server)
