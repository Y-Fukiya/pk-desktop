Sys.setenv(PK_REVIEW_ROOT = normalizePath(getwd()))
source("apps/pk-desktop/R/engine.R")
source("apps/pk-desktop/R/api.R")
source("apps/pk-desktop/R/app.R")
library(testthat)
scratch <- tempfile("pkapp-tests-")
dir.create(scratch)
bundle <- file.path(scratch, "input")
dir.create(bundle)
concentrations <- data.frame(USUBJID = rep(c("S01", "S02"), each = 3), TIME_H = rep(0:2, 2),
  AVAL = c(0, 2, 1, 0, 4, 2), AVALU = "ng/mL", SEX = rep(c("F", "M"), each = 3), BLQ = rep(c(1, 0, 0), 2))
parameters <- data.frame(USUBJID = rep(c("S01", "S02"), each = 2), PARAMCD = rep(c("CMAX", "AUC0TL"), 2),
  AVAL = c(2, 2.5, 4, 5), AVALU = rep(c("ng/mL", "ng*h/mL"), 2), SEX = rep(c("F", "M"), each = 2))
write.csv(concentrations, file.path(bundle, "ADNCA.csv"), row.names = FALSE)
write.csv(parameters, file.path(bundle, "ADPP.csv"), row.names = FALSE)
engine <- desktop_new(file.path(scratch, "project"), bundle)
id <- engine$spec$project_id
test_that("schema, validation and optimistic concurrency preserve committed state", {
  expect_error(desktop_apply(engine, "conc_plot", list(group_by = list("SEX")), 0L, id, "mcp", "test", "Group"), class = "pkapp_error")
  before <- engine$spec
  expect_error(desktop_apply(engine, "param_summary", list(paramcd = list("UNKNOWN")), 1L, id, "mcp", "test", "Select"), class = "pkapp_error")
  expect_error(desktop_apply(engine, "conc_plot", list(blq = "zero_before_tmax"), 1L, id, "mcp", "test", "BLQ"), class = "pkapp_error")
  expect_identical(engine$spec, before)
  expect_identical(desktop_read(file.path(engine$project, "state.json"))$spec, before)
})
test_that("UI and MCP conditions produce identical analysis hashes and CSV bytes", {
  desktop_apply(engine, "conc_plot", list(group_by = list("SEX")), 1L, id, "ui", "test", "Group")
  ui <- desktop_run(engine, 2L, id, "ui")
  desktop_dispatch(engine, "update", list(section = "conc_plot", values = list(group_by = list("SEX")), base_revision = 2L,
    project_id = id, actor = "Codex", reason = "Same conditions through MCP"))
  mcp <- desktop_dispatch(engine, "run", list(base_revision = 3L, project_id = id))
  expect_identical(ui$analysis_hash, mcp$analysis_hash)
  a <- file.path(engine$project, "runs", ui$run_id)
  b <- file.path(engine$project, "runs", mcp$run_id)
  expect_identical(pk_hash(file.path(a, "tables/parameter_summary.csv")), pk_hash(file.path(b, "tables/parameter_summary.csv")))
  expect_identical(pk_hash(file.path(a, "tables/concentration_summary.csv")), pk_hash(file.path(b, "tables/concentration_summary.csv")))
  expect_false(identical(pk_hash(file.path(a, "spec.json")), pk_hash(file.path(b, "spec.json"))))
  old <- desktop_result(engine, ui$run_id)
  expect_equal(old$revision, 2L)
  expect_length(desktop_runs(engine), 2L)
})
test_that("grouped and overall summary values match independent arithmetic", {
  desktop_apply(engine, "conc_plot", list(group_by = list()), 3L, id, "ui", "test", "Overall")
  r <- desktop_run(engine, 4L, id, "ui")
  cmax <- r$parameter_summary[r$parameter_summary$PARAMCD == "CMAX", ]
  expect_equal(cmax$n, 2)
  expect_equal(cmax$mean, 3)
  expect_equal(cmax$sd, sqrt(2), tolerance = 1e-14)
  expect_equal(cmax$geometric_mean, sqrt(8), tolerance = 1e-14)
  expect_equal(r$counts$blq_rows, 2)
  expect_false("concentrations" %in% names(r))
})
test_that("restart restores spec and audit without replaying changes", {
  restored <- desktop_new(engine$project, bundle)
  expect_identical(restored$spec, engine$spec)
  expect_identical(restored$audit, engine$audit)
})
test_that("external changes invalidate the actual teal module without a writeback loop", {
  shiny::testServer(desktop_module_server, args = list(engine = engine), {
    session$flushReact()
    expect_match(output$revision, "Revision 4")
    desktop_dispatch(engine, "update", list(section = "conc_plot", values = list(group_by = list("SEX")),
      base_revision = 4L, project_id = engine$spec$project_id, actor = "Codex", reason = "Synchronise GUI"))
    session$flushReact()
    expect_match(output$revision, "Revision 5")
    expect_match(output$editor$html, "SEX")
    expect_equal(engine$spec$revision, 5L)
    session$setInputs(base_revision = 5, profile = "all", x = "TIME_H", group = "NONE", scale = "linear", error = "none", blq = "exclude", reason = "GUI save", apply = 1)
    session$flushReact()
    expect_equal(engine$spec$revision, 6L)
    expect_identical(engine$spec$updated_by, "ui")
    expect_length(engine$spec$conc_plot$group_by, 0)
  })
})
test_that("initial connections authenticate before starting the PK module", {
  token <- paste(rep("a", 64), collapse = "")
  app <- desktop_app(engine, token)
  expect_s3_class(app, "shiny.appobj")
  withr::local_options(list(teal.lockfile.mode = "disabled"))
  server <- app$serverFuncSource()
  module_id <- "pk_analysis-teal_modules-nav-pk解析-module-"
  connect <- function(search) {
    session <- shiny::MockShinySession$new()
    session$clientData <- shiny::reactiveValues(url_search = search, pixelratio = 1)
    session$clientData[[paste0("output_", module_id, "plot_width")]] <- 800
    session$clientData[[paste0("output_", module_id, "plot_height")]] <- 440
    # Like runApp, call the root server outside a reactive consumer.
    shiny::withReactiveDomain(session, server(session$input, session$output, session))
    session
  }
  for (search in list(NULL, "", "?token=wrong", paste0("?token_extra=", token))) {
    expect_true(connect(search)$isClosed())
  }
  session <- connect(paste0("?token=", token))
  withr::defer(session$close())
  expect_false(session$isClosed())
  session$setInputs(workspace_navigation = "analysis", "pk_analysis-teal_modules-active_module_id" = "PK解析")
  expect_match(session$getOutput(paste0(module_id, "revision")), "2 例")
  expect_match(session$getOutput(paste0(module_id, "table")), "CMAX ng/mL", fixed = TRUE)
  expect_match(session$getOutput(paste0(module_id, "plot"))$src, "^data:image/png")
})
test_that("HTTP dispatch preserves the same canonical spec and structured envelope", {
  token <- paste(rep("b", 64), collapse = "")
  api <- desktop_http_app(engine, list(token = token, instance_id = "test-instance"))
  body <- charToRaw("{}")
  reply <- api$call(list(HTTP_AUTHORIZATION = paste("Bearer", token), REQUEST_METHOD = "POST", PATH_INFO = "/v1/spec",
    CONTENT_TYPE = "application/json", CONTENT_LENGTH = as.character(length(body)), rook.input = list(read = function() body)))
  decoded <- jsonlite::fromJSON(reply$body, simplifyVector = FALSE)
  expect_equal(reply$status, 200L)
  expect_true(decoded$ok)
  expect_identical(decoded$result$spec, engine$spec)
})
unlink(scratch, recursive = TRUE)
cat("PK Desktop M0 engine tests completed\n")
