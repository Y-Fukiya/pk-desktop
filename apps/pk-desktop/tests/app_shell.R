source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)

shell_session <- function(app, query) {
  session <- shiny::MockShinySession$new()
  session$clientData <- shiny::reactiveValues(url_search = query, pixelratio = 1)
  server <- app$serverFuncSource()
  shiny::withReactiveDomain(session, server(session$input, session$output, session))
  session
}

test_that("desktop navigation retains teal and authenticates before starting modules", {
  project <- pkapp_open(tempfile("pk-shell-auth-"))
  withr::defer(pkapp_close(project))
  app <- core$desktop_app(project$engine, "shell-test-token")
  html <- app$httpHandler(list(REQUEST_METHOD = "GET", PATH_INFO = "/", QUERY_STRING = "token=shell-test-token"))
  expect_equal(html$status, 200L)
  for (label in c("データ準備", "解析", "結果・帳票", "解析用データの設定")) expect_match(html$content, label, fixed = TRUE)
  expect_match(html$content, "pk_analysis", fixed = TRUE)
  expect_match(html$content, "workbench-root", fixed = TRUE)
  for (query in c("", "?token=wrong")) {
    bad <- shell_session(app, query)
    expect_true(bad$isClosed())
    expect_length(project$engine$jobs, 0)
  }
  good <- shell_session(app, "?token=shell-test-token")
  withr::defer(good$close())
  expect_false(good$isClosed())
  good$setInputs(workspace_navigation = "analysis")
  expect_false(good$isClosed())
  good$setInputs(workspace_navigation = "reports")
  expect_false(good$isClosed())
})

test_that("analysis data strip tracks active input rather than saved workbench tables", {
  project <- pkapp_open(tempfile("pk-shell-context-"))
  withr::defer(pkapp_close(project))
  engine <- project$engine
  app <- core$desktop_app(engine, "counts-test")
  session <- shell_session(app, "?token=counts-test")
  withr::defer(session$close())
  initial <- session$getOutput("analysis_context")$html
  expect_match(initial, "750 行", fixed = TRUE)
  expect_match(initial, "900 行", fixed = TRUE)
  expect_match(initial, "作業テーブルの保存だけでは", fixed = TRUE)
  # A saved prep table does not replace the engine's current analysis bundle.
  saved <- core$desktop_table_save(engine, "確認用の作業表", list(source = engine$data$ADNCA[1:12, ]), list(kind = "prepare", steps = list()))
  session$flushReact()
  expect_identical(session$getOutput("analysis_context")$html, initial)
  # The strip is invalidated by the same dedicated signal as input application.
  engine$data$ADNCA <- engine$data$ADNCA[1:12, ]
  engine$data$ADPP <- engine$data$ADPP[1:8, ]
  engine$data_signal(shiny::isolate(engine$data_signal()) + 1L)
  session$flushReact()
  expect_match(session$getOutput("analysis_context")$html, "12 行", fixed = TRUE)
  expect_match(session$getOutput("analysis_context")$html, "8 行", fixed = TRUE)
})

test_that("results keep the chosen frozen Run when a new Run is saved", {
  project <- pkapp_open(tempfile("pk-shell-runs-"))
  withr::defer(pkapp_close(project))
  engine <- project$engine
  first <- pkapp_run(project)
  shiny::testServer(core$desktop_module_server, args = list(engine = engine), {
    session$setInputs(report_run = first$run_id)
    before <- output$run_manifest
    pkapp_update(project, "report", list(title = "次のRunだけのタイトル"), pkapp_spec(project)$revision, "Future report setting")
    second <- pkapp_run(project)
    session$flushReact()
    expect_match(output$run_selector$html, paste0('value="', first$run_id, '" selected'), fixed = TRUE)
    expect_match(output$run_selector$html, second$run_id, fixed = TRUE)
    expect_identical(output$run_manifest, before)
    expect_match(output$selected_run_summary$html, first$run_id, fixed = TRUE)
    expect_false(grepl("次のRunだけのタイトル", output$selected_run_summary$html, fixed = TRUE))
    session$setInputs(report_reason = "Chosen frozen Run acceptance", export_report = 1)
    session$flushReact()
    expect_match(output$notice, "Word作成を受け付けました", fixed = TRUE)
    expect_identical(tail(engine$jobs, 1)[[1]]$run_id, first$run_id)
  })
})
cat("App shell tests completed\n")
