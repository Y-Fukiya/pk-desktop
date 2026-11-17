source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
library(testthat)
core <- environment(pkapp_open)

nca017_engine <- function() {
  path <- tempfile("nca017-ui-"); dir.create(path)
  core$desktop_nca_engine(path)
}
nca017_table <- function(engine, data, name = "input") core$desktop_table_save(engine, name,
  list(source = data), list(kind = "prepare", steps = list()))$id
nca017_args <- function(engine, source, specification) list(project_id = engine$nca$project_id,
  base_revision = engine$nca$revision, source = source, specification = specification, actor = "test MCP", reason = "UI integration test")
nca017_default <- function(session, source) session$setInputs(source = source, id = "ID", time = "TIME", concentration = "CONC",
  groups = character(), unit_column = "UNIT", time_unit = "h", concentration_unit = "", blq_column = "", mdv_column = "",
  scope = "all", time_origin = 0, start = 0, last_observation = TRUE, end = 24, c0_method = "none",
  predose_window_h = NA_real_, include_aucproc = FALSE, all_blq_policy = "missing", predose_qc = FALSE,
  predose_qc_threshold_pct = 5, predose_qc_window_h = NA_real_, predose_qc_lloq_source = "", predose_qc_lloq_column = "",
  predose_qc_lloq_value = NA_real_, predose_qc_supplement = FALSE, flag_convention = "strict", auc_method = "linear", blq = "exclude")
nca017_wait <- function(engine, session) {
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
nca017_selected <- function(html, id) {
  doc <- xml2::read_html(html)
  unname(xml2::xml_attr(xml2::xml_find_all(doc, paste0('.//select[@id="', id, '"]/option[@selected]')), "value"))
}

test_that("explicit shared load switches source and populates actual controls before displaying the frozen MCP result", {
  e <- nca017_engine()
  original <- data.frame(ID = "old", TIME = 0:2, CONC = c(0,2,1), UNIT = "ng/mL")
  old <- nca017_table(e, original, "old")
  target <- data.frame(ID = "default must not win", TIME = c(-30, 0, 60, 120), CONC = c(.2, 0, 8, 4), UNIT = "ng/mL",
    SUBJECT = "accepted", ELAPSED = c(-30, 0, 60, 120), VALUE = c(.2,0,6,3), PERIOD = "A", LLOQ = .1, SSNCAFL = "Y")
  source <- nca017_table(e, target, "target")
  spec <- list(id = "SUBJECT", time = "ELAPSED", concentration = "VALUE", groups = list("PERIOD"), unit_column = "UNIT",
    time_unit = "min", c0_method = "predose", predose_window_h = 1, end = 2, auc_method = "linear", include_aucproc = TRUE,
    predose_qc_threshold_pct = 7, predose_qc_window_h = 1, predose_qc_lloq_source = "column", predose_qc_lloq_column = "LLOQ")
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    messages <- list()
    host_session <- .subset2(session, "parent")
    host_session$sendInputMessage <- function(inputId, message) messages[[inputId]] <<- message
    nca017_default(session, old)
    state <- core$desktop_nca_configure(e, nca017_args(e, source, spec))
    a <- nca017_args(e, source, spec)
    public <- core$desktop_nca_run_shared(e, a)
    session$flushReact()
    expect_identical(input$source, old)
    expect_identical(input$id, "ID")
    expect_null(result())
    expect_match(output$shared_nca$html, "共有条件・結果を画面に読み込む", fixed = TRUE)
    expect_false(grepl("<pre", output$shared_nca$html, fixed = TRUE))
    messages <- list()
    session$setInputs(shared_refresh = 1)
    expect_true(loading_shared())
    expect_null(result())
    expect_identical(messages$source$value, source)
    expect_identical(messages$time_unit$value, "min")
    expect_equal(as.numeric(messages$predose_window_h$value), 1)
    expect_equal(as.numeric(messages$predose_qc_threshold_pct$value), 7)
    expect_false(messages$last_observation$value)
    session$setInputs(source = messages$source$value)
    mapping <- output$mapping$html
    for (name in c("id", "time", "concentration", "groups", "unit_column", "scope"))
      expect_identical(nca017_selected(mapping, session$ns(name)), unlist(loaded_form()$controls[[name]], use.names = FALSE))
    expect_identical(nca017_selected(output$predose_qc_lloq_controls$html, session$ns("predose_qc_lloq_column")), "LLOQ")
    values <- lapply(messages, function(x) x$value)
    values <- values[!vapply(values, is.null, logical(1))]
    for (name in c("time_origin", "start", "end", "predose_window_h", "predose_qc_threshold_pct", "predose_qc_window_h", "predose_qc_lloq_value"))
      values[[name]] <- suppressWarnings(as.numeric(values[[name]]))
    do.call(session$setInputs, values)
    session$flushReact()
    expect_false(loading_shared())
    expect_true(draft_matches_shared())
    expect_identical(input$source, e$nca$source)
    expect_identical(core$desktop_nca_specification(source_data(), specification()), e$nca$specification)
    expect_identical(result()$shared_result_id, public$result_id)
    expect_identical(result()$value, e$nca_results[[public$result_id]]$value)
    expect_identical(result()$input, target)
    expect_match(output$results$html, "濃度推移と最終定量時点", fixed = TRUE)
    expect_match(output$wide_table$html, "accepted", fixed = TRUE)
    expect_equal(nrow(core$desktop_nca_plot_data(result()$value, 1)), 4)
    expect_identical(input$time_unit, "min")
    expect_identical(input$id, "SUBJECT")
    session$setInputs(name = "frozen MCP result", save = 1)
    expect_identical(core$desktop_table_get(e, saved()$id), result()$value$data)
    expect_identical(saved()$provenance$execution, e$nca_result$execution)
    expect_identical(saved()$provenance$execution$channel, "mcp")
    expect_identical(saved()$provenance$saved_by, "local user")
    expect_false(grepl("<pre", output$conditions$html, fixed = TRUE))
    expect_match(output$conditions$html, "投与前QCの閾値", fixed = TRUE)
    # The loaded source uses minutes; 60 input minutes must remain 1 hour in the shared specification.
    session$setInputs(start = 60)
    expect_null(result())
    expect_identical(e$nca$specification$start, 0)
    revision <- e$nca$revision
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca017_wait(e, session)
    expect_equal(e$nca$revision, revision + 1)
    expect_equal(e$nca$specification$start, 1)
    expect_identical(result()$value, e$nca_result$value)
    expect_identical(result()$input, source_data())
    expect_identical(core$desktop_nca_specification(source_data(), specification()), e$nca$specification)
    session$setInputs(source = old)
    expect_null(result())
    expect_null(loaded_form())
    expect_identical(nca017_selected(output$mapping$html, session$ns("id")), "ID")
  })
})

test_that("new MCP conditions invalidate the screen result and a stale GUI cannot publish", {
  e <- nca017_engine()
  source <- nca017_table(e, data.frame(ID = "test", TIME = 0:2, CONC = c(0,8,4), UNIT = "ng/mL"))
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    nca017_default(session, source)
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca017_wait(e, session)
    expect_false(is.null(result()))
    old_revision <- shared_revision()
    a <- nca017_args(e, source, list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT", c0_method = "zero"))
    core$desktop_nca_configure(e, a)
    session$flushReact()
    expect_null(result())
    expect_identical(input$c0_method, "none")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 2)
    nca017_wait(e, session)
    expect_match(notice(), "changed")
    expect_equal(shared_revision(), old_revision)
    expect_equal(e$nca$revision, old_revision + 1)
    expect_identical(e$nca$specification$c0_method, "zero")
    expect_null(result())
  })
})

test_that("normal FILE run publishes one immutable source and exports that same shared value", {
  e <- nca017_engine()
  data <- data.frame(ID = "file", TIME = 0:2, CONC = c(0,8,4), UNIT = "ng/mL")
  csv <- tempfile(fileext = ".csv"); core$pk_write_csv(data, csv)
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    nca017_default(session, "FILE")
    session$setInputs(file = list(name = "input.csv", datapath = csv))
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca017_wait(e, session)
    expect_equal(e$nca$revision, 2)
    expect_length(core$desktop_tables(e), 1)
    expect_identical(core$desktop_table_get(e, e$nca$source), source_data())
    expect_identical(result()$value, e$nca_result$value)
    expect_identical(result()$input, core$desktop_table_get(e, e$nca$source))
    expect_identical(core$desktop_nca_specification(source_data(), specification()), e$nca$specification)
    expect_equal(as.numeric(pkapp_read_table(output$download, "csv")$AVAL), result()$value$data$AVAL)
    source_id <- e$nca$source
    session$setInputs(auc_method = "lin up/log down")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 2)
    nca017_wait(e, session)
    expect_identical(e$nca$source, source_id)
    expect_length(core$desktop_tables(e), 1)
  })
})

test_that("a later MCP run appears in the existing result view when the accepted screen conditions still match", {
  e <- nca017_engine()
  source <- nca017_table(e, data.frame(ID = "later MCP", TIME = 0:2, CONC = c(0,8,4), UNIT = "ng/mL"))
  spec <- list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT", c0_method = "none", auc_method = "linear")
  core$desktop_nca_configure(e, nca017_args(e, source, spec))
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    nca017_default(session, source)
    expect_null(result())
    public <- core$desktop_nca_run_shared(e, nca017_args(e, source, spec))
    session$flushReact()
    expect_identical(result()$shared_result_id, public$result_id)
    expect_identical(result()$value, e$nca_result$value)
    expect_match(output$table$html, "later MCP", fixed = TRUE)
    expect_match(output$profile_selector$html, "later MCP", fixed = TRUE)
    expect_equal(result()$value$specification$auc_method, input$auc_method)
    revision <- e$nca$revision
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca017_wait(e, session)
    expect_equal(e$nca$revision, revision)
    expect_false(identical(result()$shared_result_id, public$result_id))
    expect_equal(e$nca_result$execution$channel, "ui")
  })
})

test_that("prepared source defaults cannot replace an explicitly loaded interval or QC supplement", {
  e <- nca017_engine()
  raw <- data.frame(ID = "prepared", TIME = c(-.5, 0, 1, 2), CONC = c("<.2", "0", "8", "4"), BLQ = c(1,0,0,0), LIMIT = .2)
  prepared <- pkapp_prepare(raw, list(list(op = "blq_rules", mode = "positional", profile_columns = list("ID"),
    time = "TIME", concentration = "CONC", blq_column = "BLQ", zero_time = "pre", pre_rule = "zero",
    first_rule = "zero", middle_rule = "exclude", last_rule = "exclude")))$data
  source <- nca017_table(e, prepared)
  spec <- list(id = "ID", time = "TIME", concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV",
    blq = "as_recorded", c0_method = "none", start = 1, concentration_unit = "ng/mL", auc_method = "linear",
    predose_qc_threshold_pct = 5, predose_qc_window_h = 1, predose_qc_lloq_source = "column", predose_qc_lloq_column = "LIMIT")
  core$desktop_nca_configure(e, nca017_args(e, source, spec))
  public <- core$desktop_nca_run_shared(e, nca017_args(e, source, spec))
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    session$setInputs(source = "FILE")
    messages <- list()
    host_session <- .subset2(session, "parent")
    host_session$sendInputMessage <- function(inputId, message) messages[[inputId]] <<- message
    session$setInputs(shared_refresh = 1)
    session$setInputs(source = source)
    expect_identical(nca017_selected(output$mapping$html, session$ns("concentration")), "NCA_CONC")
    expect_identical(nca017_selected(output$predose_qc_lloq_controls$html, session$ns("predose_qc_lloq_column")), "LIMIT")
    values <- lapply(messages, function(x) x$value)
    values <- values[!vapply(values, is.null, logical(1))]
    for (name in c("time_origin", "start", "end", "predose_window_h", "predose_qc_threshold_pct", "predose_qc_window_h", "predose_qc_lloq_value"))
      values[[name]] <- suppressWarnings(as.numeric(values[[name]]))
    do.call(session$setInputs, values)
    expect_false(loading_shared())
    expect_equal(input$start, 1)
    expect_true(supplement_enabled())
    expect_identical(core$desktop_nca_specification(source_data(), specification()), e$nca$specification)
    expect_identical(result()$shared_result_id, public$result_id)
    expect_identical(result()$value, e$nca_result$value)
  })
})

test_that("summary mode keeps local calculation and saving independent of standalone sharing", {
  e <- nca017_engine(); e$mode <- "summary"
  data <- data.frame(ID = "summary", TIME = 0:2, CONC = c(0,8,4), UNIT = "ng/mL")
  csv <- tempfile(fileext = ".csv"); core$pk_write_csv(data, csv)
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    nca017_default(session, "FILE")
    session$setInputs(file = list(name = "summary.csv", datapath = csv))
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca017_wait(e, session)
    expect_identical(result()$value, pkapp_nca(source_data(), specification()))
    expect_null(result()$shared_result_id)
    expect_equal(e$nca$revision, 1)
    expect_null(e$nca$source)
    expect_length(core$desktop_tables(e), 0)
    session$setInputs(name = "summary result", save = 1)
    expect_identical(core$desktop_table_get(e, saved()$id), result()$value$data)
    expect_null(saved()$provenance)
    session$setInputs(start = 1)
    expect_null(result())
  })
})
