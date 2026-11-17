source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
source("apps/pk-desktop/tests/helper_nca_jobs.R")
library(testthat)
core <- environment(pkapp_open)

review_status_of <- function(output, key) xml2::xml_attr(nca_review_markup(output, key), "data-review-state")
store_source <- function(engine, data, name) {
  core$desktop_table_save(engine, name, list(source = data), list(kind = "prepare", steps = list()))$id
}
run_disabled <- function(output) grepl('disabled="disabled"', output$run_action$html, fixed = TRUE)
fixture <- data.frame(ID = "A", TIME = c(-1, 0, 1, 2), CONC = c(0, 0, 8, 4),
  BLQ = c(1, 0, 0, 0), UNIT = "ng/mL")

test_that("one closed advance panel owns five sections and leaves computation controls in view", {
  doc <- xml2::read_html(as.character(core$desktop_nca_ui("nca")))
  advanced <- xml2::xml_find_first(doc, "//details[@id='nca-advanced']")
  expect_true(is.na(xml2::xml_attr(advanced, "open")))
  expect_length(xml2::xml_find_all(advanced, "./summary"), 1L)
  expect_identical(xml2::xml_attr(xml2::xml_find_all(advanced, "./div/section"), "id"),
    paste0("nca-advanced-", c("additional", "body", "qc", "ai", "adpp")))
  for (field in c("terminal_method", "terminal_qc_action", "time_unit", "c0_method", "dosing_context", "shared_nca", "run_action", "exports")) {
    expect_length(xml2::xml_find_all(doc, paste0("//*[@id='nca-", field, "']")), 1L)
    expect_length(xml2::xml_find_all(advanced, paste0(".//*[@id='nca-", field, "']")), 0L)
  }
  expect_length(xml2::xml_find_all(advanced, ".//*[@id='nca-predose_qc_lloq_value_control']"), 1L)
  expect_length(xml2::xml_find_all(advanced, ".//*[@id='nca-disclosure']"), 1L)
  expect_length(xml2::xml_find_all(advanced, ".//*[@id='nca-period_order_controls']"), 0L)
})

test_that("only current explicit choices unlock a run and a previous source cannot revive them", {
  project <- tempfile("first-use-"); dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  a <- store_source(engine, fixture, "A")
  b <- store_source(engine, transform(fixture, CONC = CONC * 2), "B")
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), {
    session$setInputs(source = a, id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT",
      blq_column = "BLQ", time_unit = "h", c0_method = "", dosing_context = "", blq = "",
      last_observation = TRUE)
    for (key in c("dosing", "blq", "c0")) expect_identical(review_status_of(output, key), "unselected")
    expect_identical(specification()$blq, "exclude")
    expect_true(run_disabled(output))
    session$setInputs(dosing_context = "ongoing", blq = "exclude", c0_method = "none")
    for (key in c("dosing", "blq", "c0", "units")) expect_identical(review_status_of(output, key), "unconfirmed")
    wrong <- nca_review_intent(input, output, "dosing"); wrong$value <- "single"
    session$setInputs(review_choice = wrong, run = 1)
    expect_identical(review_status_of(output, "dosing"), "unconfirmed")
    expect_null(engine$nca_job)
    expect_null(result())
    nca_confirm_displayed(session, input, output, supply_defaults = FALSE)
    expect_false(run_disabled(output))
    old <- nca_review_intent(input, output, "dosing")
    session$setInputs(source = b)
    session$setInputs(source = a, review_choice = old)
    for (key in c("dosing", "blq", "c0", "units")) expect_identical(review_status_of(output, key), "unconfirmed")
    nca_confirm_displayed(session, input, output, supply_defaults = FALSE)
    session$setInputs(concentration_unit = "mg/L")
    expect_identical(review_status_of(output, "units"), "unconfirmed")
    expect_identical(review_status_of(output, "dosing"), "confirmed")
    nca_confirm_displayed(session, input, output, keys = "units", supply_defaults = FALSE)
    session$setInputs(predose_qc = TRUE, predose_qc_threshold_pct = 5)
    expect_true(run_disabled(output))
    expect_match(output$first_use_issues$html, paste0('data-nca-focus="', session$ns("predose_qc_window_h"), '"'), fixed = TRUE)
    session$setInputs(predose_qc_window_h = 1, predose_qc_lloq_source = "constant")
    expect_false(run_disabled(output))
    expect_match(output$first_use_issues$html, "QCを評価できません", fixed = TRUE)
    session$setInputs(run = 2)
    nca_wait_for_job(engine, session)
    expect_equal(result()$value$data$AVAL[result()$value$data$PARAMCD == "CMAX"], 8)
    expect_identical(result()$value$trace$groups[[1]]$predose_qc$reason, "missing_lloq")
    expect_false("terminal_method" %in% names(result()$value$specification))
  })
})

test_that("shared restore displays the frozen result without authorizing a new run or publish", {
  project <- tempfile("loaded-first-use-"); dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  source <- store_source(engine, fixture, "restored")
  spec <- list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT", blq_column = "BLQ",
    blq = "zero", c0_method = "none", include_aucproc = TRUE,
    predose_qc_threshold_pct = 7, predose_qc_window_h = 1, predose_qc_lloq_source = "constant", predose_qc_lloq_value = .2)
  args <- list(project_id = engine$nca$project_id, base_revision = engine$nca$revision,
    source = source, specification = spec, actor = "test", reason = "restore first use")
  state <- core$desktop_nca_configure(engine, args)
  args$base_revision <- state$revision
  public <- core$desktop_nca_run_shared(engine, args)
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), {
    messages <- list()
    parent <- .subset2(session, "parent")
    parent$sendInputMessage <- function(id, message) messages[[id]] <<- message
    session$setInputs(source = "FILE", shared_refresh = 1)
    expect_identical(review_status_of(output, "dosing"), "loading")
    values <- lapply(messages, `[[`, "value")
    values <- Filter(Negate(is.null), values)
    for (field in intersect(names(values), c("end", "time_origin", "start", "predose_window_h",
        "predose_qc_threshold_pct", "predose_qc_window_h", "predose_qc_lloq_value", "infusion_end_h",
        "terminal_start_h", "terminal_end_h", "terminal_min_r_squared", "terminal_min_span_ratio", "aucinf_max_extrap_pct")))
      values[[field]] <- suppressWarnings(as.numeric(values[[field]]))
    values$review_choice <- list(key = "dosing", generation = nca_review_intent(input, output, "dosing")$generation, value = "ongoing")
    do.call(session$setInputs, values)
    expect_false(loading_shared())
    for (key in c("dosing", "blq", "c0", "units")) expect_identical(review_status_of(output, key), "loaded")
    expect_true(run_disabled(output))
    expect_identical(result()$shared_result_id, public$result_id)
    expect_identical(core$desktop_nca_specification(source_data(), specification()), engine$nca$specification)
    expect_match(output$predose_qc_lloq_value_control$html, 'value="0.2"', fixed = TRUE)
    session$setInputs(name = "Frozen result", save = 1)
    before <- list(revision = engine$nca$revision, audit = engine$nca$audit, value = result(), saved = saved())
    expect_false(is.null(before$saved))
    session$setInputs(run = 1)
    session$setInputs(shared_publish = 1)
    nca_confirm_displayed(session, input, output, supply_defaults = FALSE)
    expect_false(run_disabled(output))
    expect_identical(list(revision = engine$nca$revision, audit = engine$nca$audit, value = result(), saved = saved()), before)
    expect_false(core$desktop_nca_disclosure(engine, public$result_id)$allowed)
  })
})
