source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
library(testthat)
core <- environment(pkapp_open)

test_that("non-terminating display conversions preserve exact hour boundaries and NCA results", {
  project <- tempfile(); dir.create(project); on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  data <- data.frame(ID = "001", TIME = c(0, .5, 1, 7, 8), CONC = c(0, 2, 10, 3, 2), UNIT = "mg/L")
  source <- core$desktop_table_save(engine, "boundary fixture", list(source = data), list(kind = "prepare", steps = list()))$id
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), {
    session$setInputs(source = source, id = "ID", time = "TIME", concentration = "CONC", groups = character(),
      unit_column = "UNIT", time_unit = "h", time_origin = 0, start = .5, end = 7,
      last_observation = FALSE, c0_method = "none", auc_method = "linear")
    session$flushReact()
    baseline <- pkapp_nca(data, specification())
    expect_true(baseline$trace$rows$USE_AUC[baseline$trace$rows$TIME_H == .5])
    expect_true(baseline$trace$rows$USE_AUC[baseline$trace$rows$TIME_H == 7])
    for (i in seq_len(3L)) {
      session$setInputs(time_unit = "min"); session$setInputs(time_origin = 0, start = 30, end = 420)
      session$setInputs(time_unit = "day"); session$setInputs(time_origin = 0, start = signif(.5/24, 6), end = signif(7/24, 6))
      expect_identical(specification()$start, .5)
      expect_identical(specification()$end, 7.0)
      session$setInputs(time_unit = "h"); session$setInputs(time_origin = 0, start = .5, end = 7)
      expect_identical(specification()$start, .5)
      expect_identical(specification()$end, 7.0)
    }
    after <- pkapp_nca(data, specification())
    expect_identical(after$data, baseline$data)
    expect_identical(after$trace$rows, baseline$trace$rows)
    session$setInputs(time_unit = "day"); session$setInputs(time_origin = 0, start = signif(.5/24, 6), end = signif(7/24, 6))
    session$setInputs(start = .03125)
    expect_identical(specification()$start, .75)
    session$setInputs(time_unit = "h"); session$setInputs(time_origin = 0, start = .75, end = 7)
    expect_identical(specification()$start, .75)
  })
})

test_that("explicit analysis end must be finite and later than start", {
  project <- tempfile(); dir.create(project); on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  data <- data.frame(ID = "001", TIME = 0:2, CONC = c(0, 2, 1), UNIT = "mg/L")
  source <- core$desktop_table_save(engine, "boundary fixture", list(source = data), list(kind = "prepare", steps = list()))$id
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), {
    session$setInputs(source = source, id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT",
      c0_method = "none", time_unit = "h", time_origin = 0, start = 0, last_observation = FALSE, end = NA_real_)
    nca_confirm_displayed(session, input, output)
    expect_false(is.null(time_boundary_error()))
    expect_match(output$run_action$html, "disabled", fixed = TRUE)
    expect_match(output$period_order_run_hint$html, "解析終了を入力", fixed = TRUE)
    session$setInputs(end = -5)
    expect_false(is.null(time_boundary_error()))
    expect_match(output$run_action$html, "disabled", fixed = TRUE)
    session$setInputs(end = 1)
    expect_null(time_boundary_error())
    expect_false(grepl('disabled="disabled"', output$run_action$html, fixed = TRUE))
    session$setInputs(end = NA_real_, last_observation = TRUE)
    expect_null(time_boundary_error())
  })
})

test_that("loading BLQ-prepared data restores its hour-based origin without reconversion", {
  project <- tempfile(); dir.create(project); on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  raw <- data.frame(ID = "001", TIME = c(5, 6, 7, 8), CONC = c(0, 10, 3, 1), BLQ = 0, UNIT = "mg/L")
  step <- list(op = "blq_rules", concentration = "CONC", blq_column = "BLQ", time = "TIME", mode = "positional",
    profile_columns = "ID", time_origin = 5, zero_time = "post", pre_rule = "zero", first_rule = "zero",
    middle_rule = "half_lloq", last_rule = "exclude", all_blq_rule = "zero", lloq_value = .1)
  prepared <- pkapp_prepare(raw, list(step))$data
  plain_id <- core$desktop_table_save(engine, "plain", list(source = raw), list(kind = "prepare", steps = list()))$id
  prepared_id <- core$desktop_table_save(engine, "prepared", list(source = prepared), list(kind = "prepare", steps = list()))$id
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), {
    session$setInputs(source = plain_id, id = "ID", time = "TIME", concentration = "CONC", groups = character(),
      unit_column = "UNIT", time_unit = "h", time_origin = 0, start = .5, end = 7, last_observation = FALSE, c0_method = "none")
    session$setInputs(time_unit = "min"); session$setInputs(time_origin = 0, start = 30, end = 420)
    session$setInputs(source = prepared_id)
    expect_false(is.null(pending_time_conversion()))
    session$setInputs(time_unit = "h", time_origin = 5, start = 5, end = 7)
    expect_null(pending_time_conversion())
    expect_identical(specification()$time_origin, 5.0)
    expect_identical(specification()$start, 5.0)
    expect_identical(specification()$end, 7.0)
  })
})

test_that("coded and previously localized diagnostics share the same translation", {
  code <- "fewer_than_3_positive_post_cmax"
  japanese <- core$desktop_nca_diagnostic_label(code)
  coded <- as.character(core$desktop_nca_diagnostics(code))
  localized <- as.character(core$desktop_nca_diagnostics(japanese))
  expect_match(coded, "Fewer than three positive observed quantifiable concentrations after Tmax", fixed = TRUE)
  expect_match(localized, "Fewer than three positive observed quantifiable concentrations after Tmax", fixed = TRUE)
  expect_false(grepl("Untranslated diagnostic", localized, fixed = TRUE))
  combined <- paste0(japanese, "。算出不可：LAMZ, LAMZHL, AUCIFO, AUCPEO")
  combined_html <- as.character(core$desktop_i18n_diagnostic(combined, combined))
  expect_match(combined_html, "Fewer than three positive observed quantifiable concentrations after Tmax. Not calculable: LAMZ, LAMZHL, AUCIFO, AUCPEO", fixed = TRUE)
  expect_false(grepl("Untranslated diagnostic", combined_html, fixed = TRUE))
  auc_ja <- core$desktop_nca_diagnostic_label("AUCLST must start at the dose reference; the selected interval starts later")
  auc_combined <- paste0(auc_ja, "。算出不可：AUCLST")
  expect_match(as.character(core$desktop_i18n_diagnostic(auc_combined, auc_combined)), "Not calculable: AUCLST", fixed = TRUE)
  unknown <- as.character(core$desktop_nca_diagnostics("未知診断 <script>"))
  expect_false(grepl("<script>", unknown, fixed = TRUE))
})

test_that("audit labels translate without rewriting user-supplied text", {
  event <- list(action = "configure", at = "2026-01-01", channel = "ui", actor = "未記録",
    reason = "未設定", identity = list(os_user = "未記録"), changes = list(list(field = "ID", before = NULL, after = "未設定")))
  html <- as.character(core$desktop_nca_audit_view(list(event)))
  expect_match(html, 'data-pk-en="Not set"', fixed = TRUE)
  expect_match(html, 'data-pk-en="Record"', fixed = TRUE)
  expect_match(html, "未設定", fixed = TRUE)
  expect_match(html, "未記録", fixed = TRUE)
})
