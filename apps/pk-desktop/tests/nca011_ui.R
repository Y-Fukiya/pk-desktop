source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
library(testthat)
core <- environment(pkapp_open)
source("apps/pk-desktop/tests/helper_nca_jobs.R")

nca_ui_engine <- function() {
  path <- tempfile("nca011-ui-")
  dir.create(path)
  core$desktop_nca_engine(path)
}
nca_ui_configure <- function(session, data, c0 = "none") {
  path <- tempfile(fileext = ".csv")
  core$pk_write_csv(data, path)
  session$setInputs(source = "FILE", file = list(datapath = path, name = "observations.csv"),
    id = "ID", time = "TIME", concentration = "CONC", groups = character(), time_unit = "h",
    concentration_unit = "ng/mL", unit_column = "", blq_column = "", mdv_column = "",
    scope = "all", time_origin = 0, start = 0, last_observation = TRUE, auc_method = "linear",
    blq = "exclude", c0_method = c0, all_blq_policy = "missing", name = "NCAの確認")
}

test_that("C0 requires an explicit UI choice and enabled predose QC requires its window", {
  observations <- data.frame(ID = "001", TIME = c(0, 1, 2), CONC = c(0, 8, 4))
  shiny::testServer(core$desktop_nca_server, args = list(engine = nca_ui_engine()), {
    nca_ui_configure(session, observations, c0 = "")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    expect_null(result())
    expect_match(notice(), "C0の方針を選択")
    session$setInputs(c0_method = "none")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 2)
    nca_wait_for_job(engine, session)
    expect_equal(result()$value$data$AVAL, c(8, 1, 10))
    session$setInputs(predose_qc = TRUE, predose_qc_threshold_pct = 5, predose_qc_window_h = NA_real_)
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 3)
    nca_wait_for_job(engine, session)
    expect_null(result())
    expect_match(notice(), "最大遡及時間")
    session$setInputs(predose_qc_window_h = 1)
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 4)
    nca_wait_for_job(engine, session)
    expect_equal(result()$value$data$AVAL, c(8, 1, 10))
    expect_equal(specification()$predose_qc_threshold_pct, 5)
    expect_equal(specification()$predose_qc_window_h, 1)
  })
})

test_that("prepared mapping uses recorded profile columns and time labels omit identity columns", {
  observations <- data.frame(ID = "001", ANALYTE = "parent", PERIOD = "2", TIME = c(-.25, .5, 1, 2), CONC = c(0, 3, 8, 4), BLQ = c(1, 0, 0, 0))
  step <- list(op = "blq_rules", mode = "positional", profile_columns = list("ID", "ANALYTE", "PERIOD"),
    time = "TIME", concentration = "CONC", blq_column = "BLQ", zero_time = "pre",
    pre_rule = "zero", first_rule = "zero", middle_rule = "exclude", last_rule = "exclude")
  prepared <- pkapp_prepare(observations, list(step))$data
  messages <- new.env(parent = emptyenv())
  mock <- shiny::MockShinySession$new()
  mock$sendInputMessage <- function(id, message) messages[[sub("^.*-", "", id)]] <- message
  shiny::testServer(core$desktop_nca_server, args = list(engine = nca_ui_engine()), session = mock, {
    nca_ui_configure(session, prepared, c0 = "zero")
    session$setInputs(concentration = "NCA_CONC", groups = c("ANALYTE", "PERIOD"))
    html <- output$mapping$html
    expect_match(html, 'value="ID" selected', fixed = TRUE)
    expect_match(html, 'value="ANALYTE" selected', fixed = TRUE)
    expect_match(html, 'value="PERIOD" selected', fixed = TRUE)
    expect_match(html, 'value="TIME" selected', fixed = TRUE)
    expect_match(output$time_hint$html, "TIME")
    expect_false(grepl('value="ID"', messages$time$options, fixed = TRUE))
    expect_false(grepl('value="PERIOD"', messages$time$options, fixed = TRUE))
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    expect_equal(result()$value$data$AVAL, c(8, 1, 9.5))
    session$setInputs(groups = "ANALYTE")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 2)
    nca_wait_for_job(engine, session)
    expect_null(result())
    expect_match(notice(), "解析単位")
  })
  raw <- transform(observations, APERIOD = PERIOD)
  shiny::testServer(core$desktop_nca_server, args = list(engine = nca_ui_engine()), {
    nca_ui_configure(session, raw)
    expect_match(output$mapping$html, 'value="PERIOD" selected', fixed = TRUE)
    expect_match(output$mapping$html, 'value="APERIOD" selected', fixed = TRUE)
  })
})

test_that("missing peak time is visible as review with profile counts and persisted warning", {
  observations <- data.frame(ID = "001", TIME = c(0, .5, NA, 2, 8), CONC = c(0, 3, 8, 6, 1.5))
  shiny::testServer(core$desktop_nca_server, args = list(engine = nca_ui_engine()), {
    nca_ui_configure(session, observations)
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    expect_equal(result()$value$data$AVAL[1:2], c(6, 2))
    wide <- core$desktop_nca_summary(result()$value)
    expect_false(any(c("N_MISSING_TIME", "N_EXCLUDED") %in% names(wide)))
    expect_equal(wide$PROFILE_STATUS, "review")
    expect_false(grepl("N_MISSING_TIME", output$profile_details$html, fixed = TRUE))
    expect_match(output$wide_table$html, "要確認")
    expect_match(output$profile_quality$html, "時間欠測 1 行", fixed = TRUE)
    expect_match(output$run_state, "要確認")
    session$setInputs(save = 1)
    saved_data <- core$desktop_table_get(engine, saved()$id)
    expect_true(all(saved_data$N_MISSING_TIME == 1))
    expect_match(output$run_state, "保存済み・要確認")
  })
})

test_that("all BLQ policy assignment is labeled and log display does not invalidate analysis", {
  observations <- data.frame(ID = "001", TIME = c(0, 1, 2), CONC = "<0.2", BLQ = 1L)
  shiny::testServer(core$desktop_nca_server, args = list(engine = nca_ui_engine()), {
    nca_ui_configure(session, observations, c0 = "zero")
    session$setInputs(blq_column = "BLQ")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    expect_true(all(is.na(result()$value$data$AVAL)))
    session$setInputs(all_blq_policy = "zero")
    expect_null(result())
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 2)
    nca_wait_for_job(engine, session)
    value <- result()$value
    expect_equal(value$data$AVAL, c(0, NA, 0))
    expect_match(output$table$html, "方針による値")
    expect_match(core$desktop_nca_summary(value)$NCA_WARNING, "CMAX")
    expect_match(core$desktop_nca_summary(value)$NCA_WARNING, "AUCLST")
    session$setInputs(plot_scale = "log")
    expect_identical(result()$value, value)
    expect_match(output$plot_note$html, "片対数表示")
    session$setInputs(save = 1)
    expect_false(is.null(saved()))
    session$setInputs(plot_scale = "linear")
    expect_identical(result()$value, value)
    expect_false(is.null(saved()))
  })
})

test_that("semilog plot hides nonpositive points without changing data or endpoint", {
  observations <- data.frame(ID = "001", TIME = c(-.25, .5, 1, 2, 4, 8), CONC = c(0, 3, 8, 6, 4, 1.5))
  value <- pkapp_nca(observations, list(id = "ID", time = "TIME", concentration = "CONC", concentration_unit = "ng/mL",
    c0_method = "zero", auc_method = "linear"))
  original <- value
  linear <- core$desktop_nca_plot_data(value, 1, "linear")
  logarithmic <- core$desktop_nca_plot_data(value, 1, "log")
  expect_equal(sum(linear$PLOT_VALID) - sum(logarithmic$PLOT_VALID), 2)
  expect_equal(sum(logarithmic$PLOT_NONPOSITIVE), 2)
  expect_false(any(logarithmic$PLOT_VALID & logarithmic$PLOT_CONC <= 0))
  destination <- tempfile(fileext = ".pdf")
  grDevices::pdf(destination, width = 11, height = 3.6)
  suppressWarnings(core$desktop_nca_plot(value, 1, "log"))
  grDevices::dev.off()
  expect_gt(file.info(destination)$size, 500)
  expect_identical(value, original)
  expect_equal(value$data$TLAST_QUANT_H, rep(8, 3))
  expect_error(core$desktop_nca_plot_data(value, 1, "unknown"))
  collision <- data.frame(POLICY_ASSIGNED = rep(c("001", "002"), each = 3), TIME = rep(c(0, 1, 2), 2), CONC = rep(c(0, 8, 4), 2))
  result <- pkapp_nca(collision, list(id = "POLICY_ASSIGNED", time = "TIME", concentration = "CONC",
    concentration_unit = "ng/mL", auc_method = "linear"))
  expect_equal(core$desktop_nca_summary(result)$SOURCE_POLICY_ASSIGNED, c("001", "002"))
  expect_false("POLICY_ASSIGNED" %in% names(core$desktop_nca_summary(result)))
})
