source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
library(testthat)
core <- environment(pkapp_open)
source("apps/pk-desktop/tests/helper_nca_jobs.R")
elapsed_step <- list(op = "elapsed_time", sample_column = "SAMPLE", dose_column = "DOSE", utc_offset = "+09:00", output = "NCA_TIME_H")
blq_step <- list(op = "blq_rules", concentration = "CONC", blq_column = "BLQ", time = "NCA_TIME_H",
  mdv_column = "", predose_column = "", zero_time = "pre", pre_rule = "zero", post_rule = "half_lloq",
  lloq_source = "constant", lloq_value = 0.2, lloq_column = "LLOQ")
fixture <- data.frame(ID = "001", SAMPLE = paste("2026-11-09", c("08:30:00", "09:00:00", "10:00:00", "11:00:00", "13:00:00")),
  DOSE = "2026-11-09T09:00:00+09:00", CONC = c("BLQ", "BLQ", "8", "4", "BLQ"), BLQ = c(1L, 1L, 0L, 0L, 1L), UNIT = "ng/mL")
new_engine <- function() { path <- tempfile("pk-prep-"); dir.create(path); core$desktop_nca_engine(path) }

test_that("elapsed hours use explicit offsets and preserve original rows and dates", {
  value <- pkapp_prepare(fixture, list(elapsed_step))
  expect_equal(value$data$NCA_TIME_H, c(-0.5, 0, 1, 2, 4))
  expect_identical(value$data[names(fixture)], fixture)
  expect_equal(value$trace[[1]]$rows$STATUS, rep("derived", 5))
  dates <- data.frame(SAMPLE = c("2024-03-01T00:30:00+09:00", "2024-11-03T01:30:00-05:00", "2024-03-10T03:30:00-04:00", "2024-01-01T00:00:00.5Z", NA, "2024-01-01 00:00:00"),
    DOSE = c("2024-02-29T23:30:00+09:00", "2024-11-03T01:30:00-04:00", "2024-03-10T01:30:00-05:00", "2023-12-31T23:59:59.5Z", "2024-01-01 00:00:00", NA))
  expected <- c(1, 1, 1, 1/3600, NA, NA)
  expect_equal(pkapp_prepare(dates, list(elapsed_step))$data$NCA_TIME_H, expected)
  withr::local_envvar(TZ = "America/New_York")
  expect_equal(pkapp_prepare(dates, list(elapsed_step))$data$NCA_TIME_H, expected)
  expect_equal(tail(pkapp_prepare(dates, list(elapsed_step))$trace[[1]]$rows$STATUS, 2), c("missing_sample", "missing_dose"))
  empty <- pkapp_prepare(fixture[FALSE, ], list(elapsed_step))
  expect_equal(nrow(empty$data), 0)
  expect_type(empty$data$NCA_TIME_H, "double")
})

test_that("partial and invalid datetimes cannot silently become valid measurements", {
  bad <- c("2023-02-29 12:00:00", "2024-04-31 12:00:00", "2024-01-01", "2024-01-01 24:00:00", "2024-01-01 12:60:00", "2024-01-01 12:00:60", "2024-01-01 12:00:00 garbage", "2024-01-01T12:00:00+14:01", "2024-01-01T12:00:00+09:99")
  for (sample in bad) {
    d <- data.frame(SAMPLE = sample, DOSE = "2024-01-01 12:00:00")
    expect_true(is.na(pkapp_prepare(d, list(elapsed_step))$data$NCA_TIME_H))
    expect_error(pkapp_prepare(d, list(modifyList(elapsed_step, list(invalid_datetime = "error")))))
  }
  expect_error(pkapp_prepare(fixture, list(modifyList(elapsed_step, list(utc_offset = "Asia/Tokyo")))), "UTC")
  expect_error(pkapp_prepare(fixture, list(modifyList(elapsed_step, list(output = "CONC")))), "上書き")
})

test_that("BLQ rules are rowwise, explicit at zero, and preserve exclusions", {
  data <- pkapp_prepare(fixture, list(elapsed_step))$data
  value <- pkapp_prepare(data, list(blq_step))
  expect_identical(value$data[names(data)], data)
  expect_equal(value$data$NCA_CONC, c(0, 0, 8, 4, 0.1))
  expect_equal(value$data$NCA_MDV, rep(0L, 5))
  expect_identical(value$data$NCA_BLQ, data$BLQ)
  expect_equal(value$data$NCA_BLQ_RULE, c("pre_zero", "pre_zero", "recorded", "recorded", "post_half_lloq"))
  expect_error(pkapp_prepare(data, list(modifyList(blq_step, list(zero_time = "error")))), "時刻が0")
  post <- pkapp_prepare(data, list(modifyList(blq_step, list(zero_time = "post"))))$data
  expect_equal(post$NCA_CONC[2], 0.1)
  data$PREDOSE <- c(1, 0, 0, 0, 1)
  flagged <- pkapp_prepare(data, list(modifyList(blq_step, list(predose_column = "PREDOSE", zero_time = "error"))))$data
  expect_equal(flagged$NCA_CONC[c(1,2,5)], c(0, 0.1, 0))
  data$MDV <- c(0, 0, 1, 0, 0); data$CONC[3] <- "invalid but excluded"
  excluded <- pkapp_prepare(data, list(modifyList(blq_step, list(mdv_column = "MDV", post_rule = "exclude"))))$data
  expect_equal(excluded$NCA_MDV, c(0,0,1,0,1))
  expect_equal(excluded$NCA_BLQ_RULE[c(3,5)], c("existing_mdv", "blq_excluded"))
  expect_true(all(is.na(excluded$NCA_CONC[c(3,5)])))
  expect_error(pkapp_prepare(data, list(blq_step)), "Non-numeric")
  data$CONC[3] <- NA; data$NCA_TIME_H[4] <- NA
  missing <- pkapp_prepare(data, list(blq_step))$data
  expect_equal(missing$NCA_BLQ_RULE[3:4], c("missing_concentration", "missing_time"))
  data$BLQ[1] <- 2
  expect_error(pkapp_prepare(data, list(blq_step)), "flag")
  expect_error(pkapp_prepare(value$data, list(blq_step)), "上書き")
})

test_that("LLOQ validation follows substituted rows and never infers BLQ from concentration", {
  data <- pkapp_prepare(fixture, list(elapsed_step))$data
  data$LLOQ <- c("bad", "", NA, "0", "0.4")
  from_column <- modifyList(blq_step, list(lloq_source = "column"))
  expect_equal(pkapp_prepare(data, list(from_column))$data$NCA_CONC[5], 0.2)
  for (bad in c("0", "-1", NA, "bad", "Inf", "1e309", "4e-324")) {
    data$LLOQ[5] <- bad
    expect_error(pkapp_prepare(data, list(from_column)))
  }
  data$CONC[5] <- "0.01"; data$BLQ[5] <- 0L
  expect_equal(pkapp_prepare(data, list(from_column))$data$NCA_CONC[5], 0.01)
  expect_error(pkapp_prepare(data, list(modifyList(blq_step, list(pre_rule = "keep")))), "Non-numeric")
  data$CONC[1:2] <- c("0.02", "0.03")
  kept <- pkapp_prepare(data, list(modifyList(blq_step, list(pre_rule = "keep"))))$data
  expect_equal(kept$NCA_CONC[1:2], c(0.02, 0.03))
})

test_that("saved preparation replays and NCA stops at original quantifiable endpoint", {
  value <- pkapp_prepare(fixture, list(elapsed_step, blq_step))
  engine <- new_engine()
  saved <- core$desktop_table_save(engine, "前処理例", list(source = fixture), list(kind = "prepare", steps = list(elapsed_step, blq_step)))
  folder <- core$desktop_table_path(engine, saved$id)
  expect_identical(pkapp_replay(folder)$data, value$data)
  expect_true(file.exists(file.path(folder, "trace.json")))
  trace <- core$desktop_read(file.path(folder, "trace.json"))
  expect_length(trace[[2]]$rows, 5)
  specification <- list(id = "ID", time = "NCA_TIME_H", concentration = "NCA_CONC", unit_column = "UNIT",
    blq_column = "NCA_BLQ", blq = "as_recorded", mdv_column = "NCA_MDV", auc_method = "linear")
  nca <- pkapp_nca(value$data, specification)
  expect_equal(nca$data$AVAL, c(8, 1, 10))
  expect_equal(nca$data$AUC_LAST_H, rep(2, 3))
  expect_equal(nca$data$TLAST_QUANT_H, rep(2, 3))
  expect_length(nca$warnings, 0)
  expect_equal(nca$trace$rows$CONC_USED[5], 0.1)
  expect_equal(nca$trace$rows$STATUS[1], "interval")
})

test_that("standalone workbench can apply, edit and save preprocessing before NCA", {
  engine <- new_engine()
  path <- tempfile(fileext = ".csv"); core$pk_write_csv(fixture, path)
  shiny::testServer(core$desktop_workbench_server, args = list(engine = engine), {
    session$setInputs(source = "FILE", mode = "catalog", op = "", save_name = "PK前処理")
    expect_null(base())
    expect_match(output$state_ui$html, "データはまだありません")
    expect_match(output$save_actions$html, 'disabled="disabled"')
    expect_false(grepl('ADNCA', output$catalog$html, fixed = TRUE))
    session$setInputs(file = list(name = "input.csv", datapath = path), load_source = 1)
    session$setInputs(mode = "prepare", op = "elapsed_time", sample_column = "SAMPLE", dose_column = "DOSE", utc_offset = "+09:00", elapsed_output = "NCA_TIME_H", apply_step = 1)
    expect_equal(result()$data$NCA_TIME_H, c(-0.5,0,1,2,4))
    session$setInputs(op = "blq_rules", blq_concentration = "CONC", prep_blq_column = "BLQ", blq_time = "NCA_TIME_H", prep_mdv_column = "", predose_column = "", zero_time = "pre", pre_rule = "zero", post_rule = "half_lloq", lloq_source = "constant", lloq_value = 0.2, apply_step = 2)
    expect_equal(result()$data$NCA_CONC, c(0,0,8,4,0.1))
    session$setInputs(op = "", edit_step_index = "2", edit_step = 1)
    expect_equal(editing(), 2)
    session$setInputs(op = "blq_rules", lloq_value = 0.4, apply_step = 3)
    expect_equal(result()$data$NCA_CONC[5], 0.2)
    session$setInputs(op = "", save_result = 1)
    expect_false(is.null(receipt()))
    expect_identical(pkapp_replay(core$desktop_table_path(engine, receipt()$id))$data, result()$data)
  })
  saved_manifest <- core$desktop_tables(engine)[[1]]
  mapping_updates <- new.env(parent = emptyenv())
  mock <- shiny::MockShinySession$new()
  mock$sendInputMessage <- function(id, message) { mapping_updates[[sub("^.*-", "", id)]] <- message; invisible(NULL) }
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), session = mock, {
    session$setInputs(source = saved_manifest$id, concentration = "CONC", time = "SAMPLE", time_unit = "min")
    session$setInputs(concentration = "NCA_CONC", time = "NCA_TIME_H")
    expect_equal(mapping_updates$time_unit$value, "h")
    expect_null(output$blq_controls)
    expect_equal(mapping_updates$blq_column$value, "NCA_BLQ")
    expect_equal(mapping_updates$mdv_column$value, "NCA_MDV")
    mapping_updates$blq <- NULL
    session$setInputs(blq = "zero")
    expect_null(mapping_updates$blq)
    expect_true(prepared_mapping())
    expect_match(output$mapping$html, 'value="NCA_CONC" selected')
    expect_match(output$mapping$html, 'value="NCA_TIME_H" selected')
    expect_match(output$prepared_hint$html, "再適用")
    # The mock does not apply updateNumericInput messages as a browser would.
    # Acknowledge both hour-based boundaries before requesting a run.
    session$setInputs(id = "ID", time = "NCA_TIME_H", groups = character(), time_unit = "h", unit_column = "UNIT", concentration_unit = "", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV", scope = "all", time_origin = 0, start = 0, last_observation = TRUE, auc_method = "linear", blq = "as_recorded", c0_method = "none")
    expect_identical(specification()$blq, "as_recorded")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    expect_equal(result()$value$data$AVAL, c(8,1,10))
    session$setInputs(concentration = "CONC")
    expect_match(output$blq_controls$html, 'value="as_recorded" selected', fixed = TRUE)
    expect_identical(xml2::xml_attr(nca_review_markup(output, "blq"), "data-review-state"), "unconfirmed")
    expect_equal(mapping_updates$blq_column$value, "BLQ")
    expect_equal(mapping_updates$mdv_column$value, "")
    expect_false(prepared_mapping()); expect_null(result())
    mapping_updates$time_unit <- NULL
    session$setInputs(time = "SAMPLE", time_unit = "min", concentration = "NCA_CONC")
    expect_null(mapping_updates$time_unit)
  })
  shell <- core$desktop_nca_app(engine, "test-token")
  html <- htmltools::renderTags(shell$httpHandler(list(REQUEST_METHOD = "GET", PATH_INFO = "/", QUERY_STRING = "token=test-token"))$content)$html
  expect_match(html, "データ準備")

  edit_data <- fixture
  edit_data$SAMPLE2 <- edit_data$DOSE
  edit_data$DOSE2 <- edit_data$DOSE
  edit_data$CONC2 <- "1"
  edit_data$BLQ2 <- 0L
  edit_data$MDV <- edit_data$MDV2 <- 0L
  edit_data$PREDOSE <- c(1L, 1L, 0L, 0L, 0L)
  edit_data$PREDOSE2 <- 0L
  edit_data$LLOQ <- 0.2
  edit_data$LLOQ2 <- 0.4
  edit_file <- tempfile(fileext = ".csv"); core$pk_write_csv(edit_data, edit_file)
  edit_updates <- new.env(parent = emptyenv())
  edit_session <- shiny::MockShinySession$new()
  edit_session$sendInputMessage <- function(id, message) { edit_updates[[sub("^.*-", "", id)]] <- message; invisible(NULL) }
  shiny::testServer(core$desktop_workbench_server, args = list(engine = new_engine()), session = edit_session, {
    session$setInputs(source = "FILE", mode = "catalog", op = "", file = list(name = "edit.csv", datapath = edit_file), load_source = 1)
    session$setInputs(mode = "prepare", op = "elapsed_time", sample_column = "SAMPLE", dose_column = "DOSE", utc_offset = "+09:00", elapsed_output = "NCA_TIME_H", apply_step = 1)
    session$setInputs(sample_column = "SAMPLE2", dose_column = "DOSE2", elapsed_output = "TIME2", apply_step = 2)
    first <- steps()[[1]]
    session$setInputs(op = "", edit_step_index = "1", edit_step = 1)
    expect_equal(edit_updates$sample_column$value, "SAMPLE")
    expect_equal(edit_updates$dose_column$value, "DOSE")
    echoed <- lapply(c("sample_column", "dose_column", "utc_offset", "elapsed_output"), function(id) edit_updates[[id]]$value)
    names(echoed) <- c("sample_column", "dose_column", "utc_offset", "elapsed_output")
    do.call(session$setInputs, c(echoed, list(op = "elapsed_time", apply_step = 3)))
    expect_identical(steps()[[1]], first)
    expect_equal(result()$data$NCA_TIME_H, c(-0.5, 0, 1, 2, 4))
    expect_equal(result()$data$TIME2, rep(0, 5))
    session$setInputs(op = "blq_rules", blq_concentration = "CONC", prep_blq_column = "BLQ", blq_time = "NCA_TIME_H",
      prep_mdv_column = "MDV", predose_column = "PREDOSE", zero_time = "error", pre_rule = "zero", post_rule = "half_lloq",
      lloq_source = "column", lloq_column = "LLOQ", lloq_value = 0.2, apply_step = 4)
    third <- steps()[[3]]
    session$setInputs(op = "", blq_concentration = "CONC2", prep_blq_column = "BLQ2", blq_time = "TIME2",
      prep_mdv_column = "MDV2", predose_column = "PREDOSE2", lloq_column = "LLOQ2", edit_step_index = "3", edit_step = 2)
    expected <- c(blq_concentration = "CONC", prep_blq_column = "BLQ", blq_time = "NCA_TIME_H",
      prep_mdv_column = "MDV", predose_column = "PREDOSE", lloq_column = "LLOQ")
    for (id in names(expected)) expect_equal(edit_updates[[id]]$value, unname(expected[[id]]))
    restored <- c(names(expected), "zero_time", "pre_rule", "post_rule", "lloq_source", "lloq_value")
    echoed <- stats::setNames(lapply(restored, function(id) edit_updates[[id]]$value), restored)
    echoed$lloq_value <- as.numeric(echoed$lloq_value)
    do.call(session$setInputs, c(echoed, list(op = "blq_rules", apply_step = 5)))
    expect_identical(steps()[[3]], third)
    expect_equal(result()$data$NCA_CONC, c(0, 0, 8, 4, 0.1))
  })
})
