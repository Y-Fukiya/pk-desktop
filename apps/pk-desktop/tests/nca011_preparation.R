source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
raw <- data.frame(ID = rep("001", 8), PERIOD = rep(1:2, each = 4), TIME = rep(c(0, 1/3600, 1/3, 2), 2),
  CONC = rep(c(0, 12.345678901234567, 4, NA_real_), 2), BLQ = rep(c(0, 0, 0, 1), 2), MDV = 0,
  SAMPLE = rep(c("2026-01-01T00:00:00Z", "2026-01-01T00:00:01Z", "2026-01-01T00:20:00Z", "2026-01-01T02:00:00Z"), 2),
  DOSE = "2026-01-01T00:00:00Z")
step <- list(op = "blq_rules", concentration = "CONC", blq_column = "BLQ", time = "TIME", mdv_column = "MDV",
  mode = "positional", profile_columns = c("ID", "PERIOD"), zero_time = "post", pre_rule = "zero",
  first_rule = "zero", middle_rule = "half_lloq", last_rule = "exclude", all_blq_rule = "zero", lloq_value = 0.1)
spec <- list(id = "ID", groups = "PERIOD", time = "TIME", time_unit = "h", time_origin = 0, scope = "all")
prepared <- pkapp_prepare(raw, list(step))$data

test_that("prepared context survives exact CSV export and dependency preserving operations", {
  context <- core$desktop_pk_blq_context(prepared)
  expect_identical(unlist(context$profile_columns), c("ID", "PERIOD"))
  expect_identical(prepared$NCA_ORIGINAL_MDV, rep(0L, nrow(raw)))
  expect_silent(core$desktop_pk_blq_validate(prepared, spec))
  reordered <- prepared[c(8:5, 4:1), ]
  expect_silent(core$desktop_pk_blq_validate(reordered, spec))
  expect_silent(core$desktop_pk_blq_validate(transform(prepared, UNRELATED = seq_len(nrow(prepared))), spec))
  expect_silent(core$desktop_pk_blq_validate(prepared[prepared$PERIOD == 2, ], spec))
  path <- tempfile(fileext = ".csv")
  core$pk_write_csv(prepared, path)
  imported <- pkapp_read_table(path)
  expect_silent(core$desktop_pk_blq_validate(imported, spec))
  expect_identical(imported$NCA_BLQ_PROFILE_SIGNATURE, prepared$NCA_BLQ_PROFILE_SIGNATURE)
  elapsed <- list(op = "elapsed_time", sample_column = "SAMPLE", dose_column = "DOSE", output = "CUSTOM_H")
  derived <- pkapp_prepare(raw, list(elapsed, modifyList(step, list(time = "CUSTOM_H"))))$data
  core$pk_write_csv(derived, path)
  expect_silent(core$desktop_pk_blq_validate(pkapp_read_table(path), modifyList(spec, list(time = "CUSTOM_H"))))
})

test_that("prepared contract rejects inconsistent scope mappings row membership and values", {
  expect_error(pkapp_prepare(raw, list(modifyList(step, list(time = "ID")))), "時間列")
  expect_error(pkapp_prepare(raw, list(modifyList(step, list(time = "PERIOD")))), "時間列")
  expect_error(core$desktop_pk_blq_validate(prepared, modifyList(spec, list(groups = character()))), "解析単位")
  expect_error(core$desktop_pk_blq_validate(prepared, modifyList(spec, list(time_unit = "min"))), "時間")
  expect_error(core$desktop_pk_blq_validate(prepared, modifyList(spec, list(time_origin = 1))), "時間")
  expect_error(core$desktop_pk_blq_validate(prepared[-1, ], spec), "再適用")
  expect_error(core$desktop_pk_blq_validate(rbind(prepared, prepared[1, ]), spec), "再適用")
  expect_error(core$desktop_pk_blq_validate(transform(prepared, CONC = replace(CONC, 2, 100)), spec), "再適用")
  expect_error(core$desktop_pk_blq_validate(transform(prepared, NCA_CONC = replace(NCA_CONC, 2, 100)), spec), "再適用")
  expect_error(core$desktop_pk_blq_validate(prepared, spec, scope = c(FALSE, rep(TRUE, 7))), "一部")
  expect_silent(core$desktop_pk_blq_validate(prepared, spec, scope = prepared$PERIOD == 2))
  expect_error(core$desktop_pk_blq_context(prepared[setdiff(names(prepared), "NCA_BLQ_CONTEXT")]), "再適用")
  mixed <- prepared; mixed$NCA_BLQ_CONTEXT[2] <- "{}"
  expect_error(core$desktop_pk_blq_context(mixed), "再適用")
  expect_error(core$desktop_pk_blq_validate(prepared[setdiff(names(prepared), "CONC")], spec), "元の列")
})

test_that("BLQ time mapping prefers new derived time and saved mappings with compatible choices", {
  expect_identical(core$desktop_pk_default_groups(c("PERIOD", "APERIOD", "STUDYID", "ANALYTE", "ID")), c("STUDYID", "ANALYTE", "PERIOD", "APERIOD"))
  choices <- core$desktop_pk_time_choices(raw, c("CONC", "BLQ", "MDV"))
  expect_identical(unname(choices), "TIME")
  expect_identical(names(choices), "TIME")
  imported <- raw; imported$TIME <- as.character(imported$TIME)
  expect_true("TIME" %in% core$desktop_pk_time_choices(imported))
  messages <- list()
  mock <- shiny::MockShinySession$new()
  mock$sendInputMessage <- function(id, message) { messages[[id]] <<- message }
  shiny::withReactiveDomain(mock, {
    input <- shiny::reactiveValues(blq_time = "ID", profile_columns = c("ID", "PERIOD"), blq_concentration = "CONC", prep_blq_column = "BLQ", prep_mdv_column = "MDV")
    data <- transform(raw, CUSTOM_H = TIME)
    core$desktop_pk_prepare_update_columns(mock, input, names(data), data = data, preferred_time = "CUSTOM_H")
    expect_identical(messages$blq_time$value, "CUSTOM_H")
    expect_false(grepl('value="ID"', messages$blq_time$options, fixed = TRUE))
    core$desktop_pk_prepare_update_columns(mock, input, names(data), step = step, data = data, preferred_time = "CUSTOM_H")
    expect_identical(messages$blq_time$value, "TIME")
  })
  html <- as.character(core$desktop_workbench_ui("work"))
  expect_match(html, 'id="work-apply_step_body"', fixed = TRUE)
  expect_match(html, 'type="radio"', fixed = TRUE)
  expect_match(html, 'output.save_available', fixed = TRUE)
})

test_that("workbench browser update messages replace stale ID time without losing original grid columns", {
  project <- tempfile("nca011-workbench-"); dir.create(project)
  engine <- core$desktop_nca_engine(project)
  path <- tempfile(fileext = ".csv"); core$pk_write_csv(raw, path)
  messages <- new.env(parent = emptyenv())
  mock <- shiny::MockShinySession$new()
  mock$sendInputMessage <- function(id, message) { messages[[sub("^.*-", "", id)]] <- message }
  shiny::testServer(core$desktop_workbench_server, args = list(engine = engine), session = mock, {
    session$setInputs(source = "FILE", mode = "catalog", op = "", blq_time = "ID", file = list(name = "raw.csv", datapath = path), load_source = 1)
    session$setInputs(mode = "prepare", op = "elapsed_time", sample_column = "SAMPLE", dose_column = "DOSE", elapsed_output = "SECONDS_H",
      sample_datetime_type = "iso", dose_datetime_type = "iso", utc_offset = "+00:00", invalid_datetime = "missing", apply_step_body = 1)
    expect_length(steps(), 1)
    expect_identical(messages$blq_time$value, "SECONDS_H")
    expect_match(output$blq_time_hint, "ID")
    session$setInputs(blq_time = messages$blq_time$value, op = "blq_rules", blq_mode = "positional", profile_columns = c("ID", "PERIOD"),
      blq_concentration = "CONC", prep_blq_column = "BLQ", prep_mdv_column = "MDV", predose_column = "", zero_time = "post",
      pre_rule = "zero", first_rule = "zero", middle_rule = "half_lloq", last_rule = "exclude", all_blq_rule = "zero", lloq_source = "constant", lloq_value = .1)
    expect_identical(form()$time, "SECONDS_H")
    expect_match(output$blq_time_hint, "SECONDS_H.*範囲")
    expect_identical(output$save_available, "false")
    html <- output$`grid-table`$html
    expect_match(html, "CONC")
    expect_match(html, "BLQ")
    expect_match(html, "SECONDS_H")
    session$setInputs(apply_step_body = 2)
    expect_length(steps(), 2)
    expect_identical(output$save_available, "true")
    expect_identical(steps()[[2]]$time, "SECONDS_H")
  })
})
