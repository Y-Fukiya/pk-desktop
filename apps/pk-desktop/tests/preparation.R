source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
fixture <- data.frame(STUDYID = "S", USUBJID = c("001", "010", "099", "101"),
  SAFFL = c("Y", "N", "Y", NA), TIME = c("0", "2", "4", NA), SEX = c("F", "M", "F", "M"))
steps <- list(list(op = "filter", column = "SAFFL", operator = "eq", values = list("Y")),
  list(op = "convert", column = "TIME", type = "numeric"),
  list(op = "rename", column = "TIME", name = "TAD"),
  list(op = "select", columns = list("STUDYID", "USUBJID", "TAD")))
test_that("declarative filters preserve IDs, missing policy, types and row counts", {
  result <- pkapp_prepare(fixture, steps)
  expect_identical(result$data$USUBJID, c("001", "099"))
  expect_identical(result$data$TAD, c(0, 4))
  expect_equal(unname(result$counts), c(4, 2))
  expect_length(result$trace, 4)
  expect_identical(fixture$TIME, c("0", "2", "4", NA_character_))
  expect_equal(nrow(pkapp_prepare(fixture, list(list(op = "filter", column = "TIME", operator = "ge", values = list("2"))))$data), 2)
  expect_equal(nrow(pkapp_prepare(fixture, list(list(op = "filter", column = "TIME", operator = "missing")))$data), 1)
  expect_error(pkapp_prepare(fixture, list(list(op = "convert", column = "USUBJID", type = "numeric"))), "ID列")
  expect_error(pkapp_prepare(fixture, list(list(op = "rename", column = "TIME", name = "SEX"))), "重複")
  expect_error(pkapp_prepare(fixture, list(list(op = "filter", column = "TIME", operator = "ge", values = list("bad")))), "Non-numeric")
  expect_error(pkapp_prepare(fixture, list(list(op = "eval", code = "stop('bad')"))), "未対応")
})
test_that("saved preparations are immutable, reloaded and replayed with matching outputs", {
  project <- pkapp_open(tempfile("prep-")); withr::defer(pkapp_close(project))
  saved <- pkapp_save_preparation(project, fixture, steps, "Safety lookup")
  folder <- core$desktop_table_path(project$engine, saved$id)
  expect_identical(pkapp_table(project, saved$id), pkapp_prepare(fixture, steps)$data)
  expect_identical(pkapp_replay(folder)$data, pkapp_table(project, saved$id))
  expect_equal(length(pkapp_tables(project)), 1)
  expect_identical(saved$channel, "r")
  expect_error(core$desktop_table_get(project$engine, "../state.json"), "ID")
  writeLines("tampered", file.path(folder, "recipe.json"))
  expect_error(pkapp_replay(folder), "checksum")
})
test_that("UI prepared table joins directly and persists reproducible unmatched counts", {
  project <- pkapp_open(tempfile("prep-ui-")); withr::defer(pkapp_close(project))
  engine <- project$engine
  file <- tempfile(fileext = ".csv"); core$pk_write_csv(fixture, file)
  shiny::testServer(core$desktop_prepare_server, args = list(engine = engine), {
    session$setInputs(source = "FILE", file = list(datapath = file, name = "ADSL.csv"))
    session$setInputs(op = "filter", column = "SAFFL", operator = "eq", value = "Y", add = 1)
    expect_equal(nrow(result()$data), 2)
    session$setInputs(name = "ADSL safety", publish = 1)
    expect_length(core$desktop_tables(engine), 1)
    bad <- tempfile(fileext = ".csv"); writeLines('ID,X\n"broken', bad)
    session$setInputs(file = list(datapath = bad, name = "bad.csv"))
    expect_false(valid_input())
    session$setInputs(publish = 2)
    expect_length(core$desktop_tables(engine), 1)
    session$setInputs(file = list(datapath = file, name = "good.csv"))
    expect_length(steps(), 0)
    expect_equal(nrow(result()$data), 4)
  })
  saved <- core$desktop_tables(engine)[[1]]
  observations <- tempfile(fileext = ".csv")
  core$pk_write_csv(fixture[c("STUDYID", "USUBJID")], observations)
  shiny::testServer(core$desktop_merge_server, args = list(engine = engine), {
    session$setInputs(left = "CSV", left_csv = list(datapath = observations), right = saved$id,
      keys = c("STUDYID", "USUBJID"), columns = "SEX", method = "left")
    expect_equal(unname(result()$counts), c(4, 2, 2, 2, 4))
    downloaded <- core$pk_csv(output$csv)
    expect_identical(downloaded$USUBJID, fixture$USUBJID)
    expect_identical(downloaded$SEX, c("F", NA, "F", NA))
    session$setInputs(save_name = "Joined", save_join = 1)
    expect_length(core$desktop_tables(engine), 2)
  })
  joined <- Filter(function(x) x$kind == "join", core$desktop_tables(engine))[[1]]
  expect_equal(nrow(pkapp_replay(core$desktop_table_path(engine, joined$id))$data), 4)
})
test_that("imports map columns, normalize time, reject invalid data and protect existing bundles", {
  source_bundle <- file.path(pkapp_root(), "outputs/pk-review/bundles/reviewed-repeated")
  data <- core$pk_load_bundle(source_bundle)
  conc <- data$ADNCA; names(conc)[names(conc) == "TIME_H"] <- "TIME"
  a <- tempfile(fileext = ".rds"); b <- tempfile(fileext = ".xpt")
  saveRDS(conc, a); haven::write_xpt(data$ADPP, b, version = 8)
  destination <- tempfile("import-")
  mapping <- list(ADNCA = list(TIME_H = "TIME"), ADPP = list())
  pkapp_import_bundle(a, b, destination, mapping)
  imported <- core$pk_load_bundle(destination)
  expect_equal(imported$ADNCA$AVAL, data$ADNCA$AVAL)
  expect_equal(imported$ADPP$AVAL, data$ADPP$AVAL)
  expect_equal(imported$ADNCA$TIME_H, data$ADNCA$TIME_H)
  expect_error(pkapp_import_bundle(a, b, destination, mapping), "新しい")
  expect_error(pkapp_map_columns(conc, list(AVAL = "TIME")), "重複")
  expect_error(pkapp_map_columns(conc, list(TIME_H = "invalid")), "存在")
  expect_error(pkapp_read_table(a, "exe"), "CSV")
  mins <- conc
  for (name in intersect(c("TIME", "TAD_H", "ANALYSIS_DOSE_TIME_H"), names(mins))) mins[[name]] <- as.numeric(mins[[name]]) * 60
  saveRDS(mins, a)
  converted <- pkapp_import_bundle(a, b, tempfile("minutes-"), mapping, "min")
  expect_equal(core$pk_load_bundle(converted)$ADNCA$TIME_H, data$ADNCA$TIME_H)
  project_path <- tempfile("import-project-")
  project <- pkapp_open(project_path)
  revision <- pkapp_spec(project)$revision
  pkapp_load_data(project, destination, revision, "format import test")
  expect_equal(pkapp_spec(project)$revision, revision + 1)
  pkapp_close(project)
  reopened <- pkapp_open(project_path)
  withr::defer(pkapp_close(reopened))
  expect_identical(pkapp_spec(reopened)$data$hashes, imported$hashes)
  expect_error(core$desktop_allowed_bundle(reopened$engine, destination), "inside")
})
test_that("UI import keeps separate uploads with identical temporary basenames", {
  project <- pkapp_open(tempfile("import-ui-")); withr::defer(pkapp_close(project))
  engine <- project$engine
  folder <- tempfile("uploads-"); dir.create(folder)
  withr::defer(unlink(folder, recursive = TRUE))
  for (name in c("ADNCA", "ADPP")) {
    dir.create(file.path(folder, name))
    core$pk_write_csv(engine$data[[name]], file.path(folder, name, "0.csv"))
  }
  expected <- lapply(engine$data[c("ADNCA", "ADPP")], function(x) x$AVAL)
  initial <- engine$spec$revision
  shiny::testServer(core$desktop_import_server, args = list(engine = engine), {
    session$setInputs(
      adnca = list(datapath = file.path(folder, "ADNCA/0.csv"), name = "concentrations.csv"),
      adpp = list(datapath = file.path(folder, "ADPP/0.csv"), name = "parameters.csv"),
      mapping_mode = "json", mapping = '{"ADNCA":{},"ADPP":{}}', unit = "h", reason = "UI import test")
    session$setInputs(validate_import = 1)
    expect_equal(engine$spec$revision, initial)
    session$setInputs(load = 1)
    expect_match(notice(), "取込完了")
    expect_equal(engine$spec$revision, initial + 1)
    expect_equal(lapply(engine$data[c("ADNCA", "ADPP")], function(x) x$AVAL), expected)
  })
})
test_that("multi-variable strata have independent counts and means and collision-free labels", {
  project <- pkapp_open(tempfile("strata-")); withr::defer(pkapp_close(project))
  pkapp_update(project, "conc_plot", list(group_by = c("ARM", "SEX")), 1, "ARM by SEX")
  analysed <- core$pk_analyse(project$engine$data, core$desktop_config(pkapp_spec(project)))
  raw <- project$engine$data$ADPP
  raw <- raw[raw$PARAMCD %in% unlist(pkapp_spec(project)$param_summary$paramcd), ]
  if ("ANL01FL" %in% names(raw)) raw <- raw[core$pk_flag(raw$ANL01FL), ]
  for (i in seq_len(nrow(analysed$parameter_summary))) {
    row <- analysed$parameter_summary[i, ]
    key <- jsonlite::fromJSON(row$.GROUP)
    subset <- raw[raw$ARM == key$ARM & raw$SEX == key$SEX & raw$PARAMCD == row$PARAMCD & raw$AVALU == row$AVALU, ]
    expect_equal(row$n, sum(!is.na(subset$AVAL)))
    expect_equal(row$mean, mean(subset$AVAL, na.rm = TRUE))
  }
  fake <- data.frame(ARM = c("A | B", "A", NA, "NA"), SEX = c("C", "B | C", "F", "F"))
  expect_equal(length(unique(core$pk_group(fake, c("ARM", "SEX"))$.GROUP)), 4)
})

test_that("flat Parquet preserves character IDs, doubles, missing values and integer64 text", {
  skip_if_not_installed("nanoparquet")
  path <- tempfile(fileext = ".parquet")
  value <- data.frame(USUBJID = c("001", "010"), AVAL = c(1.125, NA_real_))
  nanoparquet::write_parquet(value, path)
  expect_identical(pkapp_read_table(path), value)
  value$LARGE_ID <- bit64::as.integer64(c("9007199254740993", "9007199254740995"))
  nanoparquet::write_parquet(value, path)
  actual <- pkapp_read_table(path)
  expect_identical(actual$LARGE_ID, c("9007199254740993", "9007199254740995"))
  expect_identical(actual$USUBJID, c("001", "010"))
})
