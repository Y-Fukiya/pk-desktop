source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
html <- function(x) if (is.list(x)) x$html else as.character(x)
fixture <- function(rename = FALSE, missing_unit = FALSE, duplicate = FALSE) {
  folder <- tempfile("mapping-upload-"); dir.create(folder)
  conc <- data.frame(USUBJID = c("001", "001", "010", "010"), TIME_H = c(0, 60, 0, 60),
    AVAL = c(0, 1.25, 0, 2.5), AVALU = if (missing_unit) "" else "ng/mL")
  params <- data.frame(USUBJID = c("001", "010"), PARAMCD = "CMAX", AVAL = c(1.25, 2.5), AVALU = "ng/mL")
  if (rename) { names(conc)[1:3] <- c("ID", "TIME", "CONC"); names(params)[1] <- "ID" }
  if (duplicate) conc <- rbind(conc, conc[1, , drop = FALSE])
  for (name in c("ADNCA", "ADPP")) dir.create(file.path(folder, name))
  a <- file.path(folder, "ADNCA/0.csv"); b <- file.path(folder, "ADPP/0.csv")
  core$pk_write_csv(conc, a); core$pk_write_csv(params, b)
  list(folder = folder, a = list(datapath = a, name = "concentrations.csv"),
    b = list(datapath = b, name = "parameters.csv"), conc = conc, params = params)
}

test_that("empty import explains next action and cannot apply", {
  project <- pkapp_open(tempfile("mapping-empty-")); withr::defer(pkapp_close(project))
  before <- project$engine$spec
  shiny::testServer(core$desktop_import_server, args = list(engine = project$engine), {
    expect_match(html(output$source_adnca), "ファイルを選択してください")
    expect_match(html(output$apply_action), 'disabled="disabled"')
    expect_match(html(output$validate_action), 'disabled="disabled"')
    session$setInputs(load = 1)
    expect_match(notice(), "先に現在の入力内容を検証")
    expect_identical(engine$spec, before)
    expect_null(staged())
  })
})

test_that("dropdown mapping stages files without replacing data then applies explicit converted data", {
  project <- pkapp_open(tempfile("mapping-apply-")); withr::defer(pkapp_close(project))
  files <- fixture(rename = TRUE); withr::defer(unlink(files$folder, recursive = TRUE))
  before <- project$engine$spec; original <- project$engine$data
  shiny::testServer(core$desktop_import_server, args = list(engine = project$engine), {
    session$setInputs(adnca = files$a, adpp = files$b, unit = "min", reason = "Column mapping test")
    expect_match(html(output$mapping_summary), "必須列を選択")
    session$setInputs(map_adnca_USUBJID = "ID", map_adnca_TIME_H = "TIME", map_adnca_AVAL = "CONC", map_adpp_USUBJID = "ID")
    expect_identical(mapping()$ADNCA$TIME_H, "TIME")
    expect_match(output$sample_map_adnca_USUBJID, "001 / 001 / 010", fixed = TRUE)
    expect_match(output$sample_map_adnca_USUBJID, "文字列")
    expect_match(html(output$map_adnca), "対応する元の列")
    expect_match(html(output$source_adnca), "先頭5行")
    session$setInputs(validate_import = 1)
    expect_identical(engine$spec, before)
    expect_identical(engine$data, original)
    expect_true(is_current()); expect_true(dir.exists(staged()$folder))
    expect_match(html(output$validation_result), "分 ÷ 60 → 時間", fixed = TRUE)
    expect_false(grepl('disabled="disabled"', html(output$apply_action)))
    folder <- staged()$folder
    session$setInputs(load = 1)
    expect_equal(engine$spec$revision, before$revision + 1L)
    expect_identical(engine$data$ADNCA$USUBJID, c("001", "001", "010", "010"))
    expect_equal(engine$data$ADNCA$TIME_H, c(0, 1, 0, 1))
    expect_equal(engine$data$ADNCA$AVAL, files$conc$CONC)
    expect_equal(engine$data$ADPP$AVAL, files$params$AVAL)
    expect_match(notice(), "取込完了")
    expect_null(staged()); expect_false(dir.exists(folder))
    expect_match(html(output$apply_action), 'disabled="disabled"')
  })
})

test_that("file, column, and unit edits invalidate validation and block stale apply", {
  project <- pkapp_open(tempfile("mapping-invalidation-")); withr::defer(pkapp_close(project))
  files <- fixture(); other <- fixture()
  withr::defer(unlink(c(files$folder, other$folder), recursive = TRUE))
  before <- project$engine$spec
  shiny::testServer(core$desktop_import_server, args = list(engine = project$engine), {
    session$setInputs(adnca = files$a, adpp = files$b, unit = "h", reason = "Guard test")
    session$setInputs(validate_import = 1); expect_true(is_current())
    old <- staged()$folder
    session$setInputs(adnca = other$a)
    expect_null(staged()); expect_false(dir.exists(old))
    session$setInputs(load = 1); expect_identical(engine$spec, before)
    session$setInputs(validate_import = 2); expect_true(is_current())
    session$setInputs(map_adnca_TIME_H = "AVAL")
    expect_null(staged()); expect_match(html(output$mapping_summary), "重複")
    session$setInputs(validate_import = 3)
    expect_null(staged()); expect_identical(notice_tone(), "error")
    session$setInputs(load = 2); expect_identical(engine$spec, before)
    session$setInputs(map_adnca_TIME_H = "TIME_H", validate_import = 4); expect_true(is_current())
    session$setInputs(unit = "day")
    expect_null(staged()); expect_match(notice(), "もう一度検証")
    session$setInputs(load = 3); expect_identical(engine$spec, before)
  })
})

test_that("missing time unit, missing value units, duplicate keys and invalid JSON cannot stage", {
  project <- pkapp_open(tempfile("mapping-invalid-")); withr::defer(pkapp_close(project))
  files <- fixture(); no_unit <- fixture(missing_unit = TRUE); dup <- fixture(duplicate = TRUE)
  withr::defer(unlink(c(files$folder, no_unit$folder, dup$folder), recursive = TRUE))
  before <- project$engine$spec
  shiny::testServer(core$desktop_import_server, args = list(engine = project$engine), {
    session$setInputs(adnca = files$a, adpp = files$b, reason = "Validation test", validate_import = 1)
    expect_null(staged()); expect_match(notice(), "時間単位を選択")
    session$setInputs(unit = "fortnight", validate_import = 2)
    expect_null(staged()); expect_match(notice(), "時間単位を選択")
    session$setInputs(unit = "h", adnca = no_unit$a, validate_import = 3)
    expect_null(staged()); expect_match(notice(), "AVALU")
    session$setInputs(adnca = dup$a, validate_import = 4)
    expect_null(staged()); expect_match(notice(), "Duplicate subject/time")
    session$setInputs(adnca = files$a, mapping_mode = "json", mapping = "{broken", validate_import = 5)
    expect_null(staged()); expect_identical(notice_tone(), "error")
    session$setInputs(mapping = '{"ADNCA":{"TIME_H":"UNKNOWN"},"ADPP":{}}', validate_import = 6)
    expect_null(staged()); expect_match(notice(), "マッピング元")
    session$setInputs(load = 1); expect_identical(engine$spec, before)
  })
})

test_that("JSON has an explicit mode and requires its own current successful validation", {
  project <- pkapp_open(tempfile("mapping-json-")); withr::defer(pkapp_close(project))
  files <- fixture(); withr::defer(unlink(files$folder, recursive = TRUE))
  before <- project$engine$spec
  shiny::testServer(core$desktop_import_server, args = list(engine = project$engine), {
    session$setInputs(adnca = files$a, adpp = files$b, unit = "h", reason = "JSON mode test", mapping = "invalid JSON")
    session$setInputs(validate_import = 1)
    expect_true(is_current()) # Unselected advanced settings do not override the visible dropdown mode.
    session$setInputs(mapping_mode = "json")
    expect_null(staged()); expect_false(is_current())
    session$setInputs(load = 1); expect_identical(engine$spec, before)
    session$setInputs(mapping = '{"ADNCA":{},"ADPP":{}}', validate_import = 2)
    expect_true(is_current())
    session$setInputs(load = 2)
    expect_equal(engine$spec$revision, before$revision + 1L)
    expect_equal(engine$data$ADNCA$TIME_H, files$conc$TIME_H)
  })
})

test_that("saved preparation tables feed mapping and analysis directly with their source IDs retained", {
  project <- pkapp_open(tempfile("mapping-saved-")); withr::defer(pkapp_close(project))
  files <- fixture(); withr::defer(unlink(files$folder, recursive = TRUE))
  a <- pkapp_save_preparation(project, files$conc, list(list(op = "rename", column = "TIME_H", name = "TIME")), "前処理した濃度")
  b <- pkapp_save_preparation(project, files$params, list(), "確認済みPK値")
  before <- project$engine$spec
  shiny::testServer(core$desktop_import_server, args = list(engine = project$engine), {
    expect_identical(source_id("ADNCA"), "FILE") # Saving a result never silently changes analysis inputs.
    session$setInputs(adnca_source = a$id, adpp_source = b$id, unit = "min", reason = "Use prepared tables")
    session$setInputs(map_adnca_TIME_H = "TIME")
    expect_match(html(output$source_adnca), "前処理した濃度")
    expect_match(output$sample_map_adnca_TIME_H, "数値")
    session$setInputs(validate_import = 1)
    expect_true(is_current()); expect_identical(engine$spec, before)
    session$setInputs(adpp_source = "ADPP")
    expect_null(staged())
    session$setInputs(adpp_source = b$id, validate_import = 2)
    expect_true(is_current())
    session$setInputs(load = 1)
    expect_match(notice(), "取込完了")
    expect_equal(engine$data$ADNCA$TIME_H, c(0, 1, 0, 1))
    expect_identical(engine$data$ADNCA$USUBJID, files$conc$USUBJID)
    expect_identical(engine$data$manifest$import$workspace_sources$ADNCA$source, a$id)
    expect_identical(engine$data$manifest$import$workspace_sources$ADPP$source, b$id)
    expect_identical(engine$data$manifest$import$workspace_sources$ADNCA$kind, "saved_table")
    expect_identical(engine$data$manifest$import$source_formats$ADNCA, "rds")
    expect_identical(engine$data$manifest$import$base_revision, before$revision)
  })
})

test_that("changed engine revision and bytes on disk cannot apply an old staged result", {
  project <- pkapp_open(tempfile("mapping-revision-")); withr::defer(pkapp_close(project))
  files <- fixture(); withr::defer(unlink(files$folder, recursive = TRUE))
  shiny::testServer(core$desktop_import_server, args = list(engine = project$engine), {
    session$setInputs(adnca = files$a, adpp = files$b, unit = "h", reason = "Stale check", validate_import = 1)
    expect_true(is_current())
    pkapp_update(project, "report", list(title = "Changed elsewhere"), engine$spec$revision, "Revision check")
    session$flushReact()
    current <- engine$spec
    expect_false(is_current())
    expect_match(html(output$validation_result), "最新の状態")
    session$setInputs(load = 1); expect_identical(engine$spec, current)
    session$setInputs(validate_import = 2); expect_true(is_current())
    changed <- files$conc; changed$AVAL[2] <- 99
    core$pk_write_csv(changed, files$a$datapath)
    session$setInputs(load = 2)
    expect_identical(engine$spec, current)
    expect_null(staged()); expect_match(notice(), "検証後に入力ファイルが変わりました")
  })
})
