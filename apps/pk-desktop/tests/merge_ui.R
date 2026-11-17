source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
project <- pkapp_open(tempfile("pk-merge-ux-"))
engine <- project$engine
keys <- c("STUDYID", "USUBJID")
is_disabled <- function(html) grepl("\\sdisabled(?:\\s|=|>)", html, perl = TRUE)

test_that("join UI names source groups and distinguishes saved history from CSV download", {
  html <- as.character(core$desktop_merge_ui("join"))
  expect_match(html, "結合する表を選ぶ", fixed = TRUE)
  expect_match(html, "結合結果を確認する", fixed = TRUE)
  expect_match(html, 'optgroup label="元データ"', fixed = TRUE)
  expect_match(html, "全行・全列をPCへダウンロード", fixed = TRUE)
  expect_match(html, 'tabindex="0"', fixed = TRUE)
})

test_that("save receipts belong to current join conditions and invalid states cannot publish", {
  duplicate_file <- tempfile(fileext = ".csv")
  lookup <- engine$data$ADPP[engine$data$ADPP$PARAMCD == "CMAXSS", ]
  core$pk_write_csv(rbind(lookup, lookup[1, ]), duplicate_file)
  shiny::testServer(core$desktop_merge_server, args = list(engine = engine), {
    session$setInputs(left = "ADNCA", right = "ADPP", param = "CMAXSS", keys = keys,
      columns = c("PARAMCD", "AVAL", "AVALU"), method = "left", save_name = "UX結合確認")
    expect_equal(result()$counts[["result"]], 750)
    expect_match(output$qc_summary$html, "左表の一致", fixed = TRUE)
    expect_match(output$qc_summary$html, "右表（PARAMCD絞込後）", fixed = TRUE)
    expect_match(output$qc_summary$html, "750 行", fixed = TRUE)
    expect_match(output$preview_caption$html, "全 750 行", fixed = TRUE)
    expect_false(is_disabled(output$save_action$html))

    session$setInputs(save_join = 1)
    expect_match(saved_notice(), "保存しました", fixed = TRUE)
    expect_length(core$desktop_tables(engine), 1L)
    first_id <- save_receipt()$id
    expect_equal(nrow(core$desktop_table_get(engine, first_id)), 750)

    # Even if row values are identical, a changed join recipe is a new unsaved result.
    session$setInputs(method = "inner")
    expect_match(saved_notice(), "未保存", fixed = TRUE)
    expect_false(grepl("保存しました", output$saved_ui$html, fixed = TRUE))
    session$setInputs(save_join = 2)
    expect_match(saved_notice(), "保存しました", fixed = TRUE)
    expect_length(core$desktop_tables(engine), 2L)

    session$setInputs(save_name = "別の保存名")
    expect_match(saved_notice(), "未保存", fixed = TRUE)
    session$setInputs(right = "CSV", right_csv = NULL)
    expect_identical(result()$state, "empty")
    expect_match(output$qc_summary$html, "右表のCSVを選択", fixed = TRUE)
    expect_null(output$preview)
    expect_null(output$download_ui)
    expect_identical(saved_notice(), "")
    expect_true(is_disabled(output$save_action$html))
    session$setInputs(save_join = 3)
    expect_length(core$desktop_tables(engine), 2L)

    session$setInputs(right_csv = list(datapath = duplicate_file))
    expect_identical(result()$state, "error")
    expect_match(output$qc_summary$html, "キーが重複", fixed = TRUE)
    expect_null(output$preview)
    expect_null(output$download_ui)
    expect_true(is_disabled(output$save_action$html))
    session$setInputs(save_join = 4)
    expect_length(core$desktop_tables(engine), 2L)

    session$setInputs(right = "ADPP", method = "left", save_name = "修正後")
    expect_equal(result()$counts[["result"]], 750)
    expect_match(output$download_ui$html, "結合結果をCSVで保存", fixed = TRUE)
    expect_false(grepl("保存しました", saved_notice(), fixed = TRUE))
    exported <- core$pk_csv(output$csv)
    expect_equal(nrow(exported), 750)
    expect_identical(exported$USUBJID, engine$data$ADNCA$USUBJID)
    expect_identical(as.numeric(exported$ADPP_AVAL), lookup$AVAL[match(exported$USUBJID, lookup$USUBJID)])
  })
  unlink(duplicate_file)
})

test_that("QC describes unmatched left rows for each method without calling them subjects", {
  sample <- list(data = list(
    ADNCA = data.frame(STUDYID = "A", USUBJID = c("001", "001", "002"), AVAL = c(1, 2, 3)),
    ADPP = data.frame(STUDYID = "A", USUBJID = "001", PARAMCD = "CMAXSS", AVAL = 2)))
  shiny::testServer(core$desktop_merge_server, args = list(engine = sample), {
    session$setInputs(left = "ADNCA", right = "ADPP", param = "CMAXSS", keys = keys,
      columns = "AVAL", method = "left")
    expect_equal(unname(result()$counts), c(3, 1, 2, 1, 3))
    expect_match(output$qc_summary$html, "追加列は欠測", fixed = TRUE)
    expect_match(output$qc_summary$html, "pk-notice-warning", fixed = TRUE)
    session$setInputs(method = "inner")
    expect_equal(result()$counts[["result"]], 2)
    expect_match(output$qc_summary$html, "結合結果から除外", fixed = TRUE)
  })
})

test_that("a zero-row left input is explicitly empty rather than a successful match claim", {
  sample <- list(data = list(ADNCA = engine$data$ADNCA[0, ], ADPP = engine$data$ADPP))
  shiny::testServer(core$desktop_merge_server, args = list(engine = sample), {
    session$setInputs(left = "ADNCA", right = "ADPP", param = "CMAXSS", keys = keys,
      columns = "AVAL", method = "left")
    expect_equal(nrow(result()$data), 0L)
    expect_match(output$qc_summary$html, "左表は0行", fixed = TRUE)
    expect_false(grepl("全行に対応", output$qc_summary$html, fixed = TRUE))
    expect_false(is_disabled(output$save_action$html))
    expect_match(output$download_ui$html, "CSV", fixed = TRUE)
  })
})

pkapp_close(project)
