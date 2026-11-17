if (dir.exists(".runtime/R-library")) .libPaths(c(".runtime/R-library", .libPaths()))
library(testthat)
core <- new.env(parent = globalenv())
sys.source("apps/pk-desktop/R/grid.R", envir = core)

fixture <- data.frame(STUDYID = "S", USUBJID = sprintf("%03d", seq_len(75L)),
  AVAL = pi * seq_len(75L), AVALU = "ng/mL", stringsAsFactors = FALSE)

test_that("every observation is reachable and display choices never change source values", {
  table <- shiny::reactiveVal(fixture)
  shiny::testServer(core$desktop_grid_server, args = list(data = table,
    preferred = shiny::reactive(c("USUBJID", "AVAL"))), {
    session$flushReact()
    expect_identical(view(), fixture[1:30, c("USUBJID", "AVAL")])
    expect_identical(status()$total_rows, 75L)
    session$setInputs(next_page = 1)
    expect_identical(view(), fixture[31:60, c("USUBJID", "AVAL")])
    session$setInputs(next_page = 2)
    expect_identical(view(), fixture[61:75, c("USUBJID", "AVAL")])
    expect_identical(status()$visible_rows, 15L)
    expect_match(output$summary, "61–75 / 全 75 行", fixed = TRUE)
    session$setInputs(next_page = 3)
    expect_identical(current_page(), 3L)
    session$setInputs(previous = 1)
    expect_identical(current_page(), 2L)
    session$setInputs(page_size = "50")
    expect_identical(current_page(), 1L)
    expect_identical(view(), fixture[1:50, c("USUBJID", "AVAL")])
    session$setInputs(visible_columns = "USUBJID")
    expect_identical(names(view()), "USUBJID")
    expect_identical(view()$USUBJID[1], "001")
    expect_identical(source(), fixture)
    expect_identical(source()$AVAL, pi * seq_len(75L))
    session$setInputs(visible_columns = character())
    expect_error(output$table, "表示する列を1つ以上選択")
    expect_identical(source(), fixture)
  })
})

test_that("literal search stays within the chosen column and resets pages", {
  special <- fixture
  special$USUBJID[1:3] <- c("ID.1", "ID[1]", "IDa1")
  table <- shiny::reactiveVal(special)
  shiny::testServer(core$desktop_grid_server, args = list(data = table), {
    session$flushReact()
    session$setInputs(next_page = 1)
    expect_identical(current_page(), 2L)
    session$setInputs(search_column = "USUBJID", search = ".")
    expect_identical(current_page(), 1L)
    expect_identical(filtered(), special[1, , drop = FALSE])
    session$setInputs(search = "[")
    expect_identical(filtered(), special[2, , drop = FALSE])
    session$setInputs(search = "075")
    expect_identical(filtered(), special[75, , drop = FALSE])
    expect_match(output$summary, "検索一致 1 行（全 75 行）", fixed = TRUE)
    session$setInputs(search_column = "STUDYID")
    expect_equal(nrow(filtered()), 0L)
    expect_identical(status()$total_rows, 75L)
    expect_match(output$summary, "表示 0", fixed = TRUE)
    session$setInputs(search = "", search_column = "USUBJID", next_page = 2)
    session$setInputs(next_page = 3)
    expect_gt(current_page(), 1L)
    table(special[1:2, , drop = FALSE]); session$flushReact()
    expect_identical(current_page(), 1L)
    expect_identical(view(), special[1:2, , drop = FALSE])
    expect_identical(source(), special[1:2, , drop = FALSE])
  })
  expect_identical(special$AVAL, fixture$AVAL)
})

test_that("new preferred join columns are offered without restoring hidden existing columns", {
  table <- shiny::reactiveVal(fixture)
  preferred_columns <- shiny::reactiveVal(c("USUBJID", "AVAL"))
  messages <- list()
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendInputMessage <- function(inputId, message) {
    messages[[sub("^.*-", "", inputId)]] <<- message
  }
  shiny::testServer(core$desktop_grid_server, args = list(data = table, preferred = preferred_columns), session = mock_session, {
    session$flushReact()
    session$setInputs(visible_columns = "USUBJID")
    joined <- fixture
    joined$ADPP_AVAL <- fixture$AVAL * 2
    joined$ADPP_PARAMCD <- "CMAX"
    table(joined)
    preferred_columns(c("USUBJID", "AVAL", "ADPP_AVAL", "ADPP_PARAMCD"))
    session$flushReact()
    expect_identical(messages$visible_columns$value, c("USUBJID", "ADPP_AVAL", "ADPP_PARAMCD"))
    session$setInputs(visible_columns = messages$visible_columns$value)
    expect_identical(view(), joined[1:30, c("USUBJID", "ADPP_AVAL", "ADPP_PARAMCD")])
    session$setInputs(visible_columns = "USUBJID")
    table(joined[1:10, , drop = FALSE]); session$flushReact()
    expect_identical(columns(), "USUBJID")
    expect_identical(source(), joined[1:10, , drop = FALSE])
  })
})

test_that("an empty table is distinct from having no result", {
  table <- shiny::reactiveVal(NULL)
  shiny::testServer(core$desktop_grid_server, args = list(data = table), {
    session$flushReact()
    expect_false(status()$available)
    expect_null(view())
    expect_match(output$summary, "表示する結果はまだありません")
    table(fixture[0, , drop = FALSE]); session$flushReact()
    expect_true(status()$available)
    expect_identical(status()$total_rows, 0L)
    expect_identical(names(view()), names(fixture))
    expect_identical(current_page(), 1L)
    expect_match(output$summary, "全 0 行", fixed = TRUE)
  })
  markup <- as.character(core$desktop_grid_ui("grid"))
  expect_match(markup, "<details", fixed = TRUE)
  expect_false(grepl("<details[^>]* open", markup))
  metadata <- core$desktop_grid_columns(fixture)
  expect_identical(metadata[["型"]], c("character", "character", "numeric", "character"))
})

test_that("join diagnostics expose missing keys, all duplicate rows and composite unmatched keys", {
  left <- data.frame(STUDYID = c("A|B", "A", "A", "A", "A", NA),
    USUBJID = c("C", "B|C", "001", "999", " ", "001"),
    AVAL = pi * 1:6, .pk_diagnostic_row = 201:206, stringsAsFactors = FALSE)
  right <- data.frame(STUDYID = c("A|B", "A", "A", "A"),
    USUBJID = c("C", "001", "001", NA), PARAMCD = c("CMAX", "CMAX", "TMAX", "CMAX"),
    AVAL = c(pi, exp(1), sqrt(2), 0), stringsAsFactors = FALSE)
  left_before <- left; right_before <- right
  result <- core$desktop_join_diagnostics(left, right, c("STUDYID", "USUBJID"))
  expect_null(result$error)
  expect_identical(result$missing_left, left[5:6, , drop = FALSE])
  expect_identical(result$missing_right, right[4, , drop = FALSE])
  expect_identical(result$duplicate_right, right[2:3, , drop = FALSE])
  expect_identical(result$unmatched_left, left[c(2L, 4L), , drop = FALSE])
  expect_identical(result$row_indices$duplicate_right, 2:3)
  expect_identical(result$row_indices$unmatched_left, c(2L, 4L))
  expect_equal(result$counts$duplicate_right_keys, 1L)
  expect_equal(result$counts$duplicate_right_rows, 2L)
  expect_equal(result$counts$duplicate_right_excess, 1L)
  expect_equal(result$counts$matched_left, 2L)
  expect_equal(result$counts$unmatched_left, 2L)
  expect_identical(left, left_before)
  expect_identical(right, right_before)
})

test_that("invalid keys and incompatible types do not appear as successful diagnostics", {
  right <- fixture[0, , drop = FALSE]
  result <- core$desktop_join_diagnostics(fixture, right, c("STUDYID", "USUBJID"))
  expect_null(result$error)
  expect_identical(result$unmatched_left, fixture)
  expect_equal(result$counts$matched_left, 0L)
  expect_match(core$desktop_join_diagnostics(fixture, right, character())$error, "結合キー")
  expect_match(core$desktop_join_diagnostics(fixture, right, "UNKNOWN")$error, "結合キー")
  wrong_type <- fixture
  wrong_type$USUBJID <- as.integer(wrong_type$USUBJID)
  result <- core$desktop_join_diagnostics(fixture, wrong_type, "USUBJID")
  expect_match(result$error, "列の型を確認")
  expect_true(is.na(result$counts$unmatched_left))
})
