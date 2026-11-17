source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
test_that("predicate recipes preserve IDs, missing values and reject executable expressions", {
  d <- data.frame(ID = c("001", "002", "003"), SEX = c("F", "M", NA), AVAL = c(pi, 2, 3))
  expr <- quote(SEX %in% "F" | is.na(SEX))
  p <- core$desktop_predicate_from_expr(expr)
  p <- jsonlite::fromJSON(core$desktop_json(p), simplifyVector = FALSE)
  out <- pkapp_prepare(d, list(list(op = "where", predicate = p)))
  expect_identical(out$data$ID, c("001", "003")); expect_identical(out$data$AVAL, c(pi, 3))
  for (e in list(quote(system("id")), quote(base::system("id")), quote({print(1); TRUE}))) {
    expect_error(pkapp_prepare(d, list(list(op = "where", predicate = core$desktop_predicate_from_expr(e)))))
  }
  expect_error(core$desktop_predicate(list(column = "missing"), d))
  expect_error(core$desktop_predicate(list(op = ">", args = list(list(value = 1))), d))
})
test_that("sort distinct and predicate save replay without changing scientific values", {
  p <- pkapp_open(tempfile()); withr::defer(pkapp_close(p))
  d <- data.frame(ID = c("002", "001", "001"), AVAL = c(pi, exp(1), exp(1)))
  steps <- list(list(op = "distinct", columns = list("ID", "AVAL")), list(op = "sort", columns = list("ID")))
  s <- pkapp_save_preparation(p, d, steps, "stable IDs")
  replay <- pkapp_replay(core$desktop_table_path(p$engine, s$id))
  expect_identical(replay$data$ID, c("001", "002")); expect_identical(replay$data$AVAL, c(exp(1), pi))
  expect_identical(d$ID, c("002", "001", "001"))
})
test_that("datamods and declarative filters agree including missing and zero selection", {
  d <- data.frame(SEX = c("F", "M", NA, "F"), AVAL = c(pi, 2, NA, 7))
  shiny::testServer(datamods::filter_data_server, args = list(data = shiny::reactive(d), vars = shiny::reactive(c("SEX", "AVAL")), widget_char = "select", widget_num = "range", drop_ids = FALSE), {
    output$placeholder_filters
    session$setInputs(filter_534558 = "F", na_filter_534558 = TRUE, filter_4156414c = c(0, 4), na_filter_4156414c = TRUE)
    filtered <- session$returned$filtered(); rownames(filtered) <- NULL
    predicate <- core$desktop_predicate_from_expr(session$returned$expr())
    result <- pkapp_prepare(d, list(list(op = "where", predicate = predicate)))$data
    expect_identical(result, filtered); expect_equal(nrow(result), 2L)
    session$setInputs(filter_534558 = character(), na_filter_534558 = FALSE)
    filtered <- session$returned$filtered(); rownames(filtered) <- NULL
    result <- pkapp_prepare(d, list(list(op = "where", predicate = core$desktop_predicate_from_expr(session$returned$expr()))))$data
    expect_identical(result, filtered); expect_equal(nrow(result), 0L)
  })
})
test_that("validated table apply uses exact snapshot and rejects stale or changed data", {
  p <- pkapp_open(tempfile()); withr::defer(pkapp_close(p)); e <- p$engine
  scope <- list(base_revision = e$spec$revision, project_id = e$spec$project_id)
  args <- c(scope, list(adnca = "ADNCA", adpp = "ADPP", mapping = list(), time_unit = "h"))
  checked <- core$desktop_validate_tables_api(e, args)
  expect_equal(e$spec$revision, 1L)
  apply <- c(scope, list(validation_id = checked$validation_id, actor = "test", reason = "test import"))
  core$desktop_apply_tables_api(e, apply)
  expect_equal(e$spec$revision, 2L); expect_equal(nrow(e$data$ADNCA), 750)
  expect_error(core$desktop_apply_tables_api(e, apply), "Settings changed")
  args$base_revision <- 2L; checked <- core$desktop_validate_tables_api(e, args)
  write("changed", file.path(e$project, "validated", checked$validation_id, "ADNCA.csv"), append = TRUE)
  apply$base_revision <- 2L; apply$validation_id <- checked$validation_id
  expect_error(core$desktop_apply_tables_api(e, apply)); expect_equal(e$spec$revision, 2L)
})
test_that("MCP file import limits paths and preserves leading zeros", {
  p <- pkapp_open(tempfile()); withr::defer(pkapp_close(p)); e <- p$engine
  file <- file.path(e$project, "source.csv"); writeLines(c("ID,VALUE", "001,3.141592653589793"), file)
  args <- list(path = file, name = "uploaded", base_revision = 1L, project_id = e$spec$project_id, actor = "test")
  saved <- core$desktop_import_table_api(e, args)
  expect_identical(pkapp_table(p, saved$id)$ID, "001")
  outside <- tempfile(fileext = ".csv"); file.copy(file, outside); withr::defer(unlink(outside))
  args$path <- outside; expect_error(core$desktop_import_table_api(e, args), "inside")
})
