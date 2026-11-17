source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
test_that("standalone owner shares revision, frozen results and privacy", {
  path <- tempfile(); dir.create(path); e <- core$desktop_nca_engine(path)
  data <- data.frame(ID = rep(c("private_subject_alpha", "private_subject_beta"), each = 4), TIME = rep(c(0,1,2,4),2), CONC = c(0,8,4,2,0,16,8,4), UNIT = "ng/mL")
  table <- core$desktop_table_save(e, "input", list(source = data), list(kind = "prepare", steps = list()))
  a <- list(project_id = e$nca$project_id, base_revision = e$nca$revision, actor = "test", reason = "test", source = table$id,
    specification = list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT", c0_method = "none", auc_method = "linear"))
  missing <- a; missing$specification$c0_method <- NULL
  expect_error(core$desktop_nca_configure(e, missing), "c0_method")
  expect_equal(e$nca$revision, 1)
  state <- core$desktop_nca_configure(e, a)
  expect_error(core$desktop_nca_configure(e, a), "changed")
  a$base_revision <- state$revision
  result <- core$desktop_nca_run_shared(e, a)
  expect_true(result$is_current)
  expect_equal(result$counts$curves, 2)
  expect_false(grepl("private_subject|CONC_ORIGINAL|NCA_EXCLUDE", core$desktop_json(result)))
  a$result_id <- result$result_id; a$name <- "result"
  saved <- core$desktop_nca_dispatch(e, "nca_save_result", a)
  expect_identical(core$desktop_table_get(e, saved$id), e$nca_result$value$data)
  state <- core$desktop_nca_configure(e, a)
  expect_true(core$desktop_nca_current(e))
  expect_equal(core$desktop_nca_engine(path)$nca$revision, state$revision)
  a$base_revision <- state$revision; a$specification$start <- 1
  core$desktop_nca_configure(e, a)
  expect_false(core$desktop_nca_current(e))
  expect_error(core$desktop_nca_dispatch(e, "nca_save_result", a), "changed|stale")
})
test_that("reaccepting conditions never resurrects a result after a failed run", {
  path <- tempfile(); dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  e <- core$desktop_nca_engine(path)
  data <- data.frame(ID = "test", TIME = 0:2, CONC = c(0, 8, 4), UNIT = "ng/mL")
  table <- core$desktop_table_save(e, "input", list(source = data), list(kind = "prepare", steps = list()))
  a <- list(project_id = e$nca$project_id, base_revision = e$nca$revision, actor = "test", reason = "test", source = table$id,
    specification = list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT", c0_method = "none", auc_method = "linear"))
  a$base_revision <- core$desktop_nca_configure(e, a)$revision
  old <- core$desktop_nca_run_shared(e, a)
  original <- core$pkapp_nca
  on.exit(core$pkapp_nca <- original, add = TRUE)
  core$pkapp_nca <- function(...) stop("injected calculation failure")
  expect_error(core$desktop_nca_run_shared(e, a), "NCA failed")
  core$pkapp_nca <- original
  for (method in c("linear", "lin up/log down", "linear")) {
    a$specification$auc_method <- method
    state <- core$desktop_nca_configure(e, a)
    a$base_revision <- state$revision
    expect_identical(state$result_status, "failed")
    expect_null(state$result_id)
    expect_false(core$desktop_nca_public(e, old$result_id, a$project_id)$is_current)
    expect_error(core$desktop_nca_dispatch(e, "nca_save_result", c(a, list(result_id = old$result_id, name = "stale"))), "stale")
  }
  fresh <- core$desktop_nca_run_shared(e, a)
  expect_true(fresh$is_current)
  expect_false(identical(fresh$result_id, old$result_id))
})
test_that("HTTP guard and injected lifecycle clean up", {
  path <- tempfile(); dir.create(path); e <- core$desktop_nca_engine(path)
  runtime <- core$desktop_r_runtime(e, 3880, 3879, "nca")
  app <- core$desktop_http_app(e, runtime)
  response <- app$call(list(HTTP_AUTHORIZATION = "bad"))
  expect_equal(response$status, 403)
  health <- app$call(list(HTTP_AUTHORIZATION = paste("Bearer", runtime$token), PATH_INFO = "/health", REQUEST_METHOD = "GET"))
  expect_equal(jsonlite::fromJSON(health$body)$mode, "nca")
  stopped <- FALSE
  core$desktop_nca_launch(path, 3879, 3880, FALSE, start_api = function(...) 1, stop_api = function(x) stopped <<- TRUE,
    run_app = function(...) { expect_true(file.exists(file.path(path, ".pkapp-runtime.json"))); expect_equal(as.integer(file.info(file.path(path, ".pkapp-runtime.json"))$mode), 384L) })
  expect_true(stopped); expect_false(file.exists(file.path(path, ".pkapp-runtime.json"))); expect_false(dir.exists(file.path(path, ".pkapp-owner")))
  expect_error(core$desktop_nca_launch(path, 3879, 3880, FALSE, start_api = function(...) stop("fail")), "fail")
  expect_false(dir.exists(file.path(path, ".pkapp-owner")))
})
test_that("GUI detects a newer shared revision before publishing its draft", {
  path <- tempfile(); dir.create(path); e <- core$desktop_nca_engine(path)
  data <- data.frame(ID = "test", TIME = 0:2, CONC = c(0, 2, 1))
  table <- core$desktop_table_save(e, "input", list(source = data), list(kind = "prepare", steps = list()))
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    old <- shared_revision()
    args <- list(project_id = e$nca$project_id, base_revision = old, actor = "test", reason = "test", source = table$id,
      specification = list(id = "ID", time = "TIME", concentration = "CONC", c0_method = "none"))
    core$desktop_nca_configure(e, args)
    session$setInputs(shared_publish = 1)
    expect_match(notice(), "changed")
    expect_equal(shared_revision(), old)
    expect_equal(e$nca$revision, old + 1)
    session$setInputs(shared_refresh = 1)
    expect_equal(shared_revision(), e$nca$revision)
  })
})
