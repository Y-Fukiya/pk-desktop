source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
test_that("module add persists exact snapshot and fails atomically on invalid definitions", {
  folder <- tempfile(); p <- pkapp_open(folder); withr::defer(pkapp_close(p)); e <- p$engine
  original <- e$data$ADNCA
  saved <- pkapp_add_module(p, "pk_scatterplot", "ADNCA", "濃度・時間", list(x = "TIME_H", y = "AVAL", color = "SEX"), "test")
  expect_equal(pkapp_modules(p)$revision, 1L)
  expect_equal(e$spec$revision, 1L)
  expect_identical(readRDS(file.path(e$modules_dir, saved$entry$id, "data.rds")), original)
  expect_error(pkapp_add_module(p, "system", "ADNCA", "bad", list(), "bad"), "未対応")
  expect_error(pkapp_add_module(p, "pk_scatterplot", "ADNCA", "bad", list(x = "USUBJID", y = "AVAL"), "bad"), "数値")
  expect_error(pkapp_add_module(p, "pk_data_table", "ADNCA", "bad", list(code = "system('id')"), "bad"), "設定")
  expect_equal(pkapp_modules(p)$revision, 1L)
  pkapp_close(p); p <- pkapp_open(folder)
  expect_identical(pkapp_modules(p)$entries[[1]]$id, saved$entry$id)
})
test_that("module concurrency guard and removal retain audit and data", {
  p <- pkapp_open(tempfile()); withr::defer(pkapp_close(p)); e <- p$engine
  saved <- pkapp_add_module(p, "pk_distribution", "ADNCA", "濃度分布", list(x = "AVAL", bins = 20), "test")
  args <- list(id = saved$entry$id, base_revision = 1L, project_id = e$spec$project_id, modules_revision = 0L, actor = "test", reason = "remove")
  expect_error(core$desktop_module_mutate(e, args, remove = TRUE), "Module list changed")
  expect_true(pkapp_modules(p)$entries[[1]]$enabled)
  args$modules_revision <- 1L; core$desktop_module_mutate(e, args, remove = TRUE)
  expect_false(pkapp_modules(p)$entries[[1]]$enabled)
  expect_equal(pkapp_modules(p)$entries[[1]]$removed$reason, "remove")
  expect_true(file.exists(file.path(e$modules_dir, saved$entry$id, "data.rds")))
})
test_that("runtime constructors produce supported teal modules and explicit availability", {
  p <- pkapp_open(tempfile()); withr::defer(pkapp_close(p)); e <- p$engine
  for (module in c("pk_scatterplot", "pk_distribution", "pk_data_table")) {
    options <- switch(module, pk_scatterplot = list(x = "TIME_H", y = "AVAL"), pk_distribution = list(x = "AVAL"), list())
    entry <- list(module = module, options = options, label = "test")
    expect_s3_class(core$desktop_module_build(entry, e$data$ADNCA), "teal_module")
  }
  catalog <- pkapp_modules(p)$catalog
  expect_identical(catalog[[4]]$available, requireNamespace("teal.modules.general", quietly = TRUE))
})
