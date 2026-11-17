source("apps/pk-desktop/tests/nca024_core.R")
test_that("aliases, provenance and range screening retain inputs and isolate dependencies", {
  f <- fixture024(); f$data$DOSEU <- rep(c("MG", "mg"), 4); f$vs$UNIT[1] <- "KG"
  r <- run024(f); expect_equal(vals024(r, "CLFO"), 1, tolerance=1e-11)
  expect_identical(r$trace$derived$profiles[[1]]$dose$conversion$normalized_unit, "mg")
  expect_true(is.null(core$desktop_nca_unit("Mg", "mass")))
  f$vs$VALUE[1] <- 5000; r <- run024(f); wt <- r$trace$derived$profiles[[1]]$weight
  expect_equal(wt$converted_value, 5000); expect_true(is.na(wt$value)); expect_equal(wt$range_qc$status, "review")
  expect_equal(vals024(r, "CLFO"), 1, tolerance=1e-11); expect_true(is.na(vals024(r, "CLFOWT")))
  expect_true("outside_range_weight" %in% r$trace$groups[[1]]$warning)
})
test_that("concentration aliases are accepted without mixing scales", {
  f <- fixture024(); f$data$UNIT <- rep(c("MG/L", "mg/L"), 4); f$spec$unit_column <- "UNIT"
  expect_equal(vals024(run024(f), "CLFO"), 1, tolerance=1e-11)
  f$data$UNIT[1] <- "ng/mL"; expect_error(run024(f), "単位")
})
test_that("observed oral time zero is review only", {
  f <- fixture024(); r <- run024(f)
  expect_true("oral_quantifiable_time_zero" %in% r$trace$groups[[1]]$warning)
  expect_equal(vals024(r, "CMAX"), 10)
  f$data$ROUTE <- "IV"; r <- run024(f)
  expect_false("oral_quantifiable_time_zero" %in% r$trace$groups[[1]]$warning)
  expect_equal(vals024(r, "AUCIFOD"), 1, tolerance=1e-11)
})
