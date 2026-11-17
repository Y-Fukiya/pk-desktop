# Synthetic installation smoke test, not clinical validation or production NCA defaults.
library(testthat)

test_that("standard dependency diagnostics can load PKNCA and datamods", {
  source("apps/pk-desktop/pkapp.R", local = TRUE)
  dependencies <- pkapp_dependencies()
  for (package in c("PKNCA", "datamods")) {
    row <- dependencies[dependencies$package == package, , drop = FALSE]
    expect_equal(nrow(row), 1L)
    expect_true(row$installed)
    expect_true(row$available)
    expect_identical(row$version, as.character(utils::packageVersion(package)))
  }
})

test_that("PKNCA computes known grouped Cmax Tmax and linear AUClast without a PK bundle", {
  # Artificial observations: the linear trapezoids sum to 4 + 6 + 6 = 16.
  # The second subject has exactly twice the concentrations; IDs retain zeros.
  input <- data.frame(ID = rep(c("001", "002"), each = 4L),
    time = rep(c(0, 1, 2, 4), 2L), conc = c(0, 8, 4, 2, 0, 16, 8, 4))
  original <- input
  old_options <- PKNCA::PKNCA.options()
  concentration <- PKNCA::PKNCAconc(input, conc ~ time | ID)
  analysis <- PKNCA::PKNCAdata(concentration,
    intervals = data.frame(start = 0, end = 4, cmax = TRUE, tmax = TRUE, auclast = TRUE),
    options = list(auc.method = "linear", progress = FALSE))
  result <- as.data.frame(PKNCA::pk.nca(analysis))
  expect_equal(nrow(result), 6L)
  for (id in c("001", "002")) {
    rows <- result[result$ID == id, , drop = FALSE]
    expect_equal(nrow(rows), 3L)
    expect_setequal(rows$PPTESTCD, c("cmax", "tmax", "auclast"))
    values <- stats::setNames(rows$PPORRES, rows$PPTESTCD)
    expected <- if (id == "001") c(cmax = 8, tmax = 1, auclast = 16) else c(cmax = 16, tmax = 1, auclast = 32)
    expect_equal(values[names(expected)], expected, tolerance = 1e-12)
  }
  expect_identical(input, original)
  expect_identical(PKNCA::PKNCA.options(), old_options)
})
