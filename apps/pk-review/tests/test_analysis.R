source(file.path(Sys.getenv("PK_REVIEW_ROOT", getwd()), "apps", "pk-review", "bootstrap.R"))
library(testthat)

test_that("R CSV reader rejects ambiguous headers and accepts quoted values", {
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p))
  writeLines(c("AVAL,AVAL", "10,1000"), p)
  expect_error(pk_csv(p), "duplicate CSV")
  writeLines(c("A,B", "1,2,3"), p)
  expect_error(pk_csv(p), "field count")
  writeLines(c('A,B', '"with,comma","with ""quote"""'), p)
  expect_identical(pk_csv(p)$A, "with,comma")
  expect_identical(pk_csv(p)$B, 'with "quote"')
})

test_that("R flag interpretation matches the importer", {
  expect_identical(pk_flag(c("1.0", " Y ", "1 ", "0.0", " false ", ".1e1", "100e-2", "+01.000e+00", "-0e-999")),
                   c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
  for (x in c("", "unknown", "2", "NaN", "Inf", "0x1", "0_1", "1e-999", "1.00000000000000001", "0.99999999999999999", NA)) expect_error(pk_flag(x), "invalid")
})

test_that("identity comes from either dataset and conflicting identities are rejected", {
  d <- list(ADNCA = data.frame(USUBJID = "A", EXTRT = "Reference"),
            ADPP = data.frame(USUBJID = "A", STUDYID = "S1", ANALYTE = "Compound X"))
  expect_identical(pk_identity_label(d, "STUDYID", "A"), "S1")
  expect_identical(pk_identity_label(d, "ANALYTE", "A"), "Compound X")
  d$ADNCA$ANALYTE <- "Other compound"
  expect_error(pk_identity_label(d, "ANALYTE", "A"), "Conflicting ANALYTE")
  d$ADNCA$ANALYTE <- NULL
  d$ADPP$ANALYTE <- NULL
  expect_identical(pk_identity_label(d, "ANALYTE", "A"), "Not provided")
})

test_that("stratum labels containing separators do not merge", {
  d <- data.frame(.GROUP = c("Drug.A", "Drug"), PARAMCD = c("CMAX", "A.CMAX"), AVALU = "ng/mL", AVAL = c(10, 100))
  summary <- pk_summarise(d, c(".GROUP", "PARAMCD", "AVALU"))
  expect_equal(nrow(summary), 2)
  expect_equal(sort(summary$mean), c(10, 100))
})

fixture <- function() {
  conc <- data.frame(USUBJID = rep(c("A", "B"), each = 3), TIME_H = rep(0:2, 2),
                     AVAL = c(0, 10, 4, 0, 20, 8), AVALU = "ng/mL", SEX = rep(c("F", "M"), each = 3),
                     MDV = c(1, 0, 0, 1, 0, 0), BLQ = c(0, 0, 1, 0, 0, 0),
                     SSNCAFL = rep(c("N", "Y", "Y"), 2), TAD_H = rep(c(NA, 0, 1), 2))
  pp <- data.frame(USUBJID = c("A", "B", "A", "B"), PARAMCD = c("CMAX", "CMAX", "TRG0H", "TRG0H"),
                   AVAL = c(10, 20, 0, 0), AVALU = "ng/mL", SEX = c("F", "M", "F", "M"), ANL01FL = c("Y", "Y", "N", "N"))
  data <- list(ADNCA = conc, ADPP = pp, hashes = list("ADNCA.csv" = "a", "ADPP.csv" = "b"))
  cfg <- list(version = 1L, revision = 1L, title = "Fixture", bundle = "test", input_hashes = data$hashes,
              profile = "all", x = "TIME_H", scale = "linear", statistic = "mean", interval = "sd",
              show_individual = TRUE, group_by = "NONE", subjects = list(), parameters = list("CMAX", "TRG0H"),
              include_mdv = FALSE, blq = "exclude", digits = 3L, include_listings = FALSE)
  list(data = data, cfg = cfg)
}

test_that("flags and subject selection have explicit, consistent denominators", {
  f <- fixture()
  a <- pk_analyse(f$data, f$cfg)
  expect_equal(a$counts$concentration_rows, 3)
  expect_equal(a$counts$excluded_concentration_rows, 3)
  expect_equal(a$counts$excluded_adpp_rows, 2)
  expect_equal(a$concentration_summary$mean, c(15, 8))
  expect_equal(a$concentration_summary$n, c(2, 1))
  expect_equal(a$parameter_summary$mean, 15)
  f$cfg$subjects <- list("A")
  selected <- pk_analyse(f$data, f$cfg)
  expect_equal(selected$counts$subjects, 1)
  expect_equal(selected$parameter_summary$mean, 10)
  expect_equal(selected$concentration_summary$mean, 10)
})

test_that("log display never changes arithmetic summaries or ADPP", {
  f <- fixture()
  f$cfg$include_mdv <- TRUE
  before <- pk_analyse(f$data, f$cfg)
  f$cfg$scale <- "log10"
  after <- pk_analyse(f$data, f$cfg)
  expect_equal(before$concentration_summary, after$concentration_summary)
  expect_equal(before$parameter_summary, after$parameter_summary)
  expect_equal(before$concentration_summary$n_positive[1], 0)
  expect_equal(before$concentration_summary$n[1], 2)
  expect_s3_class(pk_plot_concentration(after), "ggplot")
})

test_that("log intervals with nonpositive lower limits are rejected explicitly", {
  f <- fixture()
  f$data$ADNCA$AVAL[c(2, 5)] <- c(1, 100)
  f$cfg$scale <- "log10"
  expect_error(pk_analyse(f$data, f$cfg), "nonpositive lower bound")
  f$cfg$interval <- "none"
  expect_equal(pk_analyse(f$data, f$cfg)$concentration_summary$mean[1], 50.5)
})

test_that("QC failures and contribution counts remain visible", {
  f <- fixture()
  f$data$manifest <- list(quality_checks = list(list(USUBJID = "A", check = "PRED_TROUGH_STABLE_QC", status = "FAIL", relative_change = "0.25", rule = "test")))
  f$data$ADPP$AVAL[f$data$ADPP$USUBJID == "B"] <- NA_real_
  a <- pk_analyse(f$data, f$cfg)
  expect_match(paste(unlist(a$warnings), collapse = " "), "UPSTREAM QC FAIL: A")
  expect_equal(a$counts$subjects, 2)
  expect_equal(a$counts$parameter_subjects, 1)
  expect_equal(a$quality_checks$status, "FAIL")
})

test_that("profiles change concentration display without recalculating parameters", {
  f <- fixture()
  baseline <- pk_analyse(f$data, f$cfg)
  f$cfg$profile <- "steady_state"
  f$cfg$x <- "TAD_H"
  ss <- pk_analyse(f$data, f$cfg)
  expect_equal(ss$concentration_summary$.TIME, c(0, 1))
  expect_equal(ss$parameter_summary, baseline$parameter_summary)
})

test_that("strata are not pooled and distinct units are not pooled", {
  f <- fixture()
  f$cfg$group_by <- "SEX"
  a <- pk_analyse(f$data, f$cfg)
  expect_equal(sort(a$parameter_summary$mean), c(10, 20))
  f$cfg$group_by <- "NONE"
  f$data$ADNCA$AVALU[f$data$ADNCA$USUBJID == "B"] <- "ug/mL"
  a <- pk_analyse(f$data, f$cfg)
  expect_equal(length(unique(a$concentration_summary$AVALU)), 2)
})

test_that("display rounding uses significant digits without changing export statistics", {
  f <- fixture()
  f$data$ADPP$AVAL[1:2] <- c(3078.56, 3078.56)
  a <- pk_analyse(f$data, f$cfg)
  expect_equal(pk_display_table(a$parameter_summary, 3, TRUE)[["Mean (SD)"]], "3080 (0)")
  expect_equal(a$parameter_summary$mean, 3078.56)
})

test_that("invalid settings fail instead of silently changing meaning", {
  f <- fixture()
  f$cfg$statistic <- "median"
  expect_error(pk_analyse(f$data, f$cfg), "SD intervals")
  f <- fixture()
  f$cfg$x <- "TAD_H"
  expect_error(pk_analyse(f$data, f$cfg), "requires the steady_state")
  f <- fixture()
  f$cfg$parameters <- list("NONEXISTENT")
  expect_error(pk_analyse(f$data, f$cfg), "Unknown ADPP")
  f <- fixture()
  f$cfg$input_hashes[[1]] <- "changed"
  expect_error(pk_analyse(f$data, f$cfg), "checksum")
})

test_that("loader rejects parameter-as-concentration and duplicate profiles", {
  temp <- tempfile(); dir.create(temp)
  on.exit(unlink(temp, recursive = TRUE))
  f <- fixture()
  utils::write.csv(f$data$ADPP, file.path(temp, "ADNCA.csv"), row.names = FALSE)
  utils::write.csv(f$data$ADPP, file.path(temp, "ADPP.csv"), row.names = FALSE)
  expect_error(pk_load_bundle(temp), "ADNCA must contain concentration")
  f$data$ADNCA <- rbind(f$data$ADNCA, f$data$ADNCA[1, ])
  utils::write.csv(f$data$ADNCA, file.path(temp, "ADNCA.csv"), row.names = FALSE)
  expect_error(pk_load_bundle(temp), "Duplicate subject/time")
})

test_that("teal modules render and follow saved settings without a network server", {
  skip_if_not_installed("teal")
  source(file.path(pk_app, "R", "modules.R"))
  temp <- tempfile(); dir.create(temp)
  on.exit(unlink(temp, recursive = TRUE))
  f <- fixture()
  utils::write.csv(f$data$ADNCA, file.path(temp, "ADNCA.csv"), row.names = FALSE)
  utils::write.csv(f$data$ADPP, file.path(temp, "ADPP.csv"), row.names = FALSE)
  bundle <- pk_load_bundle(temp)
  f$cfg$bundle <- temp
  f$cfg$input_hashes <- bundle$hashes
  path <- file.path(temp, "view.json")
  jsonlite::write_json(f$cfg, path, auto_unbox = TRUE)
  d <- shiny::reactive(list(ADNCA = bundle$ADNCA, ADPP = bundle$ADPP))
  shiny::testServer(pk_concentration_module(path, bundle)$server, args = list(data = d), {
    session$flushReact()
    expect_match(output$counts, "2 例")
    expect_true(nzchar(output$profile$src))
    expect_match(output$table, "15")
    pk_update_config(path, list(subjects = list("A")), 1L, "tester", "Select subject")
    session$elapse(1100)
    expect_match(output$counts, "1 例")
    expect_equal(result()$parameter_summary$mean, 10)
  })
  shiny::testServer(pk_parameters_module(path, bundle)$server, args = list(data = d), {
    session$flushReact()
    expect_true(nzchar(output$plot$src))
    expect_match(output$table, "CMAX")
  })
  shiny::testServer(pk_settings_module(path, bundle)$server, args = list(data = d), {
    session$flushReact()
    current <- pk_read_config(path)
    editable <- current[setdiff(names(current), c("version", "revision", "bundle", "input_hashes", "audit"))]
    editable$parameters <- unlist(editable$parameters)
    editable$subjects <- unlist(editable$subjects)
    do.call(session$setInputs, editable)
    expect_true(ready())
    session$setInputs(scale = "log10", actor = "tester", reason = "Inspect log scale")
    session$setInputs(apply = 1)
    expect_equal(pk_read_config(path)$scale, "log10")
    expect_equal(pk_read_config(path)$revision, 3L)
    expect_match(output$message$html, "保存しました")
    session$setInputs(title = "Unsaved draft")
    pk_update_config(path, list(title = "External saved title"), 3L, "external", "Concurrent change")
    session$elapse(1100)
    expect_equal(input$title, "Unsaved draft")
    expect_equal(editing_revision(), 3L)
    session$setInputs(apply = 2)
    expect_equal(pk_read_config(path)$title, "External saved title")
    expect_match(output$message$html, "changed since")
    session$setInputs(reload = 1)
    expect_false(ready())
    session$setInputs(apply = 3)
    expect_equal(pk_read_config(path)$revision, 4L)
    current <- pk_read_config(path)
    editable <- current[setdiff(names(current), c("version", "revision", "bundle", "input_hashes", "audit"))]
    editable$parameters <- unlist(editable$parameters)
    editable$subjects <- unlist(editable$subjects)
    do.call(session$setInputs, editable)
    expect_true(ready())
    expect_equal(editing_revision(), 4L)
    session$setInputs(title = "Acknowledged new edit", apply = 4)
    expect_equal(pk_read_config(path)$revision, 5L)
    expect_equal(pk_read_config(path)$audit[[5]]$channel, "gui")
  })
})
