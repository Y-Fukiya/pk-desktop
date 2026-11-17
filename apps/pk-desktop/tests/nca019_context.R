source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
folder <- "apps/pk-desktop/examples/nca_assembly"
spec <- jsonlite::fromJSON(file.path(folder, "specification.json"), simplifyVector = FALSE)
inputs <- list(conc = pkapp_read_table(file.path(folder, "conc.csv")),
  pc = pkapp_read_table(file.path(folder, "PC.sas7bdat")), ex = pkapp_read_table(file.path(folder, "EX.sas7bdat")))
assembled <- do.call(pkapp_assemble_nca, c(inputs, list(specification = spec)))
nca <- list(id = "ID", groups = list("ANALYTE", "PERIOD"), time = "NCA_TIME_H", concentration = "CONC",
  unit_column = "UNIT", blq_column = "BLQ", blq = "as_recorded", c0_method = "zero", auc_method = "linear")
test_that("assembly profile and time context survive into BLQ and NCA", {
  good <- pkapp_nca(assembled$data, nca)
  expect_equal(nrow(good$data), 12)
  expect_true(all(is.finite(good$data$AVAL)))
  bad <- nca; bad$groups <- list("ANALYTE")
  expect_error(pkapp_nca(assembled$data, bad), "結合時と解析単位")
  bad <- nca; bad$time_unit <- "min"
  expect_error(pkapp_nca(assembled$data, bad), "時間（h）", fixed = TRUE)
  bad <- nca; bad$time_origin <- -1
  expect_error(pkapp_nca(assembled$data, bad), "投与基準0")
  step <- list(op = "blq_rules", mode = "positional", profile_columns = list("ID", "ANALYTE"),
    concentration = "CONC", time = "NCA_TIME_H", blq_column = "BLQ", zero_time = "pre",
    pre_rule = "zero", first_rule = "zero", middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude")
  expect_error(pkapp_prepare(assembled$data, list(step)), "結合時と解析単位")
  step$profile_columns <- list("ID", "PERIOD", "ANALYTE")
  expect_equal(nrow(pkapp_prepare(assembled$data, list(step))$data), 12)
})
test_that("next-dose crossing excludes affected profiles with terminal calculations disabled", {
  first <- spec; first$dose_reference <- list(mode = "first")
  data <- do.call(pkapp_assemble_nca, c(inputs, list(specification = first)))$data
  answer <- pkapp_nca(data, nca)
  expect_true(all(is.na(answer$data$AVAL)))
  expect_true(all(answer$data$NCA_EXCLUDE == "intervening_dose"))
  limited <- nca; limited$end <- 23.9
  expect_silent(pkapp_nca(data, limited))
  data <- assembled$data; data$NCA_NEXT_DOSE_H <- 2
  expect_error(pkapp_nca(data, nca), "次回投与のEX行・投与キー")
})
test_that("CSV round trip retains assembly guards and computed results", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(assembled$data, path, row.names = FALSE, na = "")
  reread <- pkapp_read_table(path)
  expect_equal(pkapp_nca(reread, nca)$data$AVAL, pkapp_nca(assembled$data, nca)$data$AVAL)
})
test_that("unresolved source dose history cannot silently produce ordinary AUC", {
  selected <- spec
  selected$dose_reference <- list(mode = "selected", predicate = list(op = "%in%", args = list(list(column = "DOSE_ID"), list(value = list("D2", "D4")))))
  changed <- inputs
  changed$ex$ACTUAL[1] <- NA
  data <- do.call(pkapp_assemble_nca, c(changed, list(specification = selected)))$data
  expect_true(all(is.finite(data$NCA_TIME_H)))
  expect_error(pkapp_nca(data, nca), "投与履歴に未解決事項")
  changed <- inputs
  changed$ex$ACTUAL[1] <- changed$ex$ACTUAL[2]
  data <- do.call(pkapp_assemble_nca, c(changed, list(specification = selected)))$data
  expect_true(all(is.finite(data$NCA_TIME_H)))
  expect_error(pkapp_nca(data, nca), "投与履歴に未解決事項")
})
test_that("terminal diagnostics are understandable in Japanese", {
  text <- core$desktop_nca_diagnostic_label("Profile 1 terminal: r_squared_below_threshold;extrapolation_above_threshold")
  expect_false(is.na(text)); expect_match(text, "R²"); expect_match(text, "外挿率")
})
