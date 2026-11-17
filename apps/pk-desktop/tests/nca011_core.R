source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
spec011 <- list(id = "ID", time = "TIME", concentration = "CONC", concentration_unit = "ng/mL", auc_method = "linear")
change011 <- function(...) modifyList(spec011, list(...), keep.null = TRUE)
value011 <- function(x, code) x$data$AVAL[x$data$PARAMCD == code]

test_that("one timed observed point retains observed parameters but cannot borrow synthetic support", {
  d <- data.frame(ID = "001", TIME = 1, CONC = 8)
  for (method in c("none", "zero")) {
    r <- pkapp_nca(d, change011(c0_method = method, include_aucproc = TRUE))
    expect_equal(r$data$AVAL, c(8, 1, NA_real_, NA_real_))
    expect_equal(r$data$PARAM_STATUS, c("calculated", "calculated", "not_calculable", "not_calculable"))
    expect_true(all(r$data$PROFILE_STATUS == "partial"))
    expect_true(r$trace$rows$USE_OBS[1])
    expect_false(any(r$trace$rows$USE_AUC | r$trace$rows$USE_AUCPROC))
    expect_equal(r$data$AVALU, c("ng/mL", "h", "h*ng/mL", "h*ng/mL"))
  }
})

test_that("all BLQ output policy is explicit and never supplies observed endpoints", {
  d <- data.frame(ID = "001", TIME = c(0, 1, 2), CONC = "<LLOQ", BLQ = 1)
  for (handling in c("exclude", "zero")) {
    s <- change011(blq_column = "BLQ", blq = handling, include_aucproc = TRUE)
    missing <- pkapp_nca(d, s)
    expect_true(all(is.na(missing$data$AVAL)))
    zero <- pkapp_nca(d, modifyList(s, list(all_blq_policy = "zero")))
    expect_equal(zero$data$AVAL, c(0, NA_real_, 0, 0))
    expect_equal(zero$data$PARAM_STATUS, c("policy_assigned", "not_calculable", "policy_assigned", "policy_assigned"))
    expect_true(all(is.na(zero$data$TLAST_QUANT_H)))
    expect_true(all(is.na(zero$data$AUC_LAST_H)))
    expect_false(any(zero$trace$rows$USE_OBS | zero$trace$rows$USE_AUC | zero$trace$rows$USE_AUCPROC))
    expect_true(all(grepl("All-BLQ zero policy", zero$data$NCA_WARNING)))
    partial <- pkapp_nca(d, modifyList(s, list(all_blq_policy = "zero", start = .5)))
    expect_true(is.na(value011(partial, "AUCLST")))
    expect_equal(value011(partial, "AUCPART"), 0)
  }
  expect_error(pkapp_nca(d, change011(blq_column = "BLQ", all_blq_policy = "zero", concentration_unit = "")), "単位")
  s <- change011(blq_column = "BLQ", blq = "exclude", all_blq_policy = "zero")
  for (extra in list(data.frame(ID = "001", TIME = NA_real_, CONC = "<LLOQ", BLQ = 1),
    data.frame(ID = "001", TIME = 3, CONC = NA_character_, BLQ = 0),
    data.frame(ID = "001", TIME = 3, CONC = "0", BLQ = 0))) {
    result <- pkapp_nca(rbind(d, extra), s)
    expect_false(any(result$data$PARAM_STATUS == "policy_assigned"))
  }
  duplicate <- pkapp_nca(rbind(d, d[1, ]), s)
  expect_true(all(duplicate$data$PROFILE_STATUS == "excluded"))
  expect_false(any(duplicate$data$PARAM_STATUS == "policy_assigned"))
  excluded <- transform(rbind(d, data.frame(ID = "001", TIME = 3, CONC = "100", BLQ = 0)), MDV = c(0, 0, 0, 1))
  expect_equal(value011(pkapp_nca(excluded, modifyList(s, list(mdv_column = "MDV"))), "CMAX"), 0)
})

test_that("missingness is counted per original profile and surfaces review without changing retained values", {
  d <- data.frame(ID = "001", TIME = c(0, 1, 2, NA, 3, NA), CONC = c(0, 8, 4, 100, NA, NA))
  r <- pkapp_nca(d, spec011)
  expect_equal(r$data$AVAL, c(8, 1, 10))
  expect_true(all(r$data$PROFILE_STATUS == "review"))
  expect_equal(r$data$N_INPUT, rep(6L, 3))
  expect_equal(r$data$N_MISSING_TIME, rep(2L, 3))
  expect_equal(r$data$N_MISSING_CONCENTRATION, rep(2L, 3))
  expect_equal(r$data$N_EXCLUDED, rep(3L, 3))
  expect_true(all(grepl("requires review", r$data$NCA_WARNING)))
  scoped <- transform(d, SSNCAFL = c("Y", "Y", "Y", "N", "N", "N"))
  r <- pkapp_nca(scoped, change011(scope = "ss"))
  expect_true(all(r$data$PROFILE_STATUS == "ok"))
  expect_true(all(r$data$N_MISSING_TIME == 0L & r$data$N_MISSING_CONCENTRATION == 0L))
})

test_that("predose QC records its comparison and never excludes observations or changes C0", {
  d <- data.frame(ID = "001", TIME = c(-1, -.25, 0, 1, 2), CONC = c(.2, 2, 0, 8, 4))
  baseline <- pkapp_nca(d, spec011)
  s <- change011(predose_qc_threshold_pct = 5, predose_qc_window_h = .5)
  r <- pkapp_nca(d, s)
  expect_equal(r$data$AVAL, baseline$data$AVAL)
  expect_equal(r$trace$rows$USE_OBS, baseline$trace$rows$USE_OBS)
  expect_true(all(r$data$PREDOSE_QC_STATUS == "exceeds"))
  expect_true(all(r$data$PREDOSE_QC_SOURCE_ROW == 2L))
  expect_true(all(r$data$PREDOSE_QC_TIME_H == -.25))
  expect_true(all(r$data$PREDOSE_QC_CONC == 2 & r$data$PREDOSE_QC_CMAX == 8 & r$data$PREDOSE_QC_PCT == 25))
  expect_true(all(r$data$PROFILE_STATUS == "review"))
  expect_false(any(r$trace$rows$IMPUTED))
  equal <- pkapp_nca(d, modifyList(s, list(predose_qc_threshold_pct = 25)))
  expect_true(all(equal$data$PREDOSE_QC_STATUS == "pass"))
  unavailable <- pkapp_nca(d[d$TIME >= 0, ], s)
  expect_true(all(unavailable$data$PREDOSE_QC_STATUS == "not_evaluable"))
  expect_true(all(is.na(unavailable$data$PREDOSE_QC_PCT)))
  expect_error(pkapp_nca(d, change011(predose_qc_threshold_pct = 5)), "遡及時間")
  expect_error(pkapp_nca(d, change011(predose_qc_threshold_pct = Inf, predose_qc_window_h = 1)), "有限数")
})

test_that("prepared exclusions and CSV replay preserve missingness and all BLQ policy", {
  d <- data.frame(ID = "001", PERIOD = 1, TIME = c(0, 1, 2, NA, 3), CONC = c("0", "8", "4", "100", NA), BLQ = 0)
  step <- list(op = "blq_rules", mode = "positional", concentration = "CONC", blq_column = "BLQ", time = "TIME",
    profile_columns = c("ID", "PERIOD"), zero_time = "post", pre_rule = "zero", first_rule = "zero",
    middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude")
  prepared <- pkapp_prepare(d, list(step))$data
  s <- change011(concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV", blq = "as_recorded", groups = "PERIOD")
  r <- pkapp_nca(prepared, s)
  expect_equal(r$data$AVAL, c(8, 1, 10))
  expect_true(all(r$data$N_MISSING_TIME == 1L & r$data$N_MISSING_CONCENTRATION == 1L))
  expect_equal(r$trace$rows$REASON[4:5], c("missing_time", "missing_concentration"))
  csv <- tempfile(fileext = ".csv"); core$pk_write_csv(prepared, csv)
  expect_equal(pkapp_nca(pkapp_read_table(csv), s)$data, r$data)
  expect_error(pkapp_nca(prepared, modifyList(s, list(groups = list()))), "グループ|解析単位")
  old <- prepared; old$NCA_BLQ_CONTEXT <- NULL
  expect_error(pkapp_nca(old, s), "再|前処理")
  blq <- transform(d[1:3, ], CONC = "<LLOQ", BLQ = 1, UNIT = "ng/mL")
  prepared <- pkapp_prepare(blq, list(step))$data
  r <- pkapp_nca(prepared, modifyList(s, list(all_blq_policy = "zero", concentration_unit = "", unit_column = "UNIT")))
  expect_equal(r$data$AVAL, c(0, NA_real_, 0))
  expect_true(all(is.na(r$data$TLAST_QUANT_H)))
  expect_equal(r$data$AVALU, c("ng/mL", "h", "h*ng/mL"))
})


test_that("policy zeros cannot borrow another profile's concentration unit", {
  d <- data.frame(ID = rep(c("A", "B"), each = 3), TIME = rep(0:2, 2),
    CONC = c("0", "8", "4", "BLQ", "BLQ", "BLQ"), BLQ = rep(c(0, 1), each = 3),
    UNIT = rep(c("ng/mL", "ug/mL"), each = 3))
  s <- change011(blq_column = "BLQ", blq = "exclude", all_blq_policy = "zero", unit_column = "UNIT", concentration_unit = "")
  expect_error(pkapp_nca(d, s), "単位")
  d$UNIT[d$ID == "B"] <- "ng/mL"
  r <- pkapp_nca(d, s)
  expect_equal(r$data$AVAL, c(8, 1, 10, 0, NA_real_, 0))
  expect_equal(r$data$AVALU, rep(c("ng/mL", "h", "h*ng/mL"), 2))
})

test_that("wide policy summary names cannot overwrite profile identity", {
  d <- data.frame(POLICY_ASSIGNED = "001", TIME = 0:2, CONC = c(0, 8, 4))
  r <- pkapp_nca(d, modifyList(spec011, list(id = "POLICY_ASSIGNED")))
  expect_equal(r$data$SOURCE_POLICY_ASSIGNED, rep("001", 3))
  expect_equal(r$trace$output_key_mapping$POLICY_ASSIGNED, "SOURCE_POLICY_ASSIGNED")
})

test_that("a trailing observed zero cannot supply AUC support outside its endpoint", {
  d <- data.frame(ID = "001", TIME = c(1, 2), CONC = c(5, 0))
  r <- pkapp_nca(d, change011(c0_method = "zero", include_aucproc = TRUE))
  expect_equal(r$data$AVAL, c(5, 1, NA_real_, NA_real_))
  expect_true(all(r$data$PROFILE_STATUS == "partial"))
  expect_true(all(grepl("Fewer than two", r$data$NCA_EXCLUDE[r$data$PARAMCD %in% c("AUCLST", "AUCPROC")])))
  expect_false(any(r$trace$rows$USE_AUC | r$trace$rows$USE_AUCPROC))
  expect_true(all(r$trace$rows$USE_OBS[1:2]))
})


test_that("renaming processed columns cannot bypass the BLQ profile contract", {
  raw <- data.frame(ID = "001", PERIOD = rep(1:2, each = 3), TIME = rep(0:2, 2),
    CONC = c(0, 8, 4, 0, 4, 2), BLQ = 0)
  step <- list(op = "blq_rules", mode = "positional", concentration = "CONC", blq_column = "BLQ", time = "TIME",
    profile_columns = "ID", zero_time = "post", pre_rule = "zero", first_rule = "zero",
    middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude")
  prepared <- pkapp_prepare(raw, list(step))$data
  renamed <- prepared; names(renamed)[names(renamed) == "NCA_CONC"] <- "DV"
  processed_spec <- change011(concentration = "DV", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV", blq = "as_recorded", groups = "PERIOD")
  expect_error(pkapp_nca(renamed, processed_spec), "前処理.*再適用")
  names(renamed)[names(renamed) == "NCA_BLQ"] <- "DERIVED_BLQ"
  names(renamed)[names(renamed) == "NCA_MDV"] <- "DERIVED_MDV"
  expect_error(pkapp_nca(renamed, modifyList(processed_spec, list(blq_column = "DERIVED_BLQ", mdv_column = "DERIVED_MDV"))), "条件記録.*一致")
  original_spec <- change011(blq_column = "BLQ", groups = "PERIOD", blq = "zero")
  expect_equal(pkapp_nca(renamed, original_spec)$data, pkapp_nca(raw, original_spec)$data)
  legacy <- prepared; legacy$NCA_BLQ_CONTEXT <- NULL
  expect_error(pkapp_nca(legacy, change011(blq_column = "NCA_BLQ", mdv_column = "NCA_MDV", groups = "PERIOD")), "前処理済みフラグ")
  expect_equal(pkapp_nca(legacy, original_spec)$data, pkapp_nca(raw, original_spec)$data)
  corrupt <- renamed; corrupt$NCA_BLQ_CONTEXT <- "broken"
  expect_error(pkapp_nca(corrupt, original_spec), "条件記録.*不正")
})
