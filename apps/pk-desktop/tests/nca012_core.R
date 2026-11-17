source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
spec012 <- list(id = "ID", time = "TIME", concentration = "CONC", blq_column = "BLQ", mdv_column = "MDV",
  concentration_unit = "ng/mL", auc_method = "linear", predose_qc_threshold_pct = 5, predose_qc_window_h = 1)
change012 <- function(...) modifyList(spec012, list(...), keep.null = TRUE)
raw012 <- function() data.frame(ID = "001", TIME = c(-.75, -.25, 0, 1, 2),
  CONC = c("0.1", "BLQ", "0", "8", "4"), BLQ = c(0, 1, 0, 0, 0), MDV = 0, LLOQ = .4)
qc012 <- function(result) result$trace$groups[[1]]$predose_qc
run012 <- function(data = raw012(), ...) pkapp_nca(data, change012(...))

test_that("raw QC uses an explicit bound and never falls back to an older measurable row", {
  baseline <- run012(predose_qc_threshold_pct = NULL)
  for (limit in c(.2, .4)) {
    r <- run012(predose_qc_lloq_source = "constant", predose_qc_lloq_value = limit)
    q <- qc012(r)
    expect_equal(q$status, "pass_blq")
    expect_equal(q$source_row, 2L)
    expect_equal(q$time_h, -.25)
    expect_true(is.na(q$concentration) && is.na(q$pct))
    expect_equal(q$lloq, limit)
    expect_equal(q$pct_upper_bound, limit / 8 * 100)
    expect_true(nzchar(q$lloq_source))
    expect_equal(r$data$AVAL, baseline$data$AVAL)
    expect_equal(r$trace$rows, baseline$trace$rows)
    expect_equal(r$warnings, baseline$warnings)
    expect_true(all(r$data$PREDOSE_QC_REASON == ""))
    expect_equal(r$data$PREDOSE_QC_PCT_UPPER_BOUND, rep(limit / 8 * 100, 3))
  }
  q <- qc012(run012(predose_qc_lloq_source = "constant", predose_qc_lloq_value = .8))
  expect_equal(q$status, "not_evaluable")
  expect_equal(q$reason, "bound_above_threshold")
  expect_equal(q$pct_upper_bound, 10)
  expect_true(is.na(q$concentration) && is.na(q$pct))
})

test_that("missing and invalid optional LLOQ stay unavailable without blocking PK", {
  unmapped <- run012()
  expect_equal(qc012(unmapped)$reason, "missing_lloq")
  expect_equal(unmapped$data$AVAL, c(8, 1, 10))
  for (limit in list(NULL, NA_real_, "", "bad", Inf, 0, -1)) {
    q <- qc012(run012(predose_qc_lloq_source = "constant", predose_qc_lloq_value = limit))
    expect_equal(q$status, "not_evaluable")
    expect_true(q$reason %in% c("missing_lloq", "invalid_lloq"))
  }
  r <- run012(predose_qc_lloq_source = "column", predose_qc_lloq_column = "LLOQ")
  expect_equal(qc012(r)$status, "pass_blq")
  d <- raw012(); d$LLOQ[2] <- NA_real_
  expect_equal(qc012(run012(d, predose_qc_lloq_source = "column", predose_qc_lloq_column = "LLOQ"))$reason, "missing_lloq")
  expect_error(run012(predose_qc_lloq_source = "column", predose_qc_lloq_column = "ABSENT"), "列")
  expect_error(run012(predose_qc_lloq_source = "automatic"), "設定")
  expect_error(run012(predose_qc_lloq_value = .4), "取得方法")
})

test_that("QC selects latest before invalid concentration interpretation", {
  for (token in c("bad", NA_character_, "-1")) {
    d <- raw012(); d$CONC[2] <- token; d$BLQ[2] <- 0
    q <- qc012(run012(d))
    expect_equal(q$source_row, 2L)
    expect_equal(q$reason, "invalid_concentration")
    expect_equal(q$status, "not_evaluable")
    expect_true(is.na(q$pct))
  }
  d <- raw012(); d$CONC[2] <- ".4"; d$BLQ[2] <- 0
  expect_equal(qc012(run012(d))$status, "pass")
  d$CONC[2] <- ".8"
  expect_equal(qc012(run012(d))$status, "exceeds")
})

test_that("QC respects scope original MDV lookback boundaries and latest ties", {
  d <- raw012(); d$MDV[2] <- 1
  expect_equal(qc012(run012(d))$source_row, 1L)
  d <- raw012(); d$SSNCAFL <- c("Y", "N", "Y", "Y", "Y")
  expect_equal(qc012(run012(d, scope = "ss"))$source_row, 1L)
  d <- raw012(); d$TIME[2] <- -1
  d$MDV[1] <- 1
  expect_equal(qc012(run012(d))$source_row, 2L)
  d$TIME[2] <- -1.01
  expect_equal(qc012(run012(d))$reason, "no_candidate")
  d <- raw012(); d <- rbind(d, d[2, ])
  q <- qc012(run012(d))
  expect_equal(q$reason, "ambiguous_latest")
  expect_true(is.na(q$source_row))
})

test_that("the selected BLQ row unit is checked separately from numeric observations", {
  d <- raw012(); d$UNIT <- "ng/mL"
  for (unit in c("ug/mL", "", NA_character_)) {
    d$UNIT[2] <- unit
    r <- run012(d, unit_column = "UNIT", predose_qc_lloq_source = "constant", predose_qc_lloq_value = .4)
    expect_equal(qc012(r)$reason, if (is.na(unit) || unit == "") "missing_unit" else "unit_mismatch")
    expect_equal(r$data$AVAL, c(8, 1, 10))
    expect_true(is.na(qc012(r)$pct_upper_bound))
  }
})

test_that("nonpositive Cmax cannot support BLQ QC and bounds do not imply exceedance", {
  d <- raw012(); d$CONC[3:5] <- 0
  q <- qc012(run012(d, predose_qc_lloq_source = "constant", predose_qc_lloq_value = .4))
  expect_equal(q$reason, "invalid_cmax")
  expect_equal(q$status, "not_evaluable")
  expect_true(is.na(q$pct_upper_bound))
  d <- raw012(); d$CONC[4:5] <- c("1e308", "5e307")
  baseline <- run012(d, predose_qc_threshold_pct = NULL)
  for (limit in c(1e-17, 1e-20)) {
    r <- run012(d, predose_qc_lloq_source = "constant", predose_qc_lloq_value = limit,
      predose_qc_threshold_pct = 5e-324)
    q <- qc012(r)
    expect_equal(q$status, "not_evaluable")
    expect_equal(q$reason, if (limit == 1e-17) "bound_above_threshold" else "numeric_range")
    if (limit == 1e-17) expect_true(q$pct_upper_bound > 5e-324) else expect_true(is.na(q$pct_upper_bound))
    expect_true(is.na(q$concentration) && is.na(q$pct))
    expect_identical(r$data$AVAL, baseline$data$AVAL)
    expect_identical(r$trace$rows, baseline$trace$rows)
  }
  for (blq in c(TRUE, FALSE)) {
    evidence <- data.frame(row = 1L, time_h = -.25, blq = blq, concentration = 1e-17,
      concentration_valid = TRUE, lloq = 1e-17, lloq_reason = "", unit_reason = "")
    q <- core$desktop_nca_predose_qc(evidence, 1e308, 5e-324, 1, "constant")
    expect_equal(q$status, if (blq) "not_evaluable" else "exceeds")
    expect_true((if (blq) q$pct_upper_bound else q$pct) > 5e-324)
    for (limit in c(1e-20, 1e308)) {
      evidence$concentration <- evidence$lloq <- limit
      q <- core$desktop_nca_predose_qc(evidence, if (limit == 1e-20) 1e308 else 1e-308, 5, 1, "constant")
      expect_equal(q$status, "not_evaluable")
      expect_equal(q$reason, "numeric_range")
      expect_true(is.na(q$concentration) && is.na(q$pct) && is.na(q$pct_upper_bound))
    }
    q <- core$desktop_nca_predose_qc(evidence, 1e308, 100, 1, "constant")
    expect_equal(q$status, if (blq) "pass_blq" else "pass")
    expect_identical(if (blq) q$pct_upper_bound else q$pct, 100)
  }
  evidence$blq <- FALSE; evidence$concentration <- 0
  q <- core$desktop_nca_predose_qc(evidence, 1e308, 5e-324, 1, "constant")
  expect_equal(q$status, "pass")
  expect_identical(q$pct, 0)
  expect_identical(q$concentration, 0)
  expect_match(core$desktop_rule_label("numeric_range"), "数値範囲")
})

test_that("all BLQ policy warnings explain missing Tmax without recommending C0", {
  d <- raw012()[3:5, ]; d$BLQ <- 1; d$CONC <- "BLQ"
  r <- run012(d, all_blq_policy = "zero", predose_qc_threshold_pct = NULL)
  expect_equal(r$data$AVAL, c(0, NA_real_, 0))
  expect_true(any(grepl("全点BLQの方針.*Tmax", r$warnings)))
  expect_false(any(grepl("TMAX:|計算できないパラメータ", r$warnings)))
  expect_false(any(grepl("C0|補完設定", r$warnings)))
  missing <- run012(d, concentration_unit = "", predose_qc_lloq_source = "constant", predose_qc_lloq_value = .4)
  expect_equal(qc012(missing)$reason, "no_candidate")
  d <- raw012(); d$BLQ <- 1; d$CONC <- "BLQ"
  missing <- run012(d, concentration_unit = "", predose_qc_lloq_source = "constant", predose_qc_lloq_value = .4)
  expect_equal(qc012(missing)$reason, "missing_unit")
  expect_true(all(is.na(missing$data$AVAL)))
  d <- raw012()[4:5, ]
  r <- run012(d, predose_qc_threshold_pct = NULL)
  expect_true(any(grepl("C0", r$warnings)))
  expect_match(r$data$NCA_WARNING[1], "AUCLST: Missing concentration at the requested start")
})

test_that("new QC output names cannot overwrite profile identities", {
  for (column in c("PREDOSE_QC_REASON", "PREDOSE_QC_LLOQ", "PREDOSE_QC_PCT_UPPER_BOUND", "PREDOSE_QC_LLOQ_SOURCE")) {
    d <- raw012(); names(d)[names(d) == "ID"] <- column
    r <- run012(d, id = column)
    expect_equal(r$data[[paste0("SOURCE_", column)]], rep("001", 3))
  }
})

step012 <- list(op = "blq_rules", mode = "positional", concentration = "CONC", blq_column = "BLQ", mdv_column = "MDV", time = "TIME",
  profile_columns = "ID", zero_time = "post", pre_rule = "exclude", first_rule = "zero", middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude",
  lloq_source = "constant", lloq_value = .4)
prepared012 <- function(d = raw012(), step = step012) pkapp_prepare(d, list(step))$data
prepared_spec012 <- modifyList(spec012, list(concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV", blq = "as_recorded"))

test_that("prepared QC uses signed recipe bounds despite BLQ exclusion and survives CSV", {
  d <- prepared012()
  expect_equal(d$NCA_MDV[2], 1L)
  baseline <- pkapp_nca(d, modifyList(prepared_spec012, list(predose_qc_threshold_pct = NULL), keep.null = TRUE))
  r <- pkapp_nca(d, prepared_spec012)
  expect_equal(qc012(r)$status, "pass_blq")
  expect_equal(qc012(r)$source_row, 2L)
  expect_equal(r$data$AVAL, baseline$data$AVAL)
  expect_equal(r$trace$rows, baseline$trace$rows)
  expect_equal(r$warnings, baseline$warnings)
  expect_true(is.na(d$NCA_LLOQ_USED[2]))
  csv <- tempfile(fileext = ".csv"); core$pk_write_csv(d, csv)
  expect_equal(pkapp_nca(pkapp_read_table(csv), prepared_spec012)$data, r$data)
  expect_equal(pkapp_nca(d[5:1, ], prepared_spec012)$data$PREDOSE_QC_SOURCE_ROW, rep(4L, 3))
  supplemented <- pkapp_nca(d, modifyList(prepared_spec012, list(predose_qc_lloq_source = "constant", predose_qc_lloq_value = .2)))
  expect_identical(qc012(supplemented), qc012(r))
})

test_that("prepared optional bad LLOQ does not invalidate numeric calculations", {
  for (limit in list(NULL, "bad", 0, -1)) {
    step <- modifyList(step012, list(lloq_value = limit), keep.null = TRUE)
    d <- prepared012(step = step)
    r <- pkapp_nca(d, prepared_spec012)
    expect_equal(r$data$AVAL, c(8, 1, 10))
    expect_true(qc012(r)$reason %in% c("missing_lloq", "invalid_lloq"))
  }
})

test_that("legacy recipe constants cannot establish a BLQ pass while signed columns can", {
  legacy <- function(d) {
    context <- core$desktop_pk_blq_context(d)
    context$version <- 1; context$columns <- NULL
    d$NCA_BLQ_BASELINE <- NULL
    d$NCA_BLQ_CONTEXT <- core$desktop_pk_blq_json(context)
    d$NCA_BLQ_PROFILE_SIGNATURE <- core$desktop_pk_blq_signatures(d, context)$signature
    d
  }
  d <- legacy(prepared012())
  expect_equal(qc012(pkapp_nca(d, prepared_spec012))$reason, "unverified_lloq")
  context <- core$desktop_pk_blq_context(d); context$step$lloq_value <- .01
  d$NCA_BLQ_CONTEXT <- core$desktop_pk_blq_json(context)
  expect_equal(qc012(pkapp_nca(d, prepared_spec012))$reason, "unverified_lloq")
  context$step$lloq_source <- "column"; context$step$lloq_column <- "BLQ"
  d$NCA_BLQ_CONTEXT <- core$desktop_pk_blq_json(context)
  baseline <- pkapp_nca(d, modifyList(prepared_spec012, list(predose_qc_threshold_pct = NULL), keep.null = TRUE))
  r <- pkapp_nca(d, modifyList(prepared_spec012, list(predose_qc_threshold_pct = 20)))
  expect_equal(qc012(r)$reason, "unverified_lloq")
  expect_equal(qc012(r)$status, "not_evaluable")
  expect_true(is.na(qc012(r)$lloq) && is.na(qc012(r)$pct_upper_bound))
  expect_identical(r$data$AVAL, baseline$data$AVAL)
  for (kind in c("constant", "column")) {
    fixture <- pkapp_read_table(paste0("apps/pk-desktop/release/tests/fixtures/prepared_v1_", kind, ".csv"))
    baseline <- pkapp_nca(fixture, prepared_spec012)
    if (kind == "constant") {
      context <- core$desktop_pk_blq_context(fixture)
      context$step$lloq_source <- "column"; context$step$lloq_column <- "BLQ"
      fixture$NCA_BLQ_CONTEXT <- core$desktop_pk_blq_json(context)
    }
    r <- pkapp_nca(fixture, modifyList(prepared_spec012, list(predose_qc_threshold_pct = 20)))
    expect_equal(qc012(r)$status, if (kind == "constant") "not_evaluable" else "pass_blq")
    expect_equal(qc012(r)$reason, if (kind == "constant") "unverified_lloq" else "")
    expect_identical(r$data$AVAL, baseline$data$AVAL)
  }
  step <- modifyList(step012, list(lloq_source = "column", lloq_column = "LLOQ", lloq_value = NULL), keep.null = TRUE)
  d <- legacy(prepared012(step = step))
  expect_equal(qc012(pkapp_nca(d, prepared_spec012))$status, "pass_blq")
  d$LLOQ[2] <- .01
  expect_error(pkapp_nca(d, prepared_spec012), "再適用")
})

test_that("prepared QC preserves partial-scope refusal at the specification boundary", {
  for (column in c("NCA_BLQ_BASELINE", "NCA_BLQ_PROFILE_SIGNATURE")) {
    d <- raw012(); d$NCA_CONC <- d$CONC; d[[column]] <- "orphaned"
    expect_error(run012(d, concentration = "NCA_CONC"), "再適用")
  }
  d <- prepared012(); d$SSNCAFL <- c("N", "Y", "Y", "Y", "Y")
  expect_error(pkapp_nca(d, modifyList(prepared_spec012, list(scope = "ss"))), "部分|解析単位")
  d$SSNCAFL <- "Y"
  expect_equal(qc012(pkapp_nca(d, modifyList(prepared_spec012, list(scope = "ss"))))$status, "pass_blq")
})
