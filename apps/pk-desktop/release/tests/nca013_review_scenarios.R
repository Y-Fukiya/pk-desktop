source("apps/pk-desktop/pkapp.R")
core <- environment(pkapp_open)
passed <- 0L
check <- function(label, expression) {
  force(expression)
  passed <<- passed + 1L
  cat("PASS", passed, label, "\n")
}
raw <- data.frame(ID = "001", TIME = c(-.25, .5, 1, 2, 4, 8),
  CONC = c("<0.2", "3", "8", "6", "4", "1.5"), BLQ = c(1, 0, 0, 0, 0, 0), LLOQ = .2, UNIT = "ng/mL")
step <- list(op = "blq_rules", mode = "positional", profile_columns = "ID", concentration = "CONC", blq_column = "BLQ",
  time = "TIME", zero_time = "pre", pre_rule = "zero", first_rule = "zero", middle_rule = "exclude",
  last_rule = "exclude", all_blq_rule = "exclude", lloq_source = "constant")
spec <- list(id = "ID", time = "TIME", concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV",
  blq = "as_recorded", unit_column = "UNIT", auc_method = "linear", c0_method = "zero",
  predose_qc_threshold_pct = 5, predose_qc_window_h = 1)
supplement <- modifyList(spec, list(predose_qc_lloq_source = "column", predose_qc_lloq_column = "LLOQ"))
prepared <- pkapp_prepare(raw, list(step))$data
original <- prepared
qc <- function(x, s = supplement) pkapp_nca(x, s)$trace$groups[[1]]$predose_qc

check("blank prepared QC source remains recipe-only", {
  stopifnot(identical(qc(prepared, spec)$reason, "missing_lloq"))
})
check("explicit missing LLOQ supplement passes without changing PK or input", {
  r <- pkapp_nca(prepared, supplement)
  q <- r$trace$groups[[1]]$predose_qc
  stopifnot(q$status == "pass_blq", q$pct_upper_bound == 2.5,
    q$lloq_source == "qc_supplement:column:LLOQ", is.na(q$concentration), is.na(q$pct),
    identical(prepared, original), identical(unname(r$data$AVAL), c(8, 1, 31.5)))
})
check("supplemented and recipe rows retain different selected-row origins", {
  d <- rbind(raw, transform(raw, ID = "002"))
  d$LLOQ[d$ID == "001"] <- NA_real_
  p <- pkapp_prepare(d, list(modifyList(step, list(lloq_source = "column", lloq_column = "LLOQ"))))$data
  s <- modifyList(spec, list(predose_qc_lloq_source = "constant", predose_qc_lloq_value = .4))
  r <- pkapp_nca(p, s)
  a <- r$trace$groups[[1]]$predose_qc; b <- r$trace$groups[[2]]$predose_qc
  stopifnot(a$pct_upper_bound == 5, a$lloq_source == "qc_supplement:constant",
    b$pct_upper_bound == 2.5, b$lloq_source == "column:LLOQ", a$status == "pass_blq", b$status == "pass_blq")
})
check("existing valid recipe wins over a larger explicit supplement", {
  p <- pkapp_prepare(raw, list(modifyList(step, list(lloq_value = .2))))$data
  s <- modifyList(spec, list(predose_qc_lloq_source = "constant", predose_qc_lloq_value = 999))
  q <- qc(p, s)
  stopifnot(q$status == "pass_blq", q$lloq == .2, q$lloq_source == "constant")
})
check("invalid recipe is not repaired silently by a supplement", {
  p <- pkapp_prepare(raw, list(modifyList(step, list(lloq_value = -.2))))$data
  stopifnot(qc(p)$reason == "invalid_lloq")
})
check("unverified legacy constant remains unverified", {
  p <- pkapp_read_table("apps/pk-desktop/release/tests/fixtures/prepared_v1_constant.csv")
  stopifnot(qc(p)$reason == "unverified_lloq")
})
check("invalid explicit value remains unavailable", {
  s <- modifyList(spec, list(predose_qc_lloq_source = "constant", predose_qc_lloq_value = -.1))
  stopifnot(qc(prepared, s)$reason == "invalid_lloq")
})
check("larger BLQ bound is unavailable rather than measured exceedance", {
  s <- modifyList(spec, list(predose_qc_lloq_source = "constant", predose_qc_lloq_value = .5))
  q <- qc(prepared, s)
  stopifnot(q$status == "not_evaluable", q$reason == "bound_above_threshold", q$pct_upper_bound == 6.25)
})
check("latest BLQ is selected even after an earlier measurable predose", {
  d <- rbind(transform(raw[1, ], TIME = -.75, CONC = "9.5", BLQ = 0), raw)
  p <- pkapp_prepare(d, list(step))$data
  q <- qc(p)
  stopifnot(q$status == "pass_blq", q$source_row == 2L, q$time_h == -.25)
})
check("duplicate latest predose is not resolved by supplementation", {
  p <- pkapp_prepare(rbind(raw[1, ], raw), list(step))$data
  stopifnot(qc(p)$reason == "ambiguous_latest")
})
check("supplementation does not bypass unit mismatch", {
  d <- raw; d$UNIT[1] <- "ug/mL"
  p <- pkapp_prepare(d, list(step))$data
  error <- tryCatch({ qc(p); "" }, error = conditionMessage)
  stopifnot(grepl("単位", error, fixed = TRUE))
  excluded <- pkapp_prepare(d, list(modifyList(step, list(pre_rule = "exclude"))))$data
  stopifnot(qc(excluded)$reason == "unit_mismatch")
})
check("supplementation never rescues modified prepared evidence", {
  for (kind in c("id", "value", "row")) {
    p <- prepared
    if (kind == "id") p$ID <- "1"
    if (kind == "value") p$CONC[3] <- "9"
    if (kind == "row") p <- p[-3, ]
    error <- tryCatch({ pkapp_nca(p, supplement); "" }, error = conditionMessage)
    stopifnot(grepl("再適用", error, fixed = TRUE))
  }
})
cat("Independent 0.13 review scenarios:", passed, "/12 passed\n")
stopifnot(passed == 12L)
