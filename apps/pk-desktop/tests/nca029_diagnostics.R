source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_nca)

test_that("multiple simultaneous terminal QC reasons display once without changing results", {
  d <- data.frame(ID="001", PERIOD="1", TIME=0:7, CONC=c(0,10,7,6.9,5.5,5.4,5,4.8))
  s <- list(id="ID", groups=list("PERIOD"), time="TIME", concentration="CONC", concentration_unit="ng/mL",
    auc_method="linear", c0_method="none", terminal_method="auto", dosing_context="single", dose_history_confirmed=TRUE,
    terminal_qc_action="withhold")
  r <- pkapp_nca(d,s)
  reasons <- c("span_below_threshold", "extrapolation_above_threshold")
  labels <- vapply(reasons, core$desktop_nca_diagnostic_label, character(1), USE.NAMES=FALSE)
  for (label in labels) expect_equal(sum(grepl(label, r$warnings, fixed=TRUE)), 1)
  expect_true(all(grepl("ID=001 / PERIOD=1", r$warnings[grepl("ID=", r$warnings, fixed=TRUE)], fixed=TRUE)))
  expect_equal(unname(r$data$AVAL[match(c("CMAX","TMAX","AUCLST"), r$data$PARAMCD)]),c(10,1,42.2))
  expect_true(any(grepl("span_below_threshold;extrapolation_above_threshold", r$data$NCA_EXCLUDE, fixed=TRUE), na.rm=TRUE))
  expect_true(any(grepl("span_below_threshold;extrapolation_above_threshold", r$trace$groups[[1]]$warning, fixed=TRUE)))
  html <- as.character(core$desktop_nca_warnings(r)); doc <- xml2::read_html(html)
  displayed <- xml2::xml_text(xml2::xml_find_all(doc, '//div[@class="pk-diagnostic-message"]'))
  for (label in labels) expect_equal(sum(grepl(label, displayed, fixed=TRUE)), 1)
  wide <- core$desktop_nca_summary(r)
  for (reason in reasons) expect_equal(lengths(regmatches(wide$NCA_WARNING, gregexpr(reason, wide$NCA_WARNING, fixed=TRUE))), 1)
  expect_true(any(grepl("LAMZ", r$warnings[grepl(labels[1], r$warnings, fixed=TRUE)], fixed=TRUE)))
  expect_true(any(grepl("AUCIFO", r$warnings[grepl(labels[2], r$warnings, fixed=TRUE)], fixed=TRUE)))
  old <- core$desktop_nca_public_warnings
  core$desktop_nca_public_warnings <- function(value) value$trace$warnings
  on.exit(core$desktop_nca_public_warnings <- old)
  raw <- pkapp_nca(d,s)
  expect_identical(r$data,raw$data); expect_identical(r$trace,raw$trace); expect_identical(r$counts,raw$counts)
})

test_that("only compound reason codes are split; raw prose and unknown codes remain available", {
  expect_identical(core$desktop_nca_reason_parts("span_below_threshold;extrapolation_above_threshold"), c("span_below_threshold","extrapolation_above_threshold"))
  text <- "All-BLQ zero policy assigned CMAX/AUC; Tmax and quantifiable Tlast remain undefined"
  expect_identical(core$desktop_nca_reason_parts(text), text)
  expect_identical(core$desktop_nca_expand_messages(c("CMAX: future_code;span_below_threshold", "CMAX: span_below_threshold")), c("CMAX: future_code", "CMAX: span_below_threshold"))
  expect_match(core$desktop_nca_warning_text("future_code"), "future_code", fixed=TRUE)
})
