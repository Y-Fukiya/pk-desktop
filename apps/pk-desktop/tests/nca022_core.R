source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
times <- c(-.25, 0, 1, 2, 4, 8, 12, 24, 48)
pc <- do.call(rbind, lapply(1:2, function(p) data.frame(ID = "001", PERIOD = p,
  SAMPLE_ID = paste(p, seq_along(times), sep = "-"),
  SAMPLE_DTM = format(as.POSIXct("2026-11-01 09:00:00", tz = "UTC") + ((p-1)*168+times)*3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))))
conc <- pc[c("ID", "PERIOD", "SAMPLE_ID")]
conc$ANALYTE <- "parent"; conc$CONC <- rep(c(0, 10 * exp(-.1 * times[-1])), 2)
conc$BLQ <- rep(c(TRUE, rep(FALSE, 8)), 2); conc$MDV <- FALSE; conc$LLOQ <- .125; conc$UNIT <- "ng/mL"
ex <- data.frame(ID = "001", PERIOD = 1:2, DOSE_ID = c("D1", "D2"), DOSE_DTM = c("2026-11-01T09:00:00Z", "2026-11-08T09:00:00Z"))
mapping <- function(x) as.list(stats::setNames(names(x), names(x)))
s <- list(version = 1, mapping = list(CONC = mapping(conc), PC = mapping(pc), EX = mapping(ex)),
  sample_keys = c("ID", "SAMPLE_ID"), observation_keys = c("ID", "SAMPLE_ID", "ANALYTE"),
  profile_keys = c("ID", "PERIOD", "ANALYTE"), dose_group_keys = c("ID", "PERIOD"), dose_record_keys = c("ID", "DOSE_ID"),
  dose_reference = list(mode = "first"), datetime = list(sample_type = "iso", dose_type = "iso", utc_offset = "+00:00"),
  unmatched = "error", missing_datetime = "error", dose_history_confirmed = TRUE)
f <- list(conc = conc, pc = pc, ex = ex, specification = s)
assembled <- do.call(pkapp_assemble_nca, f)
spec <- list(id = "ID", groups = c("PERIOD", "ANALYTE"), time = "NCA_TIME_H", concentration = "CONC", unit_column = "UNIT",
  blq_column = "BLQ", mdv_column = "MDV", blq = "as_recorded", c0_method = "none", auc_method = "lin up/log down",
  terminal_method = "auto", dosing_context = "isolated_single_period", washout_attestation = "protocol_confirmed", dose_history_confirmed = TRUE,
  predose_qc_threshold_pct = 5, predose_qc_window_h = 1, predose_qc_lloq_source = "column", predose_qc_lloq_column = "LLOQ")
run <- function(data = assembled$data, ...) pkapp_nca(data, utils::modifyList(spec, list(...), keep.null = TRUE))
values <- function(x, param = "LAMZ") x$data$AVAL[x$data$PARAMCD == param]
reasons <- function(x, param = "LAMZ") x$data$NCA_EXCLUDE[x$data$PARAMCD == param]
raw <- function(data = assembled$data) data[, !grepl("^NCA_ASSEMBLY_", names(data)), drop = FALSE]
step <- list(op = "blq_rules", mode = "positional", profile_columns = list("ID", "PERIOD", "ANALYTE"),
  concentration = "CONC", time = "NCA_TIME_H", blq_column = "BLQ", mdv_column = "MDV", zero_time = "post",
  pre_rule = "exclude", first_rule = "exclude", middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude",
  lloq_source = "column", lloq_column = "LLOQ")
prepared_run <- function(data, ...) run(pkapp_prepare(data, list(step))$data,
  concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV", ...)

extra_fixture <- function(hour = 24, extra = numeric()) {
  changed <- f
  additions <- data.frame(ID = "001", PERIOD = 1, DOSE_ID = paste0("EXTRA", seq_along(c(hour, extra))),
    DOSE_DTM = format(as.POSIXct("2026-11-01 09:00:00", tz = "UTC") + c(hour, extra)*3600,
      "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  changed$ex <- rbind(changed$ex, additions)
  do.call(pkapp_assemble_nca, changed)
}
check_excluded <- function(answer) {
  bad <- answer$data$PERIOD == 1
  expect_true(all(is.na(answer$data$AVAL[bad])))
  expect_true(all(answer$data$NCA_EXCLUDE[bad] == "intervening_dose"))
  expect_true(all(answer$data$PARAM_STATUS[bad] == "not_calculable"))
  expect_true(all(answer$data$PROFILE_STATUS[bad] == "excluded"))
  expect_false(any(grepl("投与前QC：評価不可", answer$data$NCA_WARNING[bad], fixed = TRUE)))
  flags <- intersect(c("USE_OBS", "USE_AUC", "USE_AUCPROC", "USE_TERMINAL", "TERMINAL_ELIGIBLE"), names(answer$trace$rows))
  expect_false(any(as.matrix(answer$trace$rows[answer$trace$rows$PERIOD == 1, flags])))
  expect_equal(unname(answer$counts["failed_curves"]), 1)
}

test_that("actual intervening EX excludes every requested parameter only for its profile", {
  data <- extra_fixture(extra = 36)$data
  for (method in c("none", "auto", "manual")) {
    settings <- list(terminal_method = method, include_aucproc = TRUE)
    if (method == "manual") settings <- c(settings, list(terminal_start_h = 2, terminal_end_h = 12))
    answer <- do.call(run, c(list(data = data), settings))
    baseline <- do.call(run, settings)
    check_excluded(answer)
    expect_equal(answer$data[answer$data$PERIOD == 2, ], baseline$data[baseline$data$PERIOD == 2, ])
    review <- answer$trace$groups[[1]]$dose_review
    expect_identical(review$reason, "intervening_dose")
    expect_equal(review$next_h, 24)
    expect_equal(review$reference, unique(data$NCA_DOSE_REF_ID[data$PERIOD == 1]))
    expect_equal(review$next_dose$ex_row, 3)
    expect_equal(review$next_dose$key, "c3:001c6:EXTRA1")
    expect_equal(review$affected_rows$source_row, c(8L, 9L))
    expect_equal(review$affected_rows$conc_row, c(8, 9))
    expect_equal(review$affected_rows$pc_row, c(8, 9))
    expect_equal(review$affected_rows$time_h, c(24, 48))
    if (method != "none") {
      expect_null(answer$trace$terminal$profiles[[1]]$selected)
      expect_length(answer$trace$terminal$profiles[[1]]$candidates, 0)
      expect_identical(answer$trace$terminal$profiles[[1]]$eligibility$reason, "intervening_dose")
    }
  }
  check_excluded(run(data, start = 1, include_aucproc = TRUE))
})

test_that("sampling interval is inclusive and manual terminal bounds cannot hide dosing", {
  for (hour in c(23.999, 24, 24.001)) check_excluded(run(extra_fixture(hour)$data))
  expect_true(all(is.finite(values(run(extra_fixture(48.001)$data)))))
  expect_true(all(is.finite(values(run(extra_fixture(24)$data, end = 23.999, terminal_qc_action = "flag")))))
  check_excluded(run(extra_fixture(24)$data, end = 24))
})

test_that("original sampling survives BLQ preparation and missing concentration", {
  data <- extra_fixture()$data
  crossing <- data$PERIOD == 1 & data$NCA_TIME_H >= 24
  data$BLQ[crossing] <- TRUE
  check_excluded(run(data, blq = "exclude"))
  check_excluded(prepared_run(data))
  data$CONC[crossing] <- NA
  check_excluded(run(data))
  check_excluded(prepared_run(data))
  data$MDV[crossing] <- TRUE
  expect_true(all(is.finite(values(run(data, terminal_qc_action = "flag")))) )
  expect_true(all(is.finite(values(prepared_run(data, terminal_qc_action = "flag")))) )
  data <- extra_fixture()$data
  data$SSNCAFL <- TRUE
  data$SSNCAFL[data$PERIOD == 1 & data$NCA_TIME_H >= 24] <- FALSE
  expect_true(all(is.finite(values(run(data, scope = "ss", terminal_qc_action = "flag")))))
})

test_that("all-BLQ zero and synthetic C0 do not restore excluded parameters", {
  data <- extra_fixture()$data
  data$BLQ[data$PERIOD == 1] <- TRUE
  check_excluded(run(data, all_blq_policy = "zero", blq = "zero", include_aucproc = TRUE))
  data <- extra_fixture()$data
  data <- data[data$NCA_TIME_H != 0, ]
  answer <- run(data, c0_method = "zero")
  check_excluded(answer)
  expect_false(any(answer$trace$rows$IMPUTED[answer$trace$rows$PERIOD == 1]))
})

test_that("optional provenance survives CSV while legacy records remain truthful", {
  data <- extra_fixture()$data
  file <- tempfile(fileext = ".csv")
  core$pk_write_csv(data, file)
  reread <- pkapp_read_table(file)
  expect_equal(run(reread)$data, run(data)$data)
  expect_equal(run(reread)$trace$groups[[1]]$dose_review, run(data)$trace$groups[[1]]$dose_review)
  data$NCA_NEXT_EX_ROW <- NULL; data$NCA_NEXT_DOSE_REF_ID <- NULL
  answer <- run(data)
  check_excluded(answer)
  expect_null(answer$trace$groups[[1]]$dose_review$next_dose$ex_row)
  expect_null(answer$trace$groups[[1]]$dose_review$next_dose$key)
  for (column in c("NCA_DOSE_EVIDENCE_VERSION", "NCA_PREV_DOSE_H", "NCA_PRIOR_DOSE_COUNT", "NCA_LATER_DOSE_COUNT")) data[[column]] <- NULL
  check_excluded(run(data))
})

test_that("invalid next evidence and altered identity still stop the complete run", {
  for (bad in list("bad", 0, -1, Inf)) {
    data <- extra_fixture()$data; data$NCA_NEXT_DOSE_H[data$PERIOD == 1] <- bad
    expect_error(run(data), "次回投与時刻")
  }
  for (bad in list(NA, 25)) {
    data <- extra_fixture()$data; data$NCA_NEXT_DOSE_H[1] <- bad
    expect_error(run(data), "次回投与時刻")
  }
  for (column in c("NCA_NEXT_EX_ROW", "NCA_NEXT_DOSE_REF_ID")) {
    data <- extra_fixture()$data; data[[column]] <- NULL
    expect_error(run(data), "次回投与の記録")
    data <- extra_fixture()$data; data[[column]][1] <- if (column == "NCA_NEXT_EX_ROW") 99 else "other"
    expect_error(run(data), "次回投与のEX行")
    prepared <- pkapp_prepare(extra_fixture()$data, list(step))$data
    prepared[[column]][1] <- if (column == "NCA_NEXT_EX_ROW") 99 else "other"
    expect_error(run(prepared, concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV"))
  }
  data <- extra_fixture()$data; data$ID[data$PERIOD == 2] <- "different"
  expect_error(run(data), "結合時の解析単位と現在")
  data <- extra_fixture()$data; data$EX_DOSE_ID[data$PERIOD == 2] <- "different"
  expect_error(run(data), "基準投与とEX")
  data <- extra_fixture()$data; data$NCA_ASSEMBLY_STATUS[data$PERIOD == 2] <- "unresolved"
  expect_error(run(data), "未解決")
})

test_that("prior-dose QC distinguishes an exceedance and legacy provenance is explicit", {
  data <- assembled$data
  pre <- data$PERIOD == 2 & data$NCA_TIME_H < 0
  data$BLQ[pre] <- FALSE; data$CONC[pre] <- 1
  answer <- run(data)
  expect_equal(reasons(answer)[2], "predose_qc_exceeded")
  expect_equal(unique(answer$data$PREDOSE_QC_PCT[answer$data$PERIOD == 2]), 10)
  expect_match(answer$data$NCA_WARNING[answer$data$PERIOD == 2][1], "投与前QC：閾値超過", fixed = TRUE)
  expect_false(any(grepl("Predose", answer$data$NCA_WARNING, fixed = TRUE)))
  expect_equal(run(dosing_context = "repeated_final")$trace$terminal$profiles[[1]]$eligibility$evidence$provenance, "assembly")
  expect_equal(run(raw(), dosing_context = "repeated_final")$trace$terminal$profiles[[1]]$eligibility$evidence$provenance, "declared_raw")
})


test_that("assembly reserves next-dose columns and selected references keep exact provenance", {
  changed <- f
  changed$conc$NCA_NEXT_EX_ROW <- 999
  changed$conc$NCA_NEXT_DOSE_REF_ID <- "input"
  data <- do.call(pkapp_assemble_nca, changed)$data
  expect_true(all(data$CONC_NCA_NEXT_EX_ROW == 999))
  expect_true(all(data$CONC_NCA_NEXT_DOSE_REF_ID == "input"))
  expect_equal(unique(data$NCA_NEXT_EX_ROW[data$PERIOD == 1]), 2)
  expect_true(all(is.na(data$NCA_NEXT_EX_ROW[data$PERIOD == 2])))
  changed <- f
  changed$ex <- rbind(changed$ex, data.frame(ID = "001", PERIOD = 1, DOSE_ID = "EXTRA",
    DOSE_DTM = "2026-11-02T09:00:00Z"))
  changed$specification$dose_reference <- list(mode = "per_profile", assignments = list(
    list(profile = list(ID = "001", PERIOD = 1, ANALYTE = "parent"), dose = list(ID = "001", DOSE_ID = "D1")),
    list(profile = list(ID = "001", PERIOD = 2, ANALYTE = "parent"), dose = list(ID = "001", DOSE_ID = "D2"))))
  data <- do.call(pkapp_assemble_nca, changed)$data
  answer <- run(data)
  check_excluded(answer)
  expect_equal(answer$trace$groups[[1]]$dose_review$next_dose$ex_row, 3)
  prepared <- pkapp_prepare(data, list(step))$data
  file <- tempfile(fileext = ".csv")
  core$pk_write_csv(prepared, file)
  expect_equal(run(pkapp_read_table(file), concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV")$trace,
    run(prepared, concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV")$trace)
})
