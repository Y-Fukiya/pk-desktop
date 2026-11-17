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

test_that("two isolated periods use complete history and preserve closed-form PK", {
  answer <- run()
  expect_equal(values(answer), c(.1, .1), tolerance = 1e-12)
  expect_equal(values(answer, "LAMZHL"), rep(log(2)/.1, 2), tolerance = 1e-12)
  expect_equal(values(answer, "AUCIFO"), c(100, 100), tolerance = 1e-12)
  expect_equal(values(answer, "AUCLST"), rep(100 * (1-exp(-4.8)), 2), tolerance = 1e-12)
  expect_equal(values(answer, "CMAX"), c(10,10))
  expect_equal(unique(assembled$data$NCA_PREV_DOSE_H[assembled$data$PERIOD == 2]), -168)
  expect_equal(unique(assembled$data$NCA_NEXT_DOSE_H[assembled$data$PERIOD == 1]), 168)
  expect_equal(unique(assembled$data$NCA_DOSE_COUNT), 2)
  expect_equal(assembled$data$NCA_PRIOR_DOSE_COUNT, rep(0:1, each = 9))
  expect_equal(assembled$data$NCA_LATER_DOSE_COUNT, rep(1:0, each = 9))
  expect_true(all(assembled$data$NCA_DOSE_EVIDENCE_VERSION == 2))
  expect_identical(answer$trace$terminal$profiles[[2]]$eligibility$predose_qc$status, "pass_blq")
  expect_equal(answer$trace$terminal$profiles[[2]]$eligibility$predose_qc$pct_upper_bound, 1.25)
  expect_true(answer$trace$terminal$profiles[[1]]$eligibility$eligible)
  expect_equal(answer$trace$terminal$profiles[[1]]$eligibility$observation_end_h, 48)
  expect_equal(values(run(raw())), values(answer))
  expect_identical(run(raw())$trace$terminal$profiles[[1]]$eligibility$evidence$provenance, "declared_raw")
  expect_equal(values(prepared_run(assembled$data)), values(answer))
  expect_equal(values(prepared_run(assembled$data), "AUCIFO"), values(answer, "AUCIFO"))
  expect_equal(values(run(end = 200)), c(.1, .1))
})

test_that("prior doses require their own passing original predose QC and attestation", {
  for (change in list(list(washout_attestation = "unconfirmed"), list(predose_qc_threshold_pct = NULL),
      list(predose_qc_window_h = .1), list(predose_qc_lloq_source = "", predose_qc_lloq_column = ""))) {
    answer <- do.call(run, change)
    expect_equal(values(answer)[1], .1)
    expect_true(is.na(values(answer)[2]))
    expect_match(reasons(answer)[2], "washout_unconfirmed|predose_qc_required_for_prior_dose")
  }
  for (lloq in list(NA, "invalid", -1, 1)) {
    data <- assembled$data; data$LLOQ[data$PERIOD == 2 & data$NCA_TIME_H < 0] <- lloq
    expect_true(is.na(values(run(data))[2]))
  }
  data <- assembled$data; pre <- data$PERIOD == 2 & data$NCA_TIME_H < 0
  data$BLQ[pre] <- FALSE; data$CONC[pre] <- 1
  expect_true(is.na(values(run(data))[2]))
  data$CONC[pre] <- .1
  expect_equal(values(run(data)), c(.1, .1))
  data <- raw(); data$UNIT[data$NCA_TIME_H < 0 & data$PERIOD == 2] <- "mg/L"
  expect_true(is.na(values(run(data, blq = "exclude"))[2]))
  data <- raw(); data$NCA_TIME_H[data$PERIOD == 2 & data$NCA_TIME_H == 0] <- -.25
  expect_true(is.na(values(run(data))[2]))
  expect_true(all(is.na(values(run(dose_history_confirmed = FALSE)))))
})

test_that("invalid or absent evidence never implies no prior dose", {
  for (column in core$desktop_nca_dose_evidence_columns()) {
    data <- raw(); data[[column]] <- NULL
    expect_true(all(is.na(values(run(data)))))
    expect_true(all(reasons(run(data)) == "dose_evidence_requires_reassembly"))
  }
  changes <- list(NCA_DOSE_EVIDENCE_VERSION = 1, NCA_DOSE_COUNT = 3,
    NCA_PRIOR_DOSE_COUNT = -1, NCA_LATER_DOSE_COUNT = .5, NCA_PREV_DOSE_H = 0,
    NCA_NEXT_DOSE_H = -168, NCA_DOSE_HISTORY_CONFIRMED = FALSE, NCA_DOSE_REF_ID = "")
  for (column in names(changes)) {
    data <- raw(); data[[column]] <- changes[[column]]
    expect_true(all(is.na(values(run(data)))))
  }
  data <- raw(); data$NCA_PREV_DOSE_H[10] <- NA
  expect_true(is.na(values(run(data))[2]))
  data <- raw(); data$NCA_DOSE_COUNT[1] <- 9
  expect_true(is.na(values(run(data))[1]))
  data <- raw(); data$NCA_TIME_H[2] <- NA
  expect_match(reasons(run(data))[1], "incomplete_sampling_time_evidence")
  for (settings in list(list(time_unit = "min"), list(time_unit = "day"), list(time_origin = -1))) {
    answer <- do.call(run, c(list(data = raw()), settings))
    expect_true(all(is.na(values(answer))))
    expect_true(all(reasons(answer) == "dose_evidence_time_context_mismatch"))
  }
  expect_error(run(washout_attestation = "guess"), "終末相設定")
  expect_error(run(unknown = TRUE), "設定項目")
})

test_that("original sampling bounds include excluded BLQ after manual endpoint", {
  for (bound in c(24, 48)) {
    data <- raw(); data$NCA_NEXT_DOSE_H[data$PERIOD == 1] <- bound
    answer <- run(data, terminal_method = "manual", terminal_start_h = 2, terminal_end_h = 12)
    expect_match(reasons(answer)[1], "intervening_dose")
    expect_null(answer$trace$terminal$profiles[[1]]$selected)
    data$BLQ[data$PERIOD == 1 & data$NCA_TIME_H >= 24] <- TRUE
    answer <- run(data, blq = "exclude", terminal_method = "manual", terminal_start_h = 2, terminal_end_h = 12)
    expect_match(reasons(answer)[1], "intervening_dose")
    answer <- prepared_run(data, terminal_method = "manual", terminal_start_h = 2, terminal_end_h = 12)
    expect_match(reasons(answer)[1], "intervening_dose")
    expect_equal(answer$trace$terminal$profiles[[1]]$eligibility$observation_end_h, 48)
    assembled_bad <- data
    assembled_bad$NCA_ASSEMBLY_PROFILE <- assembled$data$NCA_ASSEMBLY_PROFILE
    assembled_bad$NCA_ASSEMBLY_PROFILE_COLUMNS <- assembled$data$NCA_ASSEMBLY_PROFILE_COLUMNS
    assembled_bad$NCA_ASSEMBLY_STATUS <- "ok"
    blocked <- prepared_run(assembled_bad)
    expect_true(all(is.na(blocked$data$AVAL[blocked$data$PERIOD == 1])))
    expect_true(all(blocked$data$NCA_EXCLUDE[blocked$data$PERIOD == 1] == "intervening_dose"))
    expect_equal(values(blocked)[2], .1)
    expect_equal(values(run(data, end = 12, terminal_qc_action = "flag"))[1], .1)
    data$MDV[data$PERIOD == 1 & data$NCA_TIME_H >= bound] <- TRUE
    expect_equal(values(run(data, terminal_qc_action = "flag"))[1], .1)
  }
  data <- raw(); data$NCA_NEXT_DOSE_H[data$PERIOD == 1] <- 49
  expect_equal(values(run(data)), c(.1, .1))
  data$NCA_NEXT_DOSE_H[data$PERIOD == 1] <- 24; data$SSNCAFL <- TRUE
  data$SSNCAFL[data$PERIOD == 1 & data$NCA_TIME_H >= 24] <- FALSE
  expect_equal(values(run(data, scope = "ss", terminal_qc_action = "flag"))[1], .1)
})

test_that("preprocessing protects complete dose evidence and survives CSV roundtrip", {
  prepared <- pkapp_prepare(assembled$data, list(step))$data
  for (column in core$desktop_nca_dose_evidence_columns()) {
    data <- prepared
    data[[column]][1] <- if (is.character(data[[column]])) "tamper" else 999
    expect_error(run(data, concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV"))
    data <- prepared; data[[column]] <- NULL
    expect_error(run(data, concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV"))
  }
  path <- tempfile(fileext = ".csv"); core$pk_write_csv(prepared, path)
  expect_equal(values(run(pkapp_read_table(path), concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV")), c(.1, .1))
  data <- f$conc; data$NCA_PREV_DOSE_H <- 999
  changed <- pkapp_assemble_nca(data, f$pc, f$ex, f$specification)$data
  expect_true(all(changed$CONC_NCA_PREV_DOSE_H == 999))
  expect_equal(unique(changed$NCA_PREV_DOSE_H[changed$PERIOD == 2]), -168)
})

test_that("unknown or tied clocks and one-sided studies cannot prove complete history", {
  changed <- f; changed$specification$missing_datetime <- "missing"; changed$specification$unmatched <- "missing"
  changed$ex$DOSE_DTM[1] <- NA
  result <- do.call(pkapp_assemble_nca, changed)
  expect_false(any(result$data$NCA_DOSE_HISTORY_CONFIRMED))
  expect_error(run(result$data), "未解決|結合時の基準投与とEX")
  changed$ex$DOSE_DTM[1] <- changed$ex$DOSE_DTM[2]
  result <- do.call(pkapp_assemble_nca, changed)
  expect_false(any(result$data$NCA_DOSE_HISTORY_CONFIRMED))
  expect_error(run(result$data), "未解決|結合時の基準投与とEX")
  changed <- f; changed$ex$STUDYID <- "A"; changed$specification$mapping$EX$STUDYID <- "STUDYID"
  changed$specification$dose_record_keys <- c("STUDYID", "ID", "DOSE_ID")
  expect_error(do.call(pkapp_assemble_nca, changed), "STUDYID must be mapped in both")
  changed <- f
  for (name in c("conc", "pc", "ex")) {
    changed[[name]]$STUDYID <- "A"
    other <- changed[[name]]; other$STUDYID <- "B"
    changed[[name]] <- rbind(changed[[name]], other)
    changed$specification$mapping[[toupper(name)]]$STUDYID <- "STUDYID"
  }
  for (key in c("sample_keys", "profile_keys", "dose_group_keys", "dose_record_keys", "observation_keys"))
    changed$specification[[key]] <- c("STUDYID", changed$specification[[key]])
  result <- do.call(pkapp_assemble_nca, changed)
  expect_true(all(result$data$NCA_DOSE_COUNT == 2))
  expect_equal(values(run(result$data, groups = c("STUDYID", "PERIOD", "ANALYTE"))), rep(.1, 4))
})

test_that("legacy contexts preserve restrictions and old evidence needs reassembly", {
  single <- run(dosing_context = "single"); final <- run(dosing_context = "repeated_final")
  expect_true(all(is.na(values(single))))
  expect_match(reasons(single)[1], "single_dose_context_conflict")
  expect_equal(values(final), c(NA, .1))
  expect_true(all(is.na(values(final, "AUCIFO"))))
  expect_equal(values(single, "AUCLST"), values(run(), "AUCLST"))
  plain <- data.frame(ID = "S01", TIME = 0:6, CONC = 10 * exp(-.5 * (0:6)))
  answer <- pkapp_nca(plain, list(id = "ID", time = "TIME", concentration = "CONC", concentration_unit = "ng/mL",
    terminal_method = "auto", dosing_context = "single", dose_history_confirmed = TRUE))
  expect_equal(values(answer), .5); expect_equal(values(answer, "AUCIFO"), 20)
  expect_equal(core$desktop_nca_terminal_defaults()$washout_attestation, "unconfirmed")
  expect_equal(core$desktop_nca_terminal_defaults()$dosing_context, "ongoing")
  old <- assembled$data[, !names(assembled$data) %in% c("NCA_DOSE_EVIDENCE_VERSION", "NCA_PREV_DOSE_H", "NCA_PRIOR_DOSE_COUNT", "NCA_LATER_DOSE_COUNT")]
  frozen <- old
  expect_true(all(reasons(run(old)) == "dose_evidence_requires_reassembly"))
  expect_identical(old, frozen)
  expect_equal(values(run(old, dosing_context = "repeated_final")), c(NA, .1))
})

test_that("isolated results replay exactly and UI presents explicit attestation", {
  project <- tempfile("nca021-replay-"); dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  saved <- core$desktop_table_save(engine, "isolated", list(source = assembled$data), list(kind = "nca", specification = run()$specification))
  replay <- pkapp_replay(file.path(engine$tables_dir, saved$id))
  expect_identical(replay$data, run()$data)
  expect_identical(replay$trace, run()$trace)
  text <- as.character(core$desktop_nca_ui("nca"))
  expect_match(text, "isolated_single_period")
  expect_match(text, "protocol_confirmed")
  expect_match(text, "推定半減期から休薬")
  for (reason in c("dose_evidence_requires_reassembly", "washout_unconfirmed", "predose_qc_required_for_prior_dose"))
    expect_false(identical(core$desktop_nca_diagnostic_label(reason), reason))
})


test_that("all rows agree on reference evidence and legacy manual bounds fail closed", {
  data <- assembled$data; data$NCA_ASSEMBLY_PROFILE[1] <- "different"
  expect_error(run(data), "結合時の解析単位と現在")
  data <- raw(); data$NCA_DOSE_COUNT <- 2^53; data$NCA_PRIOR_DOSE_COUNT <- 2^53 - 1; data$NCA_LATER_DOSE_COUNT <- 0
  data$NCA_PREV_DOSE_H <- -168; data$NCA_NEXT_DOSE_H <- NA_real_
  expect_true(all(is.na(values(run(data)))))
  data <- raw(); data$MDV[1] <- TRUE; data$NCA_DOSE_COUNT[1] <- 9
  expect_true(is.na(values(run(data))[1]))
  data <- raw(); data$NCA_NEXT_DOSE_H[data$PERIOD == 1] <- 24
  answer <- run(data, dosing_context = "repeated_final", terminal_method = "manual", terminal_start_h = 2, terminal_end_h = 12)
  expect_match(reasons(answer)[1], "intervening_dose")
  expect_null(answer$trace$terminal$profiles[[1]]$selected)
  data <- raw(); data <- data[data$PERIOD == 1, ]
  data$NCA_DOSE_COUNT <- 1; data$NCA_NEXT_DOSE_H <- NA_real_
  data$NCA_PRIOR_DOSE_COUNT <- 0; data$NCA_LATER_DOSE_COUNT <- 0
  expect_equal(values(run(data, predose_qc_threshold_pct = NULL, washout_attestation = "unconfirmed")), .1)
})


test_that("stored assembly identity binds ID study and period including excluded rows", {
  changed <- f
  for (name in c("conc", "pc", "ex")) {
    changed[[name]]$STUDYID <- "試験:A"
    changed$specification$mapping[[toupper(name)]]$STUDYID <- "STUDYID"
  }
  for (key in c("sample_keys", "profile_keys", "dose_group_keys", "dose_record_keys", "observation_keys"))
    changed$specification[[key]] <- c("STUDYID", changed$specification[[key]])
  data <- do.call(pkapp_assemble_nca, changed)$data
  groups <- c("STUDYID", "PERIOD", "ANALYTE")
  expect_equal(values(run(data, groups = groups)), c(.1, .1))
  for (column in c("ID", "STUDYID", "PERIOD")) {
    tampered <- data; tampered[[column]] <- if (is.numeric(tampered[[column]])) tampered[[column]] + 2 else "OTHER"
    expect_error(run(tampered, groups = groups), "結合時の解析単位と現在")
    tampered <- data; tampered$MDV[1] <- TRUE
    tampered[[column]][1] <- if (is.numeric(tampered[[column]])) 9 else "OTHER"
    expect_error(run(tampered, groups = groups), "結合時の解析単位と現在")
    tampered <- data; tampered$SSNCAFL <- TRUE; tampered$SSNCAFL[1] <- FALSE
    tampered[[column]][1] <- if (is.numeric(tampered[[column]])) 9 else "OTHER"
    expect_error(run(tampered, groups = groups, scope = "ss"), "結合時の解析単位と現在")
  }
  path <- tempfile(fileext = ".csv")
  core$pk_write_csv(data, path)
  reread <- pkapp_read_table(path)
  expect_equal(values(run(reread, groups = groups)), c(.1, .1))
  data$PERIOD <- as.character(data$PERIOD)
  expect_equal(values(run(data, groups = groups)), c(.1, .1))
  for (token in c("bad", "n1:1", paste0(data$NCA_ASSEMBLY_PROFILE[1], "extra"))) {
    tampered <- data; tampered$NCA_ASSEMBLY_PROFILE[tampered$PERIOD == "1"] <- token
    expect_error(run(tampered, groups = groups), "結合時の解析単位と現在")
  }
})

test_that("source row markers cannot masquerade as plain single-dose inputs", {
  data <- assembled$data[assembled$data$PERIOD == 2, ]
  plain <- data[, !grepl("^NCA_", names(data)) | names(data) == "NCA_TIME_H", drop = FALSE]
  expect_equal(values(run(plain, dosing_context = "single")), .1)
  expect_equal(values(run(plain, dosing_context = "single"), "AUCIFO"), 100)
  for (marker in c("NCA_EX_ROW", "NCA_PC_ROW", "NCA_CONC_ROW", "NCA_ASSEMBLY_STATUS", "NCA_ASSEMBLY_REASON", "NCA_DOSE_REFERENCE_COLUMNS")) {
    partial <- plain; partial[[marker]] <- data[[marker]]
    answer <- run(partial, dosing_context = "single")
    expect_true(all(is.na(values(answer))))
    expect_identical(reasons(answer), "dose_evidence_requires_reassembly")
    expect_null(answer$trace$terminal$profiles[[1]]$selected)
  }
})

test_that("numeric profile and EX reference keys retain full precision through app CSV", {
  changed <- f
  for (name in c("conc", "pc", "ex")) changed[[name]]$PERIOD <- changed[[name]]$PERIOD + .123456789012345
  changed$ex$DOSE_ID <- c(101.12345678901234, 102.12345678901234)
  data <- do.call(pkapp_assemble_nca, changed)$data
  expect_equal(values(run(data)), c(.1, .1))
  path <- tempfile(fileext = ".csv")
  core$pk_write_csv(data, path)
  expect_equal(values(run(pkapp_read_table(path))), c(.1, .1))
})


test_that("selected reference tokens are bound to copied EX keys", {
  binding <- core$desktop_assembly_reference_columns(assembled$data)
  expect_identical(names(binding), c("ID", "DOSE_ID"))
  expect_identical(unlist(binding, use.names = FALSE), c("EX_ID", "EX_DOSE_ID"))
  data <- assembled$data
  data$NCA_DOSE_REF_ID[data$PERIOD == 2] <- data$NCA_DOSE_REF_ID[1]
  expect_error(run(data), "結合時の基準投与とEX")
  for (column in unlist(binding, use.names = FALSE)) {
    data <- assembled$data; data[[column]][data$PERIOD == 2] <- "OTHER"
    expect_error(run(data), "結合時の基準投与とEX")
    data <- assembled$data; data$MDV[10] <- TRUE; data[[column]][10] <- "OTHER"
    expect_error(run(data), "結合時の基準投与とEX")
  }
  data <- assembled$data; data$NCA_DOSE_REFERENCE_COLUMNS <- NULL
  expect_error(run(data), "結合時の基準投与とEX")
  for (bad in c("invalid", "{}", '{"ID":"missing","DOSE_ID":"EX_DOSE_ID"}',
      '{"ID":"EX_ID","DOSE_ID":"EX_ID"}')) {
    data <- assembled$data; data$NCA_DOSE_REFERENCE_COLUMNS <- bad
    expect_error(run(data), "基準投与キー")
  }
  data <- assembled$data; data$NCA_DOSE_REFERENCE_COLUMNS[1] <- "{}"
  expect_error(run(data), "基準投与キー")
  data <- assembled$data[, c("ID", "PERIOD", "ANALYTE", "CONC", "UNIT", "BLQ", "MDV", "LLOQ",
    "NCA_TIME_H", core$desktop_nca_dose_evidence_columns())]
  expect_equal(values(run(data)), c(.1, .1))
  expect_identical(run(data)$trace$terminal$profiles[[2]]$eligibility$evidence$provenance, "declared_raw")
})

test_that("EX reference mapping follows actual collision names and CSV", {
  changed <- f
  changed$conc$EX_DOSE_ID <- "existing concentration column"
  changed$conc$SOURCE_EX_DOSE_ID <- "second existing column"
  changed$conc$NCA_DOSE_REFERENCE_COLUMNS <- "original source value"
  data <- do.call(pkapp_assemble_nca, changed)$data
  binding <- core$desktop_assembly_reference_columns(data)
  expect_identical(binding$DOSE_ID, "SOURCE_SOURCE_EX_DOSE_ID")
  expect_true(all(data$CONC_NCA_DOSE_REFERENCE_COLUMNS == "original source value"))
  expect_equal(values(run(data)), c(.1, .1))
  path <- tempfile(fileext = ".csv"); core$pk_write_csv(data, path)
  expect_equal(values(run(pkapp_read_table(path))), c(.1, .1))
  data[[binding$DOSE_ID]][data$PERIOD == 2] <- "D1"
  expect_error(run(data), "結合時の基準投与とEX")
  changed <- f
  changed$ex$EX_DOSE_ID <- "unrelated canonical role"
  changed$specification$mapping$EX$EX_DOSE_ID <- "EX_DOSE_ID"
  data <- do.call(pkapp_assemble_nca, changed)$data
  binding <- core$desktop_assembly_reference_columns(data)
  expect_identical(binding$DOSE_ID, "CONC_EX_DOSE_ID")
  expect_equal(values(run(data)), c(.1, .1))
  expect_true(all(data$EX_DOSE_ID == "unrelated canonical role"))
})

test_that("BLQ signatures protect reference binding metadata and source keys", {
  prepared <- pkapp_prepare(assembled$data, list(step))$data
  binding <- core$desktop_assembly_reference_columns(prepared)
  context <- core$desktop_pk_blq_context(prepared)
  expect_true(all(c("NCA_DOSE_REFERENCE_COLUMNS", unlist(binding)) %in% unlist(context$columns)))
  for (column in c("NCA_DOSE_REFERENCE_COLUMNS", unlist(binding, use.names = FALSE))) {
    data <- prepared; data[[column]][1] <- "tampered"
    expect_error(run(data, concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV"))
    data <- prepared; data[[column]] <- NULL
    expect_error(run(data, concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV"))
  }
  path <- tempfile(fileext = ".csv"); core$pk_write_csv(prepared, path)
  expect_equal(values(run(pkapp_read_table(path), concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV")), c(.1, .1))
  legacy <- assembled$data[, !names(assembled$data) %in% c("NCA_DOSE_EVIDENCE_VERSION", "NCA_PREV_DOSE_H",
    "NCA_PRIOR_DOSE_COUNT", "NCA_LATER_DOSE_COUNT", "NCA_DOSE_REFERENCE_COLUMNS")]
  expect_equal(values(run(legacy, dosing_context = "repeated_final")), c(NA, .1))
})
