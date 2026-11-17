source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
if (!exists("desktop_nca_terminal_apply", core, inherits = FALSE)) sys.source("apps/pk-desktop/R/nca_terminal.R", envir = core)
terminal_data <- function() data.frame(ID = "S01", TIME = 0:6, CONC = 10 * exp(-.5 * (0:6)), UNIT = "mg/L")
terminal_spec <- function(...) utils::modifyList(list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT",
  terminal_method = "auto", dosing_context = "single", dose_history_confirmed = TRUE), list(...), keep.null = TRUE)
terminal_value <- function(x, parameter) x$data$AVAL[x$data$PARAMCD == parameter]
terminal_reason <- function(x, parameter = "LAMZ") x$data$NCA_EXCLUDE[x$data$PARAMCD == parameter]
terminal_run <- function(data = terminal_data(), ...) pkapp_nca(data, terminal_spec(...))

test_that("closed-form exponential produces original observed AUC plus observed tail", {
  original_options <- PKNCA::PKNCA.options()
  answer <- terminal_run()
  expect_equal(terminal_value(answer, "LAMZ"), .5, tolerance = 1e-12)
  expect_equal(terminal_value(answer, "LAMZHL"), log(2) / .5, tolerance = 1e-12)
  expect_equal(terminal_value(answer, "AUCLST"), 20 * (1 - exp(-3)), tolerance = 1e-12)
  expect_equal(terminal_value(answer, "AUCIFO"), 20, tolerance = 1e-12)
  expect_equal(terminal_value(answer, "AUCPEO"), 100 * exp(-3), tolerance = 1e-12)
  expect_identical(tail(answer$data$AVALU, 4), c("1/h", "h", "h*mg/L", "%"))
  expect_identical(PKNCA::PKNCA.options(), original_options)
  trace <- answer$trace$groups[[1]]$terminal
  expect_identical(trace$selected$source_rows, as.list(2:7))
  expect_length(trace$candidates, 4)
  expect_equal(trace$selected$diagnostics$intercept, log(10))
  expect_equal(trace$selected$diagnostics$slope, -.5)
  expect_equal(which(answer$trace$rows$USE_TERMINAL), 2:7)
})

test_that("manual validates original count, post-Cmax and infusion end", {
  answer <- terminal_run(terminal_method = "manual", terminal_start_h = 1, terminal_end_h = 2)
  expect_true(all(is.na(tail(answer$data$AVAL, 4))))
  expect_match(terminal_reason(answer), "fewer_than_3")
  answer <- terminal_run(terminal_method = "manual", terminal_start_h = 0, terminal_end_h = 6)
  expect_identical(answer$trace$groups[[1]]$terminal$selected$source_rows, as.list(2:7))
  answer <- terminal_run(infusion_end_h = 3, terminal_qc_action = "flag")
  expect_identical(answer$trace$groups[[1]]$terminal$selected$source_rows, as.list(5:7))
  expect_error(terminal_run(terminal_method = "manual"), "手動終末相")
  expect_error(terminal_run(terminal_method = "manual", terminal_start_h = 4, terminal_end_h = 3), "手動終末相")
})

test_that("flat rising and duplicate profiles cannot yield terminal estimates", {
  data <- terminal_data(); data$CONC <- c(10, rep(5, 6))
  answer <- terminal_run(data)
  expect_true(is.na(terminal_value(answer, "LAMZ")))
  expect_match(terminal_reason(answer), "no_log_concentration_variation")
  data$CONC <- c(10, 1:6)
  answer <- terminal_run(data)
  expect_true(is.na(terminal_value(answer, "LAMZ")))
  expect_match(terminal_reason(answer), "nonnegative_slope")
  data <- terminal_data(); data$TIME[4] <- data$TIME[3]
  answer <- terminal_run(data)
  expect_true(all(is.na(tail(answer$data$AVAL, 4))))
  expect_match(terminal_reason(answer), "invalid_analysis_profile|duplicate_terminal_time")
})

test_that("positive BLQ and imputed C0 cannot enter terminal evidence", {
  data <- terminal_data(); data$BLQ <- FALSE; data$BLQ[6] <- TRUE; data$CONC[6] <- 100
  answer <- terminal_run(data, blq_column = "BLQ", blq = "as_recorded", terminal_qc_action = "flag")
  expect_false(answer$trace$rows$USE_TERMINAL[6])
  expect_identical(answer$trace$rows$TERMINAL_REASON[6], "BLQ")
  expect_equal(terminal_value(answer, "LAMZ"), .5, tolerance = 1e-12)
  data <- terminal_data()[-1, ]
  answer <- terminal_run(data, c0_method = "zero", terminal_qc_action = "flag")
  expect_false(any(answer$trace$rows$USE_TERMINAL[answer$trace$rows$IMPUTED]))
  expect_equal(terminal_value(answer, "LAMZ"), .5, tolerance = 1e-12)
  data <- terminal_data(); data$CONC[4] <- 0
  answer <- terminal_run(data, terminal_qc_action = "flag")
  expect_false(answer$trace$rows$USE_TERMINAL[4])
  expect_equal(terminal_value(answer, "LAMZ"), .5, tolerance = 1e-12)
})

test_that("AUCIFO is separate from partial AUC and earlier manual endpoint", {
  answer <- terminal_run(start = 1, terminal_qc_action = "flag")
  expect_equal(terminal_value(answer, "LAMZ"), .5)
  expect_match(terminal_reason(answer, "AUCIFO"), "partial_auc_start")
  answer <- terminal_run(terminal_method = "manual", terminal_start_h = 1, terminal_end_h = 4, terminal_qc_action = "flag")
  expect_equal(terminal_value(answer, "LAMZ"), .5)
  expect_match(terminal_reason(answer, "AUCIFO"), "terminal_excludes_tlast")
  data <- terminal_data()[-1, ]
  answer <- terminal_run(data, terminal_qc_action = "flag")
  expect_equal(terminal_value(answer, "LAMZ"), .5)
  expect_match(terminal_reason(answer, "AUCIFO"), "missing_auclast")
})

test_that("quality boundaries pass equality and enforce configured action", {
  first <- terminal_run()
  dx <- first$trace$terminal$profiles[[1]]$selected$diagnostics
  pext <- terminal_value(first, "AUCPEO")
  answer <- terminal_run(terminal_min_r_squared = dx$r.squared, terminal_min_span_ratio = dx$span.ratio, aucinf_max_extrap_pct = pext)
  expect_true(all(is.finite(tail(answer$data$AVAL, 4))))
  answer <- terminal_run(aucinf_max_extrap_pct = pext - .01)
  expect_equal(terminal_value(answer, "LAMZ"), .5)
  expect_true(is.na(terminal_value(answer, "AUCIFO")))
  expect_match(terminal_reason(answer, "AUCIFO"), "extrapolation_above_threshold")
  expect_equal(answer$trace$terminal$profiles[[1]]$extrapolated_pct, pext)
  answer <- terminal_run(terminal_min_span_ratio = dx$span.ratio + .01)
  expect_true(all(is.na(tail(answer$data$AVAL, 4))))
  answer <- terminal_run(terminal_min_span_ratio = dx$span.ratio + .01, terminal_qc_action = "flag")
  expect_true(all(is.finite(tail(answer$data$AVAL, 4))))
  expect_match(answer$data$NCA_WARNING[answer$data$PARAMCD == "LAMZ"], "span_below_threshold")
  expect_identical(answer$trace$groups[[1]]$status, "review")
  expect_error(terminal_run(terminal_min_r_squared = 2), "R二乗")
})

test_that("dose evidence overrides attestations and distinguishes repeated final", {
  answer <- terminal_run(dosing_context = "ongoing")
  expect_match(terminal_reason(answer), "ongoing_repeated_dose")
  answer <- terminal_run(dose_history_confirmed = FALSE)
  expect_match(terminal_reason(answer), "dose_history_unconfirmed")
  answer <- terminal_run(dosing_context = "repeated_final")
  expect_equal(terminal_value(answer, "LAMZ"), .5)
  expect_match(terminal_reason(answer, "AUCIFO"), "aucinf_requires_single_dose")
  data <- terminal_data(); data$NCA_DOSE_HISTORY_CONFIRMED <- FALSE
  expect_match(terminal_reason(terminal_run(data)), "source_dose_history_unconfirmed")
  data$NCA_DOSE_HISTORY_CONFIRMED <- TRUE; data$NCA_DOSE_COUNT <- 2L
  expect_match(terminal_reason(terminal_run(data)), "single_dose_context_conflict")
  data$NCA_DOSE_COUNT <- 1L; data$NCA_NEXT_DOSE_H <- 6
  expect_match(terminal_reason(terminal_run(data)), "intervening_dose")
  data$NCA_NEXT_DOSE_H <- 7
  expect_match(terminal_reason(terminal_run(data, dosing_context = "repeated_final")), "final_dose_context_conflict")
  data$NCA_NEXT_DOSE_H <- NA_real_
  expect_match(terminal_reason(terminal_run(data)), "dose_evidence_requires_reassembly")
  data$NCA_DOSE_REF_ID <- "D1"
  expect_equal(terminal_value(terminal_run(data), "LAMZ"), .5)
})

test_that("multiple profile traces do not leak prior AUC estimates", {
  data <- rbind(terminal_data(), transform(terminal_data(), ID = "S02", CONC = rep(5, 7)))
  answer <- terminal_run(data)
  expect_equal(terminal_value(answer, "LAMZ"), c(.5, NA))
  expect_null(answer$trace$terminal$profiles[[2]]$observed_aucinf)
  expect_null(answer$trace$terminal$profiles[[2]]$extrapolated_pct)
})

test_that("legacy normalization results and saved replay remain unchanged when omitted", {
  data <- terminal_data()
  legacy_spec <- list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT")
  legacy <- pkapp_nca(data, legacy_spec)
  expect_false(any(names(legacy$specification) %in% names(core$desktop_nca_terminal_defaults())))
  expect_identical(core$desktop_nca_specification(data, legacy$specification), legacy$specification)
  off <- pkapp_nca(data, c(legacy_spec, list(terminal_method = "none")))
  expect_identical(off$data, legacy$data)
  expect_identical(off$trace$rows, legacy$trace$rows)
  expect_null(legacy$trace$terminal)
  on <- terminal_run()
  expect_identical(head(on$data, 3), legacy$data)
  project <- tempfile("terminal-replay-"); dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  saved <- core$desktop_table_save(engine, "legacy", list(source = data), list(kind = "nca", specification = legacy$specification))
  replay <- pkapp_replay(file.path(engine$tables_dir, saved$id))
  expect_identical(replay$data, legacy$data)
  expect_identical(replay$specification, legacy$specification)
  expect_identical(core$desktop_hash(replay$specification), core$desktop_hash(legacy$specification))
})

test_that("automatic trailing selection keeps evidence and rejects distorted early points", {
  data <- terminal_data(); data$CONC[2] <- 8.8
  answer <- terminal_run(data)
  expect_identical(answer$trace$terminal$profiles[[1]]$selected$source_rows, as.list(3:7))
  expect_equal(terminal_value(answer, "LAMZ"), .5, tolerance = 1e-12)
  expect_true(any(!vapply(answer$trace$terminal$profiles[[1]]$candidates, `[[`, logical(1), "selected")))
  shuffled <- data[c(7, 2, 1, 4, 6, 3, 5), ]
  expect_equal(tail(terminal_run(shuffled)$data$AVAL, 4), tail(answer$data$AVAL, 4))
})

test_that("all BLQ zero policy never assigns terminal parameters and names remain safe", {
  data <- terminal_data(); data$BLQ <- TRUE
  answer <- terminal_run(data, blq_column = "BLQ", blq = "zero", all_blq_policy = "zero")
  expect_identical(terminal_value(answer, "CMAX"), 0)
  expect_true(all(is.na(tail(answer$data$AVAL, 4))))
  expect_true(all(tail(answer$data$PARAM_STATUS, 4) == "not_calculable"))
  expect_identical(answer$trace$groups[[1]]$status, "partial")
  summary <- core$desktop_nca_summary(answer)
  expect_identical(summary$PROFILE_STATUS, "partial")
  expect_match(summary$NCA_WARNING, "LAMZ")
  data <- terminal_data(); names(data)[1] <- "LAMZ"
  answer <- pkapp_nca(data, terminal_spec(id = "LAMZ"))
  expect_identical(answer$trace$output_key_mapping$LAMZ, "SOURCE_LAMZ")
  expect_equal(terminal_value(answer, "LAMZ"), .5)
  data <- terminal_data(); names(data)[1] <- "TERMINAL_REASON"
  answer <- pkapp_nca(data, terminal_spec(id = "TERMINAL_REASON"))
  expect_false(anyDuplicated(names(answer$trace$rows)) > 0)
})

test_that("legacy saved owner state reopens with the same canonical hash", {
  project <- tempfile("terminal-owner-"); dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  data <- terminal_data()
  table <- core$desktop_table_save(engine, "original", list(source = data), list(kind = "prepare", steps = list()))
  spec <- list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT", c0_method = "none")
  core$desktop_nca_configure(engine, list(source = table$id, specification = spec, base_revision = engine$nca$revision,
    project_id = engine$nca$project_id, actor = "test", reason = "legacy specification"), "r")
  hash <- core$desktop_hash(engine$nca$specification)
  restored <- core$desktop_nca_engine(project)
  expect_identical(core$desktop_hash(restored$nca$specification), hash)
  expect_false("terminal_method" %in% names(restored$nca$specification))
})

test_that("manual fit R-squared quality and invalid numeric data are explicit", {
  data <- terminal_data(); data$CONC[4] <- data$CONC[4] * 1.5
  answer <- terminal_run(data, terminal_method = "manual", terminal_start_h = 1, terminal_end_h = 6,
    terminal_min_r_squared = .999, terminal_min_span_ratio = 0, aucinf_max_extrap_pct = 100)
  expect_true(is.na(terminal_value(answer, "LAMZ")))
  expect_match(terminal_reason(answer), "r_squared_below_threshold")
  expect_true(is.finite(answer$trace$terminal$profiles[[1]]$selected$diagnostics$lambda.z))
  answer <- terminal_run(data, terminal_method = "manual", terminal_start_h = 1, terminal_end_h = 6,
    terminal_min_r_squared = .999, terminal_min_span_ratio = 0, aucinf_max_extrap_pct = 100, terminal_qc_action = "flag")
  expect_true(all(is.finite(tail(answer$data$AVAL, 4))))
  for (bad in list(Inf, -1, NaN, "broken")) {
    data <- terminal_data(); data$CONC[4] <- bad
    answer <- terminal_run(data)
    if (!is.nan(bad[1]) && !is.na(bad[1])) expect_true(all(is.na(tail(answer$data$AVAL, 4))))
  }
  data <- terminal_data(); data$MDV <- FALSE; data$MDV[4] <- TRUE
  answer <- terminal_run(data, mdv_column = "MDV")
  expect_false(answer$trace$rows$USE_TERMINAL[4])
  expect_equal(terminal_value(answer, "LAMZ"), .5)
})


test_that("CSV profile status is uniform while parameter failures remain specific", {
  for (context in c("ongoing", "repeated_final")) {
    answer <- terminal_run(dosing_context = context)
    expect_identical(unique(answer$data$PROFILE_STATUS), "partial")
    expect_identical(unique(answer$data$PROFILE_STATUS), answer$trace$groups[[1]]$status)
    expect_equal(terminal_value(answer, "CMAX"), 10)
    expect_identical(answer$data$PARAM_STATUS[answer$data$PARAMCD == "CMAX"], "calculated")
    expect_identical(answer$data$PARAM_STATUS[answer$data$PARAMCD == "AUCIFO"], "not_calculable")
  }
  flagged <- terminal_run(terminal_min_span_ratio = 10, terminal_qc_action = "flag")
  expect_identical(unique(flagged$data$PROFILE_STATUS), "review")
  withheld <- terminal_run(terminal_min_span_ratio = 10)
  expect_identical(unique(withheld$data$PROFILE_STATUS), "partial")
  duplicate <- terminal_data(); duplicate$TIME[4] <- duplicate$TIME[3]
  rejected <- terminal_run(duplicate)
  expect_identical(unique(rejected$data$PROFILE_STATUS), "excluded")
  multiple <- rbind(terminal_data(), transform(terminal_data(), ID = "S02", CONC = c(10, rep(5, 6))))
  answer <- terminal_run(multiple)
  expect_identical(unique(answer$data$PROFILE_STATUS[answer$data$ID == "S01"]), "ok")
  expect_identical(unique(answer$data$PROFILE_STATUS[answer$data$ID == "S02"]), "partial")
})
