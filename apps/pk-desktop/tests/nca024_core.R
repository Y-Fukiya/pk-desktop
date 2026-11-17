source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
fixture024 <- function() {
  times <- c(0, 1, 2, 4, 8, 12, 24, 48)
  data <- data.frame(ID = "001", TIME = times, CONC = 10 * exp(-.1 * times), DOSE = 100, DOSEU = "mg", ROUTE = "ORAL", BLQ = FALSE)
  vs <- data.frame(ID = "001", TEST = c("WEIGHT", "HEIGHT", "BSA"), VALUE = c(50, 200, 2), UNIT = c("kg", "cm", "m2"), DTC = "2026-11-01T08:00:00Z", VISIT = "BASELINE")
  prep <- list(version = 1, profile = list(id = "ID", groups = list()), vs = list(keys = list(ID = "ID"), scope = "subject",
    test_column = "TEST", value_column = "VALUE", unit_column = "UNIT", datetime_column = "DTC", visit_column = "VISIT",
    selection = list(mode = "all_unique"), baseline = list(mode = "subject_confirmed", confirmed = TRUE),
    measurements = list(list(name = "WT", test = "WEIGHT", role = "weight"), list(name = "HT", test = "HEIGHT", role = "height"), list(name = "BSA", test = "BSA", role = "bsa"))))
  base <- list(id = "ID", time = "TIME", concentration = "CONC", concentration_unit = "mg/L", blq_column = "BLQ", c0_method = "none",
    terminal_method = "auto", dosing_context = "single", dose_history_confirmed = TRUE)
  dose <- list(amount_column = "DOSE", unit_column = "DOSEU", route_column = "ROUTE", meaning = "actual_administered", reference = "declared_profile", basis = "absolute", extravascular_values = list("ORAL"))
  body <- list(weight = list(value_column = "WT", unit_column = "WT_UNIT"), bsa = list(mode = "recorded", value_column = "BSA", unit_column = "BSA_UNIT"))
  list(data = data, vs = vs, prep = prep, base = base, spec = c(base, list(dose = dose, body_size = body, derived_parameters = as.list(names(core$desktop_nca_derived_parameters())))))
}
prepare024 <- function(f) pkapp_prepare_nca_covariates(f$data, vs = f$vs, specification = f$prep)
run024 <- function(f = fixture024()) pkapp_nca(prepare024(f)$data, f$spec)
vals024 <- function(r, code) r$data$AVAL[match(code, r$data$PARAMCD)]
reason024 <- function(r, code) r$data$NCA_EXCLUDE[match(code, r$data$PARAMCD)]

test_that("analytic mass and dimensional conversions agree with independent equations", {
  f <- fixture024(); r <- run024(f); expected <- c(.1, 1-exp(-4.8), 1, 1, 10, .02, .2, .5, 5)
  expect_equal(vals024(r, unlist(f$spec$derived_parameters)), expected, tolerance = 1e-11)
  expect_identical(r$data$AVALU[r$data$PARAMCD == "CLFO"], "L/h")
  baseline <- pkapp_nca(f$data, f$base)
  expect_identical(r$data[seq_len(7), c("PARAMCD", "AVAL", "AVALU")], baseline$data[c("PARAMCD", "AVAL", "AVALU")])
  for (dose in list(c(100000, "ug"), c(.1, "g"), c(100000, "µg"))) {
    x <- f; x$data$DOSE <- as.numeric(dose[1]); x$data$DOSEU <- dose[2]
    expect_equal(vals024(run024(x), c("CLFO", "VZFO")), c(1, 10), tolerance = 1e-11)
  }
  f$data$CONC <- f$data$CONC * 1000; f$spec$concentration_unit <- "ng/mL"
  expect_equal(vals024(run024(f), c("CMAXD", "CLFO", "VZFO")), c(.1, 1, 10), tolerance = 1e-11)
  for (basis in c("per_kg", "per_m2")) {
    f <- fixture024(); f$spec$dose$basis <- basis
    f$data$DOSE <- if (basis == "per_kg") 2 else 50; f$data$DOSEU <- if (basis == "per_kg") "mg/kg" else "mg/m2"
    expect_equal(vals024(run024(f), c("CMAXD", "CLFO", "VZFO")), c(.1, 1, 10), tolerance = 1e-11)
    f$vs$VALUE[f$vs$TEST == if (basis == "per_kg") "WEIGHT" else "BSA"] <- NA_real_
    r <- run024(f); expect_true(all(is.na(vals024(r, unlist(f$spec$derived_parameters)))))
    expect_equal(vals024(r, "CMAX"), 10)
  }
})

test_that("selected body-size conversions, missingness and explicit Mosteller stay independent", {
  f <- fixture024(); f$vs$VALUE[1] <- NA_real_; r <- run024(f)
  expect_equal(vals024(r, c("CLFO", "VZFO", "CLFOBSA", "VZFOBSA")), c(1, 10, .5, 5), tolerance = 1e-11)
  expect_true(all(is.na(vals024(r, c("CLFOWT", "VZFOWT")))))
  f <- fixture024(); f$spec$body_size$bsa <- list(mode = "mosteller", height = list(value_column = "HT", unit_column = "HT_UNIT"))
  f$vs$VALUE[3] <- NA_real_; r <- run024(f)
  expect_equal(r$trace$derived$profiles[[1]]$bsa$value, 5/3, tolerance = 1e-12)
  expect_equal(vals024(r, c("CLFOBSA", "VZFOBSA")), c(.6, 6), tolerance = 1e-11)
  f$spec$body_size$bsa <- fixture024()$spec$body_size$bsa
  expect_true(is.na(vals024(run024(f), "CLFOBSA")))
  f <- fixture024(); f$vs$VALUE[1:2] <- c(50000, 2); f$vs$UNIT[1:2] <- c("g", "m")
  expect_equal(vals024(run024(f), c("CLFOWT", "VZFOWT")), c(.02, .2), tolerance = 1e-11)
  expect_equal(core$desktop_nca_unit("lb", "weight")$factor, .45359237)
  expect_equal(core$desktop_nca_unit("in", "height")$factor, 2.54)
})

test_that("dose, routes, policy zeros and terminal withholding fail per dependency", {
  for (kind in c("zero", "negative", "missing", "mixed", "unit", "basis")) {
    f <- fixture024()
    if (kind == "zero") f$data$DOSE <- 0
    if (kind == "negative") f$data$DOSE <- -1
    if (kind == "missing") f$data$DOSE <- NA_real_
    if (kind == "mixed") f$data$DOSE[1] <- 50
    if (kind == "unit") f$data$DOSEU <- "IU"
    if (kind == "basis") f$data$DOSEU <- "mg/kg"
    r <- run024(f); expect_equal(vals024(r, "CMAX"), 10)
    expect_true(all(is.na(vals024(r, unlist(f$spec$derived_parameters)))))
  }
  f <- fixture024(); f$data$ROUTE <- "IV"; r <- run024(f)
  expect_equal(vals024(r, "CMAXD"), .1)
  expect_true(all(is.na(vals024(r, c("CLFO", "VZFO")))))
  expect_equal(vals024(r, "AUCIFOD"), 1, tolerance = 1e-11)
  mixed <- fixture024(); mixed$data$ROUTE[2] <- "IV"
  expect_true(all(is.na(vals024(run024(mixed), unlist(mixed$spec$derived_parameters)))))
  f$spec$dose$extravascular_values <- list("IV")
  expect_error(run024(f), "静脈内")
  f <- fixture024(); f$data$BLQ <- TRUE; f$spec$all_blq_policy <- "zero"; r <- run024(f)
  expect_equal(vals024(r, "CMAX"), 0)
  expect_true(all(is.na(vals024(r, unlist(f$spec$derived_parameters)))))
  f <- fixture024(); f$spec$aucinf_max_extrap_pct <- .01; r <- run024(f)
  expect_equal(vals024(r, "CMAXD"), .1); expect_true(is.na(vals024(r, "CLFO")))
  f$spec$terminal_qc_action <- "flag"; r <- run024(f)
  expect_equal(vals024(r, "CLFO"), 1, tolerance = 1e-11)
  expect_match(r$data$NCA_WARNING[r$data$PARAMCD == "CLFO"], "extrapolation_above_threshold")
  for (scope in c("ongoing", "repeated_final", "isolated_single_period")) {
    f <- fixture024(); f$spec$dosing_context <- scope
    expect_true(all(is.na(vals024(run024(f), unlist(f$spec$derived_parameters)))))
  }
  f <- fixture024(); f$spec$dose_history_confirmed <- FALSE
  expect_true(all(is.na(vals024(run024(f), unlist(f$spec$derived_parameters)))))
})

test_that("pre-NCA matching preserves rows, requires baseline and validates protected context", {
  f <- fixture024(); p <- prepare024(f)
  expect_identical(p$data[names(f$data)], f$data)
  expect_equal(p$data$WT_SOURCE_ROW, rep(1, 8)); expect_equal(p$data$WT_DTC, rep(f$vs$DTC[1], 8))
  expect_equal(p$data$WT_VISIT, rep("BASELINE", 8))
  f$vs <- rbind(f$vs, f$vs[1, ]); expect_error(prepare024(f), "重複")
  f <- fixture024(); f$prep$vs$baseline$confirmed <- FALSE; expect_error(prepare024(f), "条件")
  f <- fixture024(); f$vs$ID <- "1"; expect_true(all(is.na(prepare024(f)$data$WT)))
  f <- fixture024(); f$vs$ID <- as.numeric(f$vs$ID); expect_error(prepare024(f), "文字型")
  changed <- p$data; changed$WT <- 70; expect_error(pkapp_nca(changed, fixture024()$spec), "変更")
  changed <- p$data; changed$NCA_COV_CONTEXT <- NULL; expect_error(pkapp_nca(changed, fixture024()$spec), "変更")
  changed <- p$data; changed$NCA_COV_CONTEXT <- NULL; changed$NCA_COV_SIGNATURE <- NULL
  expect_error(pkapp_nca(changed, fixture024()$spec), "採用")
  file <- tempfile(fileext = ".csv"); on.exit(unlink(file)); core$pk_write_csv(p$data, file)
  imported <- read.csv(file, colClasses = c(ID = "character"), check.names = FALSE)
  expect_equal(vals024(pkapp_nca(imported, fixture024()$spec), "CLFO"), 1, tolerance = 1e-11)
  f <- fixture024(); f$data$NCA_DOSE_EPOCH_S <- as.numeric(as.POSIXct("2026-11-01 07:00:00", tz = "UTC"))
  p <- prepare024(f); expect_true(all(p$data$WT_BASELINE_STATUS == "postdose"))
  r <- pkapp_nca(p$data, f$spec); expect_equal(vals024(r, "CLFO"), 1, tolerance = 1e-11); expect_true(is.na(vals024(r, "CLFOWT")))
})

test_that("period and study keys isolate selected covariates without dropping observations", {
  f <- fixture024(); f$data <- rbind(transform(f$data, STUDYID = "A", PERIOD = 1), transform(f$data, STUDYID = "A", PERIOD = 2), transform(f$data, STUDYID = "B", PERIOD = 1))
  f$vs <- rbind(transform(f$vs, STUDYID = "A", PERIOD = 1), transform(f$vs, STUDYID = "A", PERIOD = 2), transform(f$vs, STUDYID = "B", PERIOD = 1))
  f$vs$VALUE[f$vs$TEST == "WEIGHT"] <- c(50, 60, 70)
  f$prep$profile <- list(id = "ID", groups = list("STUDYID", "PERIOD"), study = "STUDYID", period = "PERIOD")
  f$prep$vs$keys <- list(ID = "ID", STUDYID = "STUDYID", PERIOD = "PERIOD"); f$prep$vs$scope <- "keys"; f$prep$vs$baseline$mode <- "period"
  p <- prepare024(f); expect_equal(p$data$WT, rep(c(50, 60, 70), each = 8))
  f$prep$vs$keys$STUDYID <- NULL; expect_error(prepare024(f), "STUDYID")
  f$prep$vs$keys$STUDYID <- "STUDYID"; f$prep$vs$keys$PERIOD <- NULL; expect_error(prepare024(f), "投与期")
})

test_that("positive endpoint normalization underflow is withheld", {
  f <- fixture024(); input <- prepare024(f)$data; base <- pkapp_nca(input, f$base)
  base$data$AVAL[base$data$PARAMCD == "CMAX"] <- 1e-300
  input$DOSE <- 1e100
  result <- core$desktop_nca_derived_apply(base, input, core$desktop_nca_specification(input, f$spec))
  expect_true(is.na(vals024(result, "CMAXD")))
  expect_equal(reason024(result, "CMAXD"), "numeric_range")
})

assembly024 <- function(collision = FALSE) {
  f <- fixture024(); times <- f$data$TIME
  pc <- rbind(data.frame(ID = "001", SAMPLE_ID = paste0("A", seq_along(times)), SAMPLE_DTM = format(as.POSIXct("2026-11-01 09:00:00", tz = "UTC") + times * 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    data.frame(ID = "002", SAMPLE_ID = paste0("B", seq_along(times)), SAMPLE_DTM = format(as.POSIXct("2026-11-01 09:00:00", tz = "UTC") + times * 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
  conc <- pc[c("ID", "SAMPLE_ID")]; conc$CONC <- rep(f$data$CONC, 2)
  if (collision) conc$EX_DOSE <- 777
  ex <- data.frame(ID = c("001", "002"), DOSE_ID = c("A", "B"), DOSE_DTM = "2026-11-01T09:00:00Z", DOSE = c(100, 200), DOSEU = "mg", ROUTE = "01")
  mapping <- function(x) as.list(stats::setNames(names(x), names(x)))
  spec <- list(version = 1, mapping = list(CONC = mapping(conc), PC = mapping(pc), EX = mapping(ex)), sample_keys = list("ID", "SAMPLE_ID"),
    profile_keys = list("ID"), dose_group_keys = list("ID"), dose_record_keys = list("ID", "DOSE_ID"), observation_keys = list("ID", "SAMPLE_ID"),
    dose_reference = list(mode = "first"), datetime = list(sample_type = "iso", dose_type = "iso", utc_offset = "+00:00"), unmatched = "error", missing_datetime = "error", dose_history_confirmed = TRUE)
  pkapp_assemble_nca(conc, pc, ex, spec)$data
}

test_that("reference EX binding resists source collisions, transplant and character changes", {
  f <- fixture024(); s <- f$spec; s$body_size <- NULL; s$blq_column <- ""; s$time <- "NCA_TIME_H"
  s$dose$reference <- "selected_dose"; s$dose$amount_column <- "EX_DOSE"; s$dose$unit_column <- "EX_DOSEU"; s$dose$route_column <- "EX_ROUTE"; s$dose$extravascular_values <- list("01")
  input <- assembly024(); r <- pkapp_nca(input, s)
  expect_equal(r$data$AVAL[r$data$PARAMCD == "CLFO"], c(1, 2), tolerance = 1e-11)
  bad <- input; bad$EX_ROUTE <- "1"; expect_error(pkapp_nca(bad, s), "基準EX")
  bad <- input; columns <- c(core$desktop_dose_input_columns(input), "NCA_EX_ROW", "NCA_DOSE_REF_ID", "NCA_DOSE_EPOCH_S", "NCA_DOSE_INPUT_COLUMNS", "NCA_DOSE_INPUT_SIGNATURE")
  bad[1:8, columns] <- input[9:16, columns]; expect_error(pkapp_nca(bad, s), "基準EX")
  collision <- assembly024(TRUE); expect_equal(collision$EX_DOSE, rep(777, 16))
  s$dose$amount_column <- "SOURCE_EX_DOSE"
  expect_equal(pkapp_nca(collision, s)$data$AVAL[pkapp_nca(collision, s)$data$PARAMCD == "CLFO"], c(1, 2), tolerance = 1e-11)
  s$dose$amount_column <- "EX_DOSE"; expect_error(pkapp_nca(collision, s), "基準EX")
  s$dose$reference <- "declared_profile"; expect_error(pkapp_nca(collision, s), "EX結合")
})

test_that("BLQ preparation and covariate preparation preserve each other's context and base PK", {
  f <- fixture024()
  step <- list(op = "blq_rules", mode = "positional", profile_columns = list("ID"), time = "TIME", concentration = "CONC", blq_column = "BLQ",
    zero_time = "post", pre_rule = "exclude", first_rule = "exclude", middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude")
  joined <- prepare024(f)$data
  prepared <- pkapp_prepare(joined, list(step))$data
  expect_identical(prepared[c("NCA_COV_CONTEXT", "NCA_COV_SIGNATURE")], joined[c("NCA_COV_CONTEXT", "NCA_COV_SIGNATURE")])
  spec <- utils::modifyList(f$spec, list(concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV", blq = "as_recorded"))
  result <- pkapp_nca(prepared, spec)
  expect_equal(vals024(result, c("CMAX", "AUCLST", "CLFO", "VZFO")), c(10, 100*(1-exp(-4.8)), 1, 10), tolerance = 1e-11)
  raw_prepared <- pkapp_prepare(f$data, list(step))$data
  f$data <- raw_prepared; after <- prepare024(f)$data
  expect_identical(after[c("NCA_BLQ_CONTEXT", "NCA_BLQ_PROFILE_SIGNATURE", "NCA_BLQ_BASELINE")], raw_prepared[c("NCA_BLQ_CONTEXT", "NCA_BLQ_PROFILE_SIGNATURE", "NCA_BLQ_BASELINE")])
  expect_identical(pkapp_nca(after, spec)$data, result$data)
})
