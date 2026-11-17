source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
sys.source("apps/pk-desktop/R/pk_assembly.R", envir = core)
assemble <- core$pkapp_assemble_nca

fixture <- function() {
  pc <- data.frame(SUBJECT = "001", SPECIMEN = paste0("S", 1:6),
    OCCASION = rep(c("P1", "P2"), each = 3), ACTUAL = c(
      "2026-11-02T08:45:00", "2026-11-02T10:00:00", "2026-11-02T11:00:00",
      "2026-11-09T08:45:00", "2026-11-09T10:00:00", "2026-11-09T11:00:00"))
  conc <- data.frame(PATIENT = "001", SAMPLE = rep(pc$SPECIMEN, each = 2),
    TEST = rep(c("PARENT", "METAB"), 6), VALUE = c(.1, .05, 10, 5, 5, 2.5, .2, .1, 8, 4, 4, 2),
    UNITS = "ng/mL", BELOW = FALSE, LIMIT = .01)
  ex <- data.frame(PERSON = "001", OCC = rep(c("P1", "P2"), each = 2), EVENT = c("D1", "D2", "D3", "D4"),
    ACTUAL = c("2026-11-01T09:00:00", "2026-11-02T09:00:00", "2026-11-08T09:00:00", "2026-11-09T09:00:00"))
  spec <- list(version = 1L, mapping = list(
    CONC = list(ID = "PATIENT", SAMPLE_ID = "SAMPLE", ANALYTE = "TEST", CONC = "VALUE", UNIT = "UNITS", BLQ = "BELOW", LLOQ = "LIMIT"),
    PC = list(ID = "SUBJECT", SAMPLE_ID = "SPECIMEN", PERIOD = "OCCASION", SAMPLE_DTM = "ACTUAL"),
    EX = list(ID = "PERSON", PERIOD = "OCC", DOSE_ID = "EVENT", DOSE_DTM = "ACTUAL")),
    sample_keys = list("ID", "SAMPLE_ID"), observation_keys = list("ID", "SAMPLE_ID", "ANALYTE"),
    profile_keys = list("ID", "PERIOD", "ANALYTE"), dose_group_keys = list("ID", "PERIOD"),
    dose_record_keys = list("ID", "DOSE_ID"), dose_reference = list(mode = "last"),
    datetime = list(sample_type = "iso", dose_type = "iso", utc_offset = "+09:00"),
    unmatched = "missing", missing_datetime = "missing", dose_history_confirmed = TRUE)
  list(conc = conc, pc = pc, ex = ex, specification = spec)
}

test_that("concentration rows retain two analytes, actual clocks and period-specific references", {
  f <- fixture(); got <- do.call(assemble, f)
  expect_equal(nrow(got$data), nrow(f$conc))
  expect_equal(got$data$VALUE, f$conc$VALUE)
  expect_identical(got$data$ID, rep("001", 12))
  expect_equal(got$data$NCA_PC_ROW, rep(1:6, each = 2))
  expect_equal(got$data$NCA_EX_ROW, rep(c(2L, 4L), each = 6))
  expect_equal(got$data$NCA_TIME_H, rep(rep(c(-.25, 1, 2), each = 2), 2))
  expect_true(all(got$data$NCA_DOSE_COUNT == 4L))
  expect_true(all(got$data$NCA_DOSE_HISTORY_CONFIRMED))
  expect_equal(got$data$NCA_NEXT_DOSE_H[1:6], rep(144, 6))
  expect_true(all(is.na(got$data$NCA_NEXT_DOSE_H[7:12])))
  expect_true(all(got$data$NCA_ASSEMBLY_STATUS == "ok"))
  expect_length(unique(got$data$NCA_ASSEMBLY_PROFILE), 4L)
  expect_identical(jsonlite::fromJSON(got$data$NCA_ASSEMBLY_PROFILE_COLUMNS[1]), c("ID", "PERIOD", "ANALYTE"))
  expect_identical(do.call(assemble, c(f[1:3], list(specification = got$specification)))$data, got$data)
})

test_that("first reference and explicit selections do not follow individual sampling times", {
  f <- fixture(); f$specification$dose_reference <- list(mode = "first")
  first <- do.call(assemble, f)$data
  expect_equal(first$NCA_TIME_H[1], 23.75)
  expect_true(all(first$NCA_NEXT_DOSE_H == 24))
  f$specification$dose_reference <- list(mode = "selected", predicate = list(op = "%in%", args = list(list(column = "DOSE_ID"), list(value = list("D2", "D4")))))
  expect_equal(do.call(assemble, f)$data$NCA_TIME_H[1], -.25)
  assignments <- list()
  for (period in c("P1", "P2")) for (analyte in c("PARENT", "METAB")) assignments[[length(assignments) + 1L]] <-
    list(profile = list(ID = "001", PERIOD = period, ANALYTE = analyte), dose = list(ID = "001", DOSE_ID = if (period == "P1") "D2" else "D4"))
  f$specification$dose_reference <- list(mode = "per_profile", assignments = assignments)
  expect_equal(do.call(assemble, f)$data$NCA_TIME_H[1], -.25)
  f$specification$dose_reference$assignments[[1]]$dose$DOSE_ID <- "D4"
  expect_true(any(is.na(do.call(assemble, f)$data$NCA_TIME_H)))
})

test_that("partial actual clocks and nominal-only data never produce elapsed times", {
  f <- fixture(); f$pc$ACTUAL[1] <- "2026-11-02"; f$pc$NOMINAL <- c(0, 1, 2, 0, 1, 2)
  got <- do.call(assemble, f)
  expect_true(all(is.na(got$data$NCA_TIME_H[1:2])))
  expect_true("missing_sample_datetime" %in% got$issues$code)
  f$specification$missing_datetime <- "error"
  expect_error(do.call(assemble, f), "actual datetimes")
  f <- fixture(); f$pc$ACTUAL <- NULL
  expect_error(do.call(assemble, f), "mapping")
  f <- fixture(); f$ex$ACTUAL[1] <- NA_character_
  got <- do.call(assemble, f)
  expect_true(all(is.na(got$data$NCA_TIME_H[1:6])))
  expect_false(any(got$data$NCA_DOSE_HISTORY_CONFIRMED[1:6]))
})

test_that("duplicate and unmatched samples remain explicit without multiplying rows", {
  f <- fixture(); f$pc <- rbind(f$pc, f$pc[1, ])
  expect_error(do.call(assemble, f), "Duplicate PC")
  f <- fixture(); f$conc <- rbind(f$conc, f$conc[1, ])
  expect_error(do.call(assemble, f), "Duplicate CONC")
  f <- fixture(); f$ex <- rbind(f$ex, f$ex[1, ])
  expect_error(do.call(assemble, f), "Duplicate EX")
  f <- fixture(); f$conc$SAMPLE[1] <- "ORPHAN"
  got <- do.call(assemble, f)
  expect_equal(nrow(got$data), 12)
  expect_true(is.na(got$data$NCA_TIME_H[1]))
  expect_identical(got$data$NCA_ASSEMBLY_STATUS[1], "unresolved")
  expect_equal(got$counts[["unmatched_conc"]], 1)
  f$specification$unmatched <- "error"
  expect_error(do.call(assemble, f), "Unmatched")
})

test_that("ambiguous reference and unknown history cannot claim a terminal-safe dose", {
  f <- fixture(); f$ex$ACTUAL[1] <- f$ex$ACTUAL[2]
  got <- do.call(assemble, f)
  expect_true(all(is.na(got$data$NCA_EX_ROW[1:6])))
  expect_true("ambiguous_reference_dose" %in% got$issues$code)
  f <- fixture(); f$specification$dose_history_confirmed <- FALSE
  expect_false(any(do.call(assemble, f)$data$NCA_DOSE_HISTORY_CONFIRMED))
  f$specification$dose_reference <- list(mode = "selected", predicate = list(value = TRUE))
  expect_true(all(is.na(do.call(assemble, f)$data$NCA_EX_ROW)))
})

test_that("key tuples resist delimiter collisions and identifiers keep leading zeros", {
  a <- data.frame(ID = c("a", "ab", "a:1", "a"), SAMPLE = c("bc", "c", "x", ":1x"))
  expect_false(anyDuplicated(core$desktop_assembly_tuple(a, c("ID", "SAMPLE"))) > 0)
  f <- fixture(); f$conc$PATIENT <- 1
  expect_error(do.call(assemble, f), "must be character")
  f <- fixture(); f$conc$SAMPLE <- 1:12
  expect_error(do.call(assemble, f), "key types differ")
})

test_that("SAS clocks use one epoch conversion and declared local offset", {
  f <- fixture()
  seconds <- function(x) as.numeric(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")) - as.numeric(as.Date("1960-01-01")) * 86400
  f$pc$ACTUAL <- seconds(f$pc$ACTUAL); f$ex$ACTUAL <- seconds(f$ex$ACTUAL)
  f$specification$datetime$sample_type <- "sas_seconds"
  f$specification$datetime$dose_type <- "sas_seconds"
  got <- do.call(assemble, f)
  expect_equal(got$data$NCA_TIME_H[1], -.25)
  expect_true(all(got$trace$datetime$sample$offset_assumed))
  f$pc$ACTUAL <- as.POSIXct(f$pc$ACTUAL + as.numeric(as.Date("1960-01-01")) * 86400, origin = "1970-01-01", tz = "UTC")
  attr(f$pc$ACTUAL, "format.sas") <- "DATETIME20."
  expect_equal(do.call(assemble, f)$data$NCA_TIME_H[1], -.25)
})

test_that("canonical output cannot overwrite conflicting raw concentration columns", {
  f <- fixture(); f$conc$CONC <- "raw audit text"; f$conc$NCA_TIME_H <- "raw scheduled time"
  got <- do.call(assemble, f)$data
  expect_equal(got$CONC, f$conc$VALUE)
  expect_identical(got$CONC_CONC, f$conc$CONC)
  expect_identical(got$CONC_NCA_TIME_H, f$conc$NCA_TIME_H)
  expect_equal(got$PC_SPECIMEN, f$pc$SPECIMEN[got$NCA_PC_ROW])
  expect_equal(got$EX_EVENT, f$ex$EVENT[got$NCA_EX_ROW])
})

test_that("mapped study identity cannot be dropped from matching and profile keys", {
  f <- fixture()
  for (source in c("conc", "pc", "ex")) f[[source]]$STUDY <- "STUDY-A"
  for (source in c("CONC", "PC", "EX")) f$specification$mapping[[source]]$STUDYID <- "STUDY"
  for (name in c("sample_keys", "profile_keys", "dose_group_keys", "dose_record_keys", "observation_keys")) {
    expect_error(do.call(assemble, f), "must include mapped STUDYID")
    f$specification[[name]] <- c(f$specification[[name]], "STUDYID")
  }
  expect_true(all(do.call(assemble, f)$data$NCA_ASSEMBLY_STATUS == "ok"))
  f$ex$STUDY <- "STUDY-B"
  expect_true(all(is.na(do.call(assemble, f)$data$NCA_EX_ROW)))
})

test_that("nonfinite elapsed arithmetic remains unresolved", {
  f <- fixture(); f$pc$ACTUAL <- rep(1e308, nrow(f$pc)); f$ex$ACTUAL <- rep(-1e308, nrow(f$ex))
  f$specification$datetime$sample_type <- "unix_seconds"
  f$specification$datetime$dose_type <- "unix_seconds"
  f$specification$dose_reference <- list(mode = "selected", predicate = list(op = "%in%", args = list(list(column = "DOSE_ID"), list(value = list("D2", "D4")))))
  got <- do.call(assemble, f)
  expect_true(all(is.na(got$data$NCA_TIME_H)))
  expect_true(all(got$data$NCA_ASSEMBLY_STATUS == "unresolved"))
})

test_that("another period cannot hide a subsequent administration or unknown dose clock", {
  f <- fixture()
  f$ex$ACTUAL[3] <- "2026-11-02T17:00:00"
  f$pc$ACTUAL[3] <- "2026-11-02T21:00:00"
  got <- do.call(assemble, f)
  expect_equal(got$data$NCA_TIME_H[5], 12)
  expect_equal(got$data$NCA_NEXT_DOSE_H[5], 8)
  expect_equal(got$data$NCA_DOSE_COUNT[5], 4L)
  expect_identical(got$data$NCA_EX_ROW[5], 2L)
  f$ex$ACTUAL[3] <- NA_character_
  got <- do.call(assemble, f)
  expect_equal(got$data$NCA_TIME_H[5], 12)
  expect_false(got$data$NCA_DOSE_HISTORY_CONFIRMED[5])
  expect_match(got$data$NCA_ASSEMBLY_REASON[5], "unknown_history_dose_datetime")
})

test_that("explicit reference cannot certify tied distinct administrations", {
  f <- fixture()
  f$ex$ACTUAL[1] <- f$ex$ACTUAL[2]
  f$specification$dose_reference <- list(mode = "selected", predicate = list(op = "%in%", args = list(list(column = "DOSE_ID"), list(value = list("D2", "D4")))))
  got <- do.call(assemble, f)
  expect_equal(got$data$NCA_TIME_H[1], -.25)
  expect_identical(got$data$NCA_EX_ROW[1], 2L)
  expect_false(any(got$data$NCA_DOSE_HISTORY_CONFIRMED))
  expect_true("tied_dose_history" %in% got$issues$code)
  f$ex$ACTUAL[1] <- "2026-11-09T09:00:00"
  got <- do.call(assemble, f)
  expect_false(any(got$data$NCA_DOSE_HISTORY_CONFIRMED))
})
