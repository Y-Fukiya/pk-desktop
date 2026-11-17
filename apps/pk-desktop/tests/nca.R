source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
nca_fixture <- data.frame(ID = rep(c("001", "002"), each = 4L), TIME = rep(c(0, 1, 2, 4), 2L),
  CONC = c(0, 8, 4, 2, 0, 16, 8, 4), UNIT = "ng/mL", stringsAsFactors = FALSE)
nca_spec <- list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT", auc_method = "linear")
nca_change <- function(...) utils::modifyList(nca_spec, list(...), keep.null = TRUE)

test_that("known linear values preserve input IDs and global options", {
  original <- nca_fixture
  options <- PKNCA::PKNCA.options()
  result <- pkapp_nca(nca_fixture, nca_spec)
  expect_equal(result$data$AVAL, c(8, 1, 16, 16, 1, 32), tolerance = 1e-12)
  expect_identical(result$data$ID, rep(c("001", "002"), each = 3L))
  expect_identical(nca_fixture, original)
  expect_identical(PKNCA::PKNCA.options(), options)
  csv <- tempfile(fileext = ".csv")
  core$pk_write_csv(nca_fixture, csv)
  expect_identical(pkapp_nca(pkapp_read_table(csv), nca_spec)$data, result$data)
  expect_equal(result$counts, c(input = 8, used = 8, excluded = 0, curves = 2, failed_curves = 0, imputed = 0, result = 6))
})

test_that("groups separate repeated subjects and time units convert to hours", {
  both <- rbind(transform(nca_fixture, ANALYTE = "parent"), transform(nca_fixture, ANALYTE = "metabolite", CONC = CONC * 3))
  expect_true(all(pkapp_nca(both, nca_spec)$data$PROFILE_STATUS == "excluded"))
  result <- pkapp_nca(both, nca_change(groups = "ANALYTE"))
  expect_equal(result$data$AVAL, c(8, 1, 16, 16, 1, 32, 24, 1, 48, 48, 1, 96))
  minutes <- transform(nca_fixture, TIME = TIME * 60)
  days <- transform(nca_fixture, TIME = TIME / 24)
  expect_identical(pkapp_nca(minutes, nca_change(time_unit = "min"))$data, pkapp_nca(nca_fixture, nca_spec)$data)
  expect_equal(pkapp_nca(days, nca_change(time_unit = "day"))$data, pkapp_nca(nca_fixture, nca_spec)$data)
  expect_error(pkapp_nca(transform(nca_fixture, UNIT = c("ng/mL", "mg/L")), nca_spec), "単位")
  expect_error(pkapp_nca(transform(nca_fixture, UNIT = NA), nca_spec), "単位")
})

test_that("explicit BLQ MDV and missing handling are counted and zeros retained", {
  d <- data.frame(ID = "001", TIME = c(0, 1, 2, 3, 4, 5, 6), CONC = c("0", "8", "<LLOQ", "4", NA, "bad", "2"),
    UNIT = "ng/mL", BLQ = c(0, 0, 1, 0, 0, 0, 0), MDV = c(0, 0, 0, 0, 0, 1, 0))
  s <- nca_change(blq_column = "BLQ", mdv_column = "MDV", blq = "exclude")
  excluded <- pkapp_nca(d, s)
  expect_identical(excluded$trace$rows$STATUS, c("used", "used", "BLQ", "used", "missing_concentration", "MDV", "used"))
  expect_equal(unname(excluded$counts[c("used", "excluded")]), c(4, 3))
  zero <- pkapp_nca(d, utils::modifyList(s, list(blq = "zero")))
  expect_equal(zero$trace$rows$CONC_USED[3], 0)
  expect_equal(zero$data$AVAL[3], 19)
  expect_true(all(pkapp_nca(d, utils::modifyList(s, list(blq = "as_recorded")))$data$PROFILE_STATUS == "excluded"))
  expect_error(pkapp_nca(transform(d, BLQ = "maybe"), s), "flag")
  expect_error(pkapp_nca(transform(d, MDV = NA), s), "flag")
})

test_that("invalid numeric values identities and duplicate times are refused", {
  for (bad in list(transform(nca_fixture, CONC = -1), transform(nca_fixture, CONC = Inf), transform(nca_fixture, TIME = Inf))) {
    rejected <- pkapp_nca(bad, nca_spec)
    expect_true(all(rejected$data$PROFILE_STATUS == "excluded"))
    expect_true(all(grepl("Invalid numeric", rejected$data$NCA_EXCLUDE)))
  }
  expect_true(all(pkapp_nca(transform(nca_fixture, TIME = 1e308), nca_change(time_unit = "day"))$data$PROFILE_STATUS == "excluded"))
  expect_error(pkapp_nca(transform(nca_fixture, ID = NA), nca_spec), "ID")
  duplicate <- pkapp_nca(rbind(nca_fixture, nca_fixture[1, ]), nca_spec)$data
  expect_true(all(duplicate$PROFILE_STATUS[duplicate$ID == "001"] == "excluded"))
  expect_equal(duplicate$AVAL[duplicate$ID == "002"], c(16, 1, 32))
  single <- pkapp_nca(nca_fixture[1, ], nca_spec)$data
  expect_equal(single$AVAL, c(0, NA_real_, NA_real_))
  expect_match(single$NCA_EXCLUDE[single$PARAMCD == "AUCLST"], "Fewer than two")
  expect_error(pkapp_nca(nca_fixture, nca_change(start = 4, end = 1)), "解析終了")
  expect_error(pkapp_nca(nca_fixture, nca_change(groups = "TIME")), "グループ")
  overflow <- pkapp_nca(data.frame(ID = "001", TIME = 0:2, CONC = 1e308, UNIT = "ng/mL"), nca_spec)
  expect_true(is.na(overflow$data$AVAL[3]))
  expect_match(overflow$data$NCA_EXCLUDE[3], "Numeric overflow")
})

test_that("observed bounds and missing origin do not masquerade as interval AUC", {
  d <- nca_fixture[1:4, ]
  d$CONC[4] <- 0
  r <- pkapp_nca(d, nca_spec)
  expect_equal(r$data$AVAL[3], 10)
  expect_equal(r$data$LAST_OBS_H, rep(4, 3))
  expect_equal(r$data$TLAST_QUANT_H, rep(2, 3))
  missing_origin <- pkapp_nca(nca_fixture[2:4, ], nca_spec)
  expect_true(is.na(missing_origin$data$AVAL[3]))
  expect_match(missing_origin$data$NCA_EXCLUDE[3], "Missing concentration")
  expect_true(any(grepl("計算できない", missing_origin$warnings)))
  zero <- pkapp_nca(transform(d, CONC = 0), nca_spec)
  expect_equal(zero$data$AVAL, c(0, NA_real_, NA_real_))
  expect_true(all(is.na(zero$data$TLAST_QUANT_H)))
  expect_true(any(grepl("計算できない", zero$warnings)))
})

test_that("steady state selection and earliest Tmax are explicit", {
  d <- nca_fixture
  d$SSNCAFL <- "Y"
  extra <- transform(d[1, ], TIME = 100, CONC = 500, SSNCAFL = "N")
  r <- pkapp_nca(rbind(d, extra), nca_change(scope = "ss"))
  expect_equal(r$data$AVAL, c(8, 1, 16, 16, 1, 32))
  expect_identical(tail(r$trace$rows$STATUS, 1), "scope")
  d <- data.frame(ID = "001", TIME = 0:3, CONC = c(0, 8, 8, 2), UNIT = "ng/mL")
  expect_equal(pkapp_nca(d, nca_spec)$data$AVAL[2], 1)
})

test_that("NCA table persistence replays pinned settings and verifies artifacts", {
  project <- tempfile("nca-store-")
  dir.create(project)
  engine <- core$desktop_nca_engine(project)
  saved <- core$desktop_table_save(engine, "NCA", list(source = nca_fixture), list(kind = "nca", specification = nca_spec))
  folder <- core$desktop_table_path(engine, saved$id)
  expect_true(all(file.exists(file.path(folder, c("trace.json", "row_handling.csv", "result.csv", "recipe.json")))))
  recipe <- core$desktop_read(file.path(folder, "recipe.json"))
  expect_identical(recipe$specification$pknca_version, as.character(packageVersion("PKNCA")))
  expect_identical(pkapp_replay(folder)$data, core$desktop_table_get(engine, saved$id))
  expect_length(core$desktop_tables(core$desktop_nca_engine(project)), 1)
  expect_error(pkapp_nca(nca_fixture, nca_change(pknca_version = "other")), "PKNCAの版")
  writeLines("tampered", file.path(folder, "result.csv"))
  expect_error(pkapp_replay(folder), "checksum")
})
