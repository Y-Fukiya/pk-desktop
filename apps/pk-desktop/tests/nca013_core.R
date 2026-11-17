source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
raw013 <- function() data.frame(ID = "001", TIME = c(-.75, -.25, 0, 1, 2),
  CONC = c("0.1", "BLQ", "0", "8", "4"), BLQ = c(0, 1, 0, 0, 0), MDV = 0,
  LLOQ = c(.3, NA, .3, .3, .3), LIMIT = c(.8, .2, .8, .8, .8), UNIT = "ng/mL")
step013 <- list(op = "blq_rules", mode = "positional", concentration = "CONC", blq_column = "BLQ", mdv_column = "MDV", time = "TIME",
  profile_columns = "ID", zero_time = "post", pre_rule = "exclude", first_rule = "zero", middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude",
  lloq_source = "column", lloq_column = "LLOQ")
prepared013 <- function(data = raw013(), step = step013) pkapp_prepare(data, list(step))$data
spec013 <- list(id = "ID", time = "TIME", concentration = "NCA_CONC", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV", blq = "as_recorded",
  concentration_unit = "ng/mL", auc_method = "linear", predose_qc_threshold_pct = 5, predose_qc_window_h = 1)
qc013 <- function(result, group = 1L) result$trace$groups[[group]]$predose_qc
run013 <- function(data = prepared013(), ...) pkapp_nca(data, modifyList(spec013, list(...), keep.null = TRUE))

for (source in c("column", "constant")) local({
  source <- source
  test_that(paste("prepared missing LLOQ accepts explicit QC", source, "without changing input or PK"), {
    d <- prepared013(); before <- serialize(d, NULL); baseline <- run013(d)
    spec <- if (source == "column") list(predose_qc_lloq_source = source, predose_qc_lloq_column = "LIMIT") else
      list(predose_qc_lloq_source = source, predose_qc_lloq_value = .2)
    r <- pkapp_nca(d, modifyList(spec013, spec)); q <- qc013(r)
    expect_identical(qc013(baseline)$reason, "missing_lloq")
    expect_identical(q$status, "pass_blq"); expect_identical(q$source_row, 2L)
    expect_equal(q$lloq, .2); expect_equal(q$pct_upper_bound, 2.5)
    expect_identical(q$lloq_source, paste0("qc_supplement:", source, if (source == "column") ":LIMIT"))
    expect_true(is.na(q$concentration) && is.na(q$pct))
    expect_identical(r$data$AVAL, baseline$data$AVAL)
    expect_identical(r$trace$rows, baseline$trace$rows)
    expect_identical(serialize(d, NULL), before)
    expect_identical(r$specification[names(spec)], spec)
  })
})

test_that("mixed profile origins use the selected row's recipe or supplemental bound", {
  a <- raw013(); b <- a; b$ID <- "002"; b$LLOQ[2] <- .3
  d <- prepared013(rbind(a, b)); r <- run013(d, predose_qc_lloq_source = "column", predose_qc_lloq_column = "LIMIT")
  expect_identical(qc013(r)$lloq_source, "qc_supplement:column:LIMIT")
  expect_identical(qc013(r, 2)$lloq_source, "column:LLOQ")
  expect_equal(c(qc013(r)$lloq, qc013(r, 2)$lloq), c(.2, .3))
  expect_equal(c(qc013(r)$source_row, qc013(r, 2)$source_row), c(2, 7))
  reversed <- run013(d[10:1, ], predose_qc_lloq_source = "column", predose_qc_lloq_column = "LIMIT")
  expect_identical(qc013(reversed)$lloq_source, "column:LLOQ")
  expect_identical(qc013(reversed, 2)$lloq_source, "qc_supplement:column:LIMIT")
})

test_that("valid recipe evidence takes precedence over an explicit supplement", {
  d <- raw013(); d$LLOQ[2] <- .4; d <- prepared013(d)
  before <- run013(d)
  after <- run013(d, predose_qc_lloq_source = "constant", predose_qc_lloq_value = .001)
  expect_identical(qc013(after), qc013(before)); expect_identical(after$data, before$data)
})

test_that("invalid recipe evidence stays invalid even with a valid supplement", {
  for (bad in c("bad", "0", "-1", "Inf")) {
    d <- raw013(); d$LLOQ[2] <- bad; d <- prepared013(d)
    before <- run013(d); after <- run013(d, predose_qc_lloq_source = "constant", predose_qc_lloq_value = .2)
    expect_identical(qc013(after)$reason, "invalid_lloq")
    expect_identical(qc013(after), qc013(before)); expect_identical(after$data, before$data)
  }
})

test_that("legacy unverified recipe evidence stays unverified", {
  d <- pkapp_read_table("apps/pk-desktop/release/tests/fixtures/prepared_v1_constant.csv")
  s <- modifyList(spec013, list(mdv_column = "NCA_MDV"))
  baseline <- pkapp_nca(d, s)
  r <- pkapp_nca(d, modifyList(s, list(predose_qc_lloq_source = "constant", predose_qc_lloq_value = .2)))
  expect_identical(qc013(r)$reason, "unverified_lloq")
  expect_identical(qc013(r), qc013(baseline)); expect_identical(r$data, baseline$data)
})

test_that("invalid and absent supplemental values remain unavailable", {
  for (value in list(NULL, NA_real_, "", "bad", 0, -1, Inf)) {
    r <- run013(predose_qc_lloq_source = "constant", predose_qc_lloq_value = value)
    expect_identical(qc013(r)$status, "not_evaluable")
    expect_true(qc013(r)$reason %in% c("missing_lloq", "invalid_lloq"))
    expect_true(is.na(qc013(r)$lloq)); expect_equal(r$data$AVAL, c(8, 1, 10))
  }
})

test_that("supplementation cannot bypass signed provenance validation", {
  base <- prepared013()
  cases <- list(changed = base, dropped = base[-1, ], duplicated = rbind(base, base[1, ]))
  cases$changed$CONC[4] <- "999"
  for (d in cases) expect_error(run013(d, predose_qc_lloq_source = "constant", predose_qc_lloq_value = .2), "前処理|行|変更")
  expect_error(run013(base, predose_qc_lloq_source = "column", predose_qc_lloq_column = "ABSENT"), "列")
})

test_that("latest original non-MDV row remains the only QC candidate", {
  d <- prepared013(); expect_equal(d$NCA_MDV[2], 1)
  r <- run013(d, predose_qc_lloq_source = "column", predose_qc_lloq_column = "LIMIT")
  expect_identical(qc013(r)$source_row, 2L)
  expect_identical(qc013(r)$status, "pass_blq")
  raw <- raw013(); raw$MDV[2] <- 1
  r <- run013(prepared013(raw), predose_qc_lloq_source = "constant", predose_qc_lloq_value = .2)
  expect_identical(qc013(r)$source_row, 1L); expect_identical(qc013(r)$status, "pass")
  expect_identical(qc013(r)$lloq_source, "")
})

test_that("supplemented bounds retain selected-row unit and lookback checks", {
  for (unit in c("ug/mL", "", NA_character_)) {
    raw <- raw013(); raw$UNIT[2] <- unit
    r <- run013(prepared013(raw), unit_column = "UNIT", predose_qc_lloq_source = "constant", predose_qc_lloq_value = .2)
    expect_identical(qc013(r)$reason, if (is.na(unit) || unit == "") "missing_unit" else "unit_mismatch")
    expect_true(is.na(qc013(r)$pct_upper_bound)); expect_equal(r$data$AVAL, c(8, 1, 10))
  }
  expect_identical(qc013(run013(predose_qc_window_h = .1, predose_qc_lloq_source = "constant", predose_qc_lloq_value = .2))$reason, "no_candidate")
})

test_that("saved supplement specification replays exactly after CSV interchange", {
  d <- prepared013(); path <- tempfile(fileext = ".csv"); core$pk_write_csv(d, path)
  imported <- pkapp_read_table(path)
  r <- run013(imported, predose_qc_lloq_source = "column", predose_qc_lloq_column = "LIMIT")
  folder <- tempfile(); dir.create(folder); engine <- core$desktop_nca_engine(folder)
  saved <- core$desktop_table_save(engine, "supplement", list(source = imported), list(kind = "nca", specification = r$specification))
  target <- core$desktop_table_path(engine, saved$id)
  replay <- pkapp_replay(target)
  expect_identical(replay$data, r$data); expect_identical(replay$trace, r$trace)
  recipe <- core$desktop_read(file.path(target, "recipe.json"))
  expect_identical(recipe$specification$predose_qc_lloq_source, "column")
  expect_identical(recipe$specification$predose_qc_lloq_column, "LIMIT")
  expect_identical(readRDS(file.path(target, "inputs.rds"))$source, imported)
})
