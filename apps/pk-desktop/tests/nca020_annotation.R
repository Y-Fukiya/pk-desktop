source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
fixture020 <- function() {
  input <- expand.grid(TIME = c(0, 1, 2, 4, 8), ID = c("001", "002"), PERIOD = 1:2, stringsAsFactors = FALSE)
  input$STUDYID <- "ST01"; input$CONC <- rep(c(0, 8, 4, 2, 1), 4)
  result <- pkapp_nca(input, list(id = "ID", groups = c("STUDYID", "PERIOD"), time = "TIME", concentration = "CONC", concentration_unit = "ng/mL", c0_method = "none", auc_method = "linear"))
  dm <- data.frame(STUDYID = "ST01", USUBJID = c("001", "002"), SEX = c("F", "M"), AGE = c(35, 42))
  vs <- expand.grid(USUBJID = c("001", "002"), VSTESTCD = c("HEIGHT", "WEIGHT", "BMI", "BSA"), VISIT = c("BASELINE", "FOLLOWUP"), stringsAsFactors = FALSE)
  vs$STUDYID <- "ST01"; vs$VSSTRESN <- rep(c(160, 180, 50, 80, 19.5, 24.7, 1.5, 2), 2)
  vs$VSSTRESU <- rep(rep(c("cm", "kg", "kg/m2", "m2"), each = 2), 2)
  vs$VSDTC <- ifelse(vs$VISIT == "BASELINE", "2026-11-01T09:00", "2026-11-08T09:00")
  spec <- list(version = 1, dm = list(keys = list(ID = "USUBJID", STUDYID = "STUDYID"), columns = list(DM_SEX = "SEX", DM_AGE = "AGE")),
    vs = list(keys = list(ID = "USUBJID", STUDYID = "STUDYID"), scope = "subject", test_column = "VSTESTCD", value_column = "VSSTRESN", unit_column = "VSSTRESU", datetime_column = "VSDTC", visit_column = "VISIT",
      selection = list(mode = "filter", column = "VISIT", values = list("BASELINE")), measurements = list(list(name = "VS_HEIGHT", test = "HEIGHT"), list(name = "VS_WEIGHT", test = "WEIGHT"), list(name = "VS_BMI", test = "BMI"), list(name = "VS_BSA", test = "BSA"))))
  list(input = input, result = result, dm = dm, vs = vs, spec = spec)
}

test_that("optional DM/VS append recorded values without changing PK rows or statuses", {
  f <- fixture020(); x <- pkapp_annotate_nca(f$result, f$dm, f$vs, f$spec)
  expect_identical(x$data[names(f$result$data)], f$result$data)
  expect_equal(x$counts[["profiles"]], 4)
  expect_equal(x$counts[["issues"]], 0)
  expect_equal(x$data$VS_BMI[x$data$ID == "001"], rep(19.5, 6))
  expect_true(all(x$data$VS_BMI_VISIT == "BASELINE"))
  expect_true(all(x$data$VS_BMI_DTC == "2026-11-01T09:00"))
  expect_true(all(x$data$DM_SEX[x$data$ID == "001"] == "F"))
  d <- pkapp_annotate_nca(f$result, f$dm, specification = f$spec[c("version", "dm")])
  v <- pkapp_annotate_nca(f$result, vs = f$vs, specification = f$spec[c("version", "vs")])
  expect_false(any(grepl("^VS_", names(d$data))))
  expect_false(any(grepl("^DM_", names(v$data))))
})

test_that("missing annotation data keeps every profile and reports missing fields", {
  f <- fixture020(); f$dm <- f$dm[1, ]; f$vs <- f$vs[f$vs$USUBJID == "001", ]
  f$vs$VSSTRESU[1] <- NA; f$vs$VSDTC[1] <- NA
  x <- pkapp_annotate_nca(f$result, f$dm, f$vs, f$spec)
  expect_identical(x$data[names(f$result$data)], f$result$data)
  expect_true(all(is.na(x$data$DM_SEX[x$data$ID == "002"])))
  expect_true(all(c("missing_match", "missing_unit", "missing_datetime") %in% x$trace$issues$reason))
  f$dm$USUBJID <- "1"
  x <- pkapp_annotate_nca(f$result, f$dm, specification = f$spec[c("version", "dm")])
  expect_true(all(is.na(x$data$DM_SEX)))
})

test_that("duplicates, unsafe identity, unchosen time and collisions are rejected", {
  f <- fixture020()
  expect_error(pkapp_annotate_nca(f$result, rbind(f$dm, f$dm[1, ]), f$vs, f$spec), "DM.*重複")
  s <- f$spec; s$vs$selection <- list(mode = "all_unique")
  expect_error(pkapp_annotate_nca(f$result, f$dm, f$vs, s), "VS.*重複")
  s <- f$spec; s$dm$keys$STUDYID <- NULL
  expect_error(pkapp_annotate_nca(f$result, f$dm, f$vs, s), "STUDYID")
  d <- f$dm; d$USUBJID <- as.numeric(d$USUBJID)
  expect_error(pkapp_annotate_nca(f$result, d, f$vs, f$spec), "文字型")
  s <- f$spec; s$dm$columns <- list(AVAL = "AGE")
  expect_error(pkapp_annotate_nca(f$result, f$dm, f$vs, s), "既存列")
  s <- f$spec; s$vs$measurements[[2]]$name <- "VS_HEIGHT_UNIT"
  expect_error(pkapp_annotate_nca(f$result, f$dm, f$vs, s), "既存列")
  s <- f$spec; s$vs$selection$values <- list()
  expect_error(pkapp_annotate_nca(f$result, f$dm, f$vs, s), "条件が不正")
  s <- f$spec; s$vs$scope <- "keys"
  expect_error(pkapp_annotate_nca(f$result, f$dm, f$vs, s), "投与期")
})

test_that("period keys select different recorded VS values", {
  f <- fixture020(); f$vs$PERIOD <- ifelse(f$vs$VISIT == "BASELINE", 1, 2)
  f$vs$VSSTRESN[f$vs$PERIOD == 2] <- f$vs$VSSTRESN[f$vs$PERIOD == 2] + 1
  s <- f$spec; s$vs$scope <- "keys"; s$vs$keys$PERIOD <- "PERIOD"; s$vs$selection <- list(mode = "all_unique")
  x <- pkapp_annotate_nca(f$result, f$dm, f$vs, s)
  expect_equal(unique(x$data$VS_BMI[x$data$ID == "001" & x$data$PERIOD == 2]), 20.5)
  expect_identical(x$data[names(f$result$data)], f$result$data)
  f$result$data$PERIOD <- as.character(as.numeric(f$result$data$PERIOD) + .1)
  f$vs$PERIOD <- f$vs$PERIOD + .1
  z <- pkapp_annotate_nca(f$result, f$dm, f$vs, s)
  expect_false(anyNA(z$data$VS_BMI))
  f$vs$PERIOD <- paste0("0", f$vs$PERIOD)
  y <- pkapp_annotate_nca(f$result, f$dm, f$vs, s)
  expect_true(all(is.na(y$data$VS_BMI)))
})

test_that("saved snapshots replay and SAS inputs retain identifiers and units", {
  f <- fixture020(); dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  file.copy(file.path("apps/pk-desktop/examples/nca_annotation", c("DM.sas7bdat", "VS.sas7bdat")), dir)
  dm <- pkapp_read_table(file.path(dir, "DM.sas7bdat")); vs <- pkapp_read_table(file.path(dir, "VS.sas7bdat"))
  expect_identical(dm$USUBJID, f$dm$USUBJID)
  expect_identical(vs$VSSTRESU, f$vs$VSSTRESU)
  engine <- new.env(parent = emptyenv()); engine$project <- dir; core$desktop_tables_init(engine)
  nca <- core$desktop_table_save(engine, "NCA", list(source = f$input), list(kind = "nca", specification = f$result$specification), frozen = f$result)
  original <- core$desktop_annotation_result(engine, nca$id)
  x <- pkapp_annotate_nca(original, dm, vs, f$spec)
  saved <- core$desktop_annotation_save(engine, "Annotations", list(result = original, dm = dm, vs = vs), x, list(result_source = nca$id))
  expect_identical(pkapp_replay(core$desktop_table_path(engine, saved$id))$data, x$data)
  expect_identical(core$desktop_table_get(engine, nca$id), f$result$data)
})

test_that("period scoped VS cannot omit period by choosing an unrelated grouping key", {
  f <- fixture020()
  f$result$data$ANALYTE <- "parent"
  f$result$trace$output_key_mapping$ANALYTE <- "ANALYTE"
  f$vs$ANALYTE <- "parent"; f$vs$PERIOD <- 1
  s <- f$spec; s$vs$scope <- "keys"; s$vs$keys$ANALYTE <- "ANALYTE"
  expect_error(pkapp_annotate_nca(f$result, f$dm, f$vs, s), "投与期")
  s$vs$keys$PERIOD <- "PERIOD"
  x <- pkapp_annotate_nca(f$result, f$dm, f$vs, s)
  expect_true(all(is.na(x$data$VS_BMI[x$data$PERIOD == "2"])))
  expect_false(anyNA(x$data$VS_BMI[x$data$PERIOD == "1"]))
})
