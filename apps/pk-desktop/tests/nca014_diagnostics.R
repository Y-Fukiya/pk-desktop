source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
raw014 <- function(n = 5L) data.frame(ID = "001", TIME = seq_len(n) - 1L,
  CONC = rep(c(0, 8, 4, 2, 1), length.out = n), BLQ = 0L, LLOQ = .2)
step014 <- list(op = "blq_rules", mode = "positional", profile_columns = "ID", time = "TIME",
  concentration = "CONC", blq_column = "BLQ", zero_time = "pre", pre_rule = "zero",
  first_rule = "zero", middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude",
  lloq_source = "column", lloq_column = "LLOQ")
spec014 <- list(id = "ID", time = "TIME", concentration = "NCA_CONC", blq_column = "NCA_BLQ",
  mdv_column = "NCA_MDV", blq = "as_recorded", concentration_unit = "ng/mL", auc_method = "linear")
prepare014 <- function(data = raw014(), keys = "ID") {
  pkapp_prepare(data, list(modifyList(step014, list(profile_columns = keys))))$data
}
message014 <- function(data, groups = character(), scope = NULL) {
  tryCatch({
    core$desktop_pk_blq_validate(data, modifyList(spec014, list(groups = groups)), scope)
    stop("Expected integrity refusal")
  }, error = conditionMessage)
}
movement014 <- function(message) sub(".* / 解析単位の移動先: ([^\n]*).*", "\\1", message)

test_that("leading-zero loss reports one counted source-to-destination movement", {
  data <- prepare014(); data$ID <- "1"
  before <- serialize(data, NULL)
  message <- message014(data)
  expect_identical(movement014(message), "ID=001 → ID=1（5行）")
  expect_match(message, "[ID=001] 変更列: ID", fixed = TRUE)
  expect_match(message, "現在の行: 1, 2, 3, 4, 5", fixed = TRUE)
  expect_match(message, "再適用", fixed = TRUE)
  expect_error(pkapp_nca(data, spec014), "ID=001 → ID=1（5行）", fixed = TRUE)
  expect_identical(serialize(data, NULL), before)
})

test_that("multiple keys show complete destinations in first-seen order", {
  raw <- raw014(); raw$ANALYTE <- "A"; raw$PERIOD <- "P1"
  data <- prepare014(raw, c("ID", "ANALYTE", "PERIOD"))
  data$ID <- c("2", "1", "2", "1", "1")
  data$PERIOD <- c("P2", "P1", "P2", "P1", "P1")
  message <- message014(data, c("ANALYTE", "PERIOD"))
  expect_identical(movement014(message), paste0(
    "ANALYTE=A, ID=001, PERIOD=P1 → ANALYTE=A, ID=2, PERIOD=P2（2行）, ",
    "ANALYTE=A, ID=001, PERIOD=P1 → ANALYTE=A, ID=1, PERIOD=P1（3行）"))
})

test_that("concentration-only rows do not enter movement counts", {
  data <- prepare014(); data$ID[1:2] <- "1"; data$CONC[3] <- 7
  message <- message014(data)
  expect_identical(movement014(message), "ID=001 → ID=1（2行）")
  expect_match(message, "変更列: ID, CONC / 現在の行: 1, 2, 3", fixed = TRUE)
  data <- prepare014(); data$CONC[3] <- 7
  message <- message014(data)
  expect_match(message, "変更列: CONC / 現在の行: 3", fixed = TRUE)
  expect_false(grepl("移動先", message, fixed = TRUE))
})

test_that("missing keys and literal missing labels remain distinct tuples", {
  data <- prepare014(); data$ID <- c(NA, "<欠測>", NA, "<欠測>", "<欠測>")
  expect_identical(movement014(message014(data)),
    "ID=001 → ID=<欠測>（2行）, ID=001 → ID=<欠測>（3行）")
})

test_that("display delimiter collisions do not merge profile destinations", {
  raw <- raw014(); raw$PERIOD <- "P1"
  data <- prepare014(raw, c("ID", "PERIOD"))
  data$ID <- c("x, PERIOD=y", "x", "x, PERIOD=y", "x", "x")
  data$PERIOD <- c("z", "y, PERIOD=z", "z", "y, PERIOD=z", "y, PERIOD=z")
  expect_identical(movement014(message014(data, "PERIOD")), paste0(
    "ID=001, PERIOD=P1 → ID=x, PERIOD=y, PERIOD=z（2行）, ",
    "ID=001, PERIOD=P1 → ID=x, PERIOD=y, PERIOD=z（3行）"))
})

test_that("destination deduplication happens before the twelve-entry bound", {
  data <- prepare014(raw014(30)); data$ID <- rep(c("B", "A"), 15)
  expect_identical(movement014(message014(data)), "ID=001 → ID=B（15行）, ID=001 → ID=A（15行）")
  data$ID <- rep(sprintf("D%02d", 15:1), 2)
  expected <- paste0("ID=001 → ID=", sprintf("D%02d", 15:4), "（2行）")
  expect_identical(movement014(message014(data)), paste(c(expected, "ほか3件"), collapse = ", "))
})

test_that("original source groups stay separate after converging destinations", {
  raw <- rbind(raw014(), transform(raw014(), ID = "002"))
  data <- prepare014(raw); data$ID <- "1"
  message <- message014(data)
  expect_match(message, "[ID=001] 変更列: ID", fixed = TRUE)
  expect_match(message, "[ID=002] 変更列: ID", fixed = TRUE)
  expect_match(message, "ID=001 → ID=1（5行）", fixed = TRUE)
  expect_match(message, "ID=002 → ID=1（5行）", fixed = TRUE)
  expect_false(grepl("（10行）", message, fixed = TRUE))
})

test_that("membership and metadata refusal precede changed-column details", {
  data <- prepare014(); data$ID <- "1"
  for (damaged in list(data[-1, ], rbind(data, data[1, ]))) {
    message <- message014(damaged)
    expect_match(message, "行構成が変更", fixed = TRUE)
    expect_false(grepl("変更列|移動先", message))
  }
  data$NCA_BLQ_PROFILE_SIGNATURE[1] <- "invalid"
  message <- message014(data)
  expect_match(message, "条件・ベースライン・署名の記録が破損", fixed = TRUE)
  expect_false(grepl("変更列|移動先", message))
  data <- prepare014(); data$NCA_BLQ_BASELINE[1] <- "invalid"
  expect_match(message014(data), "NCA_BLQ_BASELINE記録が欠落・不正", fixed = TRUE)
})

test_that("scope issues keep their order and the eight-issue cap", {
  raw <- do.call(rbind, lapply(seq_len(5), function(i) transform(raw014(), ID = sprintf("%03d", i))))
  data <- prepare014(raw); data$ID <- "1"
  lines <- strsplit(message014(data, scope = rep(c(TRUE, FALSE, FALSE, FALSE, FALSE), 5)), "\n", fixed = TRUE)[[1]]
  expect_match(lines[2], "[ID=001] 変更列", fixed = TRUE)
  expect_match(lines[3], "[ID=001] NCA対象範囲", fixed = TRUE)
  expect_match(lines[9], "[ID=004] NCA対象範囲", fixed = TRUE)
  expect_identical(lines[10], "ほか 2 件")
  expect_length(lines, 11)
})

test_that("long identities and details retain existing character caps", {
  raw <- raw014(); raw$ID <- paste(rep("あ", 700), collapse = "")
  data <- prepare014(raw); data$ID <- paste(rep("い", 2500), collapse = "")
  line <- strsplit(message014(data), "\n", fixed = TRUE)[[1]][2]
  expect_identical(substr(line, 1, 502), paste0("[ID=", substr(raw$ID[1], 1, 497), "]"))
  expect_equal(nchar(line), 500 + 3 + 1800)
})

test_that("validation preserves prepared data, signed metadata, and PK results", {
  data <- prepare014(); before <- serialize(data, NULL)
  signature <- core$desktop_pk_blq_signatures(data, core$desktop_pk_blq_context(data))
  result <- pkapp_nca(data, spec014)
  expect_silent(core$desktop_pk_blq_validate(data, spec014))
  expect_identical(serialize(data, NULL), before)
  expect_identical(core$desktop_pk_blq_signatures(data, core$desktop_pk_blq_context(data)), signature)
  expect_identical(pkapp_nca(data, spec014), result)
  expect_equal(result$data$AVAL, c(8, 1, 14.5))
  expect_identical(data[names(raw014())], raw014())
})
