source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)

nca023_result <- function() {
  identity <- list(ID = "SUBJ<01>", PERIOD = "2", ANALYTE = "parent", STUDY = "study-A")
  parameters <- c("CMAX", "TMAX", "AUCLST")
  warnings <- paste0(parameters, ": intervening_dose")
  data <- cbind(as.data.frame(identity), data.frame(PARAMCD = parameters, AVAL = NA_real_,
    PROFILE_STATUS = "excluded", PARAM_STATUS = "not_calculable", NCA_EXCLUDE = "intervening_dose",
    NCA_WARNING = paste(warnings, collapse = "; "), TLAST_QUANT_H = 48))
  group <- list(profile = 1L, identity = identity, status = "excluded", tlast_quant = 48,
    n_missing_time = 0L, n_missing_concentration = 0L, warning = warnings,
    dose_review = list(reason = "intervening_dose", next_h = 12, next_dose = list(ex_row = 5L)))
  list(data = data, warnings = c(paste0("Profile 1: ", paste(warnings, collapse = "; ")),
    "1 解析単位を除外しました。解析単位ごとの理由を確認してください。"),
    trace = list(output_key_mapping = as.list(setNames(names(identity), names(identity))), groups = list(group)))
}

test_that("excluded Tlast is missing only in presentation and warnings precede numeric values", {
  value <- nca023_result()
  original <- value
  wide <- core$desktop_nca_summary(value)
  long <- core$desktop_nca_display_data(value)
  expect_true(is.na(wide$TLAST_QUANT_H))
  expect_true(all(is.na(long$TLAST_QUANT_H)))
  expect_true(is.na(core$desktop_nca_display_tlast(value$trace$groups[[1]])))
  first <- c("ID", "PERIOD", "ANALYTE", "STUDY", "PROFILE_STATUS", "NCA_WARNING")
  expect_identical(names(wide)[seq_along(first)], first)
  expect_identical(names(long)[seq_along(first)], first)
  expect_identical(long$AVAL, value$data$AVAL)
  expect_identical(long[setdiff(names(long), "TLAST_QUANT_H")], value$data[setdiff(names(long), "TLAST_QUANT_H")])
  html <- as.character(core$desktop_display_table(wide))
  expect_match(html, 'class="pk-detail-text"', fixed = TRUE)
  expect_false(grepl("<td>48</td>", html, fixed = TRUE))
  expect_identical(value, original)
  for (status in c("partial", "review", "ok")) {
    value$data$PROFILE_STATUS <- status
    value$trace$groups[[1]]$status <- status
    value$data$NCA_EXCLUDE <- "washout_unconfirmed"
    expect_equal(core$desktop_nca_summary(value)$TLAST_QUANT_H, 48)
    expect_equal(core$desktop_nca_display_data(value)$TLAST_QUANT_H, rep(48, 3))
    expect_equal(core$desktop_nca_display_tlast(value$trace$groups[[1]]), 48)
  }
})

test_that("top warnings identify all analysis keys and one cause with dose evidence", {
  value <- nca023_result()
  original <- value
  html <- as.character(core$desktop_nca_warnings(value))
  for (text in c("ID=SUBJ&lt;01&gt;", "PERIOD=2", "ANALYTE=parent", "STUDY=study-A", "基準投与から 12 h", "EX元行：5", "CMAX・TMAX・AUCLST", "1 解析単位を除外"))
    expect_match(html, text, fixed = TRUE)
  expect_false(grepl("Profile|intervening_dose|未翻訳", html))
  doc <- xml2::read_html(html)
  messages <- xml2::xml_text(xml2::xml_find_all(doc, '//div[@class="pk-diagnostic-message"]'))
  expect_equal(sum(trimws(messages) == "解析区間に追加投与があります"), 1L)
  expect_identical(value, original)
  value$trace$groups[[1]]$dose_review$next_h <- NULL
  value$trace$groups[[1]]$dose_review$next_dose <- NULL
  legacy <- as.character(core$desktop_nca_warnings(value))
  expect_match(legacy, "時間の記録なし（旧形式など）", fixed = TRUE)
  expect_match(legacy, "EX元行：記録なし（旧形式など）", fixed = TRUE)
})

test_that("global terminal and unknown diagnostic semantics remain available and escaped", {
  value <- nca023_result()
  value$trace$groups[[1]]$warning <- c(value$trace$groups[[1]]$warning,
    "1 source rows excluded for missing time; profile requires review")
  value$warnings[1] <- paste0("Profile 1: ", paste(value$trace$groups[[1]]$warning, collapse = "; "))
  value$warnings <- c(value$warnings, "Profile 1 terminal: washout_unconfirmed;future_terminal <b>x</b>",
    "Profile 1: Future profile diagnosis <script>x</script>", "Global future diagnosis <extra>")
  html <- as.character(core$desktop_nca_warnings(value))
  for (text in c("時間欠測により元行 1 行を除外しました", "解析単位を確認してください",
    "前回投与からの十分な休薬期間が未確認です", "future_terminal &lt;b&gt;x&lt;/b&gt;",
    "Future profile diagnosis &lt;script&gt;x&lt;/script&gt;", "Global future diagnosis &lt;extra&gt;", "未翻訳の診断"))
    expect_match(html, text, fixed = TRUE)
  expect_match(gsub("\\s+", "", xml2::xml_text(xml2::read_html(html))), "終末相：", fixed = TRUE)
  expect_false(grepl("Profile |washout_unconfirmed|intervening_dose|<script>|<b>", html))
})

test_that("excluded real results retain observation metadata and omit the endpoint plot marker", {
  data <- data.frame(ID = "A", TIME = c(0, 1, 1, 48), CONC = c(0, 10, 8, 2))
  value <- pkapp_nca(data, list(id = "ID", time = "TIME", concentration = "CONC", concentration_unit = "ng/mL"))
  original <- value
  expect_identical(value$trace$groups[[1]]$status, "excluded")
  expect_equal(value$trace$groups[[1]]$tlast_quant, 48)
  expect_true(all(is.na(core$desktop_nca_display_data(value)$TLAST_QUANT_H)))
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({ grDevices::dev.off(); unlink(path) })
  expect_no_error(suppressWarnings(core$desktop_nca_plot(value, 1L)))
  expect_no_error(suppressWarnings(core$desktop_nca_plot(value, 1L, "log")))
  calls <- new.env(parent = emptyenv())
  calls$lines <- list()
  calls$labels <- character()
  local_mocked_bindings(abline = function(...) { calls$lines <- c(calls$lines, list(list(...))) },
    legend = function(..., legend) { calls$labels <- legend }, .package = "graphics")
  suppressWarnings(core$desktop_nca_plot(value, 1L))
  expect_length(Filter(function(x) identical(x$col, "#176b79") && identical(x$lty, 2), calls$lines), 0L)
  expect_false("最終定量時点" %in% calls$labels)
  expect_identical(value, original)
})


test_that("healthy profiles do not create an empty warning notice", {
  value <- nca023_result()
  value$warnings <- character()
  value$trace$groups[[1]]$warning <- character()
  value$trace$groups[[1]]$status <- "ok"
  value$trace$groups[[1]]$dose_review <- NULL
  expect_null(core$desktop_ui_notice(core$desktop_nca_warnings(value), "warning"))
})

test_that("terminal-added warnings share one cause with the original profile payload", {
  value <- nca023_result()
  value$trace$groups[[1]]$warning <- c(value$trace$groups[[1]]$warning, "intervening_dose")
  value$warnings <- c(value$warnings, "Profile 1 terminal: intervening_dose")
  original <- value
  html <- as.character(core$desktop_nca_warnings(value))
  doc <- xml2::read_html(html)
  messages <- xml2::xml_text(xml2::xml_find_all(doc, '//div[@class="pk-diagnostic-message"]'))
  expect_equal(sum(grepl("解析区間に追加投与があります", messages, fixed = TRUE)), 1L)
  for (text in c("CMAX・TMAX・AUCLST", "基準投与から 12 h", "EX元行：5", "1 解析単位を除外"))
    expect_match(html, text, fixed = TRUE)
  expect_match(gsub("\\s+", "", xml2::xml_text(doc)), "終末相：", fixed = TRUE)
  expect_identical(value, original)
})


test_that("all-BLQ profiles render without a quantifiable endpoint on real graphics devices", {
  data <- data.frame(ID = "A", TIME = c(0, 1, 2), CONC = .1, BLQ = 1L)
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({ grDevices::dev.off(); unlink(path) })
  for (policy in c("zero", "missing")) {
    value <- pkapp_nca(data, list(id = "ID", time = "TIME", concentration = "CONC", concentration_unit = "ng/mL",
      blq_column = "BLQ", blq = "zero", all_blq_policy = policy))
    original <- value
    expect_true(is.na(value$trace$groups[[1]]$tlast_quant))
    expect_no_error(suppressWarnings(core$desktop_nca_plot(value, 1L)))
    expect_no_error(suppressWarnings(core$desktop_nca_plot(value, 1L, "log")))
    expect_identical(value, original)
  }
})
