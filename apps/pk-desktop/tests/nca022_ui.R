source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)

nca022_result <- function(reason = "washout_unconfirmed", qc = list(status = "disabled")) {
  data <- data.frame(ID = "A", PARAMCD = c("CMAX", "LAMZ", "AUCIFO"),
    AVAL = c(10, NA, NA), PARAM_STATUS = c("calculated", "not_calculable", "not_calculable"),
    NCA_EXCLUDE = c("", reason, reason), stringsAsFactors = FALSE)
  list(data = data, trace = list(output_key_mapping = list(ID = "ID"),
    groups = list(list(profile = 1L, identity = list(ID = "A"), tlast_quant = 8, status = "partial",
      n_missing_time = 0L, n_missing_concentration = 0L, predose_qc = qc))))
}

test_that("main profile table shows the actual exclusion cause and unavailable parameters", {
  value <- nca022_result()
  html <- as.character(core$desktop_display_table(core$desktop_nca_summary(value)))
  expect_match(html, "前回投与からの十分な休薬期間が未確認です", fixed = TRUE)
  expect_match(html, "算出不可：LAMZ, AUCIFO", fixed = TRUE)
  expect_false(grepl("未翻訳", html, fixed = TRUE))
  expect_equal(value$data$NCA_EXCLUDE, c("", "washout_unconfirmed", "washout_unconfirmed"))
})

test_that("QC failure shows the measured percentage rounded only for display", {
  value <- nca022_result("predose_qc_exceeded", list(status = "exceeds", pct = 8.362, threshold_pct = 5))
  summary <- core$desktop_nca_summary(value)
  html <- as.character(core$desktop_display_table(summary))
  expect_match(html, "実測 8.4%", fixed = TRUE)
  expect_match(html, "閾値 5.0%", fixed = TRUE)
  expect_false(grepl("未翻訳", html, fixed = TRUE))
  expect_identical(value$trace$groups[[1]]$predose_qc$pct, 8.362)
  missing <- as.character(core$desktop_display_table(core$desktop_nca_summary(nca022_result("predose_qc_required_for_prior_dose"))))
  expect_match(missing, "投与前QC", fixed = TRUE)
  expect_match(missing, "未設定", fixed = TRUE)
  expect_false(grepl("閾値超過", missing, fixed = TRUE))
})

test_that("dose review distinguishes the reference and next dose and shows affected source rows", {
  value <- nca022_result("intervening_dose")
  value$trace$groups[[1]]$dose_review <- list(reason = "intervening_dose", next_h = 6,
    reference = "DOSE-REF", next_dose = list(ex_row = 11L, key = "DOSE-NEXT"),
    affected_rows = data.frame(source_row = c(4L, 5L), conc_row = c(24L, 25L), pc_row = c(34L, 35L), time_h = c(6, 8)))
  html <- as.character(core$desktop_nca_dose_details(value$trace$groups[[1]]))
  for (text in c("DOSE-REF", "DOSE-NEXT", "基準投与", "次回投与", "24", "35", "6", "8")) expect_match(html, text, fixed = TRUE)
  document <- xml2::read_html(html)
  expect_identical(xml2::xml_text(xml2::xml_find_first(document, '//dt[text()="基準投与"]/following-sibling::dd[1]')), "DOSE-REF")
  expect_identical(xml2::xml_text(xml2::xml_find_first(document, '//dt[text()="次回投与の記録キー"]/following-sibling::dd[1]')), "DOSE-NEXT")
  cells <- xml2::xml_text(xml2::xml_find_all(document, '//tbody/tr/td'))
  expect_identical(cells, c("4", "24", "34", "6", "5", "25", "35", "8"))
  expect_null(core$desktop_nca_dose_details(nca022_result()$trace$groups[[1]]))
})

test_that("dose exclusion remains the main cause when withheld Cmax prevents QC evaluation", {
  value <- nca022_result("intervening_dose", list(status = "not_evaluable", reason = "invalid_cmax"))
  value$data$AVAL <- NA_real_
  value$data$PARAM_STATUS <- "not_calculable"
  value$data$NCA_EXCLUDE <- "intervening_dose"
  value$trace$groups[[1]]$dose_review <- list(reason = "intervening_dose")
  qc <- value$trace$groups[[1]]$predose_qc
  html <- as.character(core$desktop_display_table(core$desktop_nca_summary(value)))
  expect_match(html, "解析区間に追加投与があります", fixed = TRUE)
  expect_match(html, "算出不可：CMAX, LAMZ, AUCIFO", fixed = TRUE)
  expect_false(grepl("投与前QC：評価不可", html, fixed = TRUE))
  expect_identical(value$trace$groups[[1]]$predose_qc, qc)
})

test_that("raw eligibility is presented as a declaration and assembly evidence is distinguished", {
  value <- nca022_result()
  terminal <- list(profile = 1L, method = "auto", status = "calculated", reason = "", quality_reasons = list(),
    candidates = list(), eligibility = list(evidence = list(provenance = "declared_raw")))
  value$trace$terminal <- list(profiles = list(terminal))
  html <- as.character(core$desktop_nca_terminal_details(value, 1L))
  expect_match(html, "申告のみ", fixed = TRUE)
  value$trace$terminal$profiles[[1]]$eligibility$evidence$provenance <- "assembly"
  html <- as.character(core$desktop_nca_terminal_details(value, 1L))
  expect_match(html, "EX・PC・濃度", fixed = TRUE)
  expect_false(grepl("申告のみ", html, fixed = TRUE))
  value$trace$terminal$profiles[[1]]$eligibility$evidence <- NULL
  expect_false(grepl("申告のみ", as.character(core$desktop_nca_terminal_details(value, 1L)), fixed = TRUE))
  value$trace$terminal <- NULL
  expect_null(core$desktop_nca_terminal_details(value, 1L))
})

test_that("dosing and terminal controls stay outside Advanced with contextual washout help", {
  document <- xml2::read_html(htmltools::renderTags(core$desktop_nca_ui("nca"))$html)
  dosing <- xml2::xml_find_first(document, "//*[@id='nca-judgment-dosing']")
  range_qc <- xml2::xml_find_first(document, "//*[@id='nca-range_qc']")
  advanced <- xml2::xml_find_first(document, "//*[@id='nca-advanced']")

  for (field in c("dosing_context", "dose_history_confirmed", "washout_attestation")) {
    expect_length(xml2::xml_find_all(dosing, sprintf(".//*[@id='nca-%s']", field)), 1L)
    expect_length(xml2::xml_find_all(advanced, sprintf(".//*[@id='nca-%s']", field)), 0L)
  }
  for (field in c("terminal_method", "terminal_start_h", "terminal_end_h", "terminal_qc_action")) {
    expect_length(xml2::xml_find_all(range_qc, sprintf(".//*[@id='nca-%s']", field)), 1L)
    expect_length(xml2::xml_find_all(advanced, sprintf(".//*[@id='nca-%s']", field)), 0L)
  }
  isolated <- xml2::xml_find_first(dosing, ".//select[@id='nca-dosing_context']/option[@value='isolated_single_period']")
  expect_identical(xml2::xml_text(isolated), "独立した単回投与期")
  washout <- xml2::xml_find_first(dosing, ".//*[@data-display-if][.//*[@id='nca-washout_attestation']]")
  expect_match(xml2::xml_attr(washout, "data-display-if"), "input.dosing_context === 'isolated_single_period'", fixed = TRUE)
  expect_match(xml2::xml_text(washout), "休薬期間の確認と投与前QCの合格が必要", fixed = TRUE)

  expect_match(xml2::xml_attr(range_qc, "class"), "pk-nca-qc-card", fixed = TRUE)
  styles <- htmltools::renderTags(core$desktop_workbench_styles())$head
  expect_match(styles, ".pk-workspace.pk-nca .pk-nca-qc-card { grid-column:1 / -1; }", fixed = TRUE)
})

test_that("all five assembly keys retain their accessible labels and choices", {
  engine <- new.env(parent = emptyenv())
  engine$project <- tempfile("nca022-keys-")
  dir.create(engine$project)
  on.exit(unlink(engine$project, recursive = TRUE))
  core$desktop_tables_init(engine)
  shiny::testServer(core$desktop_assembly_server, args = list(engine = engine), {
    session$setInputs(conc_ID = "ID", pc_ID = "ID", ex_ID = "ID", conc_SAMPLE_ID = "SAMPLE_ID", pc_SAMPLE_ID = "SAMPLE_ID", ex_DOSE_ID = "DOSE_ID")
    document <- xml2::read_html(output$keys$html)
    fields <- xml2::xml_find_all(document, '//div[contains(@class,"pk-key-field")]')
    expect_length(fields, 5L)
    for (field in fields) {
      label <- xml2::xml_find_first(field, './label')
      select <- xml2::xml_find_first(field, './/select')
      expect_identical(xml2::xml_attr(label, "for"), xml2::xml_attr(select, "id"))
      expect_true(nzchar(xml2::xml_text(label)))
      expect_true(length(xml2::xml_find_all(select, './option')) > 0L)
    }
  })
})
