source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
library(testthat)
core <- environment(pkapp_open)
source("apps/pk-desktop/tests/helper_nca_jobs.R")

nca012_engine <- function() {
  path <- tempfile("nca012-ui-"); dir.create(path)
  core$desktop_nca_engine(path)
}
nca012_configure <- function(session, data) {
  path <- tempfile(fileext = ".csv"); core$pk_write_csv(data, path)
  session$setInputs(source = "FILE", file = list(datapath = path, name = "qc.csv"), id = "ID", time = "TIME",
    concentration = "CONC", groups = character(), time_unit = "h", concentration_unit = "ng/mL", unit_column = "",
    blq_column = "BLQ", mdv_column = "", scope = "all", time_origin = 0, start = 0, last_observation = TRUE,
    auc_method = "linear", blq = "exclude", c0_method = "zero", all_blq_policy = "missing", name = "NCA012",
    predose_qc = TRUE, predose_qc_threshold_pct = 5, predose_qc_window_h = 1)
}

test_that("raw QC bound is explicit and changing it invalidates the result", {
  data <- data.frame(ID = rep(c("001", "002"), each = 4), TIME = rep(c(-.25, 0, 1, 2), 2),
    CONC = rep(c("<0.2", "0", "8", "4"), 2), BLQ = rep(c(1, 0, 0, 0), 2), LLOQ = .2, UNIT = c("ug/mL", rep("ng/mL", 7)),
    LIMIT = .2, LIMIT_ALT = .4, CLOCK = rep(c(-.25, 0, 1, 2), 2))
  messages <- new.env(parent = emptyenv())
  mock <- shiny::MockShinySession$new()
  mock$sendInputMessage <- function(id, message) messages[[sub("^.*-", "", id)]] <- message
  shiny::testServer(core$desktop_nca_server, args = list(engine = nca012_engine()), session = mock, {
    nca012_configure(session, data)
    session$setInputs(predose_qc_lloq_source = "column", predose_qc_lloq_column = "LIMIT")
    expect_false(grepl('value="LIMIT"', messages$time$options, fixed = TRUE))
    expect_match(messages$time$options, 'value="CLOCK"', fixed = TRUE)
    expect_identical(messages$time$value, "TIME")
    session$setInputs(time = "LIMIT_ALT", predose_qc_lloq_column = "LIMIT_ALT")
    expect_false(grepl('value="LIMIT_ALT"', messages$time$options, fixed = TRUE))
    expect_match(messages$time$options, 'value="LIMIT"', fixed = TRUE)
    expect_identical(messages$time$value, "")
    session$setInputs(time = "TIME", predose_qc_lloq_source = "constant")
    expect_match(messages$time$options, 'value="LIMIT_ALT"', fixed = TRUE)
    session$setInputs(unit_column = "LIMIT")
    expect_false(grepl('value="LIMIT"', messages$time$options, fixed = TRUE))
    expect_match(messages$time$options, 'value="CLOCK"', fixed = TRUE)
    session$setInputs(unit_column = "", predose_qc_lloq_source = "")
    expect_match(messages$time$options, 'value="LIMIT"', fixed = TRUE)
    expect_identical(specification()$predose_qc_lloq_source, "")
    expect_identical(specification()$predose_qc_lloq_column, "")
    expect_null(specification()$predose_qc_lloq_value)
    expect_match(output$predose_qc_lloq_controls$html, 'value="" selected', fixed = TRUE)
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    expect_equal(result()$value$trace$groups[[1]]$predose_qc$reason, "missing_lloq")
    expect_match(output$profile_details$html, "LLOQ未指定")
    session$setInputs(predose_qc_lloq_source = "column", predose_qc_lloq_column = "LLOQ")
    expect_null(result())
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 2)
    nca_wait_for_job(engine, session)
    qc <- result()$value$trace$groups[[1]]$predose_qc
    expect_equal(qc$status, "pass_blq")
    expect_true(is.na(qc$concentration))
    expect_true(is.na(qc$pct))
    expect_equal(qc$pct_upper_bound, 2.5)
    expect_match(output$profile_details$html, "pk-notice-info")
    expect_false(grepl("pk-notice-warning", output$profile_details$html, fixed = TRUE))
    expect_match(output$profile_details$html, "&lt; 0.2", fixed = TRUE)
    expect_match(output$profile_details$html, "&lt; 2.5%", fixed = TRUE)
    expect_match(output$profile_details$html, "確認理由")
    summary <- core$desktop_nca_summary(result()$value)
    expect_named(summary, c("ID", "PROFILE_STATUS", "NCA_WARNING", "CMAX", "TMAX", "AUCLST", "TLAST_QUANT_H"))
    expect_false(grepl("PREDOSE_QC_", output$wide_table$html, fixed = TRUE))
    expect_match(output$results$html, "解析単位の詳細")
    expect_match(output$profile_quality$html, "入力 4 行 / 時間欠測 0 行 / 濃度欠測 0 行 / 除外 1 行", fixed = TRUE)
    expect_false(grepl("pk-metric", output$profile_quality$html, fixed = TRUE))
    expect_lt(regexpr(paste0('id="', session$ns("plot"), '"'), output$results$html, fixed = TRUE), regexpr(paste0('id="', session$ns("profile_quality"), '"'), output$results$html, fixed = TRUE))
    session$setInputs(profile = "2")
    expect_match(output$profile_details$html, "ID=002", fixed = TRUE)
    expect_match(output$audit_table$html, "002", fixed = TRUE)
    session$setInputs(predose_qc_lloq_source = "constant", predose_qc_lloq_value = .4)
    expect_null(result())
    expect_identical(specification()$predose_qc_lloq_column, "")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 3)
    nca_wait_for_job(engine, session)
    expect_equal(result()$value$trace$groups[[1]]$predose_qc$pct_upper_bound, 5)
    session$setInputs(predose_qc_lloq_value = .5)
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 4)
    nca_wait_for_job(engine, session)
    expect_match(output$profile_details$html, "BLQの上限が閾値を超えるため判定できません")
    expect_match(output$profile_details$html, "pk-notice-warning")
    session$setInputs(profile = "1", unit_column = "UNIT")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 5)
    nca_wait_for_job(engine, session)
    expect_equal(result()$value$trace$groups[[1]]$predose_qc$reason, "unit_mismatch")
    expect_match(output$profile_details$html, "濃度単位が一致しません")
    expect_false(grepl("&lt; 0.5 ng/mL", output$profile_details$html, fixed = TRUE))
    expect_match(core$desktop_nca_summary(result()$value)$NCA_WARNING[1], "投与前QC：評価不可", fixed = TRUE)
  })
})

test_that("prepared QC uses recipe LLOQ and does not pass stale raw controls", {
  data <- data.frame(ID = "001", TIME = c(-.25, 0, 1, 2), CONC = c("<0.2", "0", "8", "4"), BLQ = c(1, 0, 0, 0))
  data <- pkapp_prepare(data, list(list(op = "blq_rules", mode = "positional", profile_columns = list("ID"), time = "TIME",
    concentration = "CONC", blq_column = "BLQ", zero_time = "pre", pre_rule = "zero", first_rule = "zero",
    middle_rule = "exclude", last_rule = "exclude", lloq_column = "OLD_UNUSED_LLOQ", lloq_value = .2)))$data
  shiny::testServer(core$desktop_nca_server, args = list(engine = nca012_engine()), {
    nca012_configure(session, data)
    session$setInputs(concentration = "NCA_CONC", predose_qc_lloq_source = "constant", predose_qc_lloq_value = 999)
    expect_identical(specification()$predose_qc_lloq_source, "")
    expect_identical(specification()$predose_qc_lloq_column, "")
    expect_null(specification()$predose_qc_lloq_value)
    expect_match(output$predose_qc_lloq_controls$html, "predose_qc_supplement", fixed = TRUE)
    expect_false(supplement_enabled())
    hint <- xml2::xml_text(xml2::read_html(output$prepared_hint$html))
    expect_match(gsub("[[:space:]]+", " ", hint), "保存済みの前処理の条件を参照します。 定数 0.2", fixed = TRUE)
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    expect_equal(result()$value$trace$groups[[1]]$predose_qc$status, "pass_blq")
  })
})

test_that("policy warnings and unavailable reasons are visible in the main table", {
  data <- data.frame(ID = "001", TIME = c(0, 1, 2), CONC = "<0.2", BLQ = 1)
  shiny::testServer(core$desktop_nca_server, args = list(engine = nca012_engine()), {
    nca012_configure(session, data)
    session$setInputs(all_blq_policy = "zero")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    summary <- core$desktop_nca_summary(result()$value)
    expect_match(summary$NCA_WARNING, "方針による値：CMAX, AUCLST", fixed = TRUE)
    expect_match(summary$NCA_WARNING, "算出不可：TMAX", fixed = TRUE)
    expect_match(summary$NCA_WARNING, "All original", fixed = TRUE)
    # The English translation is carried in an attribute; inspect visible Japanese text.
    expect_false(grepl("All original", xml2::xml_text(xml2::read_html(output$wide_table$html)), fixed = TRUE))
    expect_false(grepl("未翻訳", output$wide_table$html, fixed = TRUE))
    expect_match(output$profile_details$html, "方針による値：CMAX, AUCLST", fixed = TRUE)
    expect_match(output$profile_details$html, "NCA_EXCLUDE", fixed = TRUE)
  })
})

test_that("body discard shares the sidebar handler and retains applied data", {
  data <- data.frame(ID = "001", TIME = c(0, 1, 2), CONC = c(0, 8, 4), BLQ = 0,
    LIMIT = .2, LIMIT_ALT = .4, CLOCK = c(0, 1, 2), AUX = c(1, 0, 0))
  path <- tempfile(fileext = ".csv"); core$pk_write_csv(data, path)
  messages <- new.env(parent = emptyenv())
  mock <- shiny::MockShinySession$new()
  mock$sendInputMessage <- function(id, message) messages[[sub("^.*-", "", id)]] <- message
  shiny::testServer(core$desktop_workbench_server, args = list(engine = nca012_engine()), session = mock, {
    session$setInputs(source = "FILE", file = list(datapath = path, name = "raw.csv"), mode = "catalog", op = "", load_source = 1)
    original <- result()$data
    session$setInputs(mode = "prepare", op = "blq_rules", blq_mode = "positional", profile_columns = "ID", blq_time = "TIME",
      blq_concentration = "CONC", prep_blq_column = "BLQ", prep_time_origin = 0, prep_mdv_column = "", predose_column = "",
      prep_flag_convention = "strict", zero_time = "pre", pre_rule = "zero", first_rule = "zero", middle_rule = "exclude", last_rule = "exclude")
    session$setInputs(lloq_source = "column", lloq_column = "LIMIT")
    expect_false(grepl('value="LIMIT"', messages$blq_time$options, fixed = TRUE))
    expect_match(messages$blq_time$options, 'value="CLOCK"', fixed = TRUE)
    expect_identical(messages$blq_time$value, "TIME")
    mappings <- list(profile_columns = c("ID", "AUX"), blq_concentration = "AUX", prep_blq_column = "AUX",
      prep_mdv_column = "AUX", predose_column = "AUX")
    restore <- list(profile_columns = "ID", blq_concentration = "CONC", prep_blq_column = "BLQ",
      prep_mdv_column = "", predose_column = "")
    for (id in names(mappings)) {
      do.call(session$setInputs, stats::setNames(list(mappings[[id]]), id))
      expect_false(grepl('value="AUX"', messages$blq_time$options, fixed = TRUE), info = id)
      expect_false(grepl('value="LIMIT"', messages$blq_time$options, fixed = TRUE), info = id)
      expect_match(messages$blq_time$options, 'value="CLOCK"', fixed = TRUE)
      do.call(session$setInputs, stats::setNames(list(restore[[id]]), id))
      expect_match(messages$blq_time$options, 'value="AUX"', fixed = TRUE)
    }
    session$setInputs(blq_time = "LIMIT_ALT", lloq_column = "LIMIT_ALT")
    expect_false(grepl('value="LIMIT_ALT"', messages$blq_time$options, fixed = TRUE))
    expect_match(messages$blq_time$options, 'value="LIMIT"', fixed = TRUE)
    expect_identical(messages$blq_time$value, "")
    session$setInputs(blq_time = "TIME", lloq_source = "constant")
    expect_match(messages$blq_time$options, 'value="LIMIT_ALT"', fixed = TRUE)
    expect_identical(messages$blq_time$value, "TIME")
    expect_true(dirty())
    session$setInputs(discard_draft_body = 1, op = "")
    expect_false(dirty())
    expect_identical(result()$data, original)
    expect_match(notice()$message, "破棄しました")
  })
  html <- as.character(core$desktop_workbench_ui("w"))
  expect_match(html, 'id="w-discard_draft_body"', fixed = TRUE)
  expect_match(html, "input.mode === &#39;join&#39;", fixed = TRUE)
  styles <- htmltools::renderTags(core$desktop_workbench_styles())$head
  expect_match(styles, ".radio-inline:has(input:checked)", fixed = TRUE)
  expect_match(styles, ".radio-inline:has(input:focus-visible)", fixed = TRUE)
  expect_match(styles, "grid-template-columns:292px", fixed = TRUE)
})
