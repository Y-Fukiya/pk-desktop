source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
library(testthat)
core <- environment(pkapp_open)
source("apps/pk-desktop/tests/helper_nca_jobs.R")

nca014_doc <- function(tags) xml2::read_html(as.character(tags))
nca014_ids <- function(doc) xml2::xml_attr(xml2::xml_find_all(doc, ".//*[@id]"), "id")

test_that("dosing, BLQ, C0 and units are visible before the range and QC controls", {
  doc <- nca014_doc(core$desktop_nca_ui("nca"))
  controls <- xml2::xml_find_first(doc, './/div[@class="pk-controls"]')
  cards <- xml2::xml_find_all(controls, "./section")
  expect_identical(xml2::xml_attr(cards, "id"), c("nca-concentration_data", "nca-judgment-dosing", "nca-judgment-blq",
    "nca-judgment-c0", "nca-judgment-units", "nca-range_qc"))
  expect_true(all(c("nca-source", "nca-file", "nca-mapping", "nca-preview") %in% nca014_ids(cards[[1]])))
  expect_true(all(c("nca-blq_controls", "nca-prepared_hint", "nca-all_blq_policy") %in% nca014_ids(cards[[3]])))
  expect_true(all(c("nca-time_unit", "nca-concentration_unit") %in% nca014_ids(cards[[5]])))
  expect_match(xml2::xml_attr(cards[[6]], "class"), "pk-nca-qc-card", fixed = TRUE)
  qc_columns <- xml2::xml_find_all(cards[[6]], './div[@class="pk-nca-qc-grid"]/div')
  expect_length(qc_columns, 2L)
  expect_true(all(c("nca-auc_method", "nca-start", "nca-end", "nca-terminal_method") %in% nca014_ids(qc_columns[[1]])))
  expect_true(all(c("nca-predose_qc", "nca-predose_qc_lloq_controls") %in% nca014_ids(qc_columns[[2]])))
  expect_false(anyDuplicated(nca014_ids(doc)) > 0L)
  focus_ids <- xml2::xml_attr(xml2::xml_find_all(controls, './/input[@id] | .//select[@id] | .//button[@id]'), "id")
  expect_lt(match("nca-source", focus_ids), match("nca-all_blq_policy", focus_ids))
  expect_lt(match("nca-all_blq_policy", focus_ids), match("nca-c0_method", focus_ids))
  expect_lt(match("nca-c0_method", focus_ids), match("nca-predose_qc", focus_ids))
  expect_length(xml2::xml_find_all(controls, './/*[@tabindex and @tabindex != "0"]'), 0L)
})

test_that("BLQ rows keep the count with its label and four native radio options", {
  doc <- nca014_doc(core$desktop_pk_prepare_controls(shiny::NS("prep")))
  rows <- xml2::xml_find_all(doc, './/div[@class="pk-blq-rule-row"]')
  expect_length(rows, 6L)
  for (row in rows) {
    parts <- xml2::xml_children(row)
    expect_identical(xml2::xml_attr(parts[[1]], "class"), "pk-blq-rule-label")
    expect_length(xml2::xml_find_all(parts[[1]], './strong'), 1L)
    expect_identical(xml2::xml_text(xml2::xml_find_first(parts[[1]], './/span[@class="pk-blq-count-caption"]')), "対象行：")
    expect_length(xml2::xml_find_all(parts[[1]], './/span[contains(@class,"shiny-text-output")]'), 1L)
    radios <- xml2::xml_find_all(parts[[2]], './/input[@type="radio"]')
    expect_identical(xml2::xml_attr(radios, "value"), c("keep", "zero", "half_lloq", "exclude"))
    expect_false(any(xml2::xml_attr(radios, "tabindex") == "-1", na.rm = TRUE))
    expect_length(xml2::xml_find_all(parts[[2]], './/label[contains(@class,"radio-inline")]/input[@type="radio"]'), 4L)
  }
  expect_false(anyDuplicated(nca014_ids(doc)) > 0L)
  text <- xml2::xml_text(doc)
  expect_match(text, "LLOQ/2処理または投与前QCで使用", fixed = TRUE)
  expect_match(text, "LLOQ/2処理には指定が必要", fixed = TRUE)
  expect_match(text, "未指定の場合、BLQの投与前値のQCは評価できません", fixed = TRUE)
  expect_false(grepl("レシピ|任意", text))
})

test_that("QC badge tones preserve Japanese states in cards and tables", {
  states <- c("pass", "pass_blq", "exceeds", "not_evaluable", "disabled")
  labels <- c("閾値以下", "BLQの上限が閾値以下", "閾値超過", "評価不可", "未設定")
  tones <- c("is-success", "is-success", "is-qc-exceeds", "is-qc-unavailable", "is-qc-disabled")
  table <- nca014_doc(core$desktop_display_table(data.frame(PREDOSE_QC_STATUS = states)))
  badges <- xml2::xml_find_all(table, './/td/span')
  expect_identical(xml2::xml_text(badges), labels)
  expect_identical(xml2::xml_attr(badges, "class"), paste("pk-rule-label", tones))
  for (i in seq_along(states)) {
    badge <- xml2::xml_find_first(nca014_doc(core$desktop_status_badge(states[i])), './/span')
    expect_identical(xml2::xml_text(badge), labels[i])
    expect_identical(xml2::xml_attr(badge, "class"), xml2::xml_attr(badges[[i]], "class"))
  }
  expect_false(grepl("<script>", as.character(core$desktop_status_badge("<script>bad</script>")), fixed = TRUE))

  path <- tempfile("nca014-ui-"); dir.create(path)
  engine <- core$desktop_nca_engine(path)
  data <- data.frame(ID = "001", TIME = c(-.25, 0, 1, 2), CONC = c("<.2", "0", "8", "4"), BLQ = c(1, 0, 0, 0), LLOQ = .2)
  csv <- tempfile(fileext = ".csv"); core$pk_write_csv(data, csv)
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), {
    session$setInputs(source = "FILE", file = list(datapath = csv, name = "qc.csv"), id = "ID", time = "TIME", concentration = "CONC", groups = character(), time_unit = "h", concentration_unit = "ng/mL", unit_column = "", blq_column = "BLQ", mdv_column = "", scope = "all", time_origin = 0, start = 0, last_observation = TRUE, auc_method = "linear", blq = "exclude", c0_method = "none", all_blq_policy = "missing", name = "NCA014", predose_qc = TRUE, predose_qc_threshold_pct = 5, predose_qc_window_h = 1, predose_qc_lloq_source = "column", predose_qc_lloq_column = "LLOQ")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
      nca_wait_for_job(engine, session)
    expect_false(grepl("レシピ|任意", output$predose_qc_lloq_controls$html))
    original <- current()
    expect_equal(original$value$trace$groups[[1]]$predose_qc$pct_upper_bound, 2.5)
    for (upper_bound in c(2.5, NA_real_)) {
      shown <- original
      shown$value$trace$groups[[1]]$predose_qc$pct_upper_bound <- upper_bound
      current(shown); session$flushReact()
      details <- xml2::read_html(output$profile_details$html)
      bound_value <- xml2::xml_find_first(details, './/dl[@class="pk-profile-qc-metadata"]/div[dt="投与前値/Cmaxの上限（%）"]/dd')
      expect_identical(xml2::xml_text(bound_value), if (is.finite(upper_bound)) "< 2.5" else "—")
      expect_identical(current()$value$trace$groups[[1]]$predose_qc$pct_upper_bound, upper_bound)
      expect_identical(current()$value$data, original$value$data)
    }
    for (i in seq_along(states)) {
      shown <- original
      shown$value$trace$groups[[1]]$predose_qc$status <- states[i]
      current(shown); session$flushReact()
      details <- xml2::read_html(output$profile_details$html)
      badge <- xml2::xml_find_all(details, './/dl[@class="pk-profile-qc-metadata"]/div[dt="状態"]/dd/span')
      if (states[i] == "disabled") expect_length(badge, 0L) else {
        expect_length(badge, 1L)
        expect_identical(xml2::xml_text(badge), labels[i])
        expect_identical(xml2::xml_attr(badge, "class"), paste("pk-rule-label", tones[i]))
      }
      expect_identical(current()$value$data, original$value$data)
    }
    prepared <- pkapp_prepare(data, list(list(op = "blq_rules", mode = "positional", profile_columns = list("ID"),
      time = "TIME", concentration = "CONC", blq_column = "BLQ", zero_time = "pre", pre_rule = "zero",
      first_rule = "zero", middle_rule = "exclude", last_rule = "exclude", lloq_value = .2)))$data
    prepared_csv <- tempfile(fileext = ".csv"); core$pk_write_csv(prepared, prepared_csv)
    session$setInputs(file = list(datapath = prepared_csv, name = "prepared.csv"), concentration = "NCA_CONC")
    prepared_text <- xml2::xml_text(xml2::read_html(paste(output$prepared_hint$html, output$predose_qc_lloq_controls$html)))
    expect_false(grepl("レシピ|任意", prepared_text))
    expect_match(prepared_text, "保存済みの前処理の条件", fixed = TRUE)
    expect_match(prepared_text, "検証済みの前処理の条件", fixed = TRUE)
  })
  expect_identical(core$desktop_nca_lloq_origin("constant", TRUE), "保存済みの前処理の条件の定数")
})

test_that("compact styles are scoped and retain wrapping and focus affordances", {
  styles <- htmltools::renderTags(core$desktop_workbench_styles())$head
  css <- sub("</style>.*", "", sub(".*<style>", "", styles))
  expect_match(css, "@media (max-width:700px)", fixed = TRUE)
  expect_match(css, ".pk-workbench .pk-blq-rule-row { grid-template-columns:64px minmax(0,1fr); gap:8px; }", fixed = TRUE)
  expect_match(css, ".pk-workbench .pk-blq-rule-label { display:flex; flex-direction:column;", fixed = TRUE)
  expect_match(css, ".pk-workbench .pk-blq-count-caption { position:static;", fixed = TRUE)
  expect_match(css, ".pk-blq-rule-row .shiny-options-group { display:flex; flex-wrap:wrap;", fixed = TRUE)
  expect_match(css, ".radio-inline:has(input:focus-visible)", fixed = TRUE)
  expect_match(css, ".pk-workspace.pk-nca .pk-controls { display:grid; grid-template-columns:repeat(2,minmax(0,1fr));", fixed = TRUE)
  expect_match(css, ".pk-workspace.pk-nca .pk-nca-qc-card { grid-column:1 / -1; }", fixed = TRUE)
  expect_match(css, ".pk-workspace.pk-nca .pk-nca-qc-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr));", fixed = TRUE)
  expect_match(css, ".pk-workspace.pk-nca .pk-controls,.pk-workspace.pk-nca .pk-nca-qc-grid { grid-template-columns:minmax(0,1fr); }", fixed = TRUE)
  backgrounds <- c("is-success" = "#e5f4ec", "is-qc-exceeds" = "#fff1d5", "is-qc-unavailable" = "#faeae5", "is-qc-disabled" = "#edf0f2")
  for (tone in names(backgrounds)) {
    expect_match(css, paste0(".pk-workspace .pk-rule-label.", tone, ",.pk-workbench .pk-rule-label.", tone,
      " { background:", backgrounds[[tone]], ";"), fixed = TRUE)
  }
  expect_false(grepl("(^|[;{[:space:]])order[[:space:]]*:", css))
})
