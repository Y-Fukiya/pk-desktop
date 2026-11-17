source("apps/pk-desktop/tests/nca023_ui.R")

test_that("localized public warnings retain grouped identities and one cause in the GUI", {
  value <- nca023_result()
  value$trace$groups[[1]]$warning <- c(value$trace$groups[[1]]$warning, "intervening_dose", "outside_range_weight")
  value$trace$warnings <- c(value$warnings, "Profile 1 terminal: intervening_dose",
    "Profile 1: outside_range_weight", "Vendor future diagnosis <script>x</script>")
  value$warnings <- vapply(value$trace$warnings, core$desktop_nca_warning_text, character(1), USE.NAMES = FALSE)
  original <- value
  html <- as.character(core$desktop_nca_warnings(value))
  doc <- xml2::read_html(html)
  messages <- xml2::xml_text(xml2::xml_find_all(doc, '//div[@class="pk-diagnostic-message"]'))
  expect_equal(sum(trimws(messages) == "解析区間に追加投与があります"), 1)
  expect_equal(sum(trimws(messages) == core$desktop_nca_diagnostic_label("outside_range_weight")), 1)
  for (text in c("ID=SUBJ&lt;01&gt;", "PERIOD=2", "ANALYTE=parent", "STUDY=study-A",
      "基準投与から 12 h", "EX元行：5", "Vendor future diagnosis &lt;script&gt;x&lt;/script&gt;", "未翻訳の診断"))
    expect_match(html, text, fixed = TRUE)
  expect_match(gsub("\\s+", "", xml2::xml_text(doc)), "終末相：", fixed = TRUE)
  expect_false(grepl("Profile |intervening_dose|outside_range_weight|<script>", html))
  expect_identical(value, original)
  value$warnings <- c(value$warnings, "Public vendor diagnosis <extra>")
  public_html <- as.character(core$desktop_nca_warnings(value))
  expect_match(public_html, "Public vendor diagnosis &lt;extra&gt;", fixed = TRUE)
  expect_equal(lengths(regmatches(public_html, gregexpr("Public vendor diagnosis", public_html, fixed = TRUE))), 1)
  legacy <- original; legacy$warnings <- legacy$trace$warnings; legacy$trace$warnings <- NULL
  expect_identical(as.character(core$desktop_nca_warnings(legacy)), html)
})

test_that("terminal selection stays before additional PK and body-size controls", {
  html <- htmltools::renderTags(core$desktop_nca_ui("nca"))$html
  doc <- xml2::read_html(html)
  expect_length(xml2::xml_find_all(doc, '//*[@id="nca-range_qc"]//*[@id="nca-terminal_method"]'), 1)
  expect_length(xml2::xml_find_all(doc, '//*[@id="nca-advanced-additional"]//*[@id="nca-derived_controls"]'), 1)
  expect_length(xml2::xml_find_all(doc, '//*[@id="nca-advanced-body"]//*[@id="nca-body_controls"]'), 1)
  expect_lt(regexpr('id="nca-terminal_method"', html, fixed = TRUE)[1], regexpr('id="nca-derived_controls"', html, fixed = TRUE)[1])
  controls <- as.character(core$desktop_nca_derived_controls(data.frame(ID="001"), list(), identity))
  expect_match(controls, "上の投与状況", fixed = TRUE)
  expect_false(grepl("下の投与状況", controls, fixed = TRUE))
  cat("PASS NCA026 terminal selection precedes advanced dose and body-size controls.\n")
})
