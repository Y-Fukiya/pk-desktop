source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)

test_that("multiple machine reasons are translated separately without changing the saved result", {
  ids <- sprintf("S%02d", 1:6)
  groups <- lapply(seq_along(ids), function(i) list(profile = i, identity = list(ID = ids[[i]]), status = "partial",
    warning = c("LAMZ: span_below_threshold", "AUCIFO: dependency_unavailable_AUCIFO"),
    n_missing_time = 0L, n_missing_concentration = 0L, tlast_quant = 8, predose_qc = NULL, dose_review = NULL))
  d <- data.frame(ID = ids, PARAMCD = "AUCIFO", AVAL = NA_real_, PARAM_STATUS = "not_calculable", PROFILE_STATUS = "partial",
    NCA_EXCLUDE = "span_below_threshold; dependency_unavailable_AUCIFO", NCA_WARNING = "", stringsAsFactors = FALSE)
  raw <- unlist(lapply(seq_along(ids), function(i) c(paste0("Profile ", i, ": LAMZ: span_below_threshold; AUCIFO: dependency_unavailable_AUCIFO"),
    paste0("Profile ", i, " terminal: span_below_threshold;fewer_than_3_positive_post_cmax"))), use.names = FALSE)
  value <- list(data = d, warnings = raw, trace = list(output_key_mapping = list(ID = "ID"), groups = groups, warnings = raw))
  original <- value
  summary <- core$desktop_nca_summary(value, with_diagnostics = TRUE)
  parts <- attr(summary, "nca_warning_parts")
  html <- as.character(core$desktop_display_table(summary, bilingual_system_columns = "NCA_WARNING", diagnostic_parts = list(NCA_WARNING = parts)))
  expect_match(html, "Terminal-phase span is below the specified number of half-lives", fixed = TRUE)
  expect_match(html, "No acceptable positive AUCinf based on observed Clast", fixed = TRUE)
  expect_false(grepl("Untranslated diagnostic; review the Japanese: 終末相の区間幅", html, fixed = TRUE))
  expect_equal(length(parts), 6L)
  expect_identical(value, original)
  mixed <- value
  mixed$trace$groups[[1]]$status <- "ok"
  mixed$trace$groups[[1]]$warning <- character()
  mixed$data$PROFILE_STATUS[1] <- "ok"
  mixed$data$PARAM_STATUS[1] <- "calculated"
  mixed$data$NCA_EXCLUDE[1] <- NA_character_
  mixed$data$AVAL[1] <- 5
  mixed_summary <- core$desktop_nca_summary(mixed, with_diagnostics = TRUE)
  mixed_parts <- attr(mixed_summary, "nca_warning_parts")
  expect_equal(length(mixed_parts), 6L)
  expect_length(mixed_parts[[1]], 0L)
  expect_true("span_below_threshold" %in% mixed_parts[[2]])
  mixed_html <- core$desktop_display_table(mixed_summary, bilingual_system_columns = "NCA_WARNING", diagnostic_parts = list(NCA_WARNING = mixed_parts))
  cells <- xml2::xml_find_all(xml2::read_html(as.character(mixed_html)), "//tbody/tr/td[3]")
  expect_false(grepl("Terminal-phase span", as.character(cells[[1]]), fixed = TRUE))
  expect_match(as.character(cells[[2]]), "Terminal-phase span", fixed = TRUE)
  grouped <- as.character(core$desktop_nca_warnings(value))
  doc <- xml2::read_html(grouped)
  headers <- xml2::xml_find_all(doc, '//details[contains(@class,"pk-warning-group")]/summary')
  expect_gte(length(headers), 2L)
  expect_true(any(grepl("6", xml2::xml_text(headers), fixed = TRUE)))
  for (id in ids) expect_match(grouped, paste0("ID=", id), fixed = TRUE)
  expect_match(grouped, "fewer than", ignore.case = TRUE)
  expect_identical(value, original)
})

test_that("unknown diagnostics remain visible and natural-language semicolons stay intact", {
  html <- as.character(core$desktop_nca_diagnostics(c("span_below_threshold;unknown_future_code",
    "Missing concentration at the requested start; select an explicit C0 policy when starting at the dose reference")))
  expect_match(html, "Terminal-phase span", fixed = TRUE)
  expect_match(html, "unknown_future_code", fixed = TRUE)
  expect_match(html, "Missing concentration at the requested start; select an explicit C0 policy", fixed = TRUE)
  prefixed <- as.character(core$desktop_nca_diagnostics("LAMZ: span_below_threshold; AUCIFO: dependency_unavailable_AUCIFO"))
  expect_match(prefixed, "Terminal-phase span", fixed = TRUE)
  expect_match(prefixed, "No acceptable positive AUCinf", fixed = TRUE)
  expect_false(grepl("Untranslated diagnostic", prefixed, fixed = TRUE))
})

test_that("display rounding does not discard a manually entered boundary or change its stored hours", {
  project <- tempfile(); dir.create(project); on.exit(unlink(project, recursive = TRUE), add = TRUE)
  e <- core$desktop_nca_engine(project)
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    session$setInputs(time_unit = "h", time_origin = 0, start = .1234567890123, end = 7.1234567890123, last_observation = FALSE)
    session$setInputs(time_unit = "min"); session$setInputs(time_origin = 0, start = 7.40741, end = 427.407)
    session$setInputs(time_unit = "day"); session$setInputs(time_origin = 0, start = .00514403, end = .296811)
    expect_identical(specification()$start, .1234567890123)
    expect_identical(specification()$end, 7.1234567890123)
    session$setInputs(time_unit = "h"); session$setInputs(time_origin = 0, start = .1234567890123, end = 7.1234567890123)
    expect_null(pending_time_conversion())
    expect_identical(specification()$start, .1234567890123)
    expect_identical(specification()$end, 7.1234567890123)
    session$setInputs(time_unit = "day"); session$setInputs(time_origin = 0, start = .00514403, end = .296811)
    session$setInputs(start = .1234567890123)
    session$setInputs(time_unit = "h"); session$setInputs(time_origin = 0, start = 2.96296, end = 7.1234567890123)
    expect_equal(specification()$start, .1234567890123 * 24, tolerance = 0)
    expect_match(output$time_equiv_start$html, "計算に使う値", fixed = TRUE)
    expect_match(output$time_equiv_start$html, "2.9629629362952", fixed = TRUE)
    session$setInputs(time_unit = "h", time_origin = 0, start = .5, end = .50000001, last_observation = FALSE)
    session$setInputs(time_unit = "day"); session$setInputs(time_origin = 0, start = .0208333, end = .0208333)
    expect_null(pending_time_conversion())
    expect_null(time_boundary_error())
    expect_identical(specification()$start, .5)
    expect_identical(specification()$end, .50000001)
    session$setInputs(time_unit = "h"); session$setInputs(time_origin = 0, start = .5, end = .50000001)
    expect_null(pending_time_conversion())
    expect_identical(specification()$start, .5)
    expect_identical(specification()$end, .50000001)
  })
})
