source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
source("apps/pk-desktop/tests/helper_nca_jobs.R")
library(testthat)
core <- environment(pkapp_open)

test_that("group details preserve identities and parameters without repeating the reason", {
  ids <- c("S01", "S02", "S03")
  groups <- lapply(seq_along(ids), function(i) list(profile = as.integer(i), identity = list(ID = ids[[i]]), status = "partial",
    reason = if (i == 1) "dependency_unavailable_AUCIFO" else character(),
    warning = if (i <= 2) c("LAMZ: span_below_threshold", "AUCIFO: dependency_unavailable_AUCIFO") else "LAMZ: fewer_than_3_positive_post_cmax",
    n_missing_time = 0L, n_missing_concentration = 0L, tlast_quant = 8, predose_qc = NULL, dose_review = NULL))
  d <- data.frame(ID = ids, PARAMCD = "AUCIFO", AVAL = NA_real_, PARAM_STATUS = "not_calculable", PROFILE_STATUS = "partial",
    NCA_EXCLUDE = c("span_below_threshold;dependency_unavailable_AUCIFO", "span_below_threshold;dependency_unavailable_AUCIFO", "fewer_than_3_positive_post_cmax"),
    NCA_WARNING = "", stringsAsFactors = FALSE)
  raw <- c("Profile 1 terminal: span_below_threshold", "", NA_character_)
  value <- list(data = d, warnings = raw, trace = list(output_key_mapping = list(ID = "ID"), groups = groups, warnings = raw))
  original <- serialize(value, NULL)
  context <- core$desktop_nca_warning_context(value)
  expect_false(anyNA(context$remaining))
  expect_false(any(context$remaining == ""))
  html <- as.character(core$desktop_nca_warnings(value, context))
  doc <- xml2::read_html(html)
  groups_xml <- xml2::xml_find_all(doc, '//details[contains(@class,"pk-warning-group")]')
  span <- Filter(function(x) grepl("Terminal-phase span", as.character(xml2::xml_find_first(x, "./summary")), fixed = TRUE), as.list(groups_xml))
  expect_length(span, 1L)
  expect_equal(length(xml2::xml_find_all(span[[1]], './/div[contains(@class,"pk-profile-warning")]')), 2L)
  body <- xml2::xml_text(xml2::xml_find_all(span[[1]], './/div[contains(@class,"pk-profile-warning")]'))
  expect_false(any(grepl("終末相の区間幅", body, fixed = TRUE)))
  expect_true(all(grepl("LAMZ", body, fixed = TRUE)))
  expect_match(as.character(span[[1]]), "pk-warning-count", fixed = TRUE)
  # A terminal marker is retained where the same cause has different contexts.
  expect_equal(sum(grepl("終末相", body, fixed = TRUE)), 1L)
  all_terminal <- value
  all_terminal$trace$warnings <- c("Profile 1 terminal: span_below_threshold", "Profile 2 terminal: span_below_threshold")
  terminal_html <- as.character(core$desktop_nca_warnings(all_terminal))
  expect_match(terminal_html, "Includes terminal phase", fixed = TRUE)
  first <- as.character(core$desktop_nca_selected_warnings(value, 1L, context))
  third <- as.character(core$desktop_nca_selected_warnings(value, 3L, context))
  expect_match(first, "ID=S01", fixed = TRUE)
  expect_match(first, "Terminal-phase span", fixed = TRUE)
  expect_match(first, "No acceptable positive AUCinf", fixed = TRUE)
  expect_match(third, "ID=S03", fixed = TRUE)
  expect_match(third, "Fewer than three positive", fixed = TRUE)
  expect_false(grepl("Terminal-phase span", third, fixed = TRUE))
  expect_identical(serialize(value, NULL), original)
  healthy <- value
  healthy$trace$groups[[3]]$warning <- character(); healthy$trace$groups[[3]]$reason <- character()
  healthy$data$PROFILE_STATUS[3] <- "ok"; healthy$data$PARAM_STATUS[3] <- "calculated"
  healthy$data$NCA_EXCLUDE[3] <- NA_character_; healthy$data$AVAL[3] <- 5
  expect_null(core$desktop_nca_selected_warnings(healthy, 3L))
  unknown <- value
  unknown$trace$groups[[3]]$reason <- "Future diagnostic <script>x</script>"
  unknown_html <- as.character(core$desktop_nca_selected_warnings(unknown, 3L))
  expect_match(unknown_html, "Future diagnostic &lt;script&gt;x&lt;/script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", unknown_html, fixed = TRUE))
})

test_that("selection changes the nearby warning without changing NCA or revision", {
  project <- tempfile(); dir.create(project); on.exit(unlink(project, recursive = TRUE), add = TRUE)
  e <- core$desktop_nca_engine(project)
  data <- data.frame(ID = rep(c("S01", "S02"), each = 4), TIME = rep(c(0, 1, 2, 4), 2), CONC = rep(c(0, 10, 5, 2), 2), UNIT = "mg/L")
  source <- core$desktop_table_save(e, "profiles", list(source = data), list(kind = "prepare", steps = list()))$id
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    session$setInputs(source = source, id = "ID", time = "TIME", concentration = "CONC", groups = character(), unit_column = "UNIT", time_unit = "h", time_origin = 0, start = 0, last_observation = TRUE, c0_method = "none")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    modified <- current()
    modified$value$trace$groups[[1]]$warning <- "CMAX: span_below_threshold"
    modified$value$trace$groups[[1]]$reason <- character()
    modified$value$trace$groups[[2]]$warning <- "CMAX: fewer_than_3_positive_post_cmax"
    modified$value$trace$groups[[2]]$reason <- character()
    modified$value$trace$warnings <- character(); modified$value$warnings <- character()
    current(modified)
    before <- serialize(result()$value, NULL); revision <- e$nca$revision
    session$setInputs(profile = "1")
    expect_match(output$selected_warning$html, "ID=S01", fixed = TRUE)
    expect_match(output$selected_warning$html, "Terminal-phase span", fixed = TRUE)
    session$setInputs(profile = "2")
    expect_match(output$selected_warning$html, "ID=S02", fixed = TRUE)
    expect_match(output$selected_warning$html, "Fewer than three positive", fixed = TRUE)
    expect_false(grepl("Terminal-phase span", output$selected_warning$html, fixed = TRUE))
    expect_identical(serialize(result()$value, NULL), before)
    expect_identical(e$nca$revision, revision)
  })
})
