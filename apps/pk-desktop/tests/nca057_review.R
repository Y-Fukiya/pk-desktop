source("apps/pk-desktop/pkapp.R")
core <- environment(pkapp_open)

testthat::test_that("UI time conversion preserves the hour-based NCA contract", {
  testthat::expect_equal(core$desktop_nca_ui_hours(1440, "min"), 24)
  testthat::expect_equal(core$desktop_nca_ui_hours(1, "day"), 24)
  testthat::expect_equal(core$desktop_nca_ui_from_hours(24, "min"), 1440)
  testthat::expect_equal(core$desktop_nca_ui_from_hours(24, "day"), 1)
  testthat::expect_null(core$desktop_nca_ui_hours(NULL, "min"))
})

testthat::test_that("fixed options and browser labels come from one definition", {
  definitions <- core$desktop_i18n_choice_definitions()
  browser <- jsonlite::fromJSON(core$desktop_i18n_choice_json(), simplifyVector = FALSE)
  for (id in names(definitions)) {
    entries <- definitions[[id]]
    ui <- core$desktop_i18n_choices(id)
    testthat::expect_identical(unname(ui), names(entries))
    for (value in names(entries)) {
      key <- if (identical(value, "")) "__pk_empty__" else value
      testthat::expect_identical(unlist(browser[[id]][[key]], use.names = FALSE), unname(entries[[which(names(entries) == value)]]))
    }
  }
  testthat::expect_identical(unname(core$desktop_i18n_choices("nca-derived_parameters")), names(core$desktop_nca_derived_parameters()))
})

testthat::test_that("unit changes, including rapid changes, keep reference boundaries in hours", {
  project <- tempfile(); dir.create(project); on.exit(unlink(project, recursive = TRUE), add = TRUE)
  e <- core$desktop_nca_engine(project)
  data <- data.frame(ID = "001", TIME = c(0, 60, 120, 1440), CONC = c(0, 10, 6, 1), UNIT = "mg/L")
  source <- core$desktop_table_save(e, "unit test", list(source = data), list(kind = "prepare", steps = list()))$id
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    session$setInputs(source = source, id = "ID", time = "TIME", concentration = "CONC", groups = character(),
      unit_column = "UNIT", time_unit = "h", time_origin = 0, start = 0, end = 24,
      last_observation = FALSE, c0_method = "none")
    session$flushReact()
    testthat::expect_equal(specification()$end, 24)
    session$setInputs(time_unit = "min")
    session$setInputs(time_origin = 0, start = 0, end = 1440)
    session$flushReact()
    testthat::expect_equal(specification()$end, 24)
    session$setInputs(time_unit = "h")
    session$setInputs(time_unit = "day")
    session$setInputs(time_origin = 0, start = 0, end = 1)
    session$flushReact()
    testthat::expect_equal(specification()$end, 24)
  })
})

testthat::test_that("diagnostic translations do not silently translate arbitrary data cells", {
  label <- as.character(core$desktop_nca_diagnostics("fewer_than_3_positive_post_cmax"))
  testthat::expect_match(label, "Fewer than three positive", fixed = TRUE)
  unknown <- as.character(core$desktop_i18n_diagnostic("unknown_code", "未知の診断"))
  testthat::expect_match(unknown, "Untranslated diagnostic", fixed = TRUE)
  testthat::expect_match(unknown, "未知の診断", fixed = TRUE)
  data <- data.frame(STATUS = "ok", check.names = FALSE)
  generic <- as.character(core$desktop_display_table(data))
  testthat::expect_false(grepl("data-pk-en", generic, fixed = TRUE))
  known <- as.character(core$desktop_display_table(data.frame(PROFILE_STATUS = "ok"), bilingual_system_columns = "PROFILE_STATUS"))
  testthat::expect_match(known, 'data-pk-en="Calculated"', fixed = TRUE)
})
