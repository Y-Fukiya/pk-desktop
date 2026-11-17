source("apps/pk-desktop/tests/nca024_core.R")
source("apps/pk-desktop/tests/helper_nca_review.R")

test_that("pre-NCA GUI saves computational roles and the saved input feeds NCA", {
  f <- fixture024(); f$data$ROUTE_ALT <- f$data$ROUTE
  project <- tempfile("nca024-ui-"); dir.create(project); on.exit(unlink(project, recursive = TRUE))
  engine <- core$desktop_nca_engine(project)
  source <- core$desktop_table_save(engine, "synthetic concentration", list(source = f$data), list(kind = "prepare", steps = list()))
  vs <- core$desktop_table_save(engine, "synthetic VS", list(source = f$vs), list(kind = "prepare", steps = list()))
  shiny::testServer(core$desktop_annotation_server, args = list(engine = engine, input_mode = TRUE), {
    session$setInputs(result_source = source$id, profile_id = "ID", profile_groups = character(), profile_study = "", profile_period = "",
      dm_source = "NONE", vs_source = vs$id, vs_key_ID = "ID", vs_scope = "subject", vs_test = "TEST", vs_value = "VALUE", vs_unit = "UNIT",
      vs_datetime = "DTC", vs_visit = "VISIT", vs_measure_WEIGHT = "WEIGHT", vs_measure_HEIGHT = "HEIGHT", vs_measure_BSA = "BSA",
      vs_selection = "all_unique", vs_baseline_confirmed = FALSE, name = "synthetic NCA input", preview = 1)
    expect_null(result())
    session$setInputs(vs_baseline_confirmed = TRUE, preview = 2)
    expect_true(!is.null(result()))
    expect_identical(result()$value$data[names(f$data)], f$data)
    expect_match(output$preview_result$html, "VS_WEIGHT", fixed = TRUE)
    expect_match(output$preview_result$html, "濃度入力行", fixed = TRUE)
    expect_match(output$preview_result$html, "日時の前後関係は未検証", fixed = TRUE)
    session$setInputs(save = 1)
    expect_true(!is.null(saved()))
    engine$test_source <- saved()$id
    expect_identical(pkapp_replay(core$desktop_table_path(engine, saved()$id))$data, result()$value$data)
    session$setInputs(vs_baseline_confirmed = FALSE)
    expect_null(result())
  })
  spec <- f$spec
  spec$body_size <- list(weight = list(value_column = "VS_WEIGHT", unit_column = "VS_WEIGHT_UNIT"), bsa = list(mode = "recorded", value_column = "VS_BSA", unit_column = "VS_BSA_UNIT"))
  args <- list(project_id = engine$nca$project_id, base_revision = engine$nca$revision, source = engine$test_source, specification = spec, actor = "tester", reason = "UI roundtrip")
  core$desktop_nca_configure(engine, args, "ui")
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), {
    messages <- list(); host <- .subset2(session, "parent")
    host$sendInputMessage <- function(id, message) messages[[id]] <<- message
    session$setInputs(source = "FILE", shared_refresh = 1)
    expect_true(messages$derived_enabled$value)
    expect_equal(messages$dose_amount$value, "DOSE")
    expect_equal(messages$dose_ev_values$value, "ORAL")
    values <- lapply(messages, function(message) message$value)
    values <- values[!vapply(values, is.null, logical(1))]
    numbers <- c("time_origin", "start", "end", "predose_window_h", "predose_qc_threshold_pct", "predose_qc_window_h", "predose_qc_lloq_value", "infusion_end_h", "terminal_start_h", "terminal_end_h", "terminal_min_r_squared", "terminal_min_span_ratio", "aucinf_max_extrap_pct")
    for (field in intersect(numbers, names(values))) values[[field]] <- suppressWarnings(as.numeric(values[[field]]))
    do.call(session$setInputs, values)
    expect_identical(core$desktop_nca_specification(source_data(), specification()), engine$nca$specification)
    expect_match(output$body_controls$html, "Mosteller", fixed = TRUE)
    nca_confirm_displayed(session, input, output)
    session$setInputs(derived_check = 1)
    expect_true(!is.null(readiness()))
    expect_equal(sum(readiness()$counts$PROFILES), 9)
    body_markup <- output$body_controls$html
    session$setInputs(body_bsa_mode = "mosteller")
    expect_equal(specification()$body_size$bsa$mode, "mosteller")
    session$setInputs(dose_route = "ROUTE_ALT")
    expect_identical(output$body_controls$html, body_markup)
    expect_identical(input$body_bsa_mode, "mosteller")
    expect_equal(specification()$body_size$bsa$mode, "mosteller")
    session$setInputs(derived_enabled = FALSE)
    expect_false(any(c("dose", "body_size", "derived_parameters") %in% names(specification())))
  })
})

test_that("input and derived controls start unconfirmed and do not demand DM/VS for base NCA", {
  html <- as.character(core$desktop_annotation_ui("covariates", TRUE))
  expect_match(html, "計算前の濃度表", fixed = TRUE)
  expect_match(html, "DM・VSなしでも通常のNCA", fixed = TRUE)
  expect_false(core$desktop_nca_derived_form(list())$derived_enabled)
  expect_false(core$desktop_nca_derived_form(list())$dose_meaning_confirmed)
  f <- fixture024(); expect_length(core$desktop_nca_derived_form_spec(list(derived_enabled = FALSE), f$data), 0)
  expect_error(core$desktop_nca_derived_form_spec(list(derived_enabled = TRUE, dose_meaning_confirmed = FALSE), f$data), "実投与量")
  for (bsa in c("recorded", "mosteller")) {
    spec <- f$spec
    if (bsa == "mosteller") spec$body_size$bsa <- list(mode = bsa, height = list(value_column = "HT", unit_column = "HT_UNIT"))
    expect_identical(core$desktop_sort(core$desktop_nca_derived_form_spec(core$desktop_nca_derived_form(spec), prepare024(f)$data)), core$desktop_sort(spec[c("derived_parameters", "dose", "body_size")]))
  }
})

test_that("assembled dose selectors expose preserved EX columns and changing uploads clears confirmations", {
  data <- assembly024(TRUE)
  html <- as.character(core$desktop_nca_derived_controls(data, list(dose_route = ""), shiny::NS("nca")))
  document <- xml2::read_html(html)
  options <- xml2::xml_attr(xml2::xml_find_all(document, './/select[@id="nca-dose_amount"]/option'), "value")
  expect_true("SOURCE_EX_DOSE" %in% options)
  expect_false("EX_DOSE" %in% options)
  expect_false("CONC" %in% options)
  project <- tempfile("nca024-upload-"); dir.create(project); on.exit(unlink(project, recursive = TRUE))
  engine <- core$desktop_nca_engine(project)
  one <- file.path(project, "one.csv"); two <- file.path(project, "two.csv")
  core$pk_write_csv(fixture024()$data, one); core$pk_write_csv(fixture024()$data, two)
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), {
    messages <- list(); host <- .subset2(session, "parent")
    host$sendInputMessage <- function(id, message) messages[[id]] <<- message
    session$setInputs(source = "FILE", file = list(datapath = one, name = "one.csv"))
    session$setInputs(derived_enabled = TRUE, dose_meaning_confirmed = TRUE, body_weight = TRUE)
    messages <- list()
    session$setInputs(file = list(datapath = two, name = "two.csv"))
    expect_false(messages$derived_enabled$value)
    expect_false(messages$dose_meaning_confirmed$value)
    expect_false(messages$body_weight$value)
  })
})
