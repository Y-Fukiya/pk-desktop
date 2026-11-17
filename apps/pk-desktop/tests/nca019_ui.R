source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
library(testthat)
core <- environment(pkapp_open)
if (!exists("desktop_assembly_server", core, inherits = FALSE)) sys.source("apps/pk-desktop/R/assembly_ui.R", envir = core)
ui019_engine <- function() {
  engine <- new.env(parent = emptyenv())
  engine$project <- tempfile("nca019-ui-")
  dir.create(engine$project)
  core$desktop_tables_init(engine)
  engine
}
ui019_file <- function(data) {
  path <- tempfile(fileext = ".rds"); saveRDS(data, path)
  list(name = "source.rds", datapath = path)
}
ui019_data <- function() list(
  conc = data.frame(ID = "001", SAMPLE_ID = paste0("S", 0:6), ANALYTE = "parent", PERIOD = "2", CONC = 10 * exp(-.5 * (0:6))),
  pc = data.frame(ID = "001", SAMPLE_ID = paste0("S", 0:6), PERIOD = "2", PCDTC = sprintf("2026-01-02T%02d:00:00Z", 9:15)),
  ex = data.frame(ID = "001", PERIOD = "2", DOSE_ID = "D2", EXSTDTC = "2026-01-02T09:00:00Z"))
ui019_map <- function(session) session$setInputs(conc_ID = "ID", conc_SAMPLE_ID = "SAMPLE_ID", conc_ANALYTE = "ANALYTE", conc_PERIOD = "PERIOD", conc_CONC = "CONC",
  pc_ID = "ID", pc_SAMPLE_ID = "SAMPLE_ID", pc_PERIOD = "PERIOD", pc_SAMPLE_DTM = "PCDTC",
  ex_ID = "ID", ex_PERIOD = "PERIOD", ex_DOSE_ID = "DOSE_ID", ex_DOSE_DTM = "EXSTDTC",
  sample_keys = c("ID", "SAMPLE_ID"), profile_keys = c("ID", "PERIOD", "ANALYTE"), dose_group_keys = c("ID", "PERIOD"),
  dose_record_keys = c("ID", "PERIOD", "DOSE_ID"), observation_keys = c("ID", "SAMPLE_ID", "ANALYTE"),
  sample_type = "iso", dose_type = "iso", utc_offset = "+00:00", name = "Actual sample times")

test_that("assembly form exposes ordinary selectors and requires a reference choice", {
  html <- as.character(core$desktop_assembly_ui("a"))
  expect_match(html, "a-conc_file", fixed = TRUE)
  expect_match(html, "a-pc_file", fixed = TRUE)
  expect_match(html, "a-ex_file", fixed = TRUE)
  expect_match(html, "SAS7BDAT", fixed = TRUE)
  expect_match(html, 'value="" selected', fixed = TRUE)
  expect_false(grepl("textarea", html, fixed = TRUE))
  shiny::testServer(core$desktop_assembly_server, args = list(engine = ui019_engine()), {
    data <- ui019_data()
    session$setInputs(conc_source = "FILE", pc_source = "FILE", ex_source = "FILE", conc_file = ui019_file(data$conc), pc_file = ui019_file(data$pc), ex_file = ui019_file(data$ex))
    ui019_map(session)
    session$setInputs(preview = 1)
    expect_null(current())
    expect_match(notice(), "基準投与")
    session$setInputs(reference_mode = "first", preview = 2)
    expect_identical(result()$value$data$NCA_TIME_H, as.numeric(0:6))
    expect_identical(result()$value$data$NCA_CONC_ROW, 1:7)
    expect_match(output$preview_result$html, "基準投与の候補", fixed = TRUE)
    session$setInputs(dose_history_confirmed = TRUE)
    expect_null(result())
    session$setInputs(save = 1)
    expect_length(core$desktop_tables(engine), 0L)
    session$setInputs(preview = 3, save = 2)
    expect_length(core$desktop_tables(engine), 1L)
    receipt <- saved()
    expect_identical(core$desktop_table_get(engine, receipt$id), result()$value$data)
    folder <- core$desktop_table_path(engine, receipt$id)
    expect_identical(names(readRDS(file.path(folder, "inputs.rds"))), c("conc", "pc", "ex"))
    expect_identical(core$desktop_read(file.path(folder, "recipe.json"))$kind, "nca_assembly")
    expect_true(all(result()$value$data$NCA_DOSE_HISTORY_CONFIRMED))
    session$setInputs(conc_file = ui019_file(transform(data$conc, CONC = CONC * 2)))
    expect_null(result())
  })
})

test_that("assembly accepts saved sources and an explicit dose record value", {
  engine <- ui019_engine(); data <- ui019_data()
  receipts <- lapply(names(data), function(name) core$desktop_table_save(engine, name, list(source = data[[name]]), list(kind = "prepare", steps = list())))
  shiny::testServer(core$desktop_assembly_server, args = list(engine = engine), {
    session$setInputs(conc_source = receipts[[1]]$id, pc_source = receipts[[2]]$id, ex_source = receipts[[3]]$id)
    ui019_map(session)
    session$setInputs(reference_mode = "selected", reference_field = "DOSE_ID", reference_value = "D2", preview = 1)
    expect_equal(result()$value$data$NCA_TIME_H, 0:6)
    expect_identical(result()$value$specification$dose_reference$mode, "selected")
    session$setInputs(reference_value = "absent", preview = 2)
    expect_true(all(is.na(result()$value$data$NCA_TIME_H)))
    expect_true(all(result()$value$data$NCA_ASSEMBLY_STATUS == "unresolved"))
  })
})

test_that("NCA terminal settings preserve off forms and invalidate estimates when edited", {
  engine <- ui019_engine()
  data <- data.frame(ID = "001", TIME = 0:6, CONC = 10 * exp(-.5 * (0:6)))
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), {
    session$setInputs(source = "FILE", file = ui019_file(data), id = "ID", time = "TIME", concentration = "CONC", groups = character(),
      concentration_unit = "mg/L", c0_method = "none", last_observation = TRUE, terminal_method = "none", name = "Terminal results")
    expect_false("terminal_method" %in% names(specification()))
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    expect_identical(result()$value$data$PARAMCD, c("CMAX", "TMAX", "AUCLST"))
    session$setInputs(terminal_method = "auto", dosing_context = "single", dose_history_confirmed = TRUE, terminal_qc_action = "flag")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 2)
    expect_false(is.null(result()))
    expect_equal(result()$value$data$AVAL[result()$value$data$PARAMCD == "LAMZHL"], log(2) / .5)
    expect_true(any(core$desktop_nca_plot_data(result()$value, 1)$PLOT_TERMINAL))
    html <- as.character(core$desktop_nca_terminal_details(result()$value, 1))
    expect_match(html, "候補区間", fixed = TRUE)
    expect_match(html, "調整R2", fixed = TRUE)
    session$setInputs(terminal_method = "manual", terminal_start_h = 2, terminal_end_h = 6)
    expect_null(result())
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 3)
    expect_identical(result()$value$specification$terminal_start_h, 2)
    session$setInputs(terminal_end_h = 5)
    expect_null(result())
  })
})

test_that("terminal form shows hours policy and explicit confirmation", {
  html <- as.character(core$desktop_nca_ui("n"))
  for (field in names(core$desktop_nca_terminal_defaults())) expect_match(html, paste0("n-", field), fixed = TRUE)
  expect_match(html, "全解析単位に共通", fixed = TRUE)
  expect_match(html, "実測Clast", fixed = TRUE)
})

test_that("shared terminal and legacy specifications restore into controls without drift", {
  for (method in c("legacy", "manual", "none")) {
    engine <- ui019_engine(); core$desktop_nca_init(engine)
    data <- data.frame(ID = "001", TIME = 0:6, CONC = 10 * exp(-.5 * (0:6)))
    receipt <- core$desktop_table_save(engine, "source", list(source = data), list(kind = "prepare", steps = list()))
    spec <- list(id = "ID", time = "TIME", concentration = "CONC", concentration_unit = "mg/L", c0_method = "none")
    if (method != "legacy") spec <- c(spec, list(terminal_method = method, dosing_context = "single", dose_history_confirmed = TRUE,
      terminal_start_h = if (method == "manual") 1 else NULL, terminal_end_h = if (method == "manual") 6 else NULL,
      terminal_min_r_squared = .88, terminal_min_span_ratio = 1.5, aucinf_max_extrap_pct = 25, terminal_qc_action = "flag"))
    args <- list(project_id = engine$nca$project_id, base_revision = engine$nca$revision, source = receipt$id, specification = spec, actor = "tester", reason = "Restore terminal conditions")
    core$desktop_nca_configure(engine, args, "ui")
    shiny::testServer(core$desktop_nca_server, args = list(engine = engine), {
      messages <- list()
      host <- .subset2(session, "parent")
      host$sendInputMessage <- function(id, message) messages[[id]] <<- message
      session$setInputs(source = "FILE", shared_refresh = 1)
      expect_identical(messages$terminal_method$value, if (method == "legacy") "none" else method)
      expect_equal(as.numeric(messages$terminal_min_r_squared$value), if (method == "legacy") .9 else .88)
      expect_identical(messages$dose_history_confirmed$value, method != "legacy")
      values <- lapply(messages, function(message) message$value)
      values <- values[!vapply(values, is.null, logical(1))]
      numbers <- c("time_origin", "start", "end", "predose_window_h", "predose_qc_threshold_pct", "predose_qc_window_h", "predose_qc_lloq_value",
        "infusion_end_h", "terminal_start_h", "terminal_end_h", "terminal_min_r_squared", "terminal_min_span_ratio", "aucinf_max_extrap_pct")
      for (field in intersect(numbers, names(values))) values[[field]] <- suppressWarnings(as.numeric(values[[field]]))
      do.call(session$setInputs, values)
      expect_identical(core$desktop_nca_specification(source_data(), specification()), engine$nca$specification)
      if (method == "legacy") expect_false("terminal_method" %in% names(specification()))
    })
  }
})

test_that("additional key roles distinguish concentration replicates without JSON", {
  engine <- ui019_engine(); data <- ui019_data()
  data$conc$REPLICATE <- "A"
  data$conc <- rbind(data$conc, transform(data$conc, REPLICATE = "B"))
  shiny::testServer(core$desktop_assembly_server, args = list(engine = engine), {
    session$setInputs(conc_source = "FILE", pc_source = "FILE", ex_source = "FILE", conc_file = ui019_file(data$conc), pc_file = ui019_file(data$pc), ex_file = ui019_file(data$ex))
    ui019_map(session)
    session$setInputs(reference_mode = "first", preview = 1)
    expect_null(result())
    expect_match(notice(), "Duplicate CONC")
    session$setInputs(conc_add_key = 1)
    expect_match(output$conc_mapping$html, "conc_extra_1_role", fixed = TRUE)
    session$setInputs(conc_extra_1_role = "REPLICATE", conc_extra_1_column = "REPLICATE", observation_keys = c("ID", "SAMPLE_ID", "ANALYTE", "REPLICATE"), preview = 2)
    expect_identical(result()$value$specification$mapping$CONC$REPLICATE, "REPLICATE")
    expect_equal(nrow(result()$value$data), 14)
    expect_equal(result()$value$data$NCA_PC_ROW, rep(1:7, 2))
    session$setInputs(conc_extra_1_role = "ASSAY")
    expect_null(result())
  })
})

test_that("profile dose selectors require each assignment and reset when sources change", {
  engine <- ui019_engine(); data <- ui019_data()
  data$conc <- rbind(data$conc, transform(data$conc, ANALYTE = "metabolite"))
  data$pc$PCDTC <- sprintf("2026-01-02T%02d:00:00Z", 8:14)
  data$ex <- rbind(transform(data$ex, DOSE_ID = "D1", EXSTDTC = "2026-01-01T09:00:00Z"), data$ex,
    transform(data$ex, ID = "002", DOSE_ID = "OTHER"))
  shiny::testServer(core$desktop_assembly_server, args = list(engine = engine), {
    session$setInputs(conc_source = "FILE", pc_source = "FILE", ex_source = "FILE", conc_file = ui019_file(data$conc), pc_file = ui019_file(data$pc), ex_file = ui019_file(data$ex))
    ui019_map(session)
    session$setInputs(reference_mode = "per_profile", preview = 1)
    expect_null(result())
    expect_match(notice(), "基準投与を選択してください", fixed = TRUE)
    context <- reference_context()
    expect_length(context$profiles, 2L)
    expect_true(all(vapply(context$profiles, function(profile) identical(profile$candidates, 1:2), logical(1))))
    html <- output$profile_assignments$html
    expect_false(grepl("OTHER", html, fixed = TRUE))
    expect_match(html, "ANALYTE=parent", fixed = TRUE)
    expect_match(html, 'value="" selected', fixed = TRUE)
    selections <- stats::setNames(list(context$dose_record[2L], context$dose_record[1L]), vapply(context$profiles, `[[`, "", "field"))
    do.call(session$setInputs, selections)
    session$setInputs(preview = 2)
    expect_equal(result()$value$data$NCA_TIME_H, c(-1:5, 23:29))
    expect_identical(result()$value$data$NCA_EX_ROW, rep(c(2L, 1L), each = 7L))
    expect_identical(result()$value$specification$dose_reference$mode, "per_profile")
    assignments <- result()$value$specification$dose_reference$assignments
    expect_identical(assignments[[1L]]$profile$ANALYTE, "parent")
    expect_identical(assignments[[1L]]$dose$DOSE_ID, "D2")
    expect_identical(assignments[[2L]]$dose$DOSE_ID, "D1")
    session$setInputs(save = 1)
    recipe <- core$desktop_read(file.path(core$desktop_table_path(engine, saved()$id), "recipe.json"))
    expect_identical(recipe$specification$dose_reference$assignments[[2L]]$dose$DOSE_ID, "D1")
    change <- stats::setNames(list(context$dose_record[1L]), context$profiles[[1L]]$field)
    do.call(session$setInputs, change)
    expect_null(result())
    session$setInputs(preview = 3)
    expect_equal(result()$value$data$NCA_TIME_H, rep(23:29, 2L))
    old_fields <- vapply(context$profiles, `[[`, "", "field")
    session$setInputs(conc_file = ui019_file(transform(data$conc, CONC = CONC * 2)))
    expect_null(result())
    expect_false(any(vapply(reference_context()$profiles, `[[`, "", "field") %in% old_fields))
    session$setInputs(preview = 4)
    expect_null(result())
    expect_match(notice(), "基準投与を選択してください", fixed = TRUE)
  })
  html <- as.character(core$desktop_assembly_ui("a"))
  expect_match(html, 'value="per_profile"', fixed = TRUE)
  expect_false(grepl("投与群", html, fixed = TRUE))
})
