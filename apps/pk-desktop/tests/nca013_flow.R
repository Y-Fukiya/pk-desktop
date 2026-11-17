source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
library(testthat)
core <- environment(pkapp_open)
source("apps/pk-desktop/tests/helper_nca_jobs.R")
engine013 <- function() { path <- tempfile("nca013-flow-"); dir.create(path); core$desktop_nca_engine(path) }
raw_flow013 <- function(lloq = TRUE) {
  d <- data.frame(ID = "001", PCDTC = c("2026-01-01T08:45:00+09:00", "2026-01-01T09:00:00+09:00",
    "2026-01-01T10:00:00+09:00", "2026-01-01T11:00:00+09:00"), EXSTDTC = "2026-01-01T09:00:00+09:00",
    TIME = c(-.25, 0, 1, 2), CONC = c("BLQ", "0", "8", "4"), BLQ = c(1, 0, 0, 0), LIMIT = .4)
  if (lloq) d$LLOQ <- .2
  d
}
file013 <- function(data, name = "raw.csv") {
  path <- tempfile(fileext = ".csv"); core$pk_write_csv(data, path); list(datapath = path, name = name)
}
prepared_flow013 <- function() pkapp_prepare(raw_flow013(), list(list(op = "blq_rules", mode = "positional",
  profile_columns = "ID", time = "TIME", concentration = "CONC", blq_column = "BLQ", zero_time = "pre",
  pre_rule = "exclude", first_rule = "zero", middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude",
  lloq_source = "constant", lloq_value = NULL)))$data
configure013 <- function(session, data) session$setInputs(source = "FILE", file = file013(data), id = "ID", time = "TIME",
  concentration = "NCA_CONC", groups = character(), concentration_unit = "ng/mL", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV",
  c0_method = "none", predose_qc = TRUE, predose_qc_threshold_pct = 5, predose_qc_window_h = 1, name = "QC supplement")

# Server-driven bridge: obtain initial values from rendered HTML, then return actual
# sendInputMessage values as client inputs. This does not run a browser or JavaScript.
bridge013 <- function() {
  state <- new.env(parent = emptyenv()); state$pending <- list(); state$messages <- list(); state$types <- list()
  mock <- shiny::MockShinySession$new()
  clean <- function(id) sub("^.*-", "", id)
  mock$sendInputMessage <- function(id, message) {
    id <- clean(id); state$pending[[id]] <- message; state$messages[[id]] <- message
  }
  render <- function(session, html) {
    doc <- xml2::read_html(as.character(html)); values <- list()
    for (node in xml2::xml_find_all(doc, "//select | //input[@id] | //textarea[@id]")) {
      id <- clean(xml2::xml_attr(node, "id")); tag <- xml2::xml_name(node)
      type <- if (tag == "select") "select" else xml2::xml_attr(node, "type")
      state$types[[id]] <- type
      if (tag == "select") {
        options <- xml2::xml_find_all(node, ".//option[@selected]")
        if (!length(options) && is.na(xml2::xml_attr(node, "multiple"))) options <- head(xml2::xml_find_all(node, ".//option"), 1L)
        values[id] <- list(xml2::xml_attr(options, "value"))
      } else if (identical(type, "checkbox")) values[id] <- list(!is.na(xml2::xml_attr(node, "checked"))) else if (identical(type, "number")) {
        values[id] <- list(suppressWarnings(as.numeric(xml2::xml_attr(node, "value"))))
      } else if (tag == "textarea") values[id] <- list(xml2::xml_text(node)) else if (identical(type, "text")) values[id] <- list(xml2::xml_attr(node, "value"))
    }
    if (length(values)) do.call(session$setInputs, values)
  }
  flush <- function(session) {
    for (i in seq_len(20L)) {
      messages <- state$pending; state$pending <- list(); values <- list()
      for (id in names(messages)) if ("value" %in% names(messages[[id]])) {
        value <- messages[[id]]$value
        if (identical(state$types[[id]], "number")) value <- suppressWarnings(as.numeric(value))
        if (identical(state$types[[id]], "select") && is.null(value)) value <- character()
        if (!identical(shiny::isolate(session$input[[id]]), value)) values[id] <- list(value)
      }
      if (!length(values)) return(invisible(NULL))
      do.call(session$setInputs, values)
    }
    stop("server input updates did not converge")
  }
  list(session = mock, render = render, flush = flush, state = state)
}

test_that("server-driven load elapsed BLQ save NCA uses rendered LLOQ defaults without manual LLOQ input", {
  engine <- engine013(); bridge <- bridge013(); completed <- new.env()
  shiny::testServer(core$desktop_workbench_server, args = list(engine = engine), session = bridge$session, {
    bridge$render(session, core$desktop_workbench_ui("flow"))
    expect_identical(input$lloq_source, "constant")
    session$setInputs(source = "FILE", file = file013(raw_flow013()), load_source = 1)
    bridge$flush(session)
    expect_identical(input$lloq_source, "column"); expect_identical(input$lloq_column, "LLOQ")
    expect_identical(bridge$state$messages$lloq_source$value, "column")
    session$setInputs(op = "elapsed_time"); bridge$flush(session)
    expect_identical(input$sample_column, "PCDTC"); expect_identical(input$dose_column, "EXSTDTC")
    session$setInputs(apply_step_body = 1); bridge$flush(session)
    expect_length(steps(), 1); expect_equal(result()$data$NCA_TIME_H, c(-.25, 0, 1, 2))
    session$setInputs(op = "blq_rules", pre_rule = "exclude", first_rule = "zero", middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude")
    bridge$flush(session)
    expect_identical(input$blq_time, "NCA_TIME_H")
    expect_identical(form()$lloq_source, "column"); expect_identical(form()$lloq_column, "LLOQ")
    expect_false(inherits(draft_preview(), "pk_draft_error"))
    session$setInputs(apply_step_body = 2); bridge$flush(session)
    expect_length(steps(), 2); expect_false(dirty())
    context <- core$desktop_pk_blq_context(result()$data)
    expect_identical(context$step$lloq_source, "column"); expect_identical(context$step$lloq_column, "LLOQ")
    session$setInputs(save_result = 1)
    expect_true(is_saved()); completed$id <- receipt()$id; completed$data <- result()$data
    expect_identical(pkapp_replay(core$desktop_table_path(engine, completed$id))$data, completed$data)
  })
  bridge <- bridge013()
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), session = bridge$session, {
    bridge$render(session, core$desktop_nca_ui("flow")); bridge$flush(session)
    session$setInputs(source = completed$id)
    bridge$render(session, output$mapping$html); bridge$flush(session)
    bridge$render(session, output$predose_qc_lloq_controls$html); bridge$flush(session)
    expect_identical(input$concentration, "NCA_CONC"); expect_identical(input$time, "NCA_TIME_H")
    expect_false(supplement_enabled())
    session$setInputs(concentration_unit = "ng/mL", c0_method = "none", predose_qc = TRUE, predose_qc_window_h = 1)
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    if (is.null(result())) stop(paste(notice(), core$desktop_json(specification())))
    q <- result()$value$trace$groups[[1]]$predose_qc
    expect_identical(q$status, "pass_blq"); expect_identical(q$lloq_source, "column:LLOQ")
    expect_equal(q$pct_upper_bound, 2.5); expect_equal(q$source_row, 1L)
    expect_equal(result()$value$data$AVAL[1], 8)
    expect_identical(core$desktop_table_get(engine, completed$id), completed$data)
    session$setInputs(save = 1)
    expect_identical(pkapp_replay(core$desktop_table_path(engine, saved()$id))$data, result()$value$data)
  })
})

test_that("fresh source without LLOQ defaults to blank constant and clears stale values", {
  bridge <- bridge013()
  shiny::testServer(core$desktop_workbench_server, args = list(engine = engine013()), session = bridge$session, {
    bridge$render(session, core$desktop_workbench_ui("flow"))
    session$setInputs(source = "FILE", file = file013(raw_flow013(FALSE)), lloq_source = "column", lloq_column = "LIMIT", lloq_value = 999, load_source = 1)
    bridge$flush(session)
    expect_identical(input$lloq_source, "constant"); expect_true(is.na(input$lloq_value))
    expect_false("LLOQ" %in% names(result()$data))
  })
})

test_that("explicit preparation LLOQ survives schema refresh and saved step editing", {
  bridge <- bridge013()
  shiny::testServer(core$desktop_workbench_server, args = list(engine = engine013()), session = bridge$session, {
    bridge$render(session, core$desktop_workbench_ui("flow"))
    session$setInputs(source = "FILE", file = file013(raw_flow013()), load_source = 1); bridge$flush(session)
    session$setInputs(op = "blq_rules", lloq_source = "constant", lloq_value = .4, pre_rule = "exclude", first_rule = "zero", middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude")
    bridge$flush(session)
    refreshed <- result()$data; refreshed$EXTRA <- 1; base(refreshed); session$flushReact(); bridge$flush(session)
    expect_identical(input$lloq_source, "constant"); expect_equal(input$lloq_value, .4)
    session$setInputs(apply_step_body = 1); bridge$flush(session)
    expect_length(steps(), 1)
    expect_identical(input$lloq_source, "column"); expect_true(is.na(input$lloq_value))
    session$setInputs(edit_step_index = "1", edit_step = 1); bridge$flush(session)
    expect_identical(input$lloq_source, "constant"); expect_equal(input$lloq_value, .4)
    expect_identical(form()$lloq_source, steps()[[1]]$lloq_source)
    session$setInputs(discard_draft_body = 1); bridge$flush(session)
    expect_identical(input$lloq_source, "column"); expect_identical(input$lloq_column, "LLOQ")
  })
})

test_that("explicit preparation column choice survives refresh and edit restoration", {
  bridge <- bridge013()
  shiny::testServer(core$desktop_workbench_server, args = list(engine = engine013()), session = bridge$session, {
    bridge$render(session, core$desktop_workbench_ui("flow"))
    session$setInputs(source = "FILE", file = file013(raw_flow013()), load_source = 1); bridge$flush(session)
    session$setInputs(op = "blq_rules", lloq_column = "LIMIT", pre_rule = "exclude", first_rule = "zero", middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude")
    refreshed <- result()$data; refreshed$EXTRA <- 1; base(refreshed); session$flushReact(); bridge$flush(session)
    expect_identical(input$lloq_source, "column"); expect_identical(input$lloq_column, "LIMIT")
    session$setInputs(apply_step_body = 1); bridge$flush(session)
    session$setInputs(edit_step_index = "1", edit_step = 1); bridge$flush(session)
    expect_identical(input$lloq_source, "column"); expect_identical(input$lloq_column, "LIMIT")
  })
})

test_that("prepared supplement requires opt-in and invalidates saved result on changes", {
  engine <- engine013(); bridge <- bridge013(); data <- prepared_flow013()
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine), session = bridge$session, {
    configure013(session, data)
    session$setInputs(predose_qc_lloq_source = "column", predose_qc_lloq_column = "LIMIT")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    expect_false(supplement_enabled()); expect_identical(specification()$predose_qc_lloq_source, "")
    expect_identical(result()$value$trace$groups[[1]]$predose_qc$reason, "missing_lloq")
    before <- result()$value; old_fingerprint <- fingerprint()
    session$setInputs(predose_qc_supplement = TRUE)
    expect_true(supplement_enabled()); expect_null(result()); expect_false(identical(fingerprint(), old_fingerprint))
    expect_false(grepl('value="LIMIT"', bridge$state$messages$time$options, fixed = TRUE))
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 2)
    nca_wait_for_job(engine, session)
    expect_identical(result()$value$trace$groups[[1]]$predose_qc$lloq_source, "qc_supplement:column:LIMIT")
    expect_identical(result()$value$data$AVAL, before$data$AVAL); expect_identical(result()$value$trace$rows, before$trace$rows)
    session$setInputs(save = 1)
    target <- core$desktop_table_path(engine, saved()$id)
    expect_identical(pkapp_replay(target)$data, result()$value$data)
    recipe <- core$desktop_read(file.path(target, "recipe.json"))
    expect_identical(recipe$specification$predose_qc_lloq_column, "LIMIT")
    session$setInputs(predose_qc_lloq_source = "constant", predose_qc_lloq_value = .2)
    expect_null(result()); expect_identical(specification()$predose_qc_lloq_column, "")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 3)
    nca_wait_for_job(engine, session)
    expect_identical(result()$value$trace$groups[[1]]$predose_qc$lloq_source, "qc_supplement:constant")
    session$setInputs(predose_qc_supplement = FALSE)
    expect_null(result()); expect_identical(specification()$predose_qc_lloq_source, "")
    expect_null(specification()$predose_qc_lloq_value)
    expect_match(bridge$state$messages$time$options, 'value="LIMIT"', fixed = TRUE)
  })
})

test_that("raw-to-prepared mapping transition never activates stale raw QC values", {
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine013()), {
    configure013(session, prepared_flow013())
    session$setInputs(concentration = "CONC", predose_qc_lloq_source = "constant", predose_qc_lloq_value = .2)
    expect_identical(specification()$predose_qc_lloq_source, "constant")
    session$setInputs(concentration = "NCA_CONC")
    expect_false(supplement_enabled()); expect_identical(specification()$predose_qc_lloq_source, "")
    session$setInputs(predose_qc_supplement = TRUE)
    expect_identical(specification()$predose_qc_lloq_source, "constant")
    session$setInputs(concentration = "CONC")
    session$setInputs(concentration = "NCA_CONC")
    expect_false(supplement_enabled()); expect_identical(specification()$predose_qc_lloq_source, "")
  })
})

test_that("changing prepared source clears opt-in even before the client acknowledges reset", {
  bridge <- bridge013()
  shiny::testServer(core$desktop_nca_server, args = list(engine = engine013()), session = bridge$session, {
    configure013(session, prepared_flow013())
    session$setInputs(predose_qc_lloq_source = "constant", predose_qc_lloq_value = .2, predose_qc_supplement = TRUE)
    expect_true(supplement_enabled())
    session$setInputs(file = file013(prepared_flow013(), "second.csv"))
    expect_true(isTRUE(input$predose_qc_supplement))
    expect_false(supplement_enabled()); expect_identical(specification()$predose_qc_lloq_source, "")
    expect_identical(bridge$state$messages$predose_qc_supplement$value, FALSE)
    expect_match(output$predose_qc_lloq_controls$html, "LLOQが未指定の行", fixed = TRUE)
  })
})

test_that("opening a second dataset with the same schema initializes LLOQ again", {
  bridge <- bridge013()
  shiny::testServer(core$desktop_workbench_server, args = list(engine = engine013()), session = bridge$session, {
    bridge$render(session, core$desktop_workbench_ui("flow"))
    session$setInputs(source = "FILE", file = file013(raw_flow013()), load_source = 1); bridge$flush(session)
    session$setInputs(lloq_source = "constant", lloq_value = 999, lloq_column = "LIMIT")
    next_data <- raw_flow013(); next_data$LLOQ <- .4
    session$setInputs(file = file013(next_data, "second.csv"), load_source = 2); bridge$flush(session)
    expect_identical(input$lloq_source, "column"); expect_identical(input$lloq_column, "LLOQ")
    expect_true(is.na(input$lloq_value)); expect_equal(as.numeric(result()$data$LLOQ), rep(.4, 4))
  })
})


test_that("removing an explicitly selected LLOQ column leaves a visible unselected choice", {
  bridge <- bridge013()
  shiny::testServer(core$desktop_workbench_server, args = list(engine = engine013()), session = bridge$session, {
    bridge$render(session, core$desktop_workbench_ui("flow"))
    session$setInputs(source = "FILE", file = file013(raw_flow013()), load_source = 1); bridge$flush(session)
    session$setInputs(lloq_column = "LIMIT")
    refreshed <- result()$data; refreshed$LIMIT <- NULL; base(refreshed); session$flushReact(); bridge$flush(session)
    expect_identical(input$lloq_source, "column"); expect_identical(input$lloq_column, "")
    expect_match(bridge$state$messages$lloq_column$options, 'value="" selected', fixed = TRUE)
    expect_match(bridge$state$messages$lloq_column$options, 'value="LLOQ"', fixed = TRUE)
  })
})
