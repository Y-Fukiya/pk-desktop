source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
library(testthat)
core <- environment(pkapp_open)

nca018_engine <- function() {
  path <- tempfile("nca018-ui-"); dir.create(path)
  core$desktop_nca_engine(path)
}
nca018_table <- function(engine, data, name = "input") core$desktop_table_save(engine, name,
  list(source = data), list(kind = "prepare", steps = list()))$id
nca018_args <- function(engine, source, specification) list(project_id = engine$nca$project_id,
  base_revision = engine$nca$revision, source = source, specification = specification, actor = "test MCP", reason = "UI integration test")
nca018_default <- function(session, source) session$setInputs(source = source, id = "ID", time = "TIME", concentration = "CONC",
  groups = character(), unit_column = "UNIT", time_unit = "h", concentration_unit = "", blq_column = "", mdv_column = "",
  scope = "all", time_origin = 0, start = 0, last_observation = TRUE, end = 24, c0_method = "none",
  predose_window_h = NA_real_, include_aucproc = FALSE, all_blq_policy = "missing", predose_qc = FALSE,
  predose_qc_threshold_pct = 5, predose_qc_window_h = NA_real_, predose_qc_lloq_source = "", predose_qc_lloq_column = "",
  predose_qc_lloq_value = NA_real_, predose_qc_supplement = FALSE, flag_convention = "strict", auc_method = "linear", blq = "exclude")
nca018_wait <- function(engine, session) {
  deadline <- Sys.time() + 30
  repeat {
    state <- core$desktop_nca_get(engine)
    if (is.null(state$job) || state$job$status != "running") break
    core$desktop_nca_poll_job(engine, state$job$job_id)
    later::run_now(.02)
    session$flushReact()
    if (Sys.time() > deadline) stop("NCA worker did not finish")
    Sys.sleep(.02)
  }
  session$flushReact()
}

nca018_fixture <- function(subjects = 1L) {
  e <- nca018_engine()
  source <- nca018_table(e, data.frame(ID = rep(sprintf("S%04d", seq_len(subjects)), each = 3), TIME = rep(0:2, subjects), CONC = rep(c(0, 8, 4), subjects), UNIT = "ng/mL"))
  spec <- list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT", c0_method = "none", auc_method = "linear")
  core$desktop_nca_configure(e, nca018_args(e, source, spec))
  list(engine = e, source = source, spec = spec)
}

test_that("launch shutdown stops its worker before releasing the project lock", {
  path <- tempfile("nca018-shutdown-"); dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  scope <- new.env(parent = core)
  events <- character(); e <- NULL; process <- NULL
  scope$desktop_nca_engine <- function(project) {
    e <<- core$desktop_nca_engine(project)
    e
  }
  scope$desktop_nca_jobs_close <- function(engine) {
    expect_true(dir.exists(file.path(path, ".pkapp-owner")))
    core$desktop_nca_jobs_close(engine)
    expect_false(process$is_alive())
    expect_identical(engine$nca_job$status, "cancelled")
    events <<- c(events, "worker_stopped")
  }
  scope$desktop_release_project <- function(owner) {
    expect_identical(tail(events, 1), "worker_stopped")
    core$desktop_release_project(owner)
    events <<- c(events, "lock_released")
  }
  launch <- core$desktop_nca_launch; environment(launch) <- scope
  launch(path, 3899, 3900, FALSE, start_api = function(...) 1,
    stop_api = function(...) events <<- c(events, "api_stopped"), run_app = function(...) {
      source <- nca018_table(e, data.frame(ID = "S01", TIME = 0:2, CONC = c(0, 8, 4)))
      spec <- list(id = "ID", time = "TIME", concentration = "CONC", c0_method = "none", concentration_unit = "ng/mL")
      core$desktop_nca_configure(e, nca018_args(e, source, spec))
      core$desktop_nca_start_job(e, nca018_args(e, source, spec))
      process <<- e$nca_worker$process
    })
  expect_identical(events, c("api_stopped", "worker_stopped", "lock_released"))
  expect_false(dir.exists(file.path(path, ".pkapp-owner")))
  owner <- core$desktop_acquire_project(path)
  core$desktop_release_project(owner)
})

test_that("numeric disclosure requires a UI acknowledgement and is scoped to the frozen displayed result", {
  f <- nca018_fixture(); e <- f$engine
  on.exit(core$desktop_nca_jobs_close(e), add = TRUE)
  public <- core$desktop_nca_run_shared(e, nca018_args(e, f$source, f$spec))
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    nca018_default(session, f$source)
    session$setInputs(disclosure_result_id = public$result_id, disclosure_ack = FALSE)
    expect_match(output$disclosure$html, "件数・状態のみ", fixed = TRUE)
    expect_match(output$disclosure$html, "1例や絞り込み済み", fixed = TRUE)
    expect_match(output$disclosure_actions$html, 'disabled="disabled"', fixed = TRUE)
    expect_identical(result()$value, e$nca_result$value)
    session$setInputs(disclosure_allow = 1)
    expect_false(core$desktop_nca_disclosure(e, public$result_id)$allowed)
    session$setInputs(disclosure_ack = TRUE)
    expect_false(grepl('disabled="disabled"', output$disclosure_actions$html, fixed = TRUE))
    session$setInputs(disclosure_allow = 2)
    expect_true(core$desktop_nca_disclosure(e, public$result_id)$allowed)
    expect_match(output$disclosure_actions$html, "数値の返却許可を解除", fixed = TRUE)
    session$setInputs(disclosure_revoke = 1)
    expect_false(core$desktop_nca_disclosure(e, public$result_id)$allowed)

    # The confirmation was for the old ID. Neither a rerun nor a later hidden-input
    # update may silently carry the checked box onto an unseen result.
    session$setInputs(disclosure_ack = FALSE)
    session$setInputs(disclosure_ack = TRUE)
    next_result <- core$desktop_nca_run_shared(e, nca018_args(e, f$source, f$spec))
    session$flushReact()
    expect_null(acknowledged_result_id())
    session$setInputs(disclosure_allow = 3)
    expect_false(core$desktop_nca_disclosure(e, next_result$result_id)$allowed)
    session$setInputs(disclosure_result_id = next_result$result_id, disclosure_allow = 4)
    expect_false(core$desktop_nca_disclosure(e, next_result$result_id)$allowed)
    expect_match(notice(), "確認した結果が変わりました", fixed = TRUE)
    session$setInputs(disclosure_ack = FALSE)
    session$setInputs(disclosure_ack = TRUE, disclosure_allow = 5)
    expect_true(core$desktop_nca_disclosure(e, next_result$result_id)$allowed)
    core$desktop_nca_configure(e, nca018_args(e, f$source, f$spec))
    session$flushReact()
    expect_false(core$desktop_nca_disclosure(e, next_result$result_id)$allowed)
    expect_match(output$disclosure$html, "件数・状態のみ", fixed = TRUE)
  })
})

test_that("the local change history distinguishes identity sources and safely escapes exact differences", {
  f <- nca018_fixture(); e <- f$engine
  a <- nca018_args(e, f$source, modifyList(f$spec, list(c0_method = "zero")))
  a$actor <- '<script>alert("actor")</script>'
  a$reason <- '<img src=x onerror="alert(1)">'
  a[["_mcp_client"]] <- list(name = '<b>claimed client</b>', version = "18.0-test")
  core$desktop_nca_configure(e, a)
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    nca018_default(session, f$source)
    doc <- xml2::read_html(output$shared_nca$html)
    content <- xml2::xml_text(doc)
    expect_match(gsub("[[:space:]]+", "", content), "操作経路（サーバー記録）mcp", fixed = TRUE)
    expect_match(content, "OSユーザー（サーバー取得）", fixed = TRUE)
    expect_match(content, Sys.info()[["user"]], fixed = TRUE)
    expect_match(content, "申告者", fixed = TRUE)
    expect_match(content, "初期化時の自己申告", fixed = TRUE)
    expect_match(content, a$actor, fixed = TRUE)
    expect_match(content, a$reason, fixed = TRUE)
    expect_match(content, "c0_method", fixed = TRUE)
    expect_match(content, "none", fixed = TRUE)
    expect_match(content, "zero", fixed = TRUE)
    expect_length(xml2::xml_find_all(doc, ".//script|.//img|.//b"), 0L)
    expect_match(content, "本人確認を示しません", fixed = TRUE)
    expect_true(length(xml2::xml_find_all(doc, ".//details")) >= 2L)
  })
})

test_that("busy UI state blocks duplicate operations and a real background run recovers", {
  f <- nca018_fixture(); e <- f$engine
  on.exit(core$desktop_nca_jobs_close(e), add = TRUE)
  shiny::testServer(core$desktop_nca_server, args = list(engine = e), {
    nca018_default(session, f$source)
    # Fix the supplied running state while rendering and dispatching UI events;
    # the tiny real worker can otherwise finish between two browser messages.
    e$nca_job <- list(job_id = "pending-ui-test", status = "running", channel = "mcp", declared_actor = "test MCP")
    core$desktop_nca_notify(e)
    session$flushReact()
    expect_match(output$shared_nca$html, "AIがNCAを計算中", fixed = TRUE)
    expect_match(output$run_action$html, 'disabled="disabled"', fixed = TRUE)
    session$setInputs(plot_scale = "log")
    expect_identical(input$plot_scale, "log")
    expect_true(shared_busy())
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    expect_match(notice(), "計算中", fixed = TRUE)
    expect_identical(e$nca_job$job_id, "pending-ui-test")
    e$nca_job <- NULL
    core$desktop_nca_notify(e)
    job <- core$desktop_nca_start_job(e, nca018_args(e, f$source, f$spec), "mcp")
    expect_identical(job$status, "running")
    nca018_wait(e, session)
    expect_identical(e$nca_job$status, "succeeded")
    expect_false(shared_busy())
    expect_false(grepl('disabled="disabled"', output$run_action$html, fixed = TRUE))
    expect_identical(result()$value, e$nca_result$value)
    expect_equal(result()$value$data$AVAL[result()$value$data$PARAMCD == "CMAX"], 8)
    expect_match(output$shared_nca$html, "計算済み", fixed = TRUE)

    # The worker's terminal failure is a safe state supplied by the owner; UI
    # recovery must not keep actions disabled or imply success.
    e$nca_job$status <- "failed"
    e$nca_job$error <- list(code = "E_NCA_FAILED", message = "Safe failure")
    e$nca_failed <- TRUE
    core$desktop_nca_notify(e)
    session$flushReact()
    expect_false(shared_busy())
    expect_match(output$shared_nca$html, "計算に失敗しました", fixed = TRUE)
    expect_false(grepl('disabled="disabled"', output$run_action$html, fixed = TRUE))
  })
})
