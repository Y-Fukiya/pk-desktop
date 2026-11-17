source('apps/pk-desktop/tests/nca018_ui.R')
test_that('user execution scrolls only after successful jobs', {
  f <- nca018_fixture(); e <- f$engine
  on.exit(core$desktop_nca_jobs_close(e),add=TRUE)
  shiny::testServer(core$desktop_nca_server,args=list(engine=e),{
    sent <- list()
    parent_session <- get("parent", envir=session)
    parent_session$sendCustomMessage <- function(type,message) { if (identical(type,'pk-nca-results-scroll')) sent[[length(sent)+1L]] <<- message }
    nca018_default(session,f$source)
    session$setInputs(c0_method='')
    nca_confirm_displayed(session, input, output)
    session$setInputs(run=1)
    session$flushReact(); expect_length(sent,0)
    session$setInputs(c0_method='none')
    nca_confirm_displayed(session, input, output)
    session$setInputs(run=2)
    nca018_wait(e,session)
    expect_length(sent,1); expect_identical(sent[[1]]$id,session$ns('results_anchor'))
    expect_true(is.data.frame(result()$value$data))
    session$setInputs(auc_method='lin up/log down'); session$flushReact(); expect_length(sent,1)
    nca_confirm_displayed(session, input, output)
    session$setInputs(run=3); nca018_wait(e,session); expect_length(sent,2)
  })
})
