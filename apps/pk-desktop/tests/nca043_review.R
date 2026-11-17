source('apps/pk-desktop/tests/nca042_review.R')

test_that('period selection reports missing values and sorts available numeric labels', {
  p <- tempfile(); dir.create(p); on.exit(unlink(p,recursive=TRUE),add=TRUE)
  e <- core$desktop_nca_engine(p)
  d <- data.frame(ID=rep(c('a','b','c'),each=4),PERIOD=rep(c('P10','P2','P1'),each=4),TIME=rep(0:3,3),CONC=rep(c(0,8,4,2),3),UNIT='ng/mL')
  source_id <- core$desktop_table_save(e,'periods',list(source=d),list(kind='prepare',steps=list()))$id
  role <- paste0('group_role_column_',digest::digest('PERIOD',algo='sha256',serialize=FALSE))
  shiny::testServer(core$desktop_nca_server,args=list(engine=e),{
    session$setInputs(source=source_id,groups='PERIOD',id='ID',time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none')
    do.call(session$setInputs,setNames(list('period'),role));session$setInputs(manual_period_order=TRUE);session$flushReact()
    html <- output$period_order_controls$html
    expect_lt(regexpr('value="P1"',html,fixed=TRUE)[1],regexpr('value="P2"',html,fixed=TRUE)[1])
    expect_lt(regexpr('value="P2"',html,fixed=TRUE)[1],regexpr('value="P10"',html,fixed=TRUE)[1])
    expect_match(output$period_order_progress$html,'0 / 3',fixed=TRUE)
    session$setInputs(period_order_values=c('P10','P1'));session$flushReact()
    expect_match(output$period_order_progress$html,'2 / 3',fixed=TRUE)
    # Label and data value are separate DOM nodes so language switching never
    # translates the user-provided period value.
    expect_match(output$period_order_progress$html,'未選択：',fixed=TRUE)
    expect_match(output$period_order_progress$html,'P2',fixed=TRUE)
    expect_false(grepl('P1|P10',output$period_order_progress$html))
    expect_match(output$period_order_run_hint$html,'すべて選択',fixed=TRUE)
    session$setInputs(period_order_values=c('P10','P1','P2'));session$flushReact()
    expect_match(output$period_order_progress$html,'3 / 3',fixed=TRUE)
    expect_null(output$period_order_run_hint)
    expect_identical(specification()$period_order$values,as.list(c('P10','P1','P2')))
  })
})

test_that('missing machine identity cannot authorize stale owner recovery', {
  identity <- core$desktop_owner_identity(tempdir())
  identity$machine_identity <- NULL
  expect_false(core$desktop_same_host(identity))
  expect_match(core$desktop_owner_message,'機器IDを取得できない',fixed=TRUE)
})
