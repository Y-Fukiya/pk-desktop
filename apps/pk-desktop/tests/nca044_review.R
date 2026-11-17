source('apps/pk-desktop/tests/nca043_review.R')
source("apps/pk-desktop/tests/helper_nca_review.R")

test_that('incomplete period selection blocks NCA and shows missing values in choice order', {
 p <- tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE);e<-core$desktop_nca_engine(p)
 d<-data.frame(ID=rep(letters[1:4],each=4),PERIOD=rep(c('P10','P2','P4','P1'),each=4),TIME=rep(0:3,4),CONC=rep(c(0,8,4,2),4),UNIT='ng/mL')
 source_id<-core$desktop_table_save(e,'period test',list(source=d),list(kind='prepare',steps=list()))$id
 role<-paste0('group_role_column_',digest::digest('PERIOD',algo='sha256',serialize=FALSE))
 shiny::testServer(core$desktop_nca_server,args=list(engine=e),{
  session$setInputs(source=source_id,groups='PERIOD',id='ID',time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none')
  do.call(session$setInputs,setNames(list('period'),role));session$flushReact()
  nca_confirm_displayed(session, input, output)
  expect_false(grepl('disabled=',output$run_action$html,fixed=TRUE))
  session$setInputs(manual_period_order=TRUE,period_order_values=c('P1','P4'));session$flushReact()
  expect_match(output$period_order_progress$html,'未選択：',fixed=TRUE)
  expect_match(output$period_order_progress$html,'P2、P10',fixed=TRUE)
  expect_match(output$period_order_progress$html,'pk-period-warning',fixed=TRUE)
  expect_match(output$period_order_run_hint$html,'pk-period-warning',fixed=TRUE)
  expect_match(output$run_action$html,'disabled=',fixed=TRUE)
  session$setInputs(period_order_values=c('P1','P4','P2','P10'));session$flushReact()
  expect_false(grepl('disabled=',output$run_action$html,fixed=TRUE))
  expect_null(output$period_order_run_hint)
  expect_identical(specification()$period_order$values,as.list(c('P1','P4','P2','P10')))
  session$setInputs(period_order_values=c('P1','P4'));session$flushReact()
  expect_match(output$run_action$html,'disabled=',fixed=TRUE)
  session$setInputs(manual_period_order=FALSE);session$flushReact()
  expect_false(grepl('disabled=',output$run_action$html,fixed=TRUE))
 })
})

test_that('warning precedes action controls and the color has sufficient contrast', {
 html <- as.character(core$desktop_nca_ui('x'))
 run <- regexpr('id="x-run_action"',html,fixed=TRUE)[1]
 actions <- regexpr('id="x-actions"',html,fixed=TRUE)[1]
 hint <- regexpr('id="x-period_order_run_hint"',html,fixed=TRUE)[1]
 expect_gt(hint,0);expect_lt(hint,run);expect_lt(run,actions)
 css <- paste(readLines('apps/pk-desktop/R/workbench_styles.R',warn=FALSE),collapse='\n')
 expect_match(css,'.pk-workspace.pk-nca .pk-period-warning { color:#8a5a00; }',fixed=TRUE)
 expect_match(css,'.pk-period-run-hint { display:block; flex:1 0 100%',fixed=TRUE)
 rgb <- c(138,90,0)/255
 linear <- ifelse(rgb <= .04045,rgb/12.92,((rgb+.055)/1.055)^2.4)
 luminance <- sum(linear*c(.2126,.7152,.0722))
 expect_gte(1.05/(luminance+.05),4.5)
})

test_that('initial button binding and button counter resets do not start a calculation', {
 p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE);e<-core$desktop_nca_engine(p)
 shiny::testServer(core$desktop_nca_server,args=list(engine=e),{
  session$setInputs(run=0,shared_publish=0);session$flushReact()
  expect_match(notice(),'入力データと計算条件を選び',fixed=TRUE)
  session$setInputs(run=1);session$flushReact();expect_match(notice(),'計算できませんでした',fixed=TRUE)
  notice('sentinel');session$setInputs(run=0);session$flushReact();expect_identical(notice(),'sentinel')
  session$setInputs(shared_publish=1);session$flushReact();expect_match(notice(),'計算できませんでした',fixed=TRUE)
  notice('sentinel');session$setInputs(run=1);session$flushReact();expect_match(notice(),'計算できませんでした',fixed=TRUE)
  notice('sentinel');session$setInputs(run=0);session$flushReact();expect_identical(notice(),'sentinel')
 })
})
