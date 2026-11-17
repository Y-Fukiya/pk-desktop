source('apps/pk-desktop/pkapp.R')
library(testthat)
core <- environment(pkapp_open)

fixture <- function() {
  path <- tempfile('nca030-explore-'); dir.create(path)
  e <- core$desktop_nca_engine(path)
  times <- c(0,1,2,4,8,12,24,36,48)
  data <- do.call(rbind, lapply(seq_len(4), function(i) data.frame(ID = sprintf('S%02d',i), GROUP = if (i <= 2) 'A' else 'B', TIME = times, CONC = ifelse(times <= 1, (5+i)*times, (5+i)*exp(-.2*(times-1))), UNIT = 'mg/L')))
  input <- core$desktop_table_save(e, 'synthetic concentration', list(source=data), list(kind='prepare',steps=list()))
  spec <- list(id='ID',time='TIME',concentration='CONC',unit_column='UNIT',groups=list('GROUP'),c0_method='none',auc_method='lin up/log down',terminal_method='auto',dosing_context='single',dose_history_confirmed=TRUE)
  args <- list(source=input$id,specification=spec,base_revision=e$nca$revision,project_id=e$nca$project_id,actor='test',reason='synthetic')
  core$desktop_nca_configure(e,args)
  args$base_revision <- e$nca$revision
  result <- core$desktop_nca_run_shared(e,args)
  saved <- core$desktop_nca_dispatch(e,'nca_save_result',list(result_id=result$result_id,name='synthetic NCA',base_revision=e$nca$revision,project_id=e$nca$project_id,actor='test',reason='test'))
  list(engine=e,source=saved$id,result=result$result_id,args=list(source=saved$id,view='summary',parameter='CMAX',unit='mg/L',group='GROUP',explore_revision=e$explore_revision,explore_session=e$explore_session,base_revision=e$nca$revision,project_id=e$nca$project_id,actor='test',reason='synthetic exploration'))
}

test_that('saved NCA results can be selected without exposing values or changing the NCA', {
  f <- fixture(); e <- f$engine; on.exit(unlink(e$project,recursive=TRUE),add=TRUE)
  before <- e$nca_result$value$data
  cmax <- before[before$PARAMCD == 'CMAX', c('ID','AVAL')]
  expect_identical(cmax$ID, c('S01','S02','S03','S04'))
  expect_equal(cmax$AVAL, c(6,7,8,9))
  value <- core$desktop_nca_dispatch(e,'explore_set',f$args)
  expect_identical(value$selection$source,f$source)
  expect_false(grepl('AVAL|mean|minimum',jsonlite::toJSON(value,auto_unbox=TRUE)))
  expect_false(core$desktop_nca_disclosure(e,f$result)$allowed)
  expect_identical(e$nca_result$value$data,before)
  selected <- core$desktop_explore_subset(core$desktop_explore_table(e,f$source),value$selection)
  summary <- core$desktop_explore_summary(selected,'GROUP')
  expect_equal(summary$N,c(2,2))
  expect_equal(summary$平均,c(6.5,8.5))
  expect_equal(summary$最小,c(6,8))
  for (view in c('summary','boxplot','listing','qc')) {
    f$args$view <- view; f$args$explore_revision <- e$explore_revision
    expect_identical(core$desktop_nca_dispatch(e,'explore_set',f$args)$selection$view,view)
  }
  f$args$explore_revision <- e$explore_revision
  invalid <- f$args; invalid$parameter <- 'NOT_PRESENT'
  expect_error(core$desktop_explore_set(e,invalid),'パラメータと単位')
  wrong <- f$args; wrong$base_revision <- 0
  expect_error(core$desktop_explore_set(e,wrong),'Settings changed')
  wrong <- f$args; wrong$source <- '../source'
  expect_error(core$desktop_explore_set(e,wrong))
  expect_identical(e$explore$view,'qc')
})

test_that('group missing values stay distinct in summary', {
  d <- data.frame(G=c(NA,'(欠測)','A','A'),AVAL=c(10,20,1,NA))
  x <- core$desktop_explore_summary(d,'G')
  expect_equal(x$N,c(1,1,1,0)); expect_equal(x$平均,c(10,20,1,NA)); expect_equal(x[['欠測・非有限']],c(0,0,0,1))
})

test_that('mixed parameters and units are never pooled', {
  d <- data.frame(PARAMCD=c('CMAX','CMAX','CMAX','AUCLST'),AVALU=c('mg/L','mg/L','ng/mL','mg/L'),AVAL=c(1,2,3000,99))
  selected <- core$desktop_explore_subset(d,list(parameter='CMAX',unit='mg/L'))
  expect_equal(selected$AVAL,c(1,2))
  expect_equal(core$desktop_explore_summary(selected)$平均,1.5)
})

test_that('UI shows the AI-selected saved source and produces local summaries', {
  f <- fixture(); e <- f$engine; on.exit(unlink(e$project,recursive=TRUE),add=TRUE)
  core$desktop_explore_set(e,f$args)
  shiny::testServer(core$desktop_explore_server,args=list(engine=e),{
    expect_match(output$active$html,'synthetic NCA',fixed=TRUE)
    expect_false(grepl(f$source,output$active$html,fixed=TRUE))
    expect_match(output$active$html,'CMAX',fixed=TRUE)
    expect_match(output$table$html,'6.5',fixed=TRUE)
    expect_match(output$table$html,'8.5',fixed=TRUE)
    x <- f$args; x$view <- 'qc'; x$explore_revision <- e$explore_revision; core$desktop_explore_set(e,x)
    session$flushReact()
    expect_match(output$table$html,'警告・算出不可の記録はありません',fixed=TRUE)
  })
})

test_that('registered teal modules mount only vetted definitions on saved data', {
  f <- fixture(); e <- f$engine; on.exit(unlink(e$project,recursive=TRUE),add=TRUE)
  data <- data.frame(TIME=0:3,CONC=c(0,6,3,1))
  filtered <- core$desktop_table_save(e,'synthetic CMAX only',list(source=data),list(kind='prepare',steps=list()))
  module_args <- list(module='pk_distribution',source=filtered$id,label='CMAX distribution',options=list(x='CONC',bins=10),base_revision=e$nca$revision,project_id=e$nca$project_id,modules_revision=e$modules$revision,actor='test',reason='synthetic module')
  catalog <- core$desktop_nca_dispatch(e,'modules')
  expect_length(catalog$catalog,4)
  before <- e$nca_result$value$data
  if (!requireNamespace('teal',quietly=TRUE) || !requireNamespace('teal.data',quietly=TRUE)) {
    expect_true(all(!vapply(catalog$catalog, function(x) x$available, logical(1))))
    expect_error(core$desktop_nca_dispatch(e,'add_module',module_args),'optional teal')
    expect_length(e$modules$entries,0)
  } else {
    added <- core$desktop_nca_dispatch(e,'add_module',module_args)
    expect_identical(added$entry$source,filtered$id)
    expect_identical(added$entry$module,'pk_distribution')
    expect_equal(added$entry$input_rows,4)
    expect_equal(bitwAnd(as.integer(file.info(e$modules_dir)$mode),63L),0L)
    expect_equal(bitwAnd(as.integer(file.info(file.path(e$modules_dir,added$entry$id,'data.rds'))$mode),63L),0L)
    bad <- module_args; bad$modules_revision <- added$revision; bad$options <- list(x='CONC',code='system()')
    expect_error(core$desktop_nca_dispatch(e,'add_module',bad),'未対応')
    bad <- module_args; bad$modules_revision <- added$revision; bad$source <- 'ADNCA'
    expect_error(core$desktop_nca_dispatch(e,'add_module',bad),'saved work table')
    removed <- core$desktop_nca_dispatch(e,'remove_module',list(id=added$entry$id,modules_revision=added$revision,base_revision=e$nca$revision,project_id=e$nca$project_id,actor='test',reason='test'))
    expect_false(removed$entry$enabled)
  }
  expect_identical(e$nca_result$value$data,before)
  expect_false(core$desktop_nca_disclosure(e,f$result)$allowed)
})
