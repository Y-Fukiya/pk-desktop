source('apps/pk-desktop/tests/nca036_review.R')

test_that('period by treatment counts unique subjects, not analyte rows', {
 d<-expand.grid(ID=paste0('S',1:6),PERIOD=1:2,ANALYTE=c('parent','metabolite'),stringsAsFactors=FALSE)
 d$TRT<-ifelse((as.integer(sub('S','',d$ID))+d$PERIOD)%%2==0,'TEST','REF')
 d$AVAL<-1;d$AVAL[1]<-NA
 a<-core$desktop_explore_allocation(d,'ID','PERIOD','TRT')
 expect_equal(nrow(a$table),4);expect_equal(a$table$`被験者数（異なるID）`,rep(3L,4));expect_length(a$one_level,0)
 d$ID[1]<-NA;a<-core$desktop_explore_allocation(d,'ID','PERIOD','TRT')
 expect_equal(sum(a$table$`ID欠測の解析単位数`),1)
 d$TRT<-'A';a<-core$desktop_explore_allocation(d,'ID','PERIOD','TRT');expect_equal(a$one_level,'TRT')
 a<-core$desktop_explore_allocation(d,'ID','PERIOD',character());expect_true(a$no_treatment);expect_equal(nrow(a$table),2)
})

test_that('study labels prevent subject IDs in different studies being silently counted together', {
 d<-data.frame(STUDY=c('A','A','B','B'),ID='01',PERIOD=c(1,1,1,1),TRT='T',ANALYTE=c('p','m','p','m'))
 a<-core$desktop_explore_allocation(d,'ID','PERIOD','TRT','STUDY')
 expect_equal(nrow(a$table),2);expect_equal(a$table$`被験者数（異なるID）`,c(1L,1L))
})

test_that('pooled views display recorded role columns and the allocation check', {
 f<-fixture36(TRUE);on.exit(unlink(f$e$project,recursive=TRUE));core$desktop_explore_set(f$e,f$args)
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
  session$flushReact(); html<-output$allocation$html
  expect_match(html,'期×処置の確認',fixed=TRUE);expect_match(html,'PERIOD',fixed=TRUE);expect_match(html,'TRT',fixed=TRUE)
  expect_match(html,'被験者数（異なるID）',fixed=TRUE);expect_match(html,'割付表そのものではない',fixed=TRUE)
 })
})

test_that('exception input controls retain draft across unrelated changes and after apply', {
 f<-fixture36(FALSE);on.exit(unlink(f$e$project,recursive=TRUE))
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
  session$setInputs(source=f$args$source,view='summary',parameter='CMAX',unit='V:ng/mL',group='',aggregate_over='PERIOD',summary_basis='subject_geomean');session$flushReact()
  before<-output$single_controls$html
  session$setInputs(single_treatment_confirmed=TRUE,single_treatment_reason='全期同じ処置');session$flushReact()
  session$setInputs(summary_basis='analysis_unit');session$flushReact()
  expect_identical(output$single_controls$html,before);expect_identical(input$single_treatment_reason,'全期同じ処置');expect_true(input$single_treatment_confirmed)
  session$setInputs(apply=1);session$flushReact()
  expect_identical(output$single_controls$html,before);expect_equal(f$e$explore$single_treatment_reason,'全期同じ処置')
  session$setInputs(parameter='TMAX',unit='V:h');session$flushReact();session$setInputs(apply=2);session$flushReact()
  expect_equal(f$e$explore$parameter,'TMAX');expect_equal(f$e$explore$single_treatment_reason,'全期同じ処置')
  session$setInputs(source='other');session$flushReact();expect_null(confirmed_source());expect_match(notice(),'確認と理由を解除',fixed=TRUE)
 })
})

test_that('previously applied confirmation is restored only for same source', {
 f<-fixture36(FALSE);on.exit(unlink(f$e$project,recursive=TRUE));a<-f$args;a$single_treatment_confirmed<-TRUE;a$single_treatment_reason<-'確認済み'
 core$desktop_explore_set(f$e,a)
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
  session$setInputs(source=a$source);session$flushReact()
  html<-output$single_controls$html
  expect_match(html,'checked',fixed=TRUE);expect_match(html,'確認済み',fixed=TRUE);expect_equal(confirmed_source(),a$source)
  session$setInputs(source='other');session$flushReact();expect_null(confirmed_source())
  session$setInputs(source=a$source);session$flushReact()
  expect_false(grepl('確認済み',output$single_controls$html,fixed=TRUE))
 })
})

test_that('validation texts are Japanese at their source', {
 f<-fixture36(TRUE);on.exit(unlink(f$e$project,recursive=TRUE));a<-f$args
 a$aggregate_over<-list('ID');expect_error(core$desktop_explore_set(f$e,a),'解析グループ列')
 a$aggregate_over<-list('PERIOD');a$summary_basis<-'unknown';expect_error(core$desktop_explore_set(f$e,a),'要約単位')
})
