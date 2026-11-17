source('apps/pk-desktop/tests/nca038_review.R')

test_that('unmarked Japanese text sorts in allocation and hashing without changing known UTF8 hashes', {
 d<-fake_data();d$TRT<-ifelse(d$TRT=='TEST','試験','対照');Encoding(d$TRT)<-'unknown'
 m<-core$desktop_explore_allocation_matrix(d,'ID','PERIOD','TRT')
 expect_equal(m[['処置：試験']],c(3L,3L));expect_equal(m[['処置：対照']],c(3L,3L))
 x<-list(a=1,b=2); names(x)<-c('試験','対照'); Encoding(names(x))<-'unknown'
 y<-x;Encoding(names(y))<-'UTF-8'; expect_identical(core$desktop_hash(x),core$desktop_hash(y))
 legacy<-function(z){if(is.list(z)){if(!is.null(names(z)))z<-z[order(names(z),method='radix')];z<-lapply(z,legacy)};z}
 expect_identical(core$desktop_hash(y),digest::digest(core$desktop_json(legacy(y)),algo='sha256',serialize=FALSE))
})

test_that('labelled periods are ordered only when the pattern and ranks are unambiguous', {
 for(pattern in c('P%s','Period %s','第%s期')) {
  d<-fake_data();d$PERIOD<-sprintf(pattern,d$PERIOD)
  z<-core$desktop_explore_sequences(d,'ID','PERIOD','TRT')
  expect_equal(sort(z$table$被験者数),c(3L,3L));expect_match(z$note,'抽出した数字順',fixed=TRUE)
  expect_true(any(grepl(sprintf(pattern,'1'),z$table$`期順の処置`,fixed=TRUE)))
 }
 d<-data.frame(ID=rep('S1',3),PERIOD=c('P10','P2','P1'),TRT=c('T','R','T'))
 expect_equal(core$desktop_explore_allocation_matrix(d,'ID','PERIOD','TRT')$PERIOD,c('値：P1','値：P2','値：P10'))
 for(labels in list(c('P1','Q2'),c('P01','P1'),c('Day1Per2','Day2Per3'),c('P-1','P-2'),c('A','B'))) {
  d<-data.frame(ID=c('S1','S1'),PERIOD=labels,TRT=c('T','R'))
  expect_null(core$desktop_explore_sequences(d,'ID','PERIOD','TRT')$table)
 }
})

test_that('only the local conflict preview contains treatment values and disables apply', {
 f<-fixture38_false_role();on.exit(unlink(f$e$project,recursive=TRUE));a<-f$args
 check<-core$desktop_explore_treatment_integrity(core$desktop_explore_context(f$e,a$source)$data,'ID','PERIOD','ANALYTE')
 expect_true(all(grepl('PARENT',check$`処置の組合せ`,fixed=TRUE)))
 expect_true(all(grepl('METAB',check$`処置の組合せ`,fixed=TRUE)))
 a$summary_basis<-'analysis_unit'; msg<-tryCatch({core$desktop_explore_set(f$e,a);''},error=conditionMessage)
 expect_false(grepl('PARENT|METAB|S1',msg))
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
  session$setInputs(source=a$source,aggregate_over='PERIOD');session$flushReact()
  expect_match(output$integrity_preview$html,'PARENT',fixed=TRUE);expect_match(output$integrity_preview$html,'max-height:200px',fixed=TRUE)
  expect_match(output$apply_control$html,'disabled',fixed=TRUE)
  session$setInputs(aggregate_over=character());session$flushReact();expect_false(grepl('disabled',output$apply_control$html,fixed=TRUE))
 })
})

test_that('repeat note is limited to finite within-group replicated IDs', {
 d<-fake_data();d$AVAL<-seq_len(nrow(d));d$PARAM_STATUS<-'calculated';d$PROFILE_STATUS<-'ok'
 expect_false(core$desktop_explore_has_repeated(d,'TRT',c('PERIOD','ANALYTE'),'ID'))
 repeated<-d; repeated$PERIOD<-repeated$PERIOD+2; d<-rbind(d,repeated)
 expect_true(core$desktop_explore_has_repeated(d,'TRT','ANALYTE','ID'))
 d$AVAL[duplicated(d[c('ID','TRT','ANALYTE')])]<-NA_real_
 expect_false(core$desktop_explore_has_repeated(d,'TRT','ANALYTE','ID'))
})
