source('apps/pk-desktop/tests/nca036_review.R')

fixture38_false_role <- function() {
 p<-tempfile('pk038-');dir.create(p);e<-core$desktop_nca_engine(p)
 d<-do.call(rbind,lapply(1:6,function(i)do.call(rbind,lapply(1:2,function(period)do.call(rbind,lapply(c('PARENT','METAB'),function(analyte){
  trt<-if((i+period)%%2==0)'TEST' else 'REF';peak<-i*if(analyte=='PARENT')2 else 5
  data.frame(ID=paste0('S',i),PERIOD=period,ANALYTE=analyte,TRT=trt,TIME=0:3,CONC=c(0,peak,peak/2,peak/4),UNIT='ng/mL')
 }))))))
 spec<-list(id='ID',groups=c('PERIOD','ANALYTE'),group_roles=list(list(column='PERIOD',role='period'),list(column='ANALYTE',role='treatment')),time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none')
 res<-pkapp_nca(d,spec);saved<-core$desktop_table_save(e,'false role',list(source=d),list(kind='nca',specification=res$specification),frozen=res)
 a<-list(source=saved$id,view='summary',parameter='CMAX',unit='ng/mL',group='',aggregate_over=list('PERIOD'),scale='linear',summary_basis='subject_geomean',base_revision=e$nca$revision,project_id=e$nca$project_id,explore_revision=e$explore_revision,explore_session=e$explore_session,actor='test',reason='test')
 list(e=e,args=a)
}

fake_data <- function() {
 d<-expand.grid(ID=paste0('S',1:6),PERIOD=1:2,ANALYTE=c('PARENT','METAB'),stringsAsFactors=FALSE)
 d$TRT<-ifelse((as.integer(sub('S','',d$ID))+d$PERIOD)%%2==0,'TEST','REF');d
}

test_that('different analytes with same treatment do not create a conflict', {
 d<-fake_data();x<-core$desktop_explore_treatment_integrity(d,'ID','PERIOD','TRT')
 expect_equal(nrow(x$conflicts),0);expect_equal(x$unassessable,0)
 x<-core$desktop_explore_treatment_integrity(d,'ID','PERIOD','ANALYTE')
 expect_equal(nrow(x$conflicts),12);expect_true(all(x$conflicts$`異なる処置の組合せ数`==2))
 d$STUDY<-ifelse(d$ANALYTE=='PARENT','A','B')
 x<-core$desktop_explore_treatment_integrity(d,'ID','PERIOD','ANALYTE','STUDY');expect_equal(nrow(x$conflicts),0)
 d$ID[1]<-NA;x<-core$desktop_explore_treatment_integrity(d,'ID','PERIOD','ANALYTE','STUDY');expect_equal(x$unassessable,1)
})

test_that('role conflict refuses every pooled view without leaking IDs to MCP', {
 f<-fixture38_false_role();on.exit(unlink(f$e$project,recursive=TRUE));a<-f$args
 for(v in c('summary','boxplot','listing','qc')) {a$view<-v;a$summary_basis<-'analysis_unit';expect_error(core$desktop_explore_set(f$e,a),'同一被験者・同一期')}
 expect_equal(f$e$explore_revision,0)
 runtime<-core$desktop_r_runtime(f$e,3880,3879,'nca');body<-charToRaw(core$desktop_json(a))
 res<-core$desktop_http_app(f$e,runtime)$call(list(REQUEST_METHOD='POST',PATH_INFO='/v1/explore_set',HTTP_AUTHORIZATION=paste('Bearer',runtime$token),HTTP_X_PKAPP_CONTRACT='pkapp-nca-18',CONTENT_TYPE='application/json',CONTENT_LENGTH=as.character(length(body)),rook.input=list(read=function()body)))
 expect_match(res$body,'E_VALIDATION',fixed=TRUE);expect_match(res$body,'同一被験者・同一期',fixed=TRUE);expect_false(grepl('S1',res$body,fixed=TRUE))
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
  session$setInputs(source=a$source,aggregate_over='PERIOD');session$flushReact()
  expect_match(output$integrity_preview$html,'S1',fixed=TRUE);expect_match(output$integrity_preview$html,'複数処置',fixed=TRUE)
 })
})

test_that('matrix and sequence match 2x2 reference and preserve missing periods', {
 d<-fake_data();m<-core$desktop_explore_allocation_matrix(d,'ID','PERIOD','TRT')
 expect_equal(m$PERIOD,c(1L,2L));expect_equal(m$`処置：REF`,c(3L,3L));expect_equal(m$`処置：TEST`,c(3L,3L))
 seq<-core$desktop_explore_sequences(d,'ID','PERIOD','TRT');expect_equal(sort(seq$table$被験者数),c(3L,3L))
 expect_true(any(grepl('期1: TEST → 期2: REF',seq$table$`期順の処置`,fixed=TRUE)))
 d<-d[!(d$ID=='S1' & d$PERIOD==2),];seq<-core$desktop_explore_sequences(d,'ID','PERIOD','TRT')
 expect_true(any(grepl('未記録',seq$table$`期順の処置`,fixed=TRUE)));expect_equal(sum(seq$table$被験者数),6)
 d$PERIOD<-ifelse(d$PERIOD==1,'A','B');expect_null(core$desktop_explore_sequences(d,'ID','PERIOD','TRT')$table)
 d$PERIOD<-ifelse(d$PERIOD=='A','01','1');expect_match(core$desktop_explore_sequences(d,'ID','PERIOD','TRT')$note,'異なる期の表記')
})

test_that('matrix sorts numeric periods and sequences do not collapse sentinel values', {
 d<-data.frame(ID=c('A','B','A','B'),PERIOD=c('10','10','2','2'),TRT=c('未記録','X','X','X'))
 m<-core$desktop_explore_allocation_matrix(d,'ID','PERIOD','TRT');expect_equal(m$PERIOD,c('値：2','値：10'))
 d<-rbind(d,data.frame(ID='C',PERIOD='2',TRT='X'))
 seq<-core$desktop_explore_sequences(d,'ID','PERIOD','TRT');expect_equal(nrow(seq$table),3);expect_equal(sum(seq$table$被験者数),3)
})

test_that('controls are nested under conditional period selection without recreation', {
 f<-fixture36(FALSE);on.exit(unlink(f$e$project,recursive=TRUE))
 html<-as.character(core$desktop_explore_ui('review'))
 expect_match(html,'review-pooling_control',fixed=TRUE);expect_match(html,'review-single_controls',fixed=TRUE);expect_match(html,'data-display-if',fixed=TRUE)
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
  session$setInputs(source=f$args$source,view='summary',parameter='CMAX',unit='V:ng/mL',group='',aggregate_over='PERIOD',summary_basis='analysis_unit');session$flushReact()
  session$setInputs(single_treatment_confirmed=TRUE,single_treatment_reason='単一処置を確認');session$flushReact()
  before<-output$single_controls$html;session$setInputs(aggregate_over=character());session$flushReact();session$setInputs(aggregate_over='PERIOD');session$flushReact()
  expect_identical(output$single_controls$html,before);expect_true(input$single_treatment_confirmed);expect_equal(input$single_treatment_reason,'単一処置を確認')
 })
})

test_that('sequence counts reveal repeated treatment and 4-period patterns', {
 d<-expand.grid(ID=paste0('S',1:6),PERIOD=1:2,ANALYTE=c('PARENT','METAB'),stringsAsFactors=FALSE)
 d$TRT<-'TEST';x<-core$desktop_explore_sequences(d,'ID','PERIOD','TRT')
 expect_equal(nrow(x$table),1);expect_equal(x$table$被験者数,6);expect_match(x$table$`期順の処置`,'期1: TEST → 期2: TEST',fixed=TRUE)
 d<-expand.grid(ID=paste0('S',1:6),PERIOD=1:4,ANALYTE=c('PARENT','METAB'),stringsAsFactors=FALSE)
 d$TRT<-ifelse((as.integer(sub('S','',d$ID))+d$PERIOD)%%2==0,'TEST','REF')
 x<-core$desktop_explore_sequences(d,'ID','PERIOD','TRT');expect_equal(sort(x$table$被験者数),c(3L,3L))
 m<-core$desktop_explore_allocation_matrix(d,'ID','PERIOD','TRT');expect_equal(m$PERIOD,1:4);expect_equal(m$`処置：TEST`,rep(3L,4));expect_equal(m$`処置：REF`,rep(3L,4))
})

test_that('contradictions are inspected across parameter and unit rows', {
 d<-data.frame(ID=c('01','01'),PERIOD=c(1,1),TRT=c('T','R'),PARAMCD=c('CMAX','AUCLST'),AVALU=c('ng/mL','ng*h/mL'))
 x<-core$desktop_explore_treatment_integrity(d,'ID','PERIOD','TRT');expect_equal(nrow(x$conflicts),1)
})
