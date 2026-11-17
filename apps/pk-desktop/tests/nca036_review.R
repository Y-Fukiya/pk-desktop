source('apps/pk-desktop/pkapp.R')
library(testthat)
core <- environment(pkapp_open)

fixture36 <- function(treatment=TRUE, missing_treatment=FALSE, add_warning=FALSE) {
 p<-tempfile('nca036-');dir.create(p);e<-core$desktop_nca_engine(p)
 d<-do.call(rbind,lapply(1:6,function(i)do.call(rbind,lapply(1:2,function(period)do.call(rbind,lapply(c('parent','metabolite'),function(analyte){
   trt<-if ((i+period)%%2==0) 'TEST' else 'REF'; peak<-i*if(analyte=='parent') 2 else 5
   data.frame(ID=paste0('S',i),PERIOD=period,ANALYTE=analyte,TRT=trt,TIME=0:3,CONC=c(0,peak,peak/2,peak/4),UNIT='ng/mL')
 }))))))
 groups<-c('PERIOD','ANALYTE',if(treatment)'TRT')
 roles<-list(list(column='PERIOD',role='period'),list(column='ANALYTE',role='analyte'))
 if(treatment)roles<-c(roles,list(list(column='TRT',role='treatment')))
 spec<-list(id='ID',groups=groups,group_roles=roles,time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none')
 res<-pkapp_nca(d,spec)
 if(missing_treatment)res$data$TRT[1]<-NA_character_
 if(add_warning) { ix<-which(res$data$PARAMCD=='CMAX' & res$data$TRT=='REF' & res$data$ANALYTE=='parent')[1];res$data$NCA_WARNING[ix]<-'合成警告' }
 saved<-core$desktop_table_save(e,'synthetic',list(source=d),list(kind='nca',specification=res$specification),frozen=res)
 args<-list(source=saved$id,view='summary',parameter='CMAX',unit='ng/mL',group=if(treatment)'TRT' else '',aggregate_over=list('PERIOD'),scale='linear',summary_basis='subject_geomean',base_revision=e$nca$revision,project_id=e$nca$project_id,explore_revision=e$explore_revision,explore_session=e$explore_session,actor='review',reason='test')
 list(e=e,args=args)
}

test_that('period pooling requires treatment role or explicit single-treatment confirmation and reason', {
 f<-fixture36(FALSE);on.exit(unlink(f$e$project,recursive=TRUE));e<-f$e;a<-f$args
 for (v in c('summary','boxplot','listing','qc')) {
   a$view<-v;a$summary_basis<-'analysis_unit';expect_error(core$desktop_explore_set(e,a),'処置の役割')
 }
 a$view<-'summary';a$summary_basis<-'subject_geomean'
 a$single_treatment_confirmed<-TRUE;expect_error(core$desktop_explore_set(e,a),'理由')
 a$single_treatment_confirmed<-FALSE;a$single_treatment_reason<-'単一処置';expect_error(core$desktop_explore_set(e,a),'確認欄')
 a$single_treatment_confirmed<-TRUE;a$single_treatment_reason<-'  ';expect_error(core$desktop_explore_set(e,a),'理由')
 expect_equal(e$explore_revision,0)
 a$single_treatment_reason<-'全期同じ処置であることを確認';chosen<-core$desktop_explore_set(e,a)
 expect_true(chosen$selection$single_treatment_confirmed)
 expect_equal(chosen$selection$single_treatment_reason,a$single_treatment_reason)
 event<-tail(e$nca$audit,1)[[1]];expect_equal(event$exploration$single_treatment_reason,a$single_treatment_reason)
})

test_that('retained treatment values must be present', {
 f<-fixture36(TRUE,TRUE);on.exit(unlink(f$e$project,recursive=TRUE))
 expect_error(core$desktop_explore_set(f$e,f$args),'処置列に欠測')
 f$args$single_treatment_confirmed<-TRUE;f$args$single_treatment_reason<-'確認'
 expect_error(core$desktop_explore_set(f$e,f$args),'処置列に欠測')
})

test_that('one warning blocks its whole group; other treatment and analyte groups remain correct', {
 d<-expand.grid(ID=paste0('S',1:6),PERIOD=1:4,ANALYTE=c('parent','metabolite'),stringsAsFactors=FALSE)
 d$TRT<-ifelse(d$PERIOD%%2==1,'TEST','REF');d$AVAL<-as.numeric(sub('S','',d$ID))*ifelse(d$PERIOD<=2,1,4)*ifelse(d$ANALYTE=='parent',1,10)
 d$PARAMCD<-'AUCIFO';d$AVALU<-'ng*h/mL';d$PARAM_STATUS<-'calculated';d$NCA_WARNING<-NA_character_
 target<-d$ID=='S1' & d$PERIOD==2 & d$ANALYTE=='parent'
 d$NCA_WARNING[target]<-'終末相を確認';original<-d
 r<-core$desktop_explore_subject_means(d,'ID','TRT',c('ANALYTE','TRT'),c('PERIOD','ANALYTE','TRT'))
 expect_identical(d,original);expect_equal(r$blocked_groups,1);expect_equal(r$blocked_units,12);expect_equal(nrow(r$data),18)
 expect_equal(r$issues$ID,'S1');expect_equal(r$issues$PERIOD,2);expect_match(r$issues$`保留理由`,'警告付き')
 expect_false(any(r$data$ANALYTE=='parent' & r$data$TRT=='REF'))
 sum<-core$desktop_explore_summary(r$data,'TRT',c('ANALYTE','TRT'),'ID',r$counts_column)
 row<-sum[sum$ANALYTE=='値：parent' & sum$TRT=='値：TEST',]
 independent<-sqrt((1:6)*(4*(1:6)))
 expect_equal(row$`幾何平均`,exp(mean(log(independent))),tolerance=1e-12)
 expect_equal(row$`幾何CV%`,100*sqrt(exp(var(log(independent)))-1),tolerance=1e-10)
 expect_false('同一IDの複数値' %in% names(sum))
})

test_that('missing period counts are shown and problematic values never silently disappear', {
 d<-data.frame(ID=rep(c('A','B'),each=2),PERIOD=rep(1:2,2),TRT='A',PARAMCD='CMAX',AVAL=c(1,NA,4,16),PARAM_STATUS='calculated',NCA_WARNING=NA_character_)
 r<-core$desktop_explore_subject_means(d,'ID','TRT','TRT','PERIOD')
 s<-core$desktop_explore_summary(r$data,'TRT','TRT','ID',r$counts_column)
 expect_equal(s$`使用期・解析単位数`,'1–2')
 d$AVAL[1]<-0;r<-core$desktop_explore_subject_means(d,'ID','TRT','TRT','PERIOD');expect_equal(r$blocked_groups,1);expect_equal(nrow(r$data),0);expect_match(r$issues$`保留理由`,'0以下')
 d$AVAL[1]<-1;d$NCA_WARNING[2]<-'check';r<-core$desktop_explore_subject_means(d,'ID','TRT','TRT','PERIOD');expect_equal(r$blocked_groups,1)
})

test_that('error translation and table labels are understandable', {
 e<-simpleError('被験者別の幾何平均は、期をまとめた要約表でTMAX以外のパラメータを選んでください。')
 expect_match(core$desktop_explore_ui_message(e),'被験者別')
 e<-simpleError('unrecognized internal error with user value');expect_equal(core$desktop_explore_ui_message(e),'指定した条件または保存結果を確認してください。')
 html<-as.character(core$desktop_explore_display_summary(data.frame(TRT='値：REF',`欠測・非有限`=0,check.names=FALSE),'統計量の表'))
 expect_match(html,'統計量の表で0の項目',fixed=TRUE);expect_false(grepl('値：REF',html,fixed=TRUE))
})


test_that('UI identifies partial-group holds and clearly labels both summary units', {
 f<-fixture36(TRUE,FALSE,TRUE);on.exit(unlink(f$e$project,recursive=TRUE))
 core$desktop_explore_set(f$e,f$args)
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
  session$flushReact();html<-output$table$html
  for(term in c('解析単位ベースの件数','被験者ごとの平均ベースの統計量','群全体を保留','保留の原因となった行','合成警告','使用期・解析単位数'))expect_match(html,term,fixed=TRUE)
 })
})

test_that('MCP HTTP treatment error is specific without exposing input', {
 f<-fixture36(FALSE);on.exit(unlink(f$e$project,recursive=TRUE));e<-f$e
 runtime<-core$desktop_r_runtime(e,3880,3879,'nca')
 body<-charToRaw(core$desktop_json(f$args))
 response<-core$desktop_http_app(e,runtime)$call(list(REQUEST_METHOD='POST',PATH_INFO='/v1/explore_set',HTTP_AUTHORIZATION=paste('Bearer',runtime$token),HTTP_X_PKAPP_CONTRACT='pkapp-nca-18',CONTENT_TYPE='application/json',CONTENT_LENGTH=as.character(length(body)),rook.input=list(read=function()body)))
 expect_match(response$body,'E_VALIDATION',fixed=TRUE);expect_match(response$body,'処置の役割',fixed=TRUE)
 expect_false(grepl('S1',response$body,fixed=TRUE))
})

test_that('UI exception requires an explicit confirmation for the selected source', {
 f<-fixture36(FALSE);on.exit(unlink(f$e$project,recursive=TRUE))
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
  session$setInputs(source=f$args$source,view='summary',parameter='CMAX',unit='V:ng/mL',group='',aggregate_over='PERIOD',summary_basis='subject_geomean');session$flushReact()
  session$setInputs(single_treatment_confirmed=TRUE,single_treatment_reason='単一処置と確認');session$flushReact()
  expect_equal(confirmed_source(),f$args$source)
  session$setInputs(apply=1);session$flushReact()
  expect_true(isTRUE(f$e$explore$single_treatment_confirmed))
  session$setInputs(source='another-result');session$flushReact()
  expect_null(confirmed_source())
 })
})

test_that('source grouping labels survive generated count and issue labels', {
 d<-data.frame(ID=c('A','A','B','B'),PERIOD=c(1,2,1,2),`同一IDの複数値`='group key',`保留理由`='original key',PARAMCD='CMAX',AVAL=c(1,2,4,8),PARAM_STATUS='calculated',NCA_WARNING=NA_character_,check.names=FALSE)
 r<-core$desktop_explore_subject_means(d,'ID','',c('同一IDの複数値','保留理由'),'PERIOD')
 s<-core$desktop_explore_summary(r$data,'',c('同一IDの複数値','保留理由'),'ID',r$counts_column)
 expect_equal(s$`同一IDの複数値`,'値：group key');expect_equal(s$`保留理由`,'値：original key')
 d$NCA_WARNING[1]<-'check';r<-core$desktop_explore_subject_means(d,'ID','',c('同一IDの複数値','保留理由'),'PERIOD')
 expect_equal(r$issues$`保留理由`,'original key');expect_match(r$issues$`保留理由_`,'警告付き')
})
