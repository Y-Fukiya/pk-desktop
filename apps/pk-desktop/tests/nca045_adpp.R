source('apps/pk-desktop/tests/nca044_review.R')
source("apps/pk-desktop/tests/helper_nca_review.R")
source('apps/pk-desktop/tests/helper_nca_jobs.R')

adpp_fixture <- function() {
 d <- data.frame(STUDYID=rep('STUDY-01',16),USUBJID=rep(c('STUDY-01-001','STUDY-01-002'),each=8),PERIOD=rep(rep(c('1','2'),each=4),2),
                 TRT=rep(rep(c('T','R'),each=4),2),TIME=rep(c(0,1,2,4),4),CONC=rep(c(0,8,4,2),4),UNIT='ng/mL')
 spec <- list(id='USUBJID',groups=list('STUDYID','PERIOD','TRT'),group_roles=list(list(column='STUDYID',role='study'),list(column='PERIOD',role='period'),list(column='TRT',role='treatment')),time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none',auc_method='linear')
 list(data=d,spec=spec)
}

test_that('ADPP candidate preserves NCA values, group keys, warnings and metadata without inventing analysis flags', {
 f<-adpp_fixture();nca<-pkapp_nca(f$data,f$spec);result<-pkapp_adpp_draft(nca)
 expect_identical(names(result$data),c('STUDYID','USUBJID','PERIOD','TRT','PARAMCD','PARAM','AVAL','AVALU','NCAPCD','NCAPSTAT','NCAEXCL','NCAWARN','PROFSTAT'))
 expect_identical(result$data$AVAL,nca$data$AVAL)
 expect_equal(result$data$AVAL[1:3],c(8,1,16))
 expect_identical(result$record$logical_content_hashes$candidate,core$desktop_hash(result$data))
 expect_identical(result$data$USUBJID,nca$data$USUBJID)
 expect_identical(result$data$PARAMCD,nca$data$PARAMCD)
 expect_identical(result$data$PERIOD,nca$data$PERIOD)
 expect_false(any(c('ANL01FL','DTYPE','ASEQ') %in% names(result$data)))
 expect_identical(result$review,nca$data)
 expect_identical(result$trace$rows,nca$trace$rows)
 expect_identical(result$record$independent_pknca_comparison,'not_performed')
 expect_identical(result$record$software$pknca,as.character(packageVersion('PKNCA')))
 expect_identical(result$metadata$VARIABLE,names(result$data))
 expect_true(all(nzchar(result$metadata$REVIEW)))
 expect_identical(result$data$NCAPSTAT,nca$data$PARAM_STATUS)
 expect_identical(result$data$NCAWARN,nca$data$NCA_WARNING)
 expect_identical(result$data$NCAEXCL,nca$data$NCA_EXCLUDE)
})

test_that('ambiguous keys and duplicate candidate rows are rejected', {
 f<-adpp_fixture();nca<-pkapp_nca(f$data,f$spec)
 bad<-nca;bad$specification$id<-'ID';expect_error(pkapp_adpp_draft(bad),'USUBJID')
 bad<-nca;bad$specification$groups<-list('PERIOD','TRT');expect_error(pkapp_adpp_draft(bad),'STUDYID')
 bad<-nca;bad$data<-rbind(bad$data,bad$data[1,]);expect_error(pkapp_adpp_draft(bad),'重複')
 bad<-nca;bad$data$STUDYID[1]<-NA;expect_error(pkapp_adpp_draft(bad),'欠測')
 bad<-nca;bad$data$AVAL[1]<-Inf;expect_error(pkapp_adpp_draft(bad),'数値')
 bad<-nca;bad$specification$groups<-c(bad$specification$groups,list('ANL01FL'));expect_error(pkapp_adpp_draft(bad),'解析フラグ')
})

test_that('warning and missing values are kept with their original review states', {
 f<-adpp_fixture();f$data$CONC[f$data$USUBJID=='STUDY-01-001' & f$data$PERIOD=='1' & f$data$TIME==0]<-NA
 nca<-pkapp_nca(f$data,f$spec);out<-pkapp_adpp_draft(nca)
 expect_true(anyNA(out$data$AVAL));expect_true(any(!is.na(out$review$NCA_EXCLUDE)))
 expect_identical(out$data$AVAL,nca$data$AVAL);expect_identical(out$review$PARAM_STATUS,nca$data$PARAM_STATUS)
})

test_that('candidate export appears only after current NCA and writes exact values', {
 f<-adpp_fixture();p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE);e<-core$desktop_nca_engine(p)
 input_path<-tempfile(fileext='.csv');on.exit(unlink(input_path),add=TRUE);core$pk_write_csv(f$data,input_path)
 shiny::testServer(core$desktop_nca_server,args=list(engine=e),{
  session$setInputs(source='FILE',file=list(datapath=input_path,name='input.csv'),id='USUBJID',time='TIME',concentration='CONC',groups=c('STUDYID','PERIOD','TRT'),unit_column='UNIT',c0_method='none',auc_method='linear',last_observation=TRUE)
  for (item in f$spec$group_roles) do.call(session$setInputs,setNames(list(item$role),paste0('group_role_column_',digest::digest(item$column,algo='sha256',serialize=FALSE))))
  expect_null(output$adpp_draft_exports)
  nca_confirm_displayed(session, input, output)
  session$setInputs(run=1);nca_wait_for_job(e,session)
  if (is.null(result())) stop(paste('NCA failed:', notice()))
  expect_match(output$adpp_draft_exports$html,'提出用ではありません',fixed=TRUE)
  out<-pkapp_read_table(output$adpp_csv,format='csv')
  expect_identical(out$USUBJID,result()$value$data$USUBJID)
  expect_equal(as.numeric(out$AVAL),result()$value$data$AVAL)
  record<-core$desktop_read(output$adpp_record);expect_identical(record$independent_pknca_comparison,'not_performed')
  expect_identical(record$output_provenance$save_status,'unsaved')
  expect_identical(record$logical_content_hashes$source_input,core$desktop_hash(source_data()))
  session$setInputs(name='ADPP candidate test',save=1);session$flushReact()
  record_saved<-core$desktop_read(output$adpp_record)
  expect_identical(record_saved$output_provenance$save_status,'saved_ui_record')
  expect_identical(record_saved$output_provenance$saved_result_id,saved()$id)
  expect_identical(record_saved$logical_content_hashes$source_input,core$desktop_hash(source_data()))
  if(requireNamespace('zip',quietly=TRUE)) { expect_match(output$adpp_draft_exports$html,'レビュー一式ZIP',fixed=TRUE); expect_true(file.exists(output$adpp_bundle)) }
  session$setInputs(auc_method='lin up/log down');session$flushReact()
  expect_null(output$adpp_draft_exports)
 })
})

test_that('review bundle contains candidate, row trace, record and verifiable file hashes', {
 skip_if_not_installed('zip')
 f<-adpp_fixture();nca<-pkapp_nca(f$data,f$spec)
 path<-tempfile(fileext='.zip');directory<-tempfile();dir.create(directory);on.exit({unlink(path);unlink(directory,recursive=TRUE)},add=TRUE)
 pkapp_write_adpp_draft(nca,path)
 files<-utils::unzip(path,list=TRUE)$Name
 expect_setequal(files,c('ADPP_CANDIDATE.csv','ADPP_CANDIDATE_METADATA.csv','ADPP_CANDIDATE_REVIEW.csv','NCA_ROW_HANDLING.csv','NCA_TRACE.json','ADPP_CANDIDATE_RECORD.json','PARAMETER_MAPPING.csv','MANIFEST.json'))
 utils::unzip(path,exdir=directory)
 manifest<-core$desktop_read(file.path(directory,'MANIFEST.json'))
 expect_identical(manifest$status,'review_only_not_submission_ready')
 for (entry in manifest$files) expect_identical(entry$sha256,digest::digest(file=file.path(directory,entry$name),algo='sha256',serialize=FALSE))
 imported<-pkapp_read_table(file.path(directory,'ADPP_CANDIDATE.csv'),format='csv')
 expect_identical(imported$USUBJID,nca$data$USUBJID);expect_equal(as.numeric(imported$AVAL),nca$data$AVAL)
 expect_error(pkapp_write_adpp_draft(nca,path),'出力先')
})


test_that('disabled run action points assistive technology at the reason', {
 p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE);e<-core$desktop_nca_engine(p)
 f<-adpp_fixture();source_id<-core$desktop_table_save(e,'a11y',list(source=f$data),list(kind='prepare',steps=list()))$id
 role<-paste0('group_role_column_',digest::digest('PERIOD',algo='sha256',serialize=FALSE))
 shiny::testServer(core$desktop_nca_server,args=list(engine=e),{
  session$setInputs(source=source_id,groups=c('STUDYID','PERIOD'),id='USUBJID',time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none')
  do.call(session$setInputs,setNames(list('period'),role));session$setInputs(manual_period_order=TRUE,period_order_values='1');session$flushReact()
  expect_match(output$run_action$html,'aria-describedby',fixed=TRUE);expect_match(output$run_action$html,'period_order_run_hint',fixed=TRUE)
  expect_match(output$run_action$html,'title=',fixed=TRUE)
 })
})

test_that('candidate preserves terminal and dose-normalized NCA parameters and states', {
 here<-'apps/pk-desktop/examples/nca_dose_body'
 read<-function(x) pkapp_read_table(file.path(here,x))
 prep<-jsonlite::fromJSON(file.path(here,'prepare_spec.json'),simplifyVector=FALSE)
 spec<-jsonlite::fromJSON(file.path(here,'nca_spec.json'),simplifyVector=FALSE)
 conc<-read('synthetic.csv');dm<-read('DM.csv');vs<-read('VS.csv')
 for (name in c('conc','dm','vs')) {x<-get(name);x$USUBJID<-paste0(x$STUDYID,'-',x$ID);assign(name,x)}
 prep$profile$id<-'USUBJID';prep$dm$keys$USUBJID<-'USUBJID';prep$dm$keys$ID<-NULL
 prep$vs$keys$USUBJID<-'USUBJID';prep$vs$keys$ID<-NULL
 input<-pkapp_prepare_nca_covariates(conc,dm,vs,prep)$data;spec$id<-'USUBJID'
 spec$group_roles<-list(list(column='STUDYID',role='study'),list(column='PERIOD',role='period'))
 nca<-pkapp_nca(input,spec);draft<-pkapp_adpp_draft(nca)
 expect_true(all(c('AUCIFO','CLFO','VZFO') %in% draft$data$PARAMCD))
 expect_equal(draft$data$AVAL[draft$data$PARAMCD=='CLFO'],1,tolerance=1e-10)
 expect_equal(draft$data$AVAL[draft$data$PARAMCD=='VZFO'],10,tolerance=1e-10)
 expect_identical(draft$review$PARAM_STATUS,nca$data$PARAM_STATUS)
 expect_identical(draft$data$AVAL,nca$data$AVAL)
})
