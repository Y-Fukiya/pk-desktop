source('apps/pk-desktop/tests/nca048_adpp_specimen.R')
source("apps/pk-desktop/tests/helper_nca_review.R")

test_that('other is checked within analyte and specimen and still catches misclassified dimensions', {
 f<-adpp_fixture(); d<-do.call(rbind,lapply(c('PARENT','METAB'),function(a) do.call(rbind,lapply(c('PLASMA','URINE'),function(s) transform(f$data,PCTESTCD=a,PPSPEC=s,METHOD=paste(a,s,sep='_'))))))
 spec<-f$spec;spec$groups<-c(spec$groups,list('PCTESTCD','PPSPEC','METHOD'));spec$group_roles<-c(spec$group_roles,list(list(column='PCTESTCD',role='analyte'),list(column='PPSPEC',role='specimen'),list(column='METHOD',role='other')))
 nca<-pkapp_nca(d,spec);out<-pkapp_adpp_draft(nca);expect_equal(length(unique(out$data$METHOD)),4L)
 expect_match(out$metadata$REVIEW[out$metadata$VARIABLE=='PARAM'],'specimen has not been verified',fixed=TRUE)
 bad<-d; bad$METHOD[bad$USUBJID==bad$USUBJID[1] & bad$PCTESTCD=='PARENT' & bad$PPSPEC=='PLASMA']<-'METHOD_A'
 extra<-bad[bad$USUBJID==bad$USUBJID[1] & bad$PCTESTCD=='PARENT' & bad$PPSPEC=='PLASMA',];extra$METHOD<-'METHOD_B'
 expect_error(pkapp_adpp_draft(pkapp_nca(rbind(bad,extra),spec)),'METHOD.*複数')
 spec$group_roles[[length(spec$group_roles)-2L]]$role<-'other'
 # Existing method depends on the wrongly classified analyte; error can identify either ambiguous column.
 expect_error(pkapp_adpp_draft(pkapp_nca(d,spec)),'複数')
})

test_that('complete user mapping bypasses provisional collision without bypassing validation', {
 f<-adpp_fixture(); d<-do.call(rbind,lapply(sprintf('A%02d',1:12),function(a) transform(f$data,PCTESTCD=a)))
 spec<-f$spec;spec$groups<-c(spec$groups,list('PCTESTCD'));spec$group_roles<-c(spec$group_roles,list(list(column='PCTESTCD',role='analyte')))
 nca<-pkapp_nca(d,spec);x<-nca$data[nca$data$PARAMCD=='CMAX',];x$PARAMCD<-'AUCIFO';x$PARAM<-'AUC infinity'
 y<-x;y$PARAMCD<-'AUCIFOD';y$PARAM<-'Normalized AUC infinity';nca$data<-rbind(x,y)
 expect_error(pkapp_adpp_draft(nca),'8文字制限')
 m<-pkapp_adpp_mapping_template(nca);expect_equal(nrow(m),24L);m$PARAMCD<-sprintf('P%03d',seq_len(nrow(m)))
 out<-pkapp_adpp_draft(nca,parameter_mapping=m);expect_equal(nrow(unique(out$data[c('NCAPCD','PCTESTCD','PARAMCD')])),24L)
 expect_identical(out$record$parameter_mapping_source,'user_supplied');expect_true(nzchar(out$record$supplied_mapping_hash))
 expect_error(pkapp_adpp_draft(nca,parameter_mapping=m[-1,]),'不足')
 expect_error(pkapp_adpp_draft(nca,parameter_mapping=rbind(m,m[1,])),'重複')
 z<-m;z$PARAMCD[1]<-z$PARAMCD[2];expect_error(pkapp_adpp_draft(nca,parameter_mapping=z),'PARAMCDが重複')
 z<-m;z$PARAMCD[1]<-'TOO_LONG_CODE';expect_error(pkapp_adpp_draft(nca,parameter_mapping=z),'8文字')
 z<-m;z$PCTESTCD[1]<-'EXTRA';expect_error(pkapp_adpp_draft(nca,parameter_mapping=z),'不足')
 z<-m;z$PARAMCD[1]<-'';expect_error(pkapp_adpp_draft(nca,parameter_mapping=z),'空欄')
 path<-tempfile(fileext='.zip');dir<-tempfile();dir.create(dir);on.exit({unlink(path);unlink(dir,recursive=TRUE)},add=TRUE)
 pkapp_write_adpp_draft(nca,path,parameter_mapping=m);utils::unzip(path,exdir=dir)
 rec<-jsonlite::fromJSON(file.path(dir,'ADPP_CANDIDATE_RECORD.json'),simplifyVector=FALSE)
 expect_identical(rec$parameter_mapping_source,'user_supplied');expect_identical(rec$supplied_mapping_hash,out$record$supplied_mapping_hash)
})

test_that('Shiny shows progress, retains disabled actions, previews and loads mapping CSV', {
 f<-adpp_fixture();d<-rbind(transform(f$data,PCTESTCD='PARENT'),transform(f$data,PCTESTCD='METAB'))
 p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE);e<-core$desktop_nca_engine(p)
 input_path<-tempfile(fileext='.csv');on.exit(unlink(input_path),add=TRUE);core$pk_write_csv(d,input_path)
 shiny::testServer(core$desktop_nca_server,args=list(engine=e),{
  session$setInputs(source='FILE',file=list(datapath=input_path,name='input.csv'),id='USUBJID',time='TIME',concentration='CONC',groups=c('STUDYID','PERIOD','TRT','PCTESTCD'),unit_column='UNIT',c0_method='none',auc_method='linear',last_observation=TRUE)
  entries<-c(f$spec$group_roles,list(list(column='PCTESTCD',role='analyte')))
  for(item in entries) do.call(session$setInputs,setNames(list(item$role),paste0('group_role_column_',digest::digest(item$column,algo='sha256',serialize=FALSE))))
  nca_confirm_displayed(session, input, output)
  session$setInputs(run=1);nca_wait_for_job(e,session)
  session$setInputs(adpp_manual_order=TRUE,adpp_order_column='PCTESTCD',adpp_order_values='PARENT');session$flushReact()
  expect_match(output$adpp_order_progress$html,'1 / 2 選択済み',fixed=TRUE);expect_match(output$adpp_order_progress$html,'METAB',fixed=TRUE)
  expect_match(output$adpp_draft_exports$html,'disabled',fixed=TRUE);expect_match(output$adpp_draft_exports$html,'レビュー一式ZIP',fixed=TRUE)
  session$setInputs(adpp_order_values=c('PARENT','METAB'));session$flushReact();expect_match(output$adpp_draft_exports$html,'プレビュー',fixed=TRUE)
  template<-utils::read.csv(output$adpp_mapping_template,colClasses='character',check.names=FALSE);template$PARAMCD<-sprintf('P%03d',seq_len(nrow(template)))
  mapping_path<-file.path(p,'mapping.csv');utils::write.csv(template,mapping_path,row.names=FALSE,na='')
  session$setInputs(adpp_use_mapping=TRUE);session$flushReact()
  do.call(session$setInputs,setNames(list(list(datapath=mapping_path,name='mapping.csv')),adpp_mapping_input_id()));session$flushReact()
  got<-pkapp_read_table(output$adpp_csv,format='csv');expect_setequal(unique(got$PARAMCD),template$PARAMCD)
  rec<-core$desktop_read(output$adpp_record);expect_identical(rec$parameter_mapping_source,'user_supplied')
 })
})
