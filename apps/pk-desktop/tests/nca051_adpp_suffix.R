source('apps/pk-desktop/tests/nca050_adpp_mapping.R')
source("apps/pk-desktop/tests/helper_nca_review.R")

suffix_fixture <- function() {
 f<-adpp_fixture(); d<-rbind(transform(f$data,PCTESTCD='PARENT'),transform(f$data,PCTESTCD='METAB'))
 spec<-f$spec;spec$groups<-c(spec$groups,list('PCTESTCD'));spec$group_roles<-c(spec$group_roles,list(list(column='PCTESTCD',role='analyte')))
 pkapp_nca(d,spec)
}

test_that('one truly empty suffix preserves parent codes and still enforces uniqueness', {
 nca<-suffix_fixture();sm<-core$pkapp_adpp_suffix_template(nca);sm$SUFFIX<-ifelse(sm$PCTESTCD=='PARENT','','M')
 mapping<-core$pkapp_adpp_expand_suffix_mapping(nca,sm)
 expect_true(all(mapping$PARAMCD[mapping$PCTESTCD=='PARENT']==mapping$NCAPCD[mapping$PCTESTCD=='PARENT']))
 expect_true(all(mapping$PARAMCD[mapping$PCTESTCD=='METAB']==paste0(mapping$NCAPCD[mapping$PCTESTCD=='METAB'],'M')))
 expect_false(anyDuplicated(mapping$PARAMCD)>0)
 collision<-nca; extra<-collision$data[collision$data$PARAMCD=='CMAX',,drop=FALSE];extra$PARAMCD<-'CMAXM';extra$PARAM<-'Other maximum';collision$data<-rbind(collision$data,extra)
 expect_error(core$pkapp_adpp_expand_suffix_mapping(collision,sm),'重複')
 bad<-sm;bad$SUFFIX<-'';expect_error(core$pkapp_adpp_expand_suffix_mapping(nca,bad),'1組')
 bad<-sm;bad$SUFFIX[1]<-' ';expect_error(core$pkapp_adpp_expand_suffix_mapping(nca,bad),'英大文字')
 bad<-sm;bad$SUFFIX[1]<-NA_character_;expect_error(core$pkapp_adpp_expand_suffix_mapping(nca,bad),'確認')
})

test_that('record hashes actual source tables, checks consistency and preserves bundle provenance', {
 nca<-suffix_fixture();sm<-core$pkapp_adpp_suffix_template(nca);sm$SUFFIX<-ifelse(sm$PCTESTCD=='PARENT','','M')
 overrides<-pkapp_adpp_mapping_template(nca);overrides<-overrides[1,,drop=FALSE];overrides$PARAMCD<-'OVERRIDE'
 mapping<-core$pkapp_adpp_expand_suffix_mapping(nca,sm,overrides)
 inputs<-list(method='suffix',suffix_mapping=sm,overrides=overrides)
 rec<-pkapp_adpp_draft(nca,parameter_mapping=mapping,mapping_inputs=inputs)$record
 expect_identical(rec$parameter_mapping_method,'suffix');expect_identical(rec$parameter_mapping_input_hashes$suffix,core$desktop_hash(sm));expect_identical(rec$parameter_mapping_input_hashes$overrides,core$desktop_hash(overrides))
 expect_identical(rec$supplied_mapping_hash,core$desktop_hash(mapping))
 altered<-inputs;altered$suffix_mapping$SUFFIX[2]<-'X';expect_error(pkapp_adpp_draft(nca,parameter_mapping=mapping,mapping_inputs=altered),'一致')
 single<-pkapp_adpp_draft(nca,parameter_mapping=mapping,mapping_inputs=list(method='individual',mapping=mapping))$record
 expect_identical(single$parameter_mapping_method,'individual');expect_identical(single$parameter_mapping_input_hashes$individual,core$desktop_hash(mapping))
 expect_identical(pkapp_adpp_draft(nca,parameter_mapping=mapping)$record$parameter_mapping_method,'user_supplied_unspecified')
 expect_identical(pkapp_adpp_draft(nca)$record$parameter_mapping_method,'generated_provisional')
 path<-tempfile(fileext='.zip');dir<-tempfile();dir.create(dir);on.exit({unlink(path);unlink(dir,recursive=TRUE)},add=TRUE)
 pkapp_write_adpp_draft(nca,path,parameter_mapping=mapping,mapping_inputs=inputs);utils::unzip(path,exdir=dir)
 saved<-jsonlite::fromJSON(file.path(dir,'ADPP_CANDIDATE_RECORD.json'),simplifyVector=FALSE)
 expect_identical(saved$parameter_mapping_method,'suffix');expect_identical(saved$parameter_mapping_input_hashes$suffix,core$desktop_hash(sm))
})

test_that('Shiny presents suffix steps, disables unavailable download, and records actual method', {
 f<-adpp_fixture();d<-rbind(transform(f$data,PCTESTCD='PARENT'),transform(f$data,PCTESTCD='METAB'))
 p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE);e<-core$desktop_nca_engine(p)
 input_path<-file.path(p,'input.csv');core$pk_write_csv(d,input_path)
 shiny::testServer(core$desktop_nca_server,args=list(engine=e),{
  session$setInputs(source='FILE',file=list(datapath=input_path,name='input.csv'),id='USUBJID',time='TIME',concentration='CONC',groups=c('STUDYID','PERIOD','TRT','PCTESTCD'),unit_column='UNIT',c0_method='none',auc_method='linear',last_observation=TRUE)
  for(item in c(f$spec$group_roles,list(list(column='PCTESTCD',role='analyte')))) do.call(session$setInputs,setNames(list(item$role),paste0('group_role_column_',digest::digest(item$column,algo='sha256',serialize=FALSE))))
  nca_confirm_displayed(session, input, output)
  session$setInputs(run=1);nca_wait_for_job(e,session)
  session$setInputs(adpp_manual_order=TRUE,adpp_order_column='PCTESTCD',adpp_order_values='PARENT')
  session$setInputs(adpp_use_mapping=TRUE,adpp_mapping_mode='suffix');session$flushReact()
  expect_null(output$adpp_order_controls);expect_match(output$adpp_mapping_upload$html,'②',fixed=TRUE)
  expect_match(output$adpp_override_action$html,'disabled',fixed=TRUE)
  controls<-output$adpp_mapping_controls$html
  expect_lt(regexpr('adpp_suffix_template',controls)[1],regexpr('adpp_mapping_upload',controls)[1]);expect_lt(regexpr('adpp_mapping_upload',controls)[1],regexpr('adpp_override_action',controls)[1]);expect_lt(regexpr('adpp_override_action',controls)[1],regexpr('adpp_override_upload',controls)[1])
  sm<-core$pkapp_adpp_suffix_template(result()$value);sm$SUFFIX<-ifelse(sm$PCTESTCD=='PARENT','','M');path<-file.path(p,'suffix.csv');utils::write.csv(sm,path,row.names=FALSE,na='')
  do.call(session$setInputs,setNames(list(list(datapath=path,name='suffix.csv')),adpp_mapping_input_id()));session$flushReact()
  record<-core$desktop_read(output$adpp_record);expect_identical(record$parameter_mapping_method,'suffix');expect_identical(record$parameter_mapping_input_hashes$suffix,core$desktop_hash(core$desktop_adpp_read_mapping_csv(path)))
  expect_null(record$analyte_order);expect_match(output$adpp_override_action$html,'不要',fixed=TRUE)
  sm$SUFFIX<-ifelse(sm$PCTESTCD=='PARENT','PARENT','METAB');utils::write.csv(sm,path,row.names=FALSE,na='')
  do.call(session$setInputs,setNames(list(list(datapath=path,name='long_suffix.csv')),adpp_mapping_input_id()));session$flushReact()
  expect_match(output$adpp_override_action$html,'adpp_override_template',fixed=TRUE)
 })
})
