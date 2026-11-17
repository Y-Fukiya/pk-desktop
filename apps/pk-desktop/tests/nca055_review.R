source('apps/pk-desktop/tests/nca054_review.R')

test_that('dose-derived ADPP metadata flags metabolite interpretation only where relevant', {
 raw<-utils::read.csv('apps/pk-desktop/examples/adpp_auto_terminal/concentrations.csv',stringsAsFactors=FALSE,colClasses=c(PERIOD='character'))
 spec<-list(id='USUBJID',groups=list('STUDYID','PERIOD','TRT','ANALYTE'),
  group_roles=list(list(column='STUDYID',role='study'),list(column='PERIOD',role='period'),list(column='TRT',role='treatment'),list(column='ANALYTE',role='analyte')),
  time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none',auc_method='lin up/log down',terminal_method='auto',dosing_context='single',dose_history_confirmed=TRUE,
  dose=list(amount_column='DOSE',unit_column='DOSEU',basis='absolute',meaning='actual_administered',reference='declared_profile',route_column='ROUTE',extravascular_values=list('ORAL')),
  derived_parameters=list('CMAXD','AUCIFOD','CLFO','VZFO'))
 nca<-pkapp_nca(raw,spec);mapping<-pkapp_adpp_mapping_template(nca)
 mapping$PARAMCD<-paste0(mapping$NCAPCD,ifelse(mapping$ANALYTE=='METAB','M',''))
 out<-pkapp_adpp_draft(nca,parameter_mapping=mapping)
 selected<-out$metadata$REVIEW[out$metadata$VARIABLE %in% c('PARAMCD','PARAM','AVAL','AVALU')]
 expect_length(selected,4L);expect_true(all(grepl('administered compound dose',selected,fixed=TRUE)))
 expect_true(all(grepl('metabolite analytes',selected,fixed=TRUE)));expect_true(all(grepl('CL/F and Vz/F',selected,fixed=TRUE)))
 expect_identical(out$data$AVAL,nca$data$AVAL);expect_identical(out$data$NCAPSTAT,nca$data$PARAM_STATUS)
 no_derived<-spec;no_derived$derived_parameters<-NULL;no_derived$dose<-NULL
 no_dose<-pkapp_adpp_draft(pkapp_nca(raw,no_derived))
 expect_false(any(grepl('metabolite analytes',no_dose$metadata$REVIEW,fixed=TRUE)))
 no_analyte<-spec;no_analyte$groups<-list('STUDYID','PERIOD','TRT');no_analyte$group_roles<-no_analyte$group_roles[1:3]
 only_parent<-raw[raw$ANALYTE=='PARENT',,drop=FALSE]
 without_role<-pkapp_adpp_draft(pkapp_nca(only_parent,no_analyte))
 expect_false(any(grepl('metabolite analytes',without_role$metadata$REVIEW,fixed=TRUE)))
})
