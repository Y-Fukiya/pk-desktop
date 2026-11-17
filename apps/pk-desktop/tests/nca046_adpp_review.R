source('apps/pk-desktop/tests/nca045_adpp.R')

test_that('analytes receive distinct reproducible provisional parameter codes and NCA statuses remain on each row', {
 f<-adpp_fixture();d<-rbind(transform(f$data,ANALYTE='PARENT'),transform(f$data,ANALYTE='METAB'))
 d$CONC[d$ANALYTE=='METAB']<-d$CONC[d$ANALYTE=='METAB']/2
 d<-d[!(d$USUBJID=='STUDY-01-002' & d$PERIOD=='1' & d$ANALYTE=='PARENT' & d$TIME>1),]
 spec<-f$spec;spec$groups<-c(spec$groups,list('ANALYTE'));spec$group_roles<-c(spec$group_roles,list(list(column='ANALYTE',role='analyte')))
 spec$terminal_method<-'auto';spec$dosing_context<-'single';spec$dose_history_confirmed<-TRUE;spec$include_aucproc<-TRUE
 nca<-pkapp_nca(d,spec);out<-pkapp_adpp_draft(nca,source_input=d,provenance=list(save_status='unsaved'))
 expect_identical(out$data$NCAPCD,nca$data$PARAMCD)
 expect_identical(out$data$NCAPSTAT,nca$data$PARAM_STATUS)
 expect_identical(out$data$NCAEXCL,nca$data$NCA_EXCLUDE)
 expect_identical(out$data$NCAWARN,nca$data$NCA_WARNING)
 expect_identical(out$data$PROFSTAT,nca$data$PROFILE_STATUS)
 expect_identical(out$data$AVAL,nca$data$AVAL)
 expect_true(any(!is.na(out$data$NCAWARN)));expect_true(any(!is.na(out$data$NCAEXCL)))
 peak<-out$data[out$data$NCAPCD=='CMAX',]
 expect_equal(length(unique(peak$PARAMCD)),2L);expect_true(all(mapply(function(a,p) grepl(a,p,fixed=TRUE),peak$ANALYTE,peak$PARAM)))
 expect_true(all(grepl('^[A-Za-z][A-Za-z0-9_]{0,7}$',out$data$PARAMCD)))
 pairs<-unique(out$data[c('PARAMCD','PARAM')]);expect_false(anyDuplicated(pairs$PARAMCD)>0);expect_false(anyDuplicated(pairs$PARAM)>0)
 expect_identical(out$record$logical_content_hashes$source_input,core$desktop_hash(d))
 expect_identical(out$record$output_provenance$save_status,'unsaved')
 expect_match(out$metadata$REVIEW[out$metadata$VARIABLE=='ANALYTE'],'PARCAT1',fixed=TRUE)
 expect_match(out$metadata$REVIEW[out$metadata$VARIABLE=='PERIOD'],'APERIOD',fixed=TRUE)
 expect_match(out$metadata$REVIEW[out$metadata$VARIABLE=='TRT'],'TRTA',fixed=TRUE)
 expect_match(out$metadata$REVIEW[out$metadata$VARIABLE=='PARAMCD'],'provisional',fixed=TRUE)
 expect_match(out$metadata$REVIEW[out$metadata$VARIABLE=='PARAM'],'AUCPROC',fixed=TRUE)
 expect_true(all(unique(out$data$NCAPCD) %in% out$record$parameter_mapping$NCAPCD))
 nca2<-nca;nca2$data<-nca2$data[rev(seq_len(nrow(nca2$data))),]
 out2<-pkapp_adpp_draft(nca2)
 pairkey<-function(x) sort(paste(x$NCAPCD,x$PARAMCD,x$PARAM,sep='|'))
 expect_identical(pairkey(out$data),pairkey(out2$data))
})

test_that('unclassified analyte and conflicting parameter labels are refused', {
 f<-adpp_fixture();d<-transform(f$data,ANALYTE='PARENT');f$spec$groups<-c(f$spec$groups,list('ANALYTE'))
 nca<-pkapp_nca(d,f$spec);expect_error(pkapp_adpp_draft(nca),'全グループ列の役割')
 f<-adpp_fixture();nca<-pkapp_nca(f$data,f$spec)
 nca$data$PARAM[nca$data$PARAMCD=='CMAX'][1]<-'A different meaning'
 expect_error(pkapp_adpp_draft(nca),'一対一')
})
