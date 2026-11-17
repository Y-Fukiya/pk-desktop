source('apps/pk-desktop/tests/nca046_adpp_review.R')

test_that('all group roles are required regardless of source column spelling', {
  f <- adpp_fixture()
  d <- rbind(transform(f$data, PCTESTCD='PARENT'), transform(f$data, PCTESTCD='METAB'))
  d$CONC[d$PCTESTCD=='METAB'] <- d$CONC[d$PCTESTCD=='METAB']/2
  spec <- f$spec; spec$groups <- c(spec$groups,list('PCTESTCD'))
  nca <- pkapp_nca(d,spec)
  expect_error(pkapp_adpp_draft(nca),'PCTESTCD')
  spec$group_roles <- c(spec$group_roles,list(list(column='PCTESTCD',role='analyte')))
  nca <- pkapp_nca(d,spec); out <- pkapp_adpp_draft(nca)
  expect_identical(out$data$AVAL,nca$data$AVAL)
  expect_identical(out$data$NCAPSTAT,nca$data$PARAM_STATUS)
  expect_identical(out$data$NCAEXCL,nca$data$NCA_EXCLUDE)
  expect_identical(out$data$NCAWARN,nca$data$NCA_WARNING)
  cmax <- unique(out$data[out$data$NCAPCD=='CMAX',c('PCTESTCD','PARAMCD','PARAM')])
  expect_setequal(cmax$PARAMCD,c('CMAX1','CMAX2'))
  expect_true(all(grepl('Max Conc, ',cmax$PARAM,fixed=TRUE)))
  expect_false(any(grepl('PCTESTCD=',cmax$PARAM,fixed=TRUE)))
  expect_false(anyDuplicated(cmax$PARAMCD)>0)
  changed <- nca; changed$data$AVALU <- 'ug/L'
  changed$data$PARAM <- sub('Max Conc','Maximum concentration',changed$data$PARAM,fixed=TRUE)
  expect_identical(pkapp_adpp_draft(changed)$data$PARAMCD,out$data$PARAMCD)
  reversed <- nca; reversed$data <- nca$data[rev(seq_len(nrow(nca$data))),]
  again <- pkapp_adpp_draft(reversed)$data
  key <- function(x) sort(paste(x$NCAPCD,x$PCTESTCD,x$PARAMCD,sep='|'))
  expect_identical(key(again),key(out$data))
  single <- pkapp_nca(d[d$PCTESTCD=='PARENT',],spec)
  expect_identical(pkapp_adpp_draft(single)$data$PARAMCD,single$data$PARAMCD)
})

test_that('ambiguous shortened provisional codes fail closed', {
  f<-adpp_fixture();d<-rbind(transform(f$data,PCTESTCD='PARENT'),transform(f$data,PCTESTCD='METAB'))
  spec<-f$spec;spec$groups<-c(spec$groups,list('PCTESTCD'))
  spec$group_roles<-c(spec$group_roles,list(list(column='PCTESTCD',role='analyte')))
  nca<-pkapp_nca(d,spec)
  extra<-nca$data[nca$data$PARAMCD=='CMAX',];extra$PARAMCD<-'ABCDEFGH';extra$PARAM<-'First'
  extra2<-extra;extra2$PARAMCD<-'ABCDEFGI';extra2$PARAM<-'Second'
  nca$data<-rbind(nca$data,extra,extra2)
  expect_error(pkapp_adpp_draft(nca),'一対一')
})
