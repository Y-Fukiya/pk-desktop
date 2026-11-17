source('apps/pk-desktop/pkapp.R')
library(testthat)
core <- environment(pkapp_open)

test_that('subject means retain treatment and analyte and match independent arithmetic', {
 d<-expand.grid(ID=c('A','B'),PERIOD=1:4,ANALYTE=c('parent','metabolite'),stringsAsFactors=FALSE)
 d$TRT<-ifelse(d$PERIOD%%2==1,'TEST','REF')
 d$AVAL<-ifelse(d$ID=='A',ifelse(d$PERIOD<=2,1,4),ifelse(d$PERIOD<=2,4,16))*ifelse(d$ANALYTE=='parent',1,10)
 d$PARAMCD<-'CMAX';d$PARAM_STATUS<-'calculated';d$NCA_WARNING<-NA_character_
 original<-d
 res<-core$desktop_explore_subject_means(d,'ID','TRT',c('ANALYTE','TRT'))
 expect_equal(nrow(res$data),8);expect_identical(d,original)
 out<-core$desktop_explore_summary(res$data,'TRT',c('ANALYTE','TRT'),'ID')
 p<-out[out$ANALYTE=='値：parent' & out$TRT=='値：TEST',]
 expect_equal(p$N,2);expect_equal(p$`幾何平均`,4,tolerance=1e-12)
 expect_equal(p$`幾何CV%`,sqrt(exp(var(log(c(2,8))))-1)*100,tolerance=1e-10)
 expect_equal(core$desktop_explore_repeat_label(c('A','A','B','B')),'要確認（2 IDで2件ずつ）')
 d$AVAL[1]<-0;expect_equal(core$desktop_explore_subject_means(d,'ID','TRT',c('ANALYTE','TRT'))$blocked_groups,1)
 d<-original;d$NCA_WARNING[1]<-'check';expect_equal(core$desktop_explore_subject_means(d,'ID','TRT',c('ANALYTE','TRT'))$blocked_groups,1)
 d<-original;d$AVAL[1]<-NA;d$NCA_WARNING[1]<-'check';expect_equal(core$desktop_explore_subject_means(d,'ID','TRT',c('ANALYTE','TRT'))$blocked_groups,1)
 d<-original;d$ID[1]<-NA;expect_equal(core$desktop_explore_subject_means(d,'ID','TRT',c('ANALYTE','TRT'))$blocked_groups,1)
})

test_that('summary is scrollable and formatting switches after rounding', {
 d<-data.frame(TRT='値：A',平均=999999.4,`欠測・非有限`=0,check.names=FALSE)
 html<-as.character(core$desktop_explore_display_summary(d))
 expect_match(html,'pk-table-scroll',fixed=TRUE);expect_match(html,'1.000e+06',fixed=TRUE)
 expect_false(grepl('値：A',html,fixed=TRUE));expect_match(html,'この表で0の項目',fixed=TRUE)
})

test_that('points have stable bounded offsets not ordered by values', {
 d<-data.frame(ID=paste0('S',1:12),AVAL=1:12,PARAM_STATUS='calculated',NCA_WARNING=NA_character_)
 a<-core$desktop_explore_plot_groups(d);b<-core$desktop_explore_plot_groups(d[12:1,])
 expect_true(all(abs(a$offsets[[1]])<=0.08));expect_equal(a$offsets[[1]],rev(b$offsets[[1]]))
 expect_lt(abs(cor(a$offsets[[1]],d$AVAL)),0.95)
})
