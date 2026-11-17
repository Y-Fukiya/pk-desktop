source('apps/pk-desktop/pkapp.R')
library(testthat)
core <- environment(pkapp_open)

test_that('pooled crossover counts subjects and computes geometric summaries without mixing analytes', {
  d <- data.frame(ID=rep(paste0('S',1:6),2), PERIOD=rep(1:2,each=6),
    TRT=ifelse((rep(1:6,2)+rep(1:2,each=6)) %% 2 == 0,'T','R'),
    ANALYTE='parent', PARAMCD='CMAX', AVALU='ng/mL', AVAL=NA_real_, PARAM_STATUS='calculated', NCA_WARNING=NA_character_)
  d$AVAL[d$TRT=='T']<-1:6;d$AVAL[d$TRT=='R']<-2:7
  separate<-core$desktop_explore_summary(d,'TRT',c('PERIOD','ANALYTE'),'ID')
  expect_equal(nrow(separate),4)
  pooled<-core$desktop_explore_summary(d,'TRT','ANALYTE','ID')
  expect_equal(pooled$N,c(6,6));expect_equal(pooled$`有限値の被験者ID数`,c(6,6))
  expect_equal(pooled$`幾何平均`[pooled$TRT=='値：T'],exp(mean(log(1:6))))
  expect_equal(pooled$`幾何CV%`[pooled$TRT=='値：T'],100*sqrt(exp(var(log(1:6)))-1))
  expect_equal(pooled$`算術CV%`[pooled$TRT=='値：T'],100*sd(1:6)/mean(1:6))
  d$TRT <- 'T'; d$AVAL[1]<-NA
  overview<-core$desktop_explore_overview(d,'TRT','ANALYTE','ID')
  expect_equal(overview$`解析単位数`,12);expect_equal(overview$`有限値N`,11);expect_equal(overview$`欠測・非有限`,1)
  expect_equal(overview$`有限値の被験者ID数`,6)
})

test_that('geometric statistics never silently omit nonpositive values, and Tmax has no geometric statistics', {
  d<-data.frame(ID=c('A','B','C'), PARAMCD='CMAX', AVAL=c(1,0,-1),PARAM_STATUS='calculated',NCA_WARNING=NA_character_)
  x<-core$desktop_explore_summary(d,id='ID')
  expect_equal(x$`幾何計算対象外（0以下）`,2);expect_true(is.na(x$`幾何平均`));expect_true(is.na(x$`幾何CV%`))
  d$AVAL<-1:3;d$PARAMCD<-'TMAX';x<-core$desktop_explore_summary(d,id='ID')
  expect_true(is.na(x$`幾何平均`));expect_equal(x$中央値,2);expect_equal(x$最小,1);expect_equal(x$最大,3)
  d<-d[1,,drop=FALSE];d$PARAMCD<-'CMAX';x<-core$desktop_explore_summary(d,id='ID')
  expect_equal(x$`幾何平均`,1);expect_true(is.na(x$`幾何CV%`))
})

test_that('exploration explicitly records permitted pooled strata and blocks unsafe grouping', {
  p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE));e<-core$desktop_nca_engine(p)
  d<-data.frame(ID=rep(c('A','B'),each=4),PERIOD=rep(1:2,each=4),TRT=rep(c('T','R'),each=4),TIME=rep(0:3,2),CONC=rep(c(0,8,4,2),2),UNIT='ng/mL')
  result<-pkapp_nca(d,list(id='ID',groups=c('PERIOD','TRT'),group_roles=list(list(column='PERIOD',role='period'),list(column='TRT',role='treatment')),time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none'))
  saved<-core$desktop_table_save(e,'crossover',list(source=d),list(kind='nca',specification=result$specification),frozen=result)
  a<-list(source=saved$id,view='summary',parameter='CMAX',unit='ng/mL',group='TRT',aggregate_over=c('PERIOD'),scale='linear',base_revision=e$nca$revision,project_id=e$nca$project_id,explore_revision=e$explore_revision,explore_session=e$explore_session,actor='test',reason='pool period')
  chosen<-core$desktop_explore_set(e,a);expect_equal(unlist(chosen$selection$aggregate_over),'PERIOD')
  expect_equal(result$data$AVAL,core$desktop_table_get(e,saved$id)$AVAL)
  a$explore_revision<-e$explore_revision;a$aggregate_over<-'ID';expect_error(core$desktop_explore_set(e,a),'解析グループ列')
  a$aggregate_over<-character();a$group<-'AVAL';expect_error(core$desktop_explore_set(e,a),'カテゴリ列')
  a$group<-'TRT';a$scale<-'bad';expect_error(core$desktop_explore_set(e,a),'縦軸')
})
