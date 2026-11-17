source('apps/pk-desktop/pkapp.R')
library(testthat)
core <- environment(pkapp_open)

fixture33 <- function(with_roles=TRUE) {
  p<-tempfile('nca033-');dir.create(p);e<-core$desktop_nca_engine(p)
  d<-do.call(rbind,lapply(1:6,function(i) do.call(rbind,lapply(1:2,function(period) do.call(rbind,lapply(c('parent','metabolite'),function(analyte){
    trt<-if ((i+period) %% 2 == 0) 'TEST' else 'REF'; peak<-i*if(analyte=='parent') 2 else 5
    data.frame(ID=paste0('S',i),PERIOD=period,ANALYTE=analyte,TRT=trt,TIME=0:3,CONC=c(0,peak,peak/2,peak/4),UNIT='ng/mL')
  }))))))
  s<-list(id='ID',groups=c('PERIOD','ANALYTE','TRT'),time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none')
  if(with_roles)s$group_roles<-list(list(column='PERIOD',role='period'),list(column='ANALYTE',role='analyte'),list(column='TRT',role='treatment'))
  result<-pkapp_nca(d,s);saved<-core$desktop_table_save(e,'crossover',list(source=d),list(kind='nca',specification=result$specification),frozen=result)
  args<-list(source=saved$id,view='summary',parameter='CMAX',unit='ng/mL',group='TRT',aggregate_over=list('PERIOD'),scale='linear',base_revision=e$nca$revision,project_id=e$nca$project_id,explore_revision=e$explore_revision,explore_session=e$explore_session,actor='test',reason='review')
  list(e=e,result=result,saved=saved,args=args)
}

test_that('only a group recorded as period may be pooled and group cannot equal the pooled column', {
 f<-fixture33();on.exit(unlink(f$e$project,recursive=TRUE));e<-f$e;a<-f$args
 ctx<-core$desktop_explore_context(e,f$saved$id);expect_equal(ctx$period,'PERIOD')
 chosen<-core$desktop_explore_set(e,a);expect_equal(unlist(chosen$selection$aggregate_over),'PERIOD')
 d<-core$desktop_explore_subset(ctx$data,chosen$selection);z<-core$desktop_explore_summary(d,'TRT',setdiff(ctx$strata,'PERIOD'),ctx$id)
 expect_equal(nrow(z),4);expect_equal(z$N,rep(6,4));expect_equal(z$`有限値の被験者ID数`,rep(6,4))
 a$explore_revision<-e$explore_revision;a$aggregate_over<-list('ANALYTE');expect_error(core$desktop_explore_set(e,a),'期として')
 a$aggregate_over<-list('TRT');expect_error(core$desktop_explore_set(e,a),'期として')
 a$aggregate_over<-list('PERIOD');a$group<-'PERIOD';expect_error(core$desktop_explore_set(e,a),'同じ列は指定')
 expect_equal(core$desktop_table_get(e,f$saved$id)$AVAL,f$result$data$AVAL)
})

test_that('legacy and unclassified results never allow pooling, even with a reason', {
 f<-fixture33(FALSE);on.exit(unlink(f$e$project,recursive=TRUE))
 expect_error(core$desktop_explore_set(f$e,f$args),'期として')
 f$args$aggregate_over<-list();expect_no_error(core$desktop_explore_set(f$e,f$args))
})

test_that('role metadata is validated and cannot identify the same column twice', {
 f<-fixture33();on.exit(unlink(f$e$project,recursive=TRUE));s<-f$result$specification
 s$group_roles<-list(list(column='PERIOD',role='period'),list(column='PERIOD',role='analyte'))
 expect_error(core$desktop_nca_specification(data.frame(ID='S',PERIOD=1,ANALYTE='parent',TRT='TEST',TIME=0,CONC=0,UNIT='ng/mL'),s),'複数の役割')
})

test_that('role ordering is normalized consistently when controls are read back', {
 f<-fixture33();on.exit(unlink(f$e$project,recursive=TRUE));s<-f$result$specification
 input<-data.frame(ID='S',PERIOD=1,ANALYTE='parent',TRT='TEST',TIME=0,CONC=0,UNIT='ng/mL')
 reordered<-s;reordered$group_roles<-rev(s$group_roles)
 expect_identical(core$desktop_nca_specification(input,s)$group_roles,core$desktop_nca_specification(input,reordered)$group_roles)
})

test_that('TMAX arithmetic statistics are absent and numeric formatting is consistent', {
 d<-data.frame(ID=letters[1:3],PARAMCD='TMAX',AVAL=1:3,PARAM_STATUS='calculated',NCA_WARNING=NA_character_)
 a<-core$desktop_explore_summary(d,id='ID');expect_true(all(is.na(a[c('平均','SD','算術CV%','幾何平均','幾何CV%')])));expect_equal(a$中央値,2)
 d$PARAMCD<-'CMAX';b<-core$desktop_explore_summary(d,id='ID');html<-as.character(core$desktop_explore_display_summary(b))
 expect_match(html,'pk-number',fixed=TRUE);expect_match(html,'50.0',fixed=TRUE)
 expect_lt(match('平均',names(b)),match('幾何平均',names(b)));expect_lt(match('最大',names(b)),match('欠測・非有限',names(b)))
})

test_that('summary does not allocate a plot and boxplot does', {
 f<-fixture33();on.exit(unlink(f$e$project,recursive=TRUE));core$desktop_explore_set(f$e,f$args)
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
   session$flushReact();expect_null(output$plot_area)
   a<-f$args;a$view<-'boxplot';a$explore_revision<-f$e$explore_revision;core$desktop_explore_set(f$e,a);session$flushReact()
   expect_match(output$plot_area$html,'shiny-plot-output',fixed=TRUE)
 })
})
