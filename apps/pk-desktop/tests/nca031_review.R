source('apps/pk-desktop/pkapp.R')
library(testthat)
core <- environment(pkapp_open)

make_fixture <- function() {
  p <- tempfile('nca031-review-'); dir.create(p); e <- core$desktop_nca_engine(p)
  times <- c(0,1,2,4)
  d <- do.call(rbind,lapply(c('A','B'),function(id) do.call(rbind,lapply(1:2,function(period)
    data.frame(ID=id,PERIOD=period,ANALYTE='parent',TIME=times,CONC=c(0,10*period,5*period,2*period),BLQ=id=='B',UNIT='ng/mL')))))
  x <- pkapp_nca(d,list(id='ID',groups=c('PERIOD','ANALYTE'),time='TIME',concentration='CONC',blq_column='BLQ',unit_column='UNIT',c0_method='none',all_blq_policy='zero'))
  n <- core$desktop_table_save(e,'artificial NCA',list(source=d),list(kind='nca',specification=x$specification),frozen=x)
  list(e=e,n=n,result=x,input=d)
}
args_for <- function(e,id) list(source=id,view='summary',parameter='CMAX',unit='ng/mL',group='',base_revision=e$nca$revision,
  project_id=e$nca$project_id,explore_revision=e$explore_revision,explore_session=e$explore_session,actor='test',reason='review test')

test_that('policy values and periods cannot silently enter the same average', {
  f <- make_fixture(); on.exit(unlink(f$e$project,recursive=TRUE)); ctx <- core$desktop_explore_context(f$e,f$n$id)
  expect_identical(ctx$strata,c('PERIOD','ANALYTE'))
  d <- core$desktop_explore_subset(ctx$data,list(parameter='CMAX',unit='ng/mL'))
  s <- core$desktop_explore_summary(d,strata=ctx$strata)
  expect_equal(nrow(s),4)
  expect_equal(sort(s$平均),c(0,0,10,20))
  expect_equal(sum(s$`結果の状態`=='方針割当'),2)
  expect_equal(sum(s$`結果の状態`=='算出値'),2)
  expect_false(any(s$平均 == 5))
  expect_equal(sum(s$解析単位数),4)
})

test_that('annotated results preserve frozen identity and support additional grouping', {
  f <- make_fixture(); on.exit(unlink(f$e$project,recursive=TRUE))
  original <- core$desktop_annotation_result(f$e,f$n$id)
  dm <- data.frame(SUBJ=c('A','B'),SEX=c('F','M'))
  annotation <- pkapp_annotate_nca(original,dm=dm,specification=list(version=1,dm=list(keys=list(ID='SUBJ'),columns=list(DM_SEX='SEX'))))
  saved <- core$desktop_annotation_save(f$e,'annotated',list(result=original,dm=dm),annotation,list(result_source=f$n$id),channel='ui')
  ctx <- core$desktop_explore_context(f$e,saved$id)
  expect_identical(ctx$id,'ID'); expect_identical(ctx$strata,c('PERIOD','ANALYTE'))
  x <- core$desktop_explore_set(f$e,args_for(f$e,saved$id))
  expect_identical(x$selection$source,saved$id)
  d <- core$desktop_explore_subset(ctx$data,x$selection)
  s <- core$desktop_explore_summary(d,'DM_SEX',ctx$strata)
  expect_equal(nrow(s),4);expect_true(all(c('DM_SEX','PERIOD','ANALYTE') %in% names(s)))
  recreated <- core$desktop_nca_engine(f$e$project)
  expect_identical(core$desktop_explore_context(recreated,saved$id)$strata,ctx$strata)
})

test_that('missing units are selectable and not mixed with empty units', {
  d <- data.frame(PARAMCD=c('CMAX','CMAX','CMAX'),AVALU=c(NA,'','mg/L'),AVAL=c(NA,1,2))
  expect_equal(nrow(core$desktop_explore_subset(d,list(parameter='CMAX',unit='',unit_missing=TRUE))),1)
  expect_equal(core$desktop_explore_subset(d,list(parameter='CMAX',unit='',unit_missing=FALSE))$AVAL,1)
  expect_false(identical(core$desktop_explore_unit_key(NA_character_),core$desktop_explore_unit_key('')))
})

test_that('the form retains an unapplied view and refuses stale exploration revisions', {
  f<-make_fixture();e<-f$e;on.exit(unlink(e$project,recursive=TRUE))
  a<-args_for(e,f$n$id); core$desktop_explore_set(e,a); a$explore_revision<-e$explore_revision
  expect_error(core$desktop_explore_set(e,modifyList(a,list(explore_revision=0))), '表示条件が更新')
  shiny::testServer(core$desktop_explore_server,args=list(engine=e),{
    session$setInputs(source=f$n$id,view='boxplot',parameter='TMAX',unit='V:h',group='')
    expect_match(output$choices$html,'value="boxplot" selected',fixed=TRUE)
  })
})

test_that('generic chart refuses mixed PK parameters, units and policy-assigned values', {
  f<-make_fixture();on.exit(unlink(f$e$project,recursive=TRUE))
  d<-f$result$data
  expect_error(core$desktop_module_check('pk_distribution',d,list(x='AVAL')),'1パラメータ')
  d<-d[d$PARAMCD=='CMAX',,drop=FALSE]
  expect_error(core$desktop_module_check('pk_distribution',d,list(x='AVAL')),'方針割当')
  d<-d[d$PARAM_STATUS=='calculated',,drop=FALSE];d$AVALU[1]<-'other'
  expect_error(core$desktop_module_check('pk_distribution',d,list(x='AVAL')),'1パラメータ')
})

test_that('legacy module data is private and symlink migration is rejected', {
  p<-tempfile('nca031-private-');dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE)
  folder<-file.path(p,'modules');dir.create(folder,mode='0755')
  module<-file.path(folder,paste0('module-',paste(rep('a',24),collapse='')));dir.create(module,mode='0755')
  data<-file.path(module,'data.rds');saveRDS(data.frame(x=1),data);Sys.chmod(c(folder,module),'0755');Sys.chmod(data,'0644')
  e<-new.env(parent=emptyenv());e$project<-p
  core$desktop_modules_init(e)
  expect_equal(bitwAnd(as.integer(file.info(folder)$mode),63L),0L)
  expect_equal(bitwAnd(as.integer(file.info(module)$mode),63L),0L)
  expect_equal(bitwAnd(as.integer(file.info(data)$mode),63L),0L)
  external<-tempfile('nca031-external-');dir.create(external);on.exit(unlink(external,recursive=TRUE),add=TRUE)
  link<-file.path(folder,paste0('module-',paste(rep('b',24),collapse='')))
  expect_true(file.symlink(external,link));before<-file.info(external)$mode
  expect_error(core$desktop_modules_init(e),'non-symlink')
  expect_identical(file.info(external)$mode,before)
})

test_that('MCP numeric summaries separate period and policy allocation after consent', {
  f<-make_fixture();e<-f$e;on.exit(unlink(e$project,recursive=TRUE))
  input<-core$desktop_table_save(e,'input',list(source=f$input),list(kind='prepare',steps=list()))
  scope<-list(source=input$id,specification=f$result$specification,base_revision=e$nca$revision,project_id=e$nca$project_id,actor='test',reason='review')
  core$desktop_nca_configure(e,scope);scope$base_revision<-e$nca$revision
  result<-core$desktop_nca_run_shared(e,scope)
  expect_true(all(vapply(result$parameters,function(x)!('mean' %in% names(x)),logical(1))))
  core$desktop_nca_set_disclosure(e,result$result_id,TRUE,channel='ui')
  disclosed<-core$desktop_nca_public(e,result$result_id,e$nca$project_id)
  cmax<-Filter(function(x) identical(x$parameter,'CMAX'),disclosed$parameters)
  expect_length(cmax,4)
  expect_equal(sort(vapply(cmax,function(x)x$mean,numeric(1))),c(0,0,10,20))
  expect_equal(sum(vapply(cmax,function(x)identical(x$result_status,'方針割当'),logical(1))),2)
  expect_true(all(vapply(cmax,function(x)all(c('PERIOD','ANALYTE') %in% names(x$strata)),logical(1))))
})

test_that('a real all-BLQ result without unit can be inspected as missing unit', {
  p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE));e<-core$desktop_nca_engine(p)
  d<-data.frame(ID='S01',TIME=0:2,CONC=c(0,0,0),BLQ=TRUE)
  result<-pkapp_nca(d,list(id='ID',time='TIME',concentration='CONC',blq_column='BLQ',c0_method='none',all_blq_policy='missing'))
  saved<-core$desktop_table_save(e,'missing unit',list(source=d),list(kind='nca',specification=result$specification),frozen=result)
  args<-list(source=saved$id,view='qc',parameter='CMAX',unit='',unit_missing=TRUE,group='',explore_revision=0L,explore_session=e$explore_session,base_revision=e$nca$revision,project_id=e$nca$project_id,actor='test',reason='review')
  chosen<-core$desktop_explore_set(e,args)
  expect_true(chosen$selection$unit_missing)
  rows<-core$desktop_explore_subset(core$desktop_explore_table(e,saved$id),chosen$selection)
  expect_true(is.na(rows$AVALU[[1]]));expect_true(is.na(rows$AVAL[[1]]))
  shiny::testServer(core$desktop_explore_server,args=list(engine=e),{
    session$setInputs(source=saved$id,parameter='CMAX',unit='M:',view='qc',group='')
    expect_match(output$choices$html,'単位欠測',fixed=TRUE)
    expect_match(output$table$html,'算出不可',fixed=TRUE)
  })
})

test_that('stale UI drafts and prior application sessions cannot overwrite a newer exploration', {
  f<-make_fixture();e<-f$e;on.exit(unlink(e$project,recursive=TRUE));a<-args_for(e,f$n$id)
  shiny::testServer(core$desktop_explore_server,args=list(engine=e),{
    session$setInputs(source=f$n$id,view='summary',parameter='CMAX',unit='V:ng/mL',group='')
    invisible(output$choices)
    external<-modifyList(a,list(view='boxplot'))
    core$desktop_explore_set(e,external,channel='mcp');session$flushReact()
    expect_equal(draft_revision(),0)
    session$setInputs(apply=1);session$flushReact()
    expect_identical(e$explore$view,'boxplot')
    expect_match(notice(),'表示条件が更新されています',fixed=TRUE)
  })
  restarted<-core$desktop_nca_engine(e$project)
  stale<-a;stale$base_revision<-restarted$nca$revision;stale$explore_revision<-0
  expect_error(core$desktop_explore_set(restarted,stale),'表示セッションが変更')
})

test_that('NCA values cannot be charted by generic modules without identity-preserving strata', {
  skip_if_not_installed('teal');skip_if_not_installed('teal.data')
  f<-make_fixture();e<-f$e;on.exit(unlink(e$project,recursive=TRUE))
  args<-list(module='pk_distribution',source=f$n$id,label='unsafe',options=list(x='AVAL'),base_revision=e$nca$revision,
    project_id=e$nca$project_id,modules_revision=e$modules$revision,actor='test',reason='review')
  expect_error(core$desktop_module_mutate(e,args),'Result Exploration')
  expect_length(e$modules$entries,0)
})

test_that('boxplot legend distinguishes NA labels and keeps status groups separate', {
  d<-data.frame(G=c(NA,'(欠測)','A','A'),PERIOD=c(1,1,1,2),AVAL=c(10,20,1,2),
    PARAM_STATUS=c('calculated','calculated','policy_assigned','calculated'),NCA_WARNING=NA_character_)
  x<-core$desktop_explore_plot_groups(d,group='G',strata='PERIOD')
  expect_equal(length(x$values),4)
  expect_equal(names(x$values),paste0('G',1:4))
  expect_equal(x$legend$G[1:2],c('欠測（NA）','値：(欠測)'))
  expect_true('方針割当' %in% x$legend$`結果の状態`)
  expect_equal(sum(x$legend$`有限値N`),4)
})
