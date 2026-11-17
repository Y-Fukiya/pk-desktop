source('apps/pk-desktop/tests/nca039_review.R')

test_that('fullwidth digits determine order but the original labels are preserved', {
 for (pat in c('第%s期', 'Ｐ%s')) {
  d<-fake_data();d$PERIOD<-sprintf(pat,ifelse(d$PERIOD==1,'１','２'))
  d<-d[!(d$ID=='S1' & d$PERIOD==sprintf(pat,'２')),]
  result<-core$desktop_explore_sequences(d,'ID','PERIOD','TRT')
  expect_equal(sum(result$table$被験者数),6L)
  expect_true(any(grepl(sprintf(pat,'１'),result$table$`期順の処置`,fixed=TRUE)))
  expect_true(any(grepl('未記録',result$table$`期順の処置`,fixed=TRUE)))
  expect_match(result$note,'元データ',fixed=TRUE)
 }
 d<-data.frame(ID='X',PERIOD=c('第１０期','第２期','第１期'),TRT='T')
 expect_equal(core$desktop_explore_allocation_matrix(d,'ID','PERIOD','TRT')$PERIOD,c('値：第１期','値：第２期','値：第１０期'))
 d$PERIOD<-c('１０','２','１')
 expect_equal(core$desktop_explore_allocation_matrix(d,'ID','PERIOD','TRT')$PERIOD,c('値：１','値：２','値：１０'))
})

test_that('mixed numeric forms cannot collapse distinct labels or conceal ambiguous signs', {
 for (labels in list(c('第1期','第１期'),c('P1','P１'),c('第１期','第０１期'),c('Ｐ－１','Ｐ－２'),c('P1A','P1B'),c('P1','Q２'),c('第１期２','第２期３'))) {
  expect_false(core$desktop_explore_period_order(labels)$ok)
 }
 expect_match(core$desktop_explore_period_order(c('P1A','P1B'))$note,'数字以外',fixed=TRUE)
 expect_match(core$desktop_explore_period_order(c('P1','P１'))$note,'異なる期の表記',fixed=TRUE)
})

test_that('empty repeat indicator is hidden only when requested, and warnings stay visible', {
 d<-fake_data();d$AVAL<-1;d$PARAM_STATUS<-'calculated';d$PROFILE_STATUS<-'ok'
 overview<-core$desktop_explore_overview(d,'TRT',c('ANALYTE'),'ID')
 summary<-core$desktop_explore_summary(d,'TRT',c('ANALYTE'),'ID')
 for (x in list(overview,summary)) {
  expect_false(grepl('同一IDの複数値',as.character(core$desktop_explore_display_summary(x,hide_empty_repeat=TRUE)),fixed=TRUE))
  expect_true(grepl('同一IDの複数値',as.character(core$desktop_explore_display_summary(x)),fixed=TRUE))
 }
 repeated<-d;repeated$PERIOD<-repeated$PERIOD+2;d<-rbind(d,repeated)
 for (x in list(core$desktop_explore_overview(d,'TRT','ANALYTE','ID'),core$desktop_explore_summary(d,'TRT','ANALYTE','ID'))) {
  html<-as.character(core$desktop_explore_display_summary(x,hide_empty_repeat=TRUE))
  expect_match(html,'同一IDの複数値',fixed=TRUE);expect_match(html,'要確認',fixed=TRUE)
 }
})

test_that('initial instruction is hidden while conflict blocks update and returns after clearing', {
 f<-fixture38_false_role();on.exit(unlink(f$e$project,recursive=TRUE))
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
  session$setInputs(source=f$args$source,aggregate_over=character());session$flushReact()
  expect_match(output$notice,'押してください',fixed=TRUE)
  session$setInputs(aggregate_over='PERIOD');session$flushReact()
  expect_match(output$apply_control$html,'disabled',fixed=TRUE);expect_identical(output$notice,'')
  session$setInputs(aggregate_over=character());session$flushReact()
  expect_match(output$notice,'押してください',fixed=TRUE)
 })
})

test_that('fullwidth periods survive the actual NCA save and exploration flow', {
 p<-tempfile('pk040-');dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE)
 e<-core$desktop_nca_engine(p)
 d<-do.call(rbind,lapply(1:6,function(i)do.call(rbind,lapply(1:2,function(per) {
  if (i==1 && per==2) return(NULL)
  data.frame(ID=paste0('S',i),PERIOD=if(per==1)'第１期' else '第２期',ANALYTE='親化合物',TRT=if((i+per)%%2==0)'TEST' else 'REF',TIME=0:3,CONC=c(0,i+5,(i+5)/2,(i+5)/4),UNIT='ng/mL')
 }))))
 spec<-list(id='ID',groups=c('PERIOD','ANALYTE','TRT'),group_roles=list(list(column='PERIOD',role='period'),list(column='ANALYTE',role='analyte'),list(column='TRT',role='treatment')),time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none')
 result<-pkapp_nca(d,spec)
 saved<-core$desktop_table_save(e,'fullwidth synthetic',list(source=d),list(kind='nca',specification=result$specification),frozen=result)
 a<-list(source=saved$id,view='summary',parameter='CMAX',unit='ng/mL',group='TRT',aggregate_over=list('PERIOD'),scale='linear',summary_basis='analysis_unit',base_revision=e$nca$revision,project_id=e$nca$project_id,explore_revision=e$explore_revision,explore_session=e$explore_session,actor='test',reason='synthetic')
 core$desktop_explore_set(e,a)
 ctx<-core$desktop_explore_context(e,saved$id);selected<-core$desktop_explore_subset(ctx$data,e$explore)
 seq<-core$desktop_explore_sequences(selected,ctx$id,ctx$period,ctx$treatment,ctx$study)
 expect_equal(sum(seq$table$被験者数),6L)
 expect_true(any(grepl('第１期: TEST → 第２期: 未記録',seq$table$`期順の処置`,fixed=TRUE)))
 expect_equal(sum(seq$table$被験者数[grepl('未記録',seq$table$`期順の処置`,fixed=TRUE)]),1L)
 shiny::testServer(core$desktop_explore_server,args=list(engine=e),{
  session$setInputs(source=a$source);session$flushReact()
  expect_match(output$allocation$html,'第１期: TEST → 第２期: 未記録',fixed=TRUE)
 })
})
