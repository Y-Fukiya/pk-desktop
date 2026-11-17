source('apps/pk-desktop/tests/nca040_review.R')

test_that('moved legacy project with dead pid recovers runtime and owner lock', {
 p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE)
 e<-core$desktop_nca_engine(p); dir.create(file.path(p,'.pkapp-owner'));writeLines('99999999',file.path(p,'.pkapp-owner','pid'))
 previous<-core$desktop_r_runtime(e,45811,45812,'nca');previous$pid<-99999999L;previous$project<-'/old/copied/project';previous$host<-NULL;previous$system<-NULL;previous$process_start_time<-NULL;previous$machine_identity_version<-NULL;previous$machine_identity<-NULL
 core$desktop_atomic(previous,file.path(p,'.pkapp-runtime.json'))
 core$desktop_nca_launch(p,45812,45811,FALSE,start_api=function(...)1,stop_api=function(...)NULL,run_app=function(...)expect_true(file.exists(file.path(p,'.pkapp-runtime.json'))))
 archives<-list.files(p,pattern='^\\.pkapp-runtime-stale-',all.files=TRUE,full.names=TRUE)
 expect_length(archives,1);expect_equal(as.integer(file.info(archives)$mode),384L)
 expect_false(dir.exists(file.path(p,'.pkapp-owner')));expect_false(file.exists(file.path(p,'.pkapp-runtime.json')))
})

test_that('live owners and foreign hosts stay blocked and PID reuse is detected', {
 p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE)
 owner<-core$desktop_acquire_project(p);expect_error(core$desktop_acquire_project(p),'実行情報')
 identity<-core$desktop_read(file.path(owner$lock,'identity.json'))
 expect_identical(core$desktop_process_state(Sys.getpid(),identity),'live')
 identity$process_start_time<-identity$process_start_time-100
 if (!is.null(identity$linux_process_identity)) identity$linux_process_identity$start_ticks<-"0"
 expect_identical(core$desktop_process_state(Sys.getpid(),identity),'reused')
 core$desktop_atomic(identity,file.path(owner$lock,'identity.json'))
 recovered<-core$desktop_acquire_project(p);expect_true(dir.exists(recovered$lock));core$desktop_release_project(recovered)
 owner<-core$desktop_acquire_project(p);identity<-core$desktop_read(file.path(owner$lock,'identity.json'));identity$machine_identity$fingerprint<-paste(rep('0',64),collapse='');identity$pid<-99999999L
 writeLines('99999999',file.path(owner$lock,'pid'));core$desktop_atomic(identity,file.path(owner$lock,'identity.json'))
 expect_error(core$desktop_acquire_project(p),'実行情報')
})

test_that('live or unreadable runtime never replaced and symlink is not followed', {
 p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE);e<-core$desktop_nca_engine(p)
 previous<-core$desktop_r_runtime(e,45811,45812,'nca');previous$project<-'/another/project';core$desktop_atomic(previous,file.path(p,'.pkapp-runtime.json'))
 expect_error(core$desktop_write_r_runtime(e,core$desktop_r_runtime(e,45813,45814,'nca')),'実行情報')
 expect_identical(core$desktop_read(file.path(p,'.pkapp-runtime.json'))$instance_id,previous$instance_id)
 previous$process_start_time<-previous$process_start_time-100;if(!is.null(previous$linux_process_identity)) previous$linux_process_identity$start_ticks<-'0';core$desktop_atomic(previous,file.path(p,'.pkapp-runtime.json'))
 now<-core$desktop_r_runtime(e,45813,45814,'nca');core$desktop_write_r_runtime(e,now);expect_identical(core$desktop_read(file.path(p,'.pkapp-runtime.json'))$instance_id,now$instance_id)
 core$desktop_remove_r_runtime(e,now);writeLines('KEEP',file.path(p,'other'));file.symlink(file.path(p,'other'),file.path(p,'.pkapp-runtime.json'))
 expect_error(core$desktop_write_r_runtime(e,now),'実行情報');expect_identical(readLines(file.path(p,'other')),'KEEP')
})

test_that('manual period order is exact, saved and drives exploration without changing NCA values', {
 d<-do.call(rbind,lapply(1:6,function(i)do.call(rbind,lapply(1:2,function(per){
  data.frame(ID=paste0('S',i),PERIOD=if(per==1)'第一期' else '第二期',TRT=if((i+per)%%2==0)'TEST' else 'REF',TIME=0:3,CONC=c(0,i+5,(i+5)/2,(i+5)/4),UNIT='ng/mL')
 }))))
 spec<-list(id='ID',groups=c('PERIOD','TRT'),group_roles=list(list(column='PERIOD',role='period'),list(column='TRT',role='treatment')),time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none')
 old<-pkapp_nca(d,spec);spec$period_order<-list(column='PERIOD',values=list('第二期','第一期'))
 new<-pkapp_nca(d,spec);expect_equal(new$data,old$data);expect_identical(new$specification$period_order,spec$period_order)
 bad<-spec;bad$period_order$values<-list('第一期');expect_error(pkapp_nca(d,bad),'期の順序')
 bad$period_order$values<-list('第一期','第一期');expect_error(pkapp_nca(d,bad),'期の順序')
 p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE);e<-core$desktop_nca_engine(p)
 saved<-core$desktop_table_save(e,'manual-order',list(source=d),list(kind='nca',specification=new$specification),frozen=new)
 a<-list(source=saved$id,view='summary',parameter='CMAX',unit='ng/mL',group='TRT',aggregate_over=list('PERIOD'),scale='linear',summary_basis='analysis_unit',base_revision=e$nca$revision,project_id=e$nca$project_id,explore_revision=e$explore_revision,explore_session=e$explore_session,actor='test',reason='test')
 core$desktop_explore_set(e,a);ctx<-core$desktop_explore_context(e,saved$id);selected<-core$desktop_explore_subset(ctx$data,e$explore)
 expect_null(core$desktop_explore_sequences(selected,ctx$id,ctx$period,ctx$treatment,ctx$study)$table)
 z<-core$desktop_explore_sequences(selected,ctx$id,ctx$period,ctx$treatment,ctx$study,ctx$manual_order$values)
 expect_equal(sort(z$table$被験者数),c(3L,3L));expect_match(z$note,'利用者が指定',fixed=TRUE)
 expect_true(all(grepl('第二期:.*第一期:',z$table$`期順の処置`)))
 shiny::testServer(core$desktop_explore_server,args=list(engine=e),{session$setInputs(source=a$source);session$flushReact();expect_match(output$allocation$html,'利用者が指定',fixed=TRUE);expect_match(output$allocation$html,'第二期:',fixed=TRUE)})
})

test_that('manual order UI selects observed values and applies entered order', {
 p<-tempfile();dir.create(p);on.exit(unlink(p,recursive=TRUE),add=TRUE);e<-core$desktop_nca_engine(p)
 d<-data.frame(ID=rep(c('S1','S2'),each=4),PERIOD=rep(c('第一期','第二期'),each=4),TRT=rep(c('T','R'),each=4),TIME=rep(0:3,2),CONC=rep(c(0,8,4,2),2),UNIT='ng/mL')
 saved<-core$desktop_table_save(e,'manual ui',list(source=d),list(kind='prepare',steps=list()));src_id041<-saved$id
 role<-paste0('group_role_column_',digest::digest('PERIOD',algo='sha256',serialize=FALSE))
 shiny::testServer(core$desktop_nca_server,args=list(engine=e),{
  session$setInputs(source=src_id041,groups=c('PERIOD','TRT'),id='ID',time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none')
  do.call(session$setInputs,setNames(list('period'),role));session$flushReact()
  expect_match(output$period_order_controls$html,'MCP',fixed=TRUE)
  session$setInputs(manual_period_order=TRUE,period_order_values=c('第二期','第一期'));session$flushReact()
  expect_identical(specification()$period_order,list(column='PERIOD',values=list('第二期','第一期')))
 })
})


test_that('overview explanation only appears if a group has repeated finite IDs', {
 f<-fixture36(TRUE);on.exit(unlink(f$e$project,recursive=TRUE),add=TRUE)
 a<-f$args;a$summary_basis<-'analysis_unit';core$desktop_explore_set(f$e,a)
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
  session$setInputs(source=a$source);session$flushReact()
  expect_false(grepl('同一IDの複数値がある群は',output$table$html,fixed=TRUE))
 })
 d<-fake_data();d$AVAL<-1;expect_false(core$desktop_explore_overview_has_repeated(d,'TRT','ANALYTE','ID'))
 d<-rbind(d,d);expect_true(core$desktop_explore_overview_has_repeated(d,'TRT','ANALYTE','ID'))
})
