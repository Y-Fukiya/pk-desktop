source('apps/pk-desktop/pkapp.R')
library(testthat)
core <- environment(pkapp_open)

make34 <- function() {
 p<-tempfile('pk034');dir.create(p);e<-core$desktop_nca_engine(p)
 d<-do.call(rbind,lapply(1:2,function(i)do.call(rbind,lapply(1:2,function(period)
   data.frame(ID=paste0('S',i),PERIOD=period,TRT='A',TIME=0:3,CONC=c(0,i,0.5*i,0.25*i),UNIT='mg/L')))))
 spec<-list(id='ID',groups=c('PERIOD','TRT'),group_roles=list(list(column='PERIOD',role='period'),list(column='TRT',role='treatment')),time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none')
 res<-pkapp_nca(d,spec);saved<-core$desktop_table_save(e,'artificial',list(source=d),list(kind='nca',specification=res$specification),frozen=res)
 input<-core$desktop_table_save(e,'input',list(source=d),list(kind='prepare',steps=list()))
 a<-list(source=saved$id,view='summary',parameter='CMAX',unit='mg/L',group='TRT',aggregate_over=list('PERIOD'),scale='log',base_revision=e$nca$revision,project_id=e$nca$project_id,explore_revision=e$explore_revision,explore_session=e$explore_session,actor='test',reason='check')
 list(e=e,d=d,spec=spec,saved=saved,input=input,args=a)
}

test_that('each value has four significant digits without global scientific notation', {
 d<-data.frame(平均=c(.012,15400,4e-6,3,250,1.2e7),SD=rep(NA_real_,6),`算術CV%`=rep(20,6),check.names=FALSE)
 html<-as.character(core$desktop_explore_display_summary(d))
 for(x in c('0.01200','15400','4.000e-06','3.000','250.0','1.200e+07','20.0'))expect_match(html,x,fixed=TRUE)
 expect_false(grepl('15400.000000',html,fixed=TRUE));expect_false(grepl('3.000e+00',html,fixed=TRUE))
 expect_match(html,'<th class="pk-number">',fixed=TRUE)
})

test_that('repeated finite observations are flagged, not inferred from missing IDs', {
 d<-data.frame(ID=c('A','A','B',NA),PARAMCD='CMAX',AVAL=c(1,2,3,4),PARAM_STATUS='calculated',NCA_WARNING=NA_character_)
 s<-core$desktop_explore_summary(d,id='ID');o<-core$desktop_explore_overview(d,id='ID')
 expect_match(s$`同一IDの複数値`,'要確認',fixed=TRUE);expect_match(o$`同一IDの複数値`,'要確認',fixed=TRUE)
 d$ID<-c('A','B','C',NA);expect_equal(core$desktop_explore_overview(d,id='ID')$`同一IDの複数値`,'')
 d$ID<-c('A','A','B','B');d$AVAL<-c(1,NA,2,NA);expect_equal(core$desktop_explore_overview(d,id='ID')$`同一IDの複数値`,'')
})

test_that('R API and owner return safe specific role errors', {
 f<-make34();on.exit(unlink(f$e$project,recursive=TRUE));s<-f$spec
 s$group_roles<-list(list(column='PERIOD',role='period'),list(column='PERIOD',role='analyte'))
 expect_error(pkapp_nca(f$d,s),'複数の役割')
 scope<-list(source=f$input$id,specification=s,base_revision=f$e$nca$revision,project_id=f$e$nca$project_id,actor='test',reason='test')
 expect_error(core$desktop_nca_configure(f$e,scope),'複数の役割')
 s$group_roles<-list(list(column='secret_value_do_not_echo',role='period'));scope$specification<-s
 expect_error(pkapp_nca(f$d,s),'追加グループ列のみに')
 try<-tryCatch(core$desktop_nca_configure(f$e,scope),error=identity)
 expect_s3_class(try,'pkapp_public_error');expect_match(conditionMessage(try),'追加グループ列のみに');expect_false(grepl('secret_value',conditionMessage(try)))
})

test_that('log axis survives selecting summary then selecting boxplot again', {
 f<-make34();on.exit(unlink(f$e$project,recursive=TRUE));core$desktop_explore_set(f$e,f$args)
 shiny::testServer(core$desktop_explore_server,args=list(engine=f$e),{
  session$setInputs(source=f$saved$id,view='boxplot',parameter='CMAX',unit='V:mg/L',group='TRT',aggregate_over='PERIOD',scale='log');session$flushReact()
  expect_equal(draft_scale(),'log')
  session$setInputs(view='summary',scale=NULL);session$flushReact();session$setInputs(apply=1);session$flushReact()
  expect_identical(f$e$explore$scale,'log');expect_identical(f$e$explore$view,'summary')
  session$setInputs(view='boxplot');session$flushReact();expect_match(output$choices$html,'value="log" selected',fixed=TRUE)
 })
})

test_that('role selection is one control per group column', {
 f<-make34();on.exit(unlink(f$e$project,recursive=TRUE))
 # The field is keyed to the column, so one UI input produces at most one role.
 field<-function(column)paste0('group_role_column_',digest::digest(column,algo='sha256',serialize=FALSE))
 expect_false(identical(field('PERIOD'),field('TRT')))
})

test_that('HTTP exposes only fixed role-validation messages', {
 f<-make34();on.exit(unlink(f$e$project,recursive=TRUE));e<-f$e
 runtime<-core$desktop_r_runtime(e,3880,3879,'nca')
 secret<-'PRIVATE_GROUP_NAME_NEVER_ECHO';s<-f$spec;s$group_roles<-list(list(column=secret,role='period'))
 request<-list(source=f$input$id,specification=s,base_revision=e$nca$revision,project_id=e$nca$project_id,actor='test',reason='test')
 body<-charToRaw(core$desktop_json(request))
 response<-core$desktop_http_app(e,runtime)$call(list(REQUEST_METHOD='POST',PATH_INFO='/v1/nca_configure',HTTP_AUTHORIZATION=paste('Bearer',runtime$token),HTTP_X_PKAPP_CONTRACT='pkapp-nca-18',CONTENT_TYPE='application/json',CONTENT_LENGTH=as.character(length(body)),rook.input=list(read=function()body)))
 expect_match(response$body,'E_VALIDATION',fixed=TRUE);expect_match(response$body,'追加グループ列',fixed=TRUE)
 expect_false(grepl(secret,response$body,fixed=TRUE))
})
