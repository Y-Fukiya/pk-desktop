source('apps/pk-desktop/tests/nca041_review.R')

test_that('machine identity survives a hostname change but refuses a different or absent identity', {
  identity <- core$desktop_owner_identity(tempdir())
  expect_true(core$desktop_same_host(identity))
  expect_true(is.list(identity$machine_identity))
  expect_false(any(grepl('^[0-9A-Fa-f-]{36}$', unlist(identity$machine_identity))))
  identity$host <- 'new-host-name'
  expect_true(core$desktop_same_host(identity))
  identity$machine_identity$fingerprint <- paste(rep('0', 64), collapse = '')
  expect_false(core$desktop_same_host(identity))
  identity$machine_identity <- NULL
  expect_false(core$desktop_same_host(identity))
})

test_that('stale runtime archives omit credentials and preserve a live runtime', {
  p <- tempfile(); dir.create(p); on.exit(unlink(p, recursive = TRUE), add = TRUE)
  e <- core$desktop_nca_engine(p)
  old <- core$desktop_r_runtime(e, 45811, 45812, 'nca')
  old$pid <- 99999999L; old$token <- 'SECRET_TOKEN_TEST'; old$instance_id <- 'SECRET_INSTANCE_TEST'
  core$desktop_atomic(old, file.path(p, '.pkapp-runtime.json'))
  now <- core$desktop_r_runtime(e, 45813, 45814, 'nca'); core$desktop_write_r_runtime(e, now)
  paths <- list.files(p, pattern = '^\\.pkapp-runtime-stale-', all.files = TRUE, full.names = TRUE)
  expect_length(paths, 1)
  expect_false(any(grepl('SECRET_', readLines(paths), fixed = TRUE)))
  expect_identical(core$desktop_read(file.path(p, '.pkapp-runtime.json'))$instance_id, now$instance_id)
  core$desktop_remove_r_runtime(e, now)
})

test_that('observed period values are selected in order without acknowledgment and remain exact', {
  p <- tempfile(); dir.create(p); on.exit(unlink(p, recursive = TRUE), add = TRUE)
  e <- core$desktop_nca_engine(p)
  d <- data.frame(ID = rep(c('S1','S2'), each = 4), PERIOD = rep(c('Ⅰ期','I期 '), each = 4),
                  TRT = rep(c('T','R'), each = 4), TIME = rep(0:3, 2), CONC = rep(c(0,8,4,2), 2), UNIT = 'ng/mL')
  saved <- core$desktop_table_save(e, 'ui', list(source=d), list(kind='prepare', steps=list()))
  role <- paste0('group_role_column_', digest::digest('PERIOD', algo='sha256', serialize=FALSE)); source_id <- saved$id
  shiny::testServer(core$desktop_nca_server, args=list(engine=e), {
    session$setInputs(source=source_id, groups=c('PERIOD','TRT'), id='ID', time='TIME', concentration='CONC', unit_column='UNIT', c0_method='none')
    do.call(session$setInputs, setNames(list('period'),role)); session$flushReact()
    expect_match(output$period_order_controls$html, 'I期␠', fixed=TRUE)
    expect_match(output$period_order_controls$html, 'Ⅰ期', fixed=TRUE)
    expect_false(grepl('period_order_disclose', output$period_order_controls$html, fixed=TRUE))
    session$setInputs(manual_period_order=TRUE,period_order_values=c('I期 ','Ⅰ期')); session$flushReact()
    expect_identical(specification()$period_order$values, list('I期 ','Ⅰ期'))
    expect_identical(core$desktop_nca_specification(d,specification())$period_order$values,list('I期 ','Ⅰ期'))
  })
})

if (identical(unname(Sys.info()[['sysname']]), 'Linux')) {
  test_that('Linux does not confuse missing and live process IDs', {
    expect_true(core$desktop_linux_pid_absent(99999999L))
    expect_false(core$desktop_linux_pid_absent(Sys.getpid()))
    expect_identical(core$desktop_process_state(99999999L), 'dead')
    identity <- core$desktop_owner_identity(tempdir())
    expect_identical(core$desktop_process_state(Sys.getpid(), identity), 'live')
    expect_true(is.list(identity$linux_process_identity))
    identity$linux_process_identity$pid_namespace <- 'pid:[0]'
    expect_identical(core$desktop_process_state(99999999L, identity), 'unknown')
  })
}

test_that('shared manual period order is restored in the actual selected option order', {
  p <- tempfile(); dir.create(p); on.exit(unlink(p,recursive=TRUE),add=TRUE)
  e <- core$desktop_nca_engine(p)
  d <- data.frame(ID=rep(c('a','b'),each=4),TIME=rep(0:3,2),CONC=rep(c(0,8,4,2),2),UNIT='ng/mL',PERIOD=rep(c('第一期','第二期'),each=4))
  source <- core$desktop_table_save(e,'shared',list(source=d),list(kind='prepare',steps=list()))$id
  spec <- list(id='ID',time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none',groups=list('PERIOD'),group_roles=list(list(column='PERIOD',role='period')),period_order=list(column='PERIOD',values=list('第二期','第一期')))
  core$desktop_nca_configure(e,list(project_id=e$nca$project_id,base_revision=e$nca$revision,source=source,specification=spec,actor='test',reason='restore test'))
  role <- paste0('group_role_column_',digest::digest('PERIOD',algo='sha256',serialize=FALSE)); source_id <- source
  shiny::testServer(core$desktop_nca_server,args=list(engine=e),{
    session$setInputs(source=source_id,id='ID',time='TIME',concentration='CONC',unit_column='UNIT',c0_method='none')
    session$setInputs(shared_refresh=1); session$setInputs(source=source_id,groups='PERIOD')
    do.call(session$setInputs,setNames(list('period'),role)); session$flushReact()
    html <- output$period_order_controls$html
    a <- regexpr('value="第二期" selected',html,fixed=TRUE)[1]; b <- regexpr('value="第一期" selected',html,fixed=TRUE)[1]
    expect_gt(a,0); expect_gt(b,0); expect_lt(a,b)
  })
})
