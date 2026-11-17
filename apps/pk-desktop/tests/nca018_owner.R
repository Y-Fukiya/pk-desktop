source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)

fixture <- function(ids = c("S01", "S02", "S03")) {
  project <- tempfile("nca018-owner-"); dir.create(project)
  e <- core$desktop_nca_engine(project)
  full <- data.frame(ID = rep(c("S01", "S02", "S03"), each = 4), TIME = rep(c(0, 1, 2, 4), 3),
    CONC = c(0, 8, 4, 2, 0, 6, 3, 1, 0, 11.7, 6, 3), UNIT = "ng/mL")
  data <- pkapp_prepare(full, list(list(op = "filter", column = "ID", operator = "in", values = as.list(ids))))$data
  saved <- core$desktop_table_save(e, "Synthetic test", list(source = data), list(kind = "prepare", steps = list()))
  args <- list(project_id = e$nca$project_id, base_revision = e$nca$revision, actor = "Declared operator", reason = "Test synthetic PK",
    source = saved$id, specification = list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT", c0_method = "zero", auc_method = "linear"))
  args$base_revision <- core$desktop_nca_configure(e, args)$revision
  list(e = e, args = args)
}
close_fixture <- function(f) {
  core$desktop_nca_jobs_close(f$e)
  unlink(f$e$project, recursive = TRUE)
}
expect_counts_only <- function(result) {
  expect_identical(result$disclosure$mode, "counts_only")
  expect_false(result$disclosure$allowed)
  expect_true(all(vapply(result$parameters, function(x) !any(c("mean", "min", "max") %in% names(x)), logical(1))))
  for (parameter in result$parameters) expect_named(parameter, c("parameter", "unit", "n", "missing"))
}
wait_job <- function(e, id, timeout = 20) {
  deadline <- Sys.time() + timeout
  repeat {
    core$desktop_nca_poll_job(e, id)
    if (!core$desktop_nca_busy(e)) break
    if (Sys.time() > deadline) stop("Test NCA worker did not finish")
    later::run_now(.02)
    Sys.sleep(.02)
  }
  core$desktop_nca_get_job(e, id, e$nca$project_id)
}

test_that("MCP numeric summaries require UI consent even after filtering to one subject", {
  for (ids in list(c("S01", "S02", "S03"), "S03")) {
    f <- fixture(ids); e <- f$e
    result <- core$desktop_nca_run_shared(e, f$args)
    expect_counts_only(result)
    expect_equal(result$counts$curves, length(ids))
    expect_error(core$desktop_nca_dispatch(e, "nca_set_disclosure", list(result_id = result$result_id, allow = TRUE)), "Unknown NCA operation")
    expect_error(core$desktop_nca_set_disclosure(e, result$result_id, TRUE, "mcp"), "local UI")
    expect_counts_only(core$desktop_nca_public(e, result$result_id, f$args$project_id))
    grant <- core$desktop_nca_set_disclosure(e, result$result_id, TRUE)
    expect_true(grant$allowed)
    allowed <- core$desktop_nca_public(e, result$result_id, f$args$project_id)
    expect_true(all(vapply(allowed$parameters, function(x) all(c("mean", "min", "max") %in% names(x)), logical(1))))
    if (length(ids) == 1L) expect_equal(allowed$parameters[[1]]$mean, 11.7)
    core$desktop_nca_set_disclosure(e, result$result_id, FALSE)
    expect_counts_only(core$desktop_nca_public(e, result$result_id, f$args$project_id))
    close_fixture(f)
  }
})

test_that("consent is bound to one frozen result and expires on accepted configure, rerun and restart", {
  f <- fixture(); on.exit(close_fixture(f), add = TRUE); e <- f$e
  first <- core$desktop_nca_run_shared(e, f$args)
  core$desktop_nca_set_disclosure(e, first$result_id, TRUE)
  invalid <- f$args; invalid$specification$c0_method <- "INVALID"
  expect_error(core$desktop_nca_configure(e, invalid), "c0_method")
  expect_true(core$desktop_nca_disclosure(e, first$result_id)$allowed)
  core$desktop_nca_configure(e, f$args)
  expect_counts_only(core$desktop_nca_public(e, first$result_id, f$args$project_id))
  core$desktop_nca_set_disclosure(e, first$result_id, TRUE)
  second <- core$desktop_nca_run_shared(e, f$args)
  expect_counts_only(second)
  expect_counts_only(core$desktop_nca_public(e, first$result_id, f$args$project_id))
  expect_error(core$desktop_nca_set_disclosure(e, first$result_id, TRUE), "stale")
  core$desktop_nca_set_disclosure(e, second$result_id, TRUE)
  restarted <- core$desktop_nca_engine(e$project)
  expect_null(restarted$nca_disclosure)
  expect_false(core$desktop_nca_get(restarted)$disclosure$allowed)
  f$args$specification$c0_method <- "none"
  f$args$base_revision <- core$desktop_nca_configure(e, f$args)$revision
  expect_counts_only(core$desktop_nca_public(e, second$result_id, f$args$project_id))
  expect_false(core$desktop_nca_public(e, second$result_id, f$args$project_id)$is_current)
})

test_that("audit distinguishes declared actors, owner OS context and initialization client claims", {
  f <- fixture("S03"); on.exit(close_fixture(f), add = TRUE); e <- f$e
  f$args$actor <- "Named person (approver)"
  f$args$os_user <- "FORGED_OS_USER"
  f$args[["_mcp_client"]] <- list(name = "Test client", version = "2.1")
  f$args$specification$c0_method <- "none"
  f$args$base_revision <- core$desktop_nca_configure(e, f$args)$revision
  event <- tail(e$nca$audit, 1)[[1]]
  expect_identical(event$identity$declared_actor, f$args$actor)
  expect_identical(event$identity$os_user, unname(Sys.info()[["user"]]))
  expect_identical(event$identity$os_pid, Sys.getpid())
  expect_identical(event$identity$client_name, "Test client")
  expect_identical(event$identity$client_identity_source, "mcp_initialization_self_reported")
  expect_false(event$identity$authenticated_human_identity)
  change <- Filter(function(x) identical(x$field, "c0_method"), event$changes)[[1]]
  expect_identical(change$before, "zero"); expect_identical(change$after, "none")
  result <- core$desktop_nca_run_shared(e, f$args)
  saved <- core$desktop_nca_dispatch(e, "nca_save_result", c(f$args, list(result_id = result$result_id, name = "audit result")))
  expect_identical(saved$provenance$execution$identity$os_user, event$identity$os_user)
  expect_identical(saved$provenance$saved_identity$client_version, "2.1")
  expect_identical(saved$provenance$execution$channel, "mcp")
  expect_identical(saved$provenance$execution$actor, f$args$actor)
  expect_false(grepl('11.7', core$desktop_json(saved), fixed = TRUE))
})

test_that("unknown result IDs and known stale results have distinct errors", {
  f <- fixture(); on.exit(close_fixture(f), add = TRUE); e <- f$e
  unknown <- c(f$args, list(result_id = paste0("nca-", strrep("0", 24)), name = "unknown"))
  err <- tryCatch(core$desktop_nca_dispatch(e, "nca_save_result", unknown), pkapp_error = identity)
  expect_identical(err$code, "E_NOT_FOUND")
  result <- core$desktop_nca_run_shared(e, f$args)
  f$args$specification$start <- 1
  f$args$base_revision <- core$desktop_nca_configure(e, f$args)$revision
  err <- tryCatch(core$desktop_nca_dispatch(e, "nca_save_result", c(f$args, list(result_id = result$result_id, name = "stale"))), pkapp_error = identity)
  expect_identical(err$code, "E_STALE_RESULT")
})

test_that("background NCA yields to the owner, blocks conflicting runs and publishes the same local values", {
  f <- fixture("S03"); on.exit(close_fixture(f), add = TRUE); e <- f$e
  reference <- pkapp_nca(core$desktop_table_get(e, f$args$source), f$args$specification)$data
  job <- core$desktop_nca_start_job(e, f$args)
  expect_identical(job$status, "running")
  expect_identical(core$desktop_nca_get(e)$result_status, "running")
  expect_error(core$desktop_nca_configure(e, f$args), "NCA is running")
  expect_error(core$desktop_nca_run_shared(e, f$args), "NCA is running")
  expect_error(core$desktop_nca_start_job(e, f$args), "NCA is running")
  heartbeat <- FALSE
  later::later(function() heartbeat <<- TRUE, .001)
  later::run_now(.02)
  expect_true(heartbeat)
  expect_identical(core$desktop_nca_get(e)$job$declared_actor, f$args$actor)
  completed <- wait_job(e, job$job_id)
  expect_identical(completed$status, "succeeded")
  expect_counts_only(completed$result)
  expect_equal(e$nca_result$value$data$AVAL, reference$AVAL, tolerance = 1e-12)
  expect_null(e$nca_worker)
  expect_identical(core$desktop_nca_get(e)$job$result_id, completed$result_id)
  expect_false(dir.exists(file.path(e$project, ".nca-jobs", job$job_id)))
})

test_that("worker publication failure cannot reveal logs or revive an earlier result", {
  f <- fixture("S03"); on.exit(close_fixture(f), add = TRUE); e <- f$e
  old <- core$desktop_nca_run_shared(e, f$args)
  core$desktop_nca_set_disclosure(e, old$result_id, TRUE)
  original <- core$desktop_nca_publish
  on.exit(core$desktop_nca_publish <- original, add = TRUE)
  core$desktop_nca_publish <- function(...) stop("PRIVATE_VALUE_11.7")
  job <- core$desktop_nca_start_job(e, f$args)
  expect_counts_only(core$desktop_nca_public(e, old$result_id, f$args$project_id))
  err <- tryCatch(wait_job(e, job$job_id), pkapp_error = identity)
  expect_identical(err$code, "E_NCA_FAILED")
  expect_false(grepl("PRIVATE_VALUE", conditionMessage(err)))
  expect_false(file.exists(file.path(e$project, ".nca-jobs", job$job_id, "input.rds")))
  expect_true(file.exists(file.path(e$project, ".nca-jobs", job$job_id, "worker.log")))
  expect_identical(as.integer(file.info(file.path(e$project, ".nca-jobs", job$job_id, "worker.log"))$mode), 384L)
  expect_identical(core$desktop_nca_get(e)$result_status, "failed")
  expect_false(core$desktop_nca_public(e, old$result_id, f$args$project_id)$is_current)
  core$desktop_nca_publish <- original
  core$desktop_nca_configure(e, f$args)
  expect_identical(core$desktop_nca_get(e)$result_status, "failed")
})

test_that("closing an owner stops its owned worker and removes the private snapshot", {
  f <- fixture("S03"); on.exit(unlink(f$e$project, recursive = TRUE), add = TRUE); e <- f$e
  job <- core$desktop_nca_start_job(e, f$args)
  process <- e$nca_worker$process
  core$desktop_nca_jobs_close(e)
  expect_false(process$is_alive())
  expect_null(e$nca_worker)
  expect_identical(core$desktop_nca_get(e)$job$status, "cancelled")
  expect_false(dir.exists(file.path(e$project, ".nca-jobs", job$job_id)))
  expect_error(core$desktop_nca_run_shared(e, f$args), "closing")
})


test_that("worker execution timeout preserves a safe failure state", {
  f <- fixture("S03"); on.exit(close_fixture(f), add = TRUE); e <- f$e
  old <- core$desktop_nca_run_shared(e, f$args)
  core$desktop_nca_set_disclosure(e, old$result_id, TRUE)
  e$nca_job_timeout <- -1
  job <- core$desktop_nca_start_job(e, f$args)
  process <- e$nca_worker$process
  err <- tryCatch(wait_job(e, job$job_id), pkapp_error = identity)
  expect_identical(err$code, "E_TIMEOUT")
  expect_false(process$is_alive())
  expect_null(e$nca_worker)
  expect_identical(core$desktop_nca_get(e)$result_status, "failed")
  expect_counts_only(core$desktop_nca_public(e, old$result_id, f$args$project_id))
})
