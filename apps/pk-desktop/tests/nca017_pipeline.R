source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)

test_that("authenticated handler prepares actual-time data, runs, saves and records exact provenance", {
  project <- tempfile("nca-api-"); dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  runtime <- core$desktop_r_runtime(engine, 3880, 3879, "nca")
  api <- core$desktop_http_app(engine, runtime)
  request <- function(operation, args = list(), origin = NULL) {
    payload <- charToRaw(if (!length(args)) "{}" else core$desktop_json(args))
    response <- api$call(list(REQUEST_METHOD = "POST", PATH_INFO = paste0("/v1/", operation),
      HTTP_AUTHORIZATION = paste("Bearer", runtime$token), HTTP_ORIGIN = origin,
      HTTP_X_PKAPP_CONTRACT = "pkapp-nca-18", CONTENT_TYPE = "application/json", CONTENT_LENGTH = as.character(length(payload)),
      rook.input = list(read = function() payload)))
    jsonlite::fromJSON(response$body, simplifyVector = FALSE)
  }
  call <- function(operation, args = list()) {
    answer <- request(operation, args)
    expect_true(answer$ok, info = core$desktop_json(answer))
    if (!isTRUE(answer$ok)) stop("HTTP operation failed: ", operation)
    answer$result
  }
  expect_identical(request("status", origin = "http://example.invalid")$error$code, "E_AUTH")
  status <- call("status")
  context <- list(project_id = status$project_id, base_revision = status$revision,
    actor = "integration operator", reason = "Approved artificial example")
  data <- pkapp_read_table("apps/pk-desktop/examples/nca_actual_time_example.csv")
  data$ID <- "PRIVATE_PROFILE_919_NOT_FOR_MCP"
  path <- file.path(project, "example.csv"); core$pk_write_csv(data, path)
  input <- call("import_table", c(context, list(path = path, name = "artificial input")))
  steps <- list(list(op = "elapsed_time", sample_column = "SAMPLE_DTM", dose_column = "DOSE_DTM", utc_offset = "+09:00"),
    list(op = "blq_rules", mode = "positional", profile_columns = c("ID", "ANALYTE", "PERIOD"),
      concentration = "CONC", time = "NCA_TIME_H", blq_column = "BLQ", mdv_column = "MDV", flag_convention = "y_blank",
      zero_time = "pre", pre_rule = "zero", first_rule = "zero", middle_rule = "exclude", last_rule = "exclude",
      all_blq_rule = "exclude", lloq_source = "column", lloq_column = "LLOQ"))
  prepared <- call("prepare", c(context, list(source = input$id, steps = steps, name = "prepared")))
  spec <- list(id = "ID", groups = c("ANALYTE", "PERIOD"), time = "NCA_TIME_H", concentration = "NCA_CONC",
    unit_column = "UNIT", blq_column = "NCA_BLQ", mdv_column = "NCA_MDV", blq = "as_recorded", c0_method = "zero",
    auc_method = "linear", predose_qc_threshold_pct = 5, predose_qc_window_h = 1)
  state <- call("nca_configure", c(context, list(source = prepared$id, specification = spec)))
  expect_identical(request("nca_run", context)$error$code, "E_REVISION_CONFLICT")
  context$base_revision <- state$revision
  job <- call("nca_run", context)
  deadline <- Sys.time() + 60
  repeat {
    state_job <- call("nca_get_job", list(project_id = context$project_id, job_id = job$job_id))
    if (identical(state_job$status, "succeeded")) break
    expect_false(state_job$status %in% c("failed", "cancelled"))
    if (Sys.time() > deadline) stop("NCA worker timed out")
    later::run_now(0.05)
    Sys.sleep(0.05)
  }
  result <- state_job$result
  expect_true(all(vapply(result$parameters, function(x) is.null(x$mean) && is.null(x$min) && is.null(x$max), logical(1))))
  expect_equal(engine$nca_result$value$data$AVAL, c(8, 1, 31.5), tolerance = 1e-12)
  expect_equal(result$qc_status$pass_blq, 1)
  expect_false(grepl(data$ID[1], core$desktop_json(result), fixed = TRUE))
  expect_identical(engine$nca_result$execution$actor, context$actor)
  expect_identical(tail(engine$nca$audit, 1)[[1]]$specification, engine$nca$specification)
  original_nca <- core$pkapp_nca
  core$pkapp_nca <- function(...) stop("Save must not recalculate")
  on.exit(core$pkapp_nca <- original_nca, add = TRUE)
  saved <- call("nca_save_result", c(context, list(result_id = result$result_id, name = "frozen result")))
  expect_identical(saved$provenance$execution$result_id, result$result_id)
  expect_identical(saved$provenance$save_reason, context$reason)
  core$pkapp_nca <- original_nca
  expect_equal(pkapp_replay(core$desktop_table_path(engine, saved$id))$data$AVAL, c(8, 1, 31.5), tolerance = 1e-12)
  expect_identical(core$desktop_nca_engine(project)$nca$state_hash, engine$nca$state_hash)
  bad <- spec; bad$groups <- "ANALYTE"
  denied <- request("nca_configure", c(context, list(source = prepared$id, specification = bad)))
  expect_false(denied$ok)
  expect_false(grepl(data$ID[1], core$desktop_json(denied), fixed = TRUE))
  expect_equal(engine$nca$revision, state$revision)
  persisted <- core$desktop_read(file.path(project, "nca-state.json"))
  persisted$specification$auc_method <- "lin up/log down"
  core$desktop_atomic(persisted, file.path(project, "nca-state.json"))
  expect_error(core$desktop_nca_engine(project), "Invalid NCA state")
})
