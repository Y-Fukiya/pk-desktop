source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
valid <- jsonlite::fromJSON('{"version":1,"mapping":{"CONC":{"ID":"ID","CONC":"CONC"},"PC":{"ID":"ID","SAMPLE_DTM":"DT"},"EX":{"ID":"ID","DOSE_ID":"DOSE","DOSE_DTM":"DT"}},"sample_keys":["ID"],"profile_keys":["ID"],"dose_group_keys":["ID"],"dose_record_keys":["ID","DOSE_ID"],"observation_keys":["ID"],"dose_reference":{"mode":"first"},"datetime":{"sample_type":"iso","dose_type":"iso","utc_offset":"+00:00"},"unmatched":"missing","missing_datetime":"missing","dose_history_confirmed":true}', simplifyVector = FALSE)
test_that("assembly specification schema rejects malformed values without echoing them", {
  expect_invisible(core$desktop_validate_assembly(valid))
  invalid <- list(NULL, list(), modifyList(valid, list(version = 2)), modifyList(valid, list(sample_keys = list("ID", "ID"))), modifyList(valid, list(datetime = list(utc_offset = "PRIVATE_INPUT_VALUE_919"))), modifyList(valid, list(extra = "PRIVATE_INPUT_VALUE_919")), modifyList(valid, list(mapping = list(CONC = list(ID = 2)))))
  invalid[[4]]$sample_keys <- list("ID", "ID")
  for (item in invalid) {
    err <- tryCatch({ core$desktop_validate_assembly(item); NULL }, error = identity)
    expect_s3_class(err, "pkapp_public_error")
    expect_identical(err$code, "E_VALIDATION")
    expect_false(grepl("PRIVATE_INPUT_VALUE_919", conditionMessage(err), fixed = TRUE))
  }
})

test_that("assembly snapshots sources and audits revision without returning rows", {
  skip_if_not(exists("pkapp_assemble_nca", envir = core, inherits = FALSE))
  project <- tempfile("nca019-"); dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  data <- list(conc = data.frame(ID = "SYNTHETIC", CONC = 3), pc = data.frame(ID = "SYNTHETIC", DT = "2025-01-01T01:00:00"), ex = data.frame(ID = "SYNTHETIC", DOSE = "D1", DT = "2025-01-01T00:00:00"))
  ids <- lapply(names(data), function(k) core$desktop_table_save(engine, k, list(source = data[[k]]), list(kind = "prepare", steps = list()))$id)
  names(ids) <- names(data)
  args <- c(ids, list(specification = valid, name = "assembly", base_revision = engine$nca$revision, project_id = engine$nca$project_id, actor = "test", reason = "synthetic test"))
  saved <- core$desktop_assemble_nca(engine, args)
  expect_equal(saved$revision, 2)
  expect_equal(saved$counts$result, 1)
  expect_false(grepl("SYNTHETIC", core$desktop_json(saved), fixed = TRUE))
  expect_identical(saved$provenance$source_ids, ids)
  expect_equal(readRDS(file.path(core$desktop_table_path(engine, saved$id), "inputs.rds")), data)
  event <- tail(engine$nca$audit, 1)[[1]]
  expect_identical(event$action, "nca_table_operation")
  expect_identical(event$source_ids, ids)
  expect_error(core$desktop_assemble_nca(engine, args), class = "pkapp_error")
  expect_identical(pkapp_replay(core$desktop_table_path(engine, saved$id))$data, core$desktop_table_get(engine, saved$id))
})

test_that("HTTP validates assembly independently without echoing invalid input", {
  project <- tempfile("nca019-http-"); dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  runtime <- core$desktop_r_runtime(engine, 3880, 3879, "nca")
  api <- core$desktop_http_app(engine, runtime)
  bad <- valid; bad$datetime$utc_offset <- "PRIVATE_INPUT_VALUE_919"
  payload <- charToRaw(core$desktop_json(list(specification = bad)))
  response <- api$call(list(REQUEST_METHOD = "POST", PATH_INFO = "/v1/assemble_nca",
    HTTP_AUTHORIZATION = paste("Bearer", runtime$token), HTTP_X_PKAPP_CONTRACT = "pkapp-nca-18",
    CONTENT_TYPE = "application/json", CONTENT_LENGTH = as.character(length(payload)), rook.input = list(read = function() payload)))
  answer <- jsonlite::fromJSON(response$body, simplifyVector = FALSE)
  expect_identical(answer$error$code, "E_VALIDATION")
  expect_false(grepl("PRIVATE_INPUT_VALUE_919", response$body, fixed = TRUE))
  expect_equal(engine$nca$revision, 1)
})
