source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)

test_that("R HTTP independently validates preparation shapes and returns fixed, repairable errors", {
  project <- tempfile("nca-contract-"); dir.create(project)
  on.exit(unlink(project, recursive = TRUE), add = TRUE)
  engine <- core$desktop_nca_engine(project)
  runtime <- core$desktop_r_runtime(engine, 3880, 3879, "nca")
  api <- core$desktop_http_app(engine, runtime)
  request <- function(operation, args = list(), contract = "pkapp-nca-18", body = NULL) {
    payload <- charToRaw(if (!is.null(body)) body else if (!length(args)) "{}" else core$desktop_json(args))
    response <- api$call(list(REQUEST_METHOD = "POST", PATH_INFO = paste0("/v1/", operation),
      HTTP_AUTHORIZATION = paste("Bearer", runtime$token), HTTP_X_PKAPP_CONTRACT = contract,
      CONTENT_TYPE = "application/json", CONTENT_LENGTH = as.character(length(payload)),
      rook.input = list(read = function() payload)))
    jsonlite::fromJSON(response$body, simplifyVector = FALSE)
  }
  expect_identical(request("status", contract = NULL)$error$code, "E_VERSION")
  expect_identical(request("status", contract = "pkapp-nca-1")$error$code, "E_VERSION")
  expect_true(request("status")$ok)
  for (body in c("[]", "PRIVATE_INPUT_VALUE_919", '{"source":"PRIVATE_INPUT_VALUE_919",}')) {
    answer <- request("prepare", body = body)
    expect_identical(answer$error$code, "E_VALIDATION")
    expect_false(grepl("PRIVATE_INPUT_VALUE_919", core$desktop_json(answer), fixed = TRUE))
  }
  data <- data.frame(ID = c("S01", "S02", "S03"), CONC = c("5", "PRIVATE_INPUT_VALUE_919", "11.7"), stringsAsFactors = FALSE)
  input <- core$desktop_table_save(engine, "synthetic", list(source = data), list(kind = "prepare", steps = list()))
  context <- list(source = input$id, name = "prepared", base_revision = engine$nca$revision, project_id = engine$nca$project_id,
    actor = "self declared", `_mcp_client` = list(name = "test client", version = "1", source = "mcp_initialization_self_reported"))
  parsed <- function(json) jsonlite::fromJSON(json, simplifyVector = FALSE)
  invalid <- c(
    '[{"op":"filter","column":"ID","operator":"eq","value":"PRIVATE_INPUT_VALUE_919"}]',
    '[{"op":"filter","column":"ID","operator":"PRIVATE_INPUT_VALUE_919","values":["S03"]}]',
    '[{"op":"filter","column":"ID","operator":"eq","values":"S03"}]',
    '[{"op":"filter","column":"ID","operator":"eq","values":[]}]',
    '[{"op":"filter","column":"ID","operator":"eq","values":[1,2]}]',
    '[{"op":"select","columns":["ID","ID"]}]',
    '[{"op":"rename","column":"ID","new_name":"PRIVATE_INPUT_VALUE_919"}]',
    '[{"op":"convert","column":"ID","type":"PRIVATE_INPUT_VALUE_919"}]',
    '[{"op":"sort","columns":["ID"],"decreasing":"PRIVATE_INPUT_VALUE_919"}]',
    '[{"op":"distinct","columns":"ID"}]',
    '[{"op":"where","predicate":{"op":"system","args":[{"value":"PRIVATE_INPUT_VALUE_919"}]}}]',
    '[{"op":"elapsed_time","sample_column":"SAMPLE","dose_column":"DOSE","utc_offset":"bad"}]',
    '[{"op":"blq_rules","mode":"positional","concentration":"CONC","time":"TIME","blq_column":"BLQ","pre_rule":"zero"}]',
    '[{"op":["PRIVATE_INPUT_VALUE_919"]}]',
    '["PRIVATE_INPUT_VALUE_919"]')
  for (json in invalid) {
    answer <- request("prepare", c(context, list(steps = parsed(json))))
    expect_identical(answer$error$code, "E_VALIDATION", info = json)
    expect_false(grepl("PRIVATE_INPUT_VALUE_919", core$desktop_json(answer), fixed = TRUE))
    expect_false(grepl("Operation failed", answer$error$message, fixed = TRUE))
  }
  semantic <- c(
    '[{"op":"filter","column":"PRIVATE_INPUT_VALUE_919","operator":"eq","values":["S03"]}]',
    '[{"op":"convert","column":"CONC","type":"numeric"}]',
    '[{"op":"rename","column":"ID","name":"CONC"}]',
    '[{"op":"where","predicate":{"op":"==","args":[{"column":"PRIVATE_INPUT_VALUE_919"},{"value":"S03"}]}}]')
  for (json in semantic) {
    answer <- request("prepare", c(context, list(steps = parsed(json))))
    expect_identical(answer$error$code, "E_VALIDATION")
    expect_false(grepl("PRIVATE_INPUT_VALUE_919", core$desktop_json(answer), fixed = TRUE))
    expect_false(grepl("Operation failed", answer$error$message, fixed = TRUE))
  }
  expect_length(core$desktop_tables(engine), 1L)
  steps <- parsed('[{"op":"filter","column":"ID","operator":"eq","values":["S03"]},{"op":"select","columns":["ID","CONC"]}]')
  answer <- request("prepare", c(context, list(steps = steps)))
  expect_true(answer$ok, info = core$desktop_json(answer))
  expect_equal(answer$result$counts$input, 3)
  expect_equal(answer$result$counts$result, 1)
  saved <- core$desktop_table_get(engine, answer$result$id)
  expect_identical(saved$ID, "S03")
  expect_identical(saved$CONC, "11.7")
  expect_identical(pkapp_replay(core$desktop_table_path(engine, answer$result$id))$data, saved)
  expect_identical(answer$result$provenance$identity$declared_actor, context$actor)
})
