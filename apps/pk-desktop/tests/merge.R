source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
left <- data.frame(STUDYID = c("A", "A", "B", "A"), USUBJID = c("001", "001", "001", "002"),
                   AVAL = c(2, 2, 7, 5))
right <- data.frame(STUDYID = c("B", "A"), USUBJID = "001", AVAL = c(31, 11), AVALU = "ng/mL")
keys <- c("STUDYID", "USUBJID")

test_that("explicit composite joins preserve observations, units and source columns", {
  m <- pkapp_merge(left, right, keys, right_name = "ADPP")
  expect_identical(m$data[names(left)], left)
  expect_equal(m$data$ADPP_AVAL, c(11, 11, 31, NA))
  expect_equal(m$data$AVALU, c(rep("ng/mL", 3), NA))
  expect_equal(unname(m$counts), c(4, 2, 3, 1, 4))
  matched <- pkapp_merge(left, right, keys, join = "inner")
  expect_identical(matched$data$USUBJID, rep("001", 3))
  expect_equal(matched$data$RIGHT_AVAL, c(11, 11, 31))
  absent <- pkapp_merge(left, right[0, ], keys, join = "inner")
  expect_equal(nrow(absent$data), 0)
  expect_equal(absent$counts[["unmatched"]], 4)
})

test_that("ambiguous joins fail before exporting any rows", {
  expect_error(pkapp_merge(left, right, "USUBJID"), "重複")
  expect_error(pkapp_merge(left, right, character()), "結合キー")
  expect_error(pkapp_merge(left, right, keys, "invalid"), "追加する列")
  bad <- right
  bad$USUBJID[1] <- NA_character_
  expect_error(pkapp_merge(left, bad, keys), "欠測")
  bad <- left
  bad$USUBJID[2] <- " "
  expect_error(pkapp_merge(bad, right, keys), "空欄")
  collision <- left
  collision$ADPP_AVAL <- 1
  expect_error(pkapp_merge(collision, right, keys, right_name = "ADPP"), "衝突")
})

test_that("PK tab supports an ADPP parameter, uploaded lookup, CSV download and invalidation", {
  engine <- list(data = list(ADNCA = left, ADPP = transform(right, PARAMCD = "CMAXSS")))
  file <- tempfile(fileext = ".csv")
  observations <- tempfile(fileext = ".csv")
  on.exit(unlink(c(file, observations)), add = TRUE)
  writeLines(c("STUDYID,USUBJID,COHORT", "A,001,Alpha", "B,001,Beta"), file)
  writeLines(c("STUDYID,USUBJID,VISIT", "A,001,1", "A,001,2", "A,999,1"), observations)
  shiny::testServer(core$desktop_merge_server, args = list(engine = engine), {
    session$setInputs(left = "ADNCA", right = "ADPP", param = "CMAXSS", keys = keys,
                      columns = c("AVAL", "AVALU"), method = "left")
    expect_match(output$qc, "左表の一致 3 行／不一致 1 行")
    expect_match(output$preview, "ADPP_AVAL")
    expect_match(output$download_ui$html, "結合結果をCSVで保存")
    session$setInputs(right = "CSV", right_csv = list(datapath = file), columns = "COHORT")
    saved <- core$pk_csv(output$csv)
    expect_identical(saved$USUBJID, engine$data$ADNCA$USUBJID)
    expect_identical(saved$COHORT, c("Alpha", "Alpha", "Beta", NA))
    session$setInputs(keys = "USUBJID")
    expect_match(output$qc, "重複")
    expect_null(output$download_ui)
    expect_null(result()$data)
    session$setInputs(keys = keys, method = "inner")
    expect_equal(nrow(core$pk_csv(output$csv)), 3L)
    session$setInputs(left = "CSV", left_csv = list(datapath = observations), method = "left")
    external <- core$pk_csv(output$csv)
    expect_identical(external$USUBJID, c("001", "001", "999"))
    expect_identical(external$COHORT, c("Alpha", "Alpha", NA))
    expect_match(output$qc, "一致 2 行／不一致 1 行")
  })
})

test_that("numeric time keys from CSV are parsed against the PK type while IDs stay text", {
  csv <- data.frame(USUBJID = c("001", "002"), TIME_H = c("1.0", "2.50"))
  typed <- transform(csv, TIME_H = c(1, 2.5))
  expect_identical(core$desktop_merge_csv_keys(csv, typed, names(csv)), typed)
  csv$TIME_H[1] <- "invalid"
  expect_error(core$desktop_merge_csv_keys(csv, typed, names(csv)), "Non-numeric")
})

test_that("the project sample adds the correct CMAXSS to all 750 concentrations", {
  data <- core$pk_load_bundle(file.path(pkapp_root(), "outputs/pk-review/bundles/reviewed-repeated"))
  pk <- data$ADPP[data$ADPP$PARAMCD == "CMAXSS", ]
  result <- pkapp_merge(data$ADNCA, pk, keys, c("PARAMCD", "AVAL", "AVALU"), right_name = "ADPP")
  expect_equal(unname(result$counts), c(750L, 50L, 750L, 0L, 750L))
  expect_identical(result$data[names(data$ADNCA)], data$ADNCA)
  idx <- match(data$ADNCA$USUBJID, pk$USUBJID)
  expect_equal(result$data$ADPP_AVAL, pk$AVAL[idx])
  expect_identical(result$data$ADPP_AVALU, pk$AVALU[idx])
})

test_that("authenticated workspace navigation applies a join and downloads all rows", {
  project <- pkapp_open(tempfile("pk-merge-ui-"))
  withr::defer(pkapp_close(project))
  app <- core$desktop_app(project$engine, "merge-test")
  withr::local_options(list(teal.lockfile.mode = "disabled"))
  withr::local_envvar(c(R_USER_CACHE_DIR = tempdir()))
  html <- app$httpHandler(list(REQUEST_METHOD = "GET", PATH_INFO = "/", QUERY_STRING = "token=merge-test"))
  expect_equal(html$status, 200L)
  expect_match(html$content, "データ準備", fixed = TRUE)
  session <- shiny::MockShinySession$new()
  withr::defer(session$close())
  session$clientData <- shiny::reactiveValues(url_search = "?token=merge-test", pixelratio = 1)
  server <- app$serverFuncSource()
  shiny::withReactiveDomain(session, server(session$input, session$output, session))
  session$setInputs(workspace_navigation = "data")
  id <- "workbench-"
  values <- list(mode = "join", right_source = "ADPP", keys = keys, join_columns = c("AVAL", "AVALU"),
                 method = "left", right_param = "CMAXSS")
  do.call(session$setInputs, stats::setNames(values, paste0(id, names(values))))
  session$setInputs("workbench-apply_join" = 1)
  expect_match(session$getOutput(paste0(id, "notice_ui"))$html, "結合を適用しました。750 行", fixed = TRUE)
  # A Shiny download request executes its filename and content callbacks in isolation.
  path <- shiny::isolate(session$getOutput(paste0(id, "csv")))
  joined <- core$pk_csv(path)
  expect_equal(nrow(joined), 750)
  expect_true(all(c("ADPP_AVAL", "ADPP_AVALU") %in% names(joined)))
})
