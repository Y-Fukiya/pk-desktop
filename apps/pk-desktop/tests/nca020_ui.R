source("apps/pk-desktop/tests/nca020_annotation.R")
make_ui020 <- function() {
  f <- fixture020(); e <- new.env(parent = emptyenv()); e$project <- tempfile("annotation-ui-"); dir.create(e$project)
  core$desktop_tables_init(e)
  nca <- core$desktop_table_save(e, "NCA synthetic", list(source = f$input), list(kind = "nca", specification = f$result$specification), frozen = f$result)
  dm <- core$desktop_table_save(e, "DM synthetic", list(source = f$dm), list(kind = "prepare", steps = list()))
  vs <- core$desktop_table_save(e, "VS synthetic", list(source = f$vs), list(kind = "prepare", steps = list()))
  list(engine = e, nca = nca$id, dm = dm$id, vs = vs$id, f = f)
}
map_ui020 <- function(session, x) {
  session$setInputs(result_source = x$nca, dm_source = x$dm, vs_source = x$vs, dm_key_ID = "USUBJID", dm_key_STUDYID = "STUDYID", dm_columns = c("SEX", "AGE"),
    vs_key_ID = "USUBJID", vs_key_STUDYID = "STUDYID", vs_scope = "subject", vs_test = "VSTESTCD", vs_value = "VSSTRESN", vs_unit = "VSSTRESU",
    vs_datetime = "VSDTC", vs_visit = "VISIT", vs_measure_BMI = "BMI", vs_selection = "filter", vs_filter_column = "VISIT", vs_filter_values = "BASELINE", name = "Annotations")
}
test_that("annotation UI is optional and saves a separate result with replay", {
  x <- make_ui020(); on.exit(unlink(x$engine$project, recursive = TRUE))
  html <- as.character(core$desktop_annotation_ui("annotation"))
  expect_match(html, "SAS7BDAT", fixed = TRUE)
  expect_match(html, "任意", fixed = TRUE)
  expect_match(html, 'value="NONE" selected', fixed = TRUE)
  shiny::testServer(core$desktop_annotation_server, args = list(engine = x$engine), {
    map_ui020(session, x)
    expect_false(grepl("vs_key_PERIOD", output$vs_mapping$html, fixed = TRUE))
    session$setInputs(preview = 1)
    expect_identical(result()$value$data[names(x$f$result$data)], x$f$result$data)
    expect_match(output$preview_result$html, "19.5", fixed = TRUE)
    session$setInputs(save = 1)
    expect_equal(length(core$desktop_tables(x$engine)), 4)
    expect_identical(pkapp_replay(core$desktop_table_path(x$engine, saved()$id))$data, result()$value$data)
    session$setInputs(vs_filter_values = "FOLLOWUP")
    expect_null(result())
    session$setInputs(save = 2)
    expect_equal(length(core$desktop_tables(x$engine)), 4)
    session$setInputs(vs_scope = "keys")
    expect_match(output$vs_mapping$html, "vs_key_PERIOD", fixed = TRUE)
  })
})
test_that("ambiguous VS or no explicit selection cannot be saved", {
  x <- make_ui020(); on.exit(unlink(x$engine$project, recursive = TRUE))
  shiny::testServer(core$desktop_annotation_server, args = list(engine = x$engine), {
    map_ui020(session, x)
    session$setInputs(vs_selection = "", preview = 1)
    expect_null(result())
    session$setInputs(vs_selection = "all_unique", preview = 2)
    expect_null(result()); expect_match(notice(), "重複")
    session$setInputs(vs_source = "NONE", preview = 3)
    expect_true(!is.null(result()))
    expect_false(any(grepl("^VS_", names(result()$value$data))))
  })
})
