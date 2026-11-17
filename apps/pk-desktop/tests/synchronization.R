source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
test_that("MCP updates reach UI and UI/MCP produce identical aggregate CSVs", {
  project <- pkapp_open(tempfile("sync-")); withr::defer(pkapp_close(project))
  engine <- project$engine
  shiny::testServer(core$desktop_module_server, args = list(engine = engine), {
    scope <- list(base_revision = engine$spec$revision, project_id = engine$spec$project_id)
    value <- core$desktop_dispatch(engine, "update", c(scope, list(section = "conc_plot",
      values = list(group_by = list("ARM", "SEX")), actor = "test", reason = "sync")))
    session$flushReact()
    expect_match(output$revision, "Revision 2")
    expect_match(output$editor$html, "SEX")
    session$setInputs(profile = "all", x = "TIME_H", group = c("ARM", "SEX"), scale = "linear", error = "none",
      blq = "exclude", reason = "same UI settings", base_revision = 2, apply = 1)
    expect_identical(engine$spec$updated_by, "ui")
    first <- core$desktop_run(engine, engine$spec$revision, engine$spec$project_id, "ui")
    second <- core$desktop_dispatch(engine, "run", list(base_revision = engine$spec$revision, project_id = engine$spec$project_id))
    expect_identical(first$analysis_hash, second$analysis_hash)
    paths <- lapply(c(first$run_id, second$run_id), function(id) file.path(core$desktop_run_folder(engine, id), "tables", "parameter_summary.csv"))
    expect_identical(core$pk_hash(paths[[1]]), core$pk_hash(paths[[2]]))
  })
})
