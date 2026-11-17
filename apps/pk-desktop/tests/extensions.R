source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
base <- tempfile("pk-ext-"); dir.create(base)
bundle <- file.path(base, "input"); dir.create(bundle)
subjects <- data.frame(USUBJID = sprintf("%03d", 1:8), TRT01A = rep(c("Placebo", "Active"), each = 4),
  SEX = rep(c("F", "F", "M", "M"), 2), REGION = rep(c("001", "010"), 4))
conc <- subjects[rep(1:8, each = 3), ]
conc$TIME_H <- rep(0:2, 8); conc$AVAL <- as.numeric(rep(1:8, each = 3) + conc$TIME_H); conc$AVALU <- "ng/mL"
params <- subjects; params$PARAMCD <- "CMAX"; params$AVAL <- as.numeric(1:8 + 2); params$AVALU <- "ng/mL"
core$pk_write_csv(conc, file.path(bundle, "ADNCA.csv")); core$pk_write_csv(params, file.path(bundle, "ADPP.csv"))
p <- pkapp_open(file.path(base, "project"), bundle)

test_that("arbitrary strata and panels have independent mean and SE with a fixed denominator", {
  expect_true("TRT01A" %in% pkapp_status(p)$group_columns)
  pkapp_update(p, "conc_plot", list(group_by = "TRT01A", facet_by = "SEX", error = "se"), pkapp_spec(p)$revision, "SE by treatment and sex")
  a <- core$pk_analyse(p$engine$data, core$desktop_config(pkapp_spec(p)))
  expect_equal(nrow(a$concentration_summary), 12)
  for (i in seq_len(nrow(a$concentration_summary))) {
    s <- a$concentration_summary[i, ]
    x <- conc$AVAL[conc$TRT01A == s$.GROUP & conc$SEX == s$.FACET & conc$TIME_H == s$.TIME]
    expect_equal(s$n, length(x)); expect_equal(s$center, mean(x))
    expect_equal(s$lower, mean(x) - sd(x) / sqrt(length(x)))
    expect_equal(s$upper, mean(x) + sd(x) / sqrt(length(x)))
  }
  expect_equal(sum(a$parameter_summary$n), 8)
  expect_s3_class(ggplot2::ggplotGrob(core$pk_plot_concentration(a)), "gtable")
  before <- pkapp_spec(p)
  expect_error(pkapp_update(p, "conc_plot", list(facet_by = "UNKNOWN"), before$revision, "invalid"), "facet_by")
  expect_identical(pkapp_spec(p), before)
  expect_error(pkapp_update(p, "conc_plot", list(group_by = "USUBJID"), before$revision, "invalid"), "group_by")
  expect_error(pkapp_update(p, "conc_plot", list(statistic = "median"), before$revision, "invalid"), "arithmetic mean")
  cfg <- core$desktop_config(before); altered <- p$engine$data; altered$ADPP$TRT01A[1] <- "inconsistent"
  expect_error(core$pk_validate_config(cfg, altered), "Inconsistent grouping")
})

saved <- pkapp_run(p)
test_that("replay retains live settings and verifies original inputs and byte-identical CSVs", {
  pkapp_update(p, "conc_plot", list(group_by = "SEX", facet_by = character(), error = "none"), pkapp_spec(p)$revision, "different live view")
  current <- pkapp_spec(p)
  replay <- pkapp_replay_run(p, saved$run_id)
  expect_identical(pkapp_spec(p), current)
  expect_identical(replay$analysis_hash, saved$analysis_hash)
  m <- pkapp_run_manifest(p, replay$run_id)
  expect_identical(m$replay_of, saved$run_id)
  for (name in c("tables/concentration_summary.csv", "tables/parameter_summary.csv"))
    expect_identical(m$outputs[[name]], pkapp_run_manifest(p, saved$run_id)$outputs[[name]])
  expect_error(pkapp_replay_run(p, saved$run_id, current$revision - 1), class = "pkapp_error")
  expect_error(pkapp_replay_run(p, "../state.json"), class = "pkapp_error")
})

test_that("UI and MCP replay use the same saved Run without changing current revision", {
  engine <- p$engine; current <- engine$spec
  n <- length(pkapp_runs(p))
  shiny::testServer(core$desktop_module_server, args = list(engine = engine), {
    session$setInputs(report_run = saved$run_id, base_revision = current$revision, replay_run = 1)
    expect_match(notice(), "CSV一致")
    expect_match(output$run_manifest, "analysis_hash")
  })
  expect_length(pkapp_runs(p), n + 1)
  other <- core$desktop_dispatch(engine, "replay_run", list(run_id = saved$run_id, base_revision = current$revision, project_id = current$project_id))
  expect_identical(other$analysis_hash, saved$analysis_hash)
  expect_identical(engine$spec, current)
})

test_that("tampered inputs and changed renderer records stop replay before publication", {
  path <- file.path(pkapp_spec(p)$data$bundle, "ADNCA.csv")
  original <- readBin(path, "raw", n = file.info(path)$size)
  n <- length(pkapp_runs(p))
  cat("\n", file = path, append = TRUE)
  expect_error(pkapp_replay_run(p, saved$run_id))
  writeBin(original, path)
  expect_length(pkapp_runs(p), n)
  manifest_path <- file.path(p$engine$project, "runs", saved$run_id, "manifest.json")
  old <- core$desktop_read(manifest_path); changed <- old; changed$environment$code_hashes[[1]] <- paste(rep("0", 64), collapse = "")
  core$desktop_atomic(changed, manifest_path)
  expect_error(pkapp_replay_run(p, saved$run_id), "different code")
  core$desktop_atomic(old, manifest_path)
})

test_that("frozen Word rendering records panels, all grouping variables and Run footer", {
  pkapp_update(p, "conc_plot", list(group_by = c("TRT01A", "SEX"), facet_by = "REGION"), pkapp_spec(p)$revision, "Word panels")
  run <- pkapp_run(p)
  folder <- core$desktop_run_folder(p$engine, run$run_id)
  target <- Sys.getenv("PK_TEST_REPORT_DIR", tempfile("pk-render-"))
  job <- list(job_id = "job-20261107T000000-abcdef123456", run_manifest_sha256 = core$pk_hash(file.path(folder, "manifest.json")),
    project_id = pkapp_spec(p)$project_id, triggered_by = "r", actor = "Renderer test", reason = "Frozen report verification", requested_at = core$desktop_time())
  receipt <- core$desktop_render_report(folder, target, job)
  manifest <- core$desktop_read(file.path(target, "REPORT_MANIFEST.json"))
  expect_identical(receipt$report_manifest_sha256, core$pk_hash(file.path(target, "REPORT_MANIFEST.json")))
  expect_silent(core$desktop_verify_files(target, manifest$outputs))
  xml_dir <- tempfile("xml-"); utils::unzip(file.path(target, "report.docx"), exdir = xml_dir)
  doc <- xml2::read_xml(file.path(xml_dir, "word/document.xml"))
  texts <- xml2::xml_text(xml2::xml_find_all(doc, "//w:t", xml2::xml_ns(doc)))
  expect_true(any(grepl("TRT01A × SEX", texts, fixed = TRUE)))
  expect_true(any(grepl("Panel: 001", texts, fixed = TRUE)))
  expect_true(any(grepl("Panel: 010", texts, fixed = TRUE)))
  footer <- readLines(file.path(xml_dir, "word/footer1.xml"), warn = FALSE)
  expect_true(any(grepl(run$run_id, footer, fixed = TRUE)))
  unlink(xml_dir, recursive = TRUE)
  cat("Verified report:", file.path(target, "report.docx"), "\n")
})
pkapp_close(p)
