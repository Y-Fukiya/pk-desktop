source("apps/pk-desktop/tests/nca024_core.R")

test_that("public warnings explain a real additional EX dose in Japanese and preserve diagnostics", {
  fixture <- new.env(parent = globalenv())
  capture.output(sys.source("apps/pk-desktop/examples/nca_intervening_dose/reproduce.R", envir = fixture))
  value <- fixture$result
  expect_type(value$warnings, "character")
  expect_true(any(grepl("ID=", value$warnings, fixed = TRUE)))
  expect_true(any(grepl("CMAX・TMAX・AUCLST", value$warnings, fixed = TRUE)))
  expect_true(any(grepl("解析区間に追加投与があります", value$warnings, fixed = TRUE)))
  expect_true(all(grepl("[ぁ-んァ-ン一-龠]", value$warnings)))
  expect_false(any(grepl("Profile |intervening_dose|未翻訳", value$warnings)))
  expect_true(any(grepl("Profile 1 terminal: intervening_dose", value$trace$warnings, fixed = TRUE)))
  expect_true(all(value$data$NCA_EXCLUDE[fixture$affected] == "intervening_dose"))
  group <- Filter(function(x) x$status == "excluded", value$trace$groups)[[1]]
  expect_identical(group$dose_review$reason, "intervening_dose")
  expect_equal(group$dose_review$next_h, 12)
  expect_equal(group$dose_review$next_dose$ex_row, 5)
  expect_identical(fixture$prepared$data$NCA_ASSEMBLY_SIGNATURE, fixture$assembled$data$NCA_ASSEMBLY_SIGNATURE)
  analysis_core <- environment(pkapp_nca)
  before <- analysis_core$desktop_nca_warning_text
  analysis_core$desktop_nca_warning_text <- identity
  projection_before <- analysis_core$desktop_nca_public_warnings
  analysis_core$desktop_nca_public_warnings <- function(value) value$trace$warnings
  on.exit({ analysis_core$desktop_nca_warning_text <- before; analysis_core$desktop_nca_public_warnings <- projection_before })
  raw <- pkapp_nca(fixture$prepared$data, specification)
  expect_identical(value$data, raw$data)
  expect_identical(value$counts, raw$counts)
  expect_identical(value$trace, raw$trace)
  expect_identical(raw$warnings, raw$trace$warnings)
  cat("PASS NCA026 public Japanese warnings; terminal and EX 12 h codes, values, counts, source trace retained.\n")
})

test_that("derived human warnings project existing review and exclusion reasons once", {
  f <- fixture024(); f$vs$VALUE[1] <- 5000
  value <- run024(f)
  for (reason in c("outside_range_weight", "oral_quantifiable_time_zero")) {
    expect_true(reason %in% value$trace$groups[[1]]$warning)
    expect_equal(sum(grepl(reason, value$trace$warnings, fixed = TRUE)), 1)
    expect_false(any(grepl(reason, value$warnings, fixed = TRUE)))
    expect_true(any(grepl(core$desktop_nca_diagnostic_label(reason), value$warnings, fixed = TRUE)))
  }
  expect_equal(reason024(value, "CLFOWT"), "outside_range_weight")
  expect_equal(vals024(value, "CLFO"), 1, tolerance = 1e-11)
  expect_true(is.na(vals024(value, "CLFOWT")))
  f <- fixture024(); f$data$DOSE <- 0
  value <- run024(f)
  expect_true(any(grepl("実投与量が欠測または正の有限値ではありません", value$warnings, fixed = TRUE)))
  expect_true("invalid_dose" %in% value$trace$groups[[1]]$warning)
  expect_equal(vals024(value, "CMAX"), 10)
})

test_that("untranslated vendor warnings retain the exact original diagnostic", {
  original <- "Profile 7 terminal: vendor_failure <script>alert(1)</script>"
  expect_identical(core$desktop_nca_warning_text(original), paste0("未翻訳の診断があります（原文を確認）：", original))
  expect_identical(core$desktop_nca_warning_text("Profile 7 terminal: intervening_dose"),
    "解析単位 7 終末相: 解析区間に追加投与があります")
})
