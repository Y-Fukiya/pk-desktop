source("apps/pk-desktop/pkapp.R")
core <- environment(pkapp_open)

testthat::test_that("explicit UI translation markers retain both fixed texts", {
  x <- as.character(core$desktop_i18n_text("濃度データ", "Concentration data"))
  testthat::expect_match(x, 'data-pk-ja="濃度データ"', fixed = TRUE)
  testthat::expect_match(x, 'data-pk-en="Concentration data"', fixed = TRUE)
  testthat::expect_match(x, '>濃度データ</span>', fixed = TRUE)
})

testthat::test_that("language controls disclose the incomplete scope and do not bind an action input", {
  x <- as.character(core$desktop_i18n_controls())
  testthat::expect_match(x, "English (preview)", fixed = TRUE)
  testthat::expect_match(x, "some labels, warnings, errors, plots and exported files remain in Japanese", fixed = TRUE)
  testthat::expect_match(x, 'type="button"', fixed = TRUE)
  testthat::expect_false(grepl('class="action-button', x, fixed = TRUE))
})

testthat::test_that("NCA UI contains labelled clinical cautions in both languages", {
  x <- as.character(core$desktop_nca_ui("nca"))
  testthat::expect_match(x, "in the selected input time unit", fixed = TRUE)
  testthat::expect_match(x, "Automatic terminal-phase selection uses adjusted R². QC uses unadjusted R².", fixed = TRUE)
  testthat::expect_match(x, "When the maximum concentration occurs more than once, Tmax is the first such time.", fixed = TRUE)
  testthat::expect_match(x, "do not determine whether a subject or study period is eligible", fixed = TRUE)
})
