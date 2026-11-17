source("apps/pk-desktop/tests/nca023_ui.R")

test_that("API warnings group parameter causes by all identifying keys", {
  value <- nca023_result()
  group <- value$trace$groups[[1]]
  second <- group; second$profile <- 2L; second$identity$ID <- "001"; second$identity$PERIOD <- "01"
  second$warning <- c("CMAX: intervening_dose", "TMAX: intervening_dose", "AUCLST: intervening_dose")
  second_data <- value$data; second_data$ID <- "001"; second_data$PERIOD <- "01"
  value$data <- rbind(value$data, second_data)
  value$trace$groups <- list(group, second)
  value$trace$warnings <- c(value$warnings, paste0("Profile 2: ", paste(second$warning, collapse = "; ")),
    "Profile 2 terminal: intervening_dose", "Global vendor diagnostic <x>")
  original <- value
  warnings <- core$desktop_nca_public_warnings(value)
  expect_equal(length(grep("解析区間に追加投与があります", warnings, fixed = TRUE)), 2)
  expect_match(warnings[1], "ID=SUBJ<01> / PERIOD=2 / ANALYTE=parent / STUDY=study-A：CMAX・TMAX・AUCLST：解析区間に追加投与があります", fixed = TRUE)
  expect_match(warnings[2], "ID=001 / PERIOD=01", fixed = TRUE)
  expect_match(warnings[2], "CMAX・TMAX・AUCLST・終末相", fixed = TRUE)
  expect_true(any(grepl("Global vendor diagnostic <x>", warnings, fixed = TRUE)))
  expect_identical(value, original)
  value$warnings <- warnings
  html <- as.character(core$desktop_nca_warnings(value))
  doc <- xml2::read_html(html)
  grouped <- xml2::xml_find_all(doc, '//details[contains(@class,"pk-warning-group")]')
  matched <- Filter(function(x) grepl("解析区間に追加投与があります", xml2::xml_text(xml2::xml_find_first(x, "./summary")), fixed = TRUE), as.list(grouped))
  expect_length(matched, 1L)
  expect_equal(length(xml2::xml_find_all(matched[[1]], './/div[contains(@class,"pk-profile-warning")]')), 2L)
  expect_match(xml2::xml_text(matched[[1]]), "ID=001 / PERIOD=01", fixed = TRUE)
  expect_match(xml2::xml_text(matched[[1]]), "ID=SUBJ<01>", fixed = TRUE)
  expect_false(grepl("ID=001 / PERIOD=01 / ANALYTE=parent / STUDY=study-A：", html, fixed = TRUE))
})
