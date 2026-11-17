source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
# Generated once by the unchanged 0.12 release, not by the serializer under test.
fixture <- readRDS("apps/pk-desktop/release/tests/fixtures/nca013_serialization_v012.rds")
bytes <- function(x) lapply(x, function(value) charToRaw(enc2utf8(value)))

test_that("column encoding preserves 0.12 escaping nulls and delimiters byte for byte", {
  actual <- core$desktop_pk_blq_rows(fixture$canonical, named = TRUE)
  expect_identical(actual, fixture$row_json)
  expect_identical(bytes(actual), bytes(fixture$row_json))
  expect_identical(core$desktop_pk_blq_rows(fixture$canonical), fixture$array_json)
  n <- length(fixture$array_json)
  expect_identical(core$desktop_pk_blq_baseline_json(fixture$canonical, seq_len(n), rep(n, n)), fixture$base_json)
  expect_identical(core$desktop_pk_blq_json_atoms(character()), character())
  expect_identical(core$desktop_pk_blq_json_atoms(NA_character_), "null")
  expect_identical(core$desktop_pk_blq_rows(list(value = character())), character())
  expect_identical(core$desktop_pk_blq_rows(list(), n = 3), rep("[]", 3))
})

test_that("singleton columns preserve scalar string null and named row shapes", {
  for (i in seq_along(fixture$array_json)) {
    canonical <- lapply(fixture$canonical, `[`, i)
    expect_identical(core$desktop_pk_blq_rows(canonical), fixture$array_json[i])
    expect_identical(core$desktop_pk_blq_rows(canonical, named = TRUE), fixture$row_json[i])
  }
})

for (name in names(fixture$cases)) local({
  kind <- name; case <- fixture$cases[[kind]]
  test_that(paste("fresh prepared output and imported v1/v2 match 0.12:", kind), {
    actual <- core$desktop_prepare_blq(case$raw, case$step)
    expect_identical(actual, case$prepared)
    expect_identical(bytes(actual$data$NCA_BLQ_BASELINE), bytes(case$prepared$data$NCA_BLQ_BASELINE))
    expect_identical(core$desktop_pk_blq_signatures(case$prepared$data, case$context), case$signatures)
    expect_identical(core$desktop_pk_blq_signatures(case$legacy, case$v1context), case$v1signatures)
  })
})

test_that("numeric normalization preserves text identity 001 separately from 1", {
  case <- fixture$cases$multiple_keys
  canonical <- core$desktop_pk_blq_canonical(case$prepared$data, unlist(case$context$columns), unlist(case$context$profile_columns))
  expect_identical(canonical, case$signatures$canonical)
  expect_identical(unique(canonical$ID), c("001", "1", '日本"\\},{'))
  expect_identical(canonical$CONC[c(2, 3, 4)], c("0", "12.345678901234567", "4"))
  expect_identical(core$desktop_pk_blq_signatures(case$prepared$data, case$context)$group, rep(1:3, each = 4))
})

for (name in names(fixture$scenarios)) local({
  kind <- name; scenario <- fixture$scenarios[[kind]]
  test_that(paste("import validation keeps rejection and versioned diagnostic:", kind), {
    actual <- tryCatch({core$desktop_pk_blq_validate(scenario$data, scenario$spec); ""}, error = conditionMessage)
    expected <- if (kind == "moved_identity") sub("解析単位の移動先: ID=moved",
      "解析単位の移動先: ID=001, PERIOD=01 → ID=moved, PERIOD=01（1行）", scenario$message, fixed = TRUE) else scenario$message
    expect_identical(actual, expected)
  })
})

test_that("imported baselines retain every structural guard", {
  case <- fixture$cases$multiple_keys
  original <- jsonlite::fromJSON(case$prepared$data$NCA_BLQ_BASELINE[1], simplifyVector = FALSE)
  invalid <- list("{}", "null", "[1]", "invalid JSON")
  for (field in c("i", "n")) for (value in list(NULL, 0, -1, 1.5, .Machine$integer.max + 1, "1", TRUE, list(1, 2))) {
    changed <- original; changed[field] <- list(value)
    invalid[[length(invalid) + 1L]] <- core$desktop_pk_blq_json(changed)
  }
  for (value in list(NULL, list(), list("short"), stats::setNames(original$v, seq_along(original$v)))) {
    changed <- original; changed["v"] <- list(value)
    invalid[[length(invalid) + 1L]] <- core$desktop_pk_blq_json(changed)
  }
  for (value in list(1, TRUE, list("nested"), c("two", "values"))) {
    changed <- original; changed$v[1] <- list(value)
    invalid[[length(invalid) + 1L]] <- core$desktop_pk_blq_json(changed)
  }
  changed <- original; changed$i <- changed$n + 1
  invalid[[length(invalid) + 1L]] <- core$desktop_pk_blq_json(changed)
  changed <- original; changed$extra <- 1
  invalid[[length(invalid) + 1L]] <- core$desktop_pk_blq_json(changed)
  for (text in invalid) {
    data <- case$prepared$data; data$NCA_BLQ_BASELINE[1] <- text
    expect_error(core$desktop_pk_blq_signatures(data, case$context), "NCA_BLQ_BASELINE記録が欠落・不正", fixed = TRUE)
  }
})

for (kind in c("constant", "column")) local({
  label <- kind
  test_that(paste("historical v1 CSV signature remains unchanged:", label), {
    data <- pkapp_read_table(paste0("apps/pk-desktop/release/tests/fixtures/prepared_v1_", label, ".csv"))
    context <- core$desktop_pk_blq_context(data)
    expect_identical(core$desktop_pk_blq_signatures(data, context)$signature, data$NCA_BLQ_PROFILE_SIGNATURE)
    spec <- list(id = "ID", groups = character(), time = "TIME", time_unit = "h", time_origin = 0, scope = "all")
    expect_silent(core$desktop_pk_blq_validate(data, spec))
    expect_silent(core$desktop_pk_blq_validate(data[rev(seq_len(nrow(data))), ], spec))
    data$NCA_CONC[1] <- 99
    expect_error(core$desktop_pk_blq_validate(data, spec), "旧形式v1", fixed = TRUE)
  })
})
