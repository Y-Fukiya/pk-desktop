source("apps/pk-desktop/pkapp.R")
library(testthat)
core <- environment(pkapp_open)
raw <- data.frame(ID = rep(c("001", "002"), each = 4), PERIOD = 1,
  TIME = rep(c(-.5, 0, 1/3, 2), 2), CONC = rep(c("BLQ", "0", "12.345678901234567", "4"), 2),
  BLQ = rep(c(1, 0, 0, 0), 2), MDV = 0, LLOQ = .1, UNIT = "ng/mL", SSNCAFL = TRUE)
step <- list(op = "blq_rules", concentration = "CONC", blq_column = "BLQ", time = "TIME", mdv_column = "MDV",
  mode = "positional", profile_columns = c("ID", "PERIOD"), zero_time = "post", pre_rule = "exclude",
  first_rule = "zero", middle_rule = "half_lloq", last_rule = "exclude", all_blq_rule = "zero", lloq_source = "column", lloq_column = "LLOQ")
spec <- list(id = "ID", groups = "PERIOD", time = "TIME", time_unit = "h", time_origin = 0, scope = "all")
prepared <- core$desktop_prepare_blq(raw, step)$data
validate <- function(x, s = spec) core$desktop_pk_blq_validate(x, s)
message_for <- function(x) tryCatch({ validate(x); "" }, error = conditionMessage)
context_change <- function(x, fun) {
  context <- core$desktop_pk_blq_context(x)
  x$NCA_BLQ_CONTEXT <- core$desktop_pk_blq_json(fun(context))
  x
}
legacy <- function(x) {
  context <- core$desktop_pk_blq_context(x); context$version <- 1L; context$columns <- NULL
  x$NCA_BLQ_BASELINE <- NULL
  x$NCA_BLQ_CONTEXT <- core$desktop_pk_blq_json(context)
  x$NCA_BLQ_PROFILE_SIGNATURE <- core$desktop_pk_blq_signatures(x, context)$signature
  x
}

test_that("v2 self-contained CSV preserves exact keys values and whole profiles", {
  expect_equal(core$desktop_pk_blq_context(prepared)$version, 2)
  path <- tempfile(fileext = ".csv"); core$pk_write_csv(prepared, path)
  imported <- pkapp_read_table(path)
  expect_silent(validate(imported))
  expect_identical(imported$ID, raw$ID)
  expect_silent(validate(imported[c(8:5, 4:1), ]))
  expect_silent(validate(imported[imported$ID == "001", ]))
  imported$TIME <- sprintf("%.17g", as.numeric(imported$TIME))
  imported$UNRELATED <- seq_len(nrow(imported))
  expect_silent(validate(imported))
  context <- core$desktop_pk_blq_context(imported)
  imported$NCA_BLQ_CONTEXT[1] <- as.character(jsonlite::toJSON(rev(context), auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null"))
  expect_silent(validate(imported))
})

test_that("intact metadata names original identity and each changed dependency", {
  for (column in c("CONC", "NCA_CONC", "TIME", "BLQ", "MDV", "LLOQ", "NCA_ORIGINAL_CONC", "NCA_ORIGINAL_MDV")) {
    changed <- prepared; changed[[column]][2] <- 99
    message <- message_for(changed)
    expect_match(message, "ID=001", fixed = TRUE)
    expect_match(message, paste0("変更列: ", column), fixed = TRUE)
    expect_match(message, "現在の行: 2", fixed = TRUE)
  }
  for (indices in list(1L, 1:4)) {
    changed <- prepared; changed$ID[indices] <- "002"
    message <- message_for(changed)
    expect_match(message, "[ID=001, PERIOD=1]", fixed = TRUE)
    expect_match(message, "変更列: ID", fixed = TRUE)
    expect_match(message, paste0("ID=001, PERIOD=1 → ID=002, PERIOD=1（", length(indices), "行）"), fixed = TRUE)
  }
  changed <- prepared; changed$ID[1] <- "1"
  expect_match(message_for(changed), "ID=001, PERIOD=1 → ID=1, PERIOD=1（1行）", fixed = TRUE)
  changed <- prepared; changed$PERIOD[1] <- 2
  expect_match(message_for(changed), "変更列: PERIOD", fixed = TRUE)
  changed <- prepared; changed$CONC[c(3,4)] <- changed$CONC[c(4,3)]
  expect_match(message_for(changed), "変更列: CONC", fixed = TRUE)
})

test_that("missing duplicated and partial rows retain recorded original profiles", {
  expect_match(message_for(prepared[-2, ]), "欠落行番号=2", fixed = TRUE)
  expect_match(message_for(prepared[c(8:3, 1), ]), "ID=001", fixed = TRUE)
  expect_match(message_for(rbind(prepared, prepared[2, ])), "重複行番号=2", fixed = TRUE)
  expect_match(message_for(rbind(prepared, prepared)), "現在行数=8", fixed = TRUE)
  changed <- prepared[-2, ]; changed$CONC[1] <- "changed"
  expect_match(message_for(changed), "未検証", fixed = TRUE)
  expect_false(grepl("変更列", message_for(changed), fixed = TRUE))
  changed <- prepared; changed$SSNCAFL[2] <- FALSE
  expect_error(validate(changed, modifyList(spec, list(scope = "ss"))), "ID=001.*選択 3 / 全 4")
  sole <- rbind(raw, transform(raw[1, ], ID = "003"))
  sole <- core$desktop_prepare_blq(sole, step)$data
  expect_silent(validate(sole[sole$ID != "003", ]))
})

test_that("unverified metadata never pretends to diagnose original cell changes", {
  constant <- core$desktop_prepare_blq(raw, modifyList(step, list(lloq_source = "constant", lloq_value = .1)))$data
  changed <- context_change(constant, function(context) { context$step$lloq_value <- .2; context })
  expect_match(message_for(changed), "記録が破損", fixed = TRUE)
  expect_false(grepl("変更列", message_for(changed), fixed = TRUE))
  changed <- prepared
  baseline <- jsonlite::fromJSON(changed$NCA_BLQ_BASELINE[1], simplifyVector = FALSE)
  baseline$v[[match("CONC", unlist(core$desktop_pk_blq_context(changed)$columns))]] <- "100"
  changed$NCA_BLQ_BASELINE[1] <- core$desktop_pk_blq_json(baseline)
  expect_match(message_for(changed), "記録が破損", fixed = TRUE)
  expect_match(message_for(prepared[setdiff(names(prepared), "NCA_BLQ_BASELINE")]), "NCA_BLQ_BASELINE", fixed = TRUE)
  changed$NCA_BLQ_BASELINE[1] <- "{}"
  expect_match(message_for(changed), "不正", fixed = TRUE)
  changed <- prepared; changed$NCA_BLQ_BASELINE[1] <- changed$NCA_BLQ_BASELINE[2]
  expect_match(message_for(changed), "重複行番号=2", fixed = TRUE)
  changed <- prepared; baseline$v <- baseline$v[-1]
  changed$NCA_BLQ_BASELINE[1] <- core$desktop_pk_blq_json(baseline)
  expect_match(message_for(changed), "不正", fixed = TRUE)
  changed <- context_change(prepared, function(context) { context$version <- 9; context })
  expect_match(message_for(changed), "不正", fixed = TRUE)
  changed <- prepared; changed$NCA_BLQ_CONTEXT[2] <- "{}"
  expect_match(message_for(changed), "不正", fixed = TRUE)
  expect_match(message_for(prepared[setdiff(names(prepared), "NCA_CONC")]), "NCA_CONC", fixed = TRUE)
})

test_that("v1 validation remains supported with honest limited diagnostics", {
  old <- legacy(prepared)
  expect_silent(validate(old))
  expect_silent(validate(old[4:1, ]))
  old$NCA_CONC[2] <- 99
  expect_match(message_for(old), "旧形式v1", fixed = TRUE)
  expect_match(message_for(old), "特定できません", fixed = TRUE)
  old_constant <- legacy(core$desktop_prepare_blq(raw, modifyList(step, list(lloq_source = "constant", lloq_value = .1)))$data)
  changed <- context_change(old_constant, function(context) { context$step$lloq_value <- .001; context })
  expect_silent(validate(changed))
  expect_true(all(core$desktop_pk_blq_lloq(changed)$reason == "unverified_lloq"))
  expect_true(all(is.na(core$desktop_pk_blq_lloq(changed)$value)))
  expect_equal(core$desktop_pk_blq_lloq(legacy(prepared))$value, rep(.1, 8))
  context <- core$desktop_pk_blq_context(old_constant)
  for (column in core$desktop_pk_blq_columns(context)) {
    changed <- context_change(old_constant, function(context) {
      context$step$lloq_source <- "column"; context$step$lloq_column <- column; context
    })
    expect_silent(validate(changed))
    evidence <- core$desktop_pk_blq_lloq(changed)
    expect_true(all(evidence$reason == "unverified_lloq"))
    expect_true(all(is.na(evidence$value)))
    expect_identical(evidence$source, "")
  }
})

test_that("optional LLOQ is explicit strict and never changes substitution semantics", {
  limits <- data.frame(LLOQ = c("0.1", "", "bad", "0", "-1", "Inf", NA, "1e309"))
  expect_identical(core$desktop_pk_lloq(limits)$reason, rep("missing_lloq", 8))
  evidence <- core$desktop_pk_lloq(limits, "column", "LLOQ")
  expect_equal(evidence$value, c(.1, rep(NA_real_, 7)))
  expect_identical(evidence$reason, c("", "missing_lloq", "invalid_lloq", "invalid_lloq", "invalid_lloq", "invalid_lloq", "missing_lloq", "invalid_lloq"))
  expect_identical(evidence$source, "column:LLOQ")
  expect_equal(core$desktop_pk_lloq(limits, "constant", value = .2)$value, rep(.2, 8))
  expect_error(core$desktop_pk_lloq(limits, "column", "missing"), "LLOQ")
  expect_true(all(is.na(prepared$NCA_LLOQ_USED)))
  expect_equal(core$desktop_pk_blq_lloq(prepared)$value, rep(.1, 8))
  invalid <- raw; invalid$LLOQ[1] <- "bad"
  invalid <- core$desktop_prepare_blq(invalid, step)$data
  expect_identical(core$desktop_pk_blq_lloq(invalid)$reason[1], "invalid_lloq")
  expect_error(core$desktop_prepare_blq(transform(raw, LLOQ = "bad"), modifyList(step, list(pre_rule = "half_lloq"))), "Non-numeric")
  half <- core$desktop_prepare_blq(raw, modifyList(step, list(pre_rule = "half_lloq")))$data
  expect_equal(half$NCA_CONC[c(1,5)], rep(.05, 2))
  expect_equal(half$NCA_LLOQ_USED[c(1,5)], rep(.1, 2))
})

test_that("time choices are bare names and exclude semantic flags without mappings", {
  choices <- core$desktop_pk_time_choices(transform(raw, BLQFL = BLQ, CUSTOM_H = TIME, NCA_CUSTOM_H = TIME))
  expect_false(any(c("ID", "PERIOD", "BLQ", "BLQFL", "MDV", "LLOQ") %in% choices))
  expect_true(all(c("TIME", "CUSTOM_H", "NCA_CUSTOM_H") %in% choices))
  expect_identical(names(choices), unname(choices))
  expect_match(core$desktop_pk_time_hint(raw, "TIME"), "範囲")
})

test_that("native rule radios start empty and restore all saved choices", {
  html <- as.character(core$desktop_pk_prepare_controls(shiny::NS("prep")))
  radios <- regmatches(html, gregexpr('<input[^>]+type="radio"[^>]*>', html))[[1]]
  expect_length(radios, 24)
  expect_false(any(grepl("checked", radios, fixed = TRUE)))
  expect_false(any(grepl('value=""', radios, fixed = TRUE)))
  expect_match(html, 'class="pk-time-field"', fixed = TRUE)
  expect_match(html, 'class="pk-lloq-controls"', fixed = TRUE)
  expect_false(grepl("indexOf('half_lloq')", html, fixed = TRUE))
  messages <- list(); mock <- shiny::MockShinySession$new()
  mock$sendInputMessage <- function(id, message) messages[[id]] <<- message
  core$desktop_pk_prepare_restore(mock, step)
  expect_identical(messages$pre_rule$value, "exclude")
  expect_identical(messages$middle_rule$value, "half_lloq")
  expect_identical(messages$all_blq_rule$value, "zero")
  expect_identical(messages$post_rule$value, character(0))
  form <- core$desktop_pk_prepare_form(list(op = "blq_rules", blq_mode = "positional", blq_concentration = "CONC", prep_blq_column = "BLQ", blq_time = "TIME", profile_columns = c("ID", "PERIOD")))
  expect_error(core$desktop_prepare_blq(raw, form), "BLQ規則")
})
