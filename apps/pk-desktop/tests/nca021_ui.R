source("apps/pk-desktop/tests/nca020_ui.R")

test_that("mapping changes renew key suggestions and unchanged mappings preserve edits", {
  x <- make_ui020(); on.exit(unlink(x$engine$project, recursive = TRUE))
  shiny::testServer(core$desktop_assembly_server, args = list(engine = x$engine), {
    session$setInputs(conc_ID = "ID", pc_ID = "ID", ex_ID = "ID", profile_keys = "ID")
    initial <- output$keys$html
    session$setInputs(conc_PERIOD = "PERIOD", pc_PERIOD = "PERIOD", ex_PERIOD = "PERIOD")
    html <- output$keys$html
    section <- regmatches(html, regexpr('(?s)<select[^>]+id="[^"]*profile_keys".*?</select>', html, perl = TRUE))
    expect_match(section, '<option value="PERIOD" selected>')
    session$setInputs(profile_keys = "ID")
    session$setInputs(conc_PERIOD = "PERIOD")
    html <- output$keys$html
    section <- regmatches(html, regexpr('(?s)<select[^>]+id="[^"]*profile_keys".*?</select>', html, perl = TRUE))
    expect_false(grepl('<option value="PERIOD" selected>', section, fixed = TRUE))
  })
})

test_that("assembly presents a responsive sequence and complete history attestation", {
  html <- as.character(core$desktop_assembly_ui("assembly"))
  expect_match(html, "PK DESKTOP / ASSEMBLY", fixed = TRUE)
  expect_match(html, "pk-source-grid", fixed = TRUE)
  expect_match(html, "repeat(3,minmax(0,1fr))", fixed = TRUE)
  expect_match(html, "max-width:800px", fixed = TRUE)
  expect_match(html, "それ以前の投与を含む完全な投与履歴", fixed = TRUE)
  expect_lt(regexpr("dose_history_confirmed", html)[1], regexpr("reference_mode", html)[1])
  for (id in c("conc_source", "pc_source", "ex_source", "keys", "preview", "save_action"))
    expect_match(html, paste0("assembly-", id), fixed = TRUE)
})

test_that("mixed units are readable in annotation preview", {
  x <- make_ui020(); on.exit(unlink(x$engine$project, recursive = TRUE))
  shiny::testServer(core$desktop_annotation_server, args = list(engine = x$engine), {
    map_ui020(session, x); session$setInputs(preview = 1)
    v <- current()
    v$value$trace$issues <- data.frame(field = "VS_BMI", reason = "mixed_units")
    current(NULL); session$flushReact(); current(v); session$flushReact()
    expect_match(output$preview_result$html, "単位が混在", fixed = TRUE)
    expect_match(output$preview_result$html, "異なる単位の値をそのまままとめて集計しない", fixed = TRUE)
  })
})
