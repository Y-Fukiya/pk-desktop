source("apps/pk-desktop/pkapp.R")
source("apps/pk-desktop/tests/helper_nca_review.R")
library(testthat)
core <- environment(pkapp_open)
source("apps/pk-desktop/tests/helper_nca_jobs.R")

nca013_ui_engine <- function() {
  path <- tempfile("nca013-ui-"); dir.create(path)
  core$desktop_nca_engine(path)
}
nca013_ui_configure <- function(session, data) {
  path <- tempfile(fileext = ".csv"); core$pk_write_csv(data, path)
  session$setInputs(source = "FILE", file = list(datapath = path, name = "qc.csv"), id = "ID", time = "TIME",
    concentration = "CONC", groups = character(), time_unit = "h", concentration_unit = "ng/mL", unit_column = "",
    blq_column = "BLQ", mdv_column = "", scope = "all", time_origin = 0, start = 0, last_observation = TRUE,
    auc_method = "linear", blq = "exclude", c0_method = "none", all_blq_policy = "missing", name = "NCA013",
    predose_qc = TRUE, predose_qc_threshold_pct = 5, predose_qc_window_h = 1,
    predose_qc_lloq_source = "column", predose_qc_lloq_column = "LLOQ")
}
nca013_ui_text <- function(html) xml2::xml_text(xml2::read_html(html))
nca013_ui_count <- function(text, needle) sum(gregexpr(needle, text, fixed = TRUE)[[1]] > 0L)

test_that("profile details show one QC summary and only parameter-specific table columns", {
  data <- data.frame(ID = "001", TIME = c(-.5, -.25, 0, 1, 2), CONC = c(".1", "<.2", "0", "8", "4"),
    BLQ = c(0, 1, 0, 0, 0), LLOQ = .2)
  shiny::testServer(core$desktop_nca_server, args = list(engine = nca013_ui_engine()), {
    nca013_ui_configure(session, data); nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    details <- xml2::read_html(output$profile_details$html)
    table <- xml2::xml_find_first(details, ".//table")
    expect_identical(xml2::xml_text(xml2::xml_find_all(table, ".//th")), c("PARAMCD", "PARAM", "AVAL", "AVALU", "PARAM_STATUS", "NCA_EXCLUDE"))
    expect_equal(length(xml2::xml_find_all(details, './/div[@class="pk-profile-qc"]')), 1L)
    expect_equal(length(xml2::xml_find_all(details, './/dl[@class="pk-profile-qc-metadata"]/div')), 12L)
    text <- xml2::xml_text(details)
    for (label in c("閾値（%）", "最大遡及時間（h）", "確認対象の元行", "投与基準からの時間（h）", "LLOQの参照元", "確認理由"))
      expect_equal(nca013_ui_count(text, label), 1L, info = label)
    expect_match(text, "指定した列：LLOQ", fixed = TRUE)
    expect_match(text, "最新の元行", fixed = TRUE)
    expect_match(text, "それより前に定量可能な値があっても、最新行を確認します", fixed = TRUE)
    expect_match(text, "方針による値はありません。", fixed = TRUE)
    expect_false(grepl("方針による値：。", text, fixed = TRUE))
    expect_match(nca013_ui_text(output$profile_quality$html), "入力 5 行", fixed = TRUE)
    expect_false(grepl("N_INPUT|PREDOSE_QC_|NCA_WARNING", as.character(table)))
    expect_equal(result()$value$trace$groups[[1]]$predose_qc$source_row, 2L)
    expect_equal(result()$value$trace$groups[[1]]$predose_qc$status, "pass_blq")
  })
})

test_that("known engine warnings and reasons are Japanese while exports remain original", {
  fixtures <- list(
    data.frame(ID = "001", TIME = c(0, 1, 1), CONC = c(0, 8, 4), BLQ = 0, LLOQ = .2),
    data.frame(ID = "001", TIME = c(0, 1, 2, NA), CONC = c(0, 8, 4, NA), BLQ = 0, LLOQ = .2),
    data.frame(ID = "001", TIME = c(0, 1, 2), CONC = "<.2", BLQ = 1, LLOQ = .2))
  for (data in fixtures) shiny::testServer(core$desktop_nca_server, args = list(engine = nca013_ui_engine()), {
    nca013_ui_configure(session, data); nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    original <- result()$value
    html <- paste(output$results$html, output$plot_note$html, output$profile_details$html, output$audit_table$html)
    japanese_text <- xml2::xml_text(xml2::read_html(html))
    expect_false(grepl("Duplicate observation|source rows excluded|All original|Predose percent|No eligible|no_original_quantifiable|outside_analysis_interval", japanese_text))
    expect_false(grepl("pk-original-diagnostic", html, fixed = TRUE))
    expect_identical(result()$value, original)
    expect_true(any(grepl("解析単位|CMAX|AUCLST", original$warnings)))
    if (any(grepl("Duplicate observation", original$data$NCA_EXCLUDE)))
      expect_match(html, "観測時刻が重複しています", fixed = TRUE)
  })
})

test_that("unknown engine diagnostics are explicitly expandable and escaped", {
  raw <- 'Engine future failure <script>alert("x")</script>; detail 未対応'
  html <- as.character(core$desktop_nca_diagnostics(raw))
  expect_match(html, "<details", fixed = TRUE)
  expect_match(html, "未翻訳の診断があります（原文を確認）", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_identical(xml2::xml_text(xml2::xml_find_first(xml2::read_html(html), ".//pre")), raw)
  table <- as.character(core$desktop_display_table(data.frame(NCA_EXCLUDE = raw, REASON = "unrecognized_engine_code")))
  expect_equal(nca013_ui_count(table, "pk-original-diagnostic"), 2L)
})

test_that("every known scientific diagnostic is mapped without discarding unknown additions", {
  known <- c("No quantifiable endpoint after the interval start",
    "Missing concentration at the requested start; select an explicit C0 policy when starting at the dose reference",
    "Numeric overflow: the calculated value exceeds the supported range", "PKNCA returned a missing AUC",
    "Invalid numeric data in source rows 1, 3", "Ambiguous latest predose measurement",
    "No eligible predose measurement within the specified lookback", "All observed non-BLQ concentrations are zero; Tmax is undefined",
    "AUCLST must start at the dose reference; the selected interval starts later",
    "All-BLQ zero policy assigned CMAX/AUC; Tmax and quantifiable Tlast remain undefined",
    "Predose concentration exceeds the selected percent-of-Cmax QC threshold; no automatic exclusion",
    "Predose percent-of-Cmax QC is not evaluable (missing_lloq)", "Copied to dose reference for AUC only",
    "invalid_time_non_numeric", "invalid_concentration_negative", "invalid_concentration_underflow", "invalid_time_non_finite")
  for (diagnostic in known) {
    expect_false(is.na(core$desktop_nca_diagnostic_label(diagnostic)), info = diagnostic)
    expect_false(grepl("pk-original-diagnostic", as.character(core$desktop_nca_diagnostics(diagnostic)), fixed = TRUE), info = diagnostic)
  }
  expect_true(is.na(core$desktop_nca_diagnostic_label("Profile 1: AUCLST: new_engine_code")))
})

test_that("QC origins distinguish recipe and explicit missing-only supplements", {
  expect_identical(core$desktop_nca_lloq_origin("column:LLOQ", TRUE), "保存済みの前処理の条件の列：LLOQ")
  expect_identical(core$desktop_nca_lloq_origin("constant", TRUE), "保存済みの前処理の条件の定数")
  expect_identical(core$desktop_nca_lloq_origin("qc_supplement:column:LIMIT"), "QC専用の補完列：LIMIT")
  expect_identical(core$desktop_nca_lloq_origin("qc_supplement:constant"), "QC専用の補完定数")
  expect_identical(core$desktop_nca_lloq_origin(""), "参照元なし")
  expect_match(as.character(core$desktop_nca_lloq_origin("future_origin")), "pk-original-diagnostic", fixed = TRUE)
})

test_that("all-BLQ assigned parameters are listed explicitly", {
  data <- data.frame(ID = "001", TIME = c(0, 1, 2), CONC = "<.2", BLQ = 1, LLOQ = .2)
  shiny::testServer(core$desktop_nca_server, args = list(engine = nca013_ui_engine()), {
    nca013_ui_configure(session, data); session$setInputs(all_blq_policy = "zero")
    nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    expect_match(nca013_ui_text(output$profile_details$html), "方針による値：CMAX, AUCLST", fixed = TRUE)
    expect_false(grepl("方針による値はありません", output$profile_details$html, fixed = TRUE))
  })
})

test_that("BLQ radios retain native inputs and their accessible group labels", {
  html <- as.character(core$desktop_pk_prepare_controls(shiny::NS("prep")))
  doc <- xml2::read_html(html)
  rows <- xml2::xml_find_all(doc, './/div[@class="pk-blq-rule-row"]')
  expect_gt(length(rows), 0L)
  for (row in rows) {
    expect_gt(length(xml2::xml_find_all(row, './/input[@type="radio"]')), 0L)
    expect_match(xml2::xml_text(xml2::xml_find_first(row, './/span[@class="pk-visually-hidden"]')), "の処置", fixed = TRUE)
    expect_equal(length(xml2::xml_find_all(row, './/input[@type="radio"]')), length(xml2::xml_find_all(row, './/label[contains(@class,"radio-inline")]/input[@type="radio"]')))
    expect_equal(length(xml2::xml_find_all(row, './/input[@tabindex="-1" or @aria-hidden="true"]')), 0L)
  }
  styles <- htmltools::renderTags(core$desktop_workbench_styles())$head
  expect_match(styles, "label.radio-inline > input[type=radio] { position:static !important;", fixed = TRUE)
  expect_match(styles, "margin:0 !important; width:14px; height:14px;", fixed = TRUE)
  expect_match(styles, ".radio-inline:has(input:focus-visible)", fixed = TRUE)
})

test_that("fixed action-bar markup includes measured clearance and delegated focus handling", {
  html <- as.character(core$desktop_nca_ui("nca"))
  expect_match(html, "pk-nca-runbar", fixed = TRUE)
  expect_match(html, "nca-run_action", fixed = TRUE)
  expect_match(html, 'role="region" aria-label="NCAの実行と保存 / Run and save NCA"', fixed = TRUE)
  styles <- htmltools::renderTags(core$desktop_workbench_styles())$head
  expect_match(styles, "position:fixed", fixed = TRUE)
  expect_match(styles, "new ResizeObserver(schedule)", fixed = TRUE)
  expect_match(styles, "scroll-padding-bottom:var(--pk-nca-bottom-clearance", fixed = TRUE)
  expect_match(styles, "body.pk-nca-runbar-visible { padding-bottom:var(--pk-nca-bottom-clearance", fixed = TRUE)
  expect_match(styles, 'document.addEventListener("focusin", schedule)', fixed = TRUE)
  expect_match(styles, "resizeObserver.unobserve(bar)", fixed = TRUE)
  expect_match(styles, "resizeObserver.disconnect()", fixed = TRUE)
  expect_match(styles, "mutationObserver.disconnect()", fixed = TRUE)
})

test_that("action-bar JavaScript parses and reveals focus with wrapped-height and observer fallbacks", {
  node <- Sys.which("node")
  expect_true(nzchar(node))
  head <- htmltools::renderTags(core$desktop_workbench_styles())$head
  script <- xml2::xml_text(xml2::xml_find_first(xml2::read_html(head), ".//script"))
  script_path <- tempfile(fileext = ".js"); writeLines(script, script_path)
  harness_path <- tempfile(fileext = ".js")
  writeLines(r"---(
const assert = require("node:assert/strict");
const vm = require("node:vm");
const fs = require("node:fs");
const script = fs.readFileSync(process.argv[2], "utf8");
new vm.Script(script);
let checks = 0;
function equal(actual, expected) { assert.deepEqual(actual, expected); checks++; }
function install(observers) {
  const handlers = {}, events = {}, values = {}, classes = {}, queue = [], scrolls = [], resizers = [], mutators = [];
  let shown = true, focusCalls = 0, insideBar = false;
  let barRect = {top:700, bottom:788, left:20, right:980, height:88};
  let targetRect = {top:720, bottom:758, left:100, right:200, height:38};
  const target = {getBoundingClientRect:() => targetRect, focus:() => { focusCalls++; }};
  const bar = {isConnected:true, getClientRects:() => shown ? [barRect] : [], getBoundingClientRect:() => barRect, contains:() => insideBar};
  const body = {classList:{toggle:(name, on) => {classes[name] = on;}}};
  const document = {hidden:false, body, activeElement:body,
    documentElement:{style:{getPropertyValue:key => values[key], setProperty:(key, value) => {values[key] = value;}}},
    querySelectorAll:() => [bar], addEventListener:(event, cb) => {handlers[event] = cb;}};
  const window = {innerHeight:800, requestAnimationFrame:cb => {queue.push(cb); return queue.length;},
    scrollBy:args => {scrolls.push(args.top); targetRect = {...targetRect, top:targetRect.top - args.top, bottom:targetRect.bottom - args.top};},
    addEventListener:(event, cb) => {events[event] = cb;}, visualViewport:{addEventListener:() => {}}};
  const $ = input => {if (typeof input === "function") input(); return {on:() => {}};};
  const context = {window, document, $, Set, Shiny:{addCustomMessageHandler:() => {}}};
  if (observers) {
    context.ResizeObserver = class {
      constructor(callback) {this.callback = callback; this.observed = new Set(); this.disconnects = 0; resizers.push(this);}
      observe(bar) {this.observed.add(bar);} unobserve(bar) {this.observed.delete(bar);}
      disconnect() {this.observed.clear(); this.disconnects++;}
    };
    context.MutationObserver = class {
      constructor(callback) {this.callback = callback; this.disconnects = 0; mutators.push(this);}
      observe() {} disconnect() {this.disconnects++;}
    };
  }
  vm.runInNewContext(script, context);
  function flush() {let n = 0; while (queue.length) {assert.ok(++n < 50); queue.shift()();}}
  flush();
  return {document, handlers, events, values, classes, scrolls, resizers, mutators, target, bar, flush,
    focusCalls:() => focusCalls, setShown:value => {shown = value;}, setInside:value => {insideBar = value;},
    setTarget:value => {targetRect = value;}, setBar:value => {barRect = value;}};
}
const app = install(true);
equal(app.values["--pk-nca-bottom-clearance"], "116px");
equal(app.classes["pk-nca-runbar-visible"], true);
equal(app.resizers[0].observed.has(app.bar), true);
app.document.activeElement = app.target; app.handlers.focusin(); app.flush();
equal(app.scrolls, [70]);
equal(app.focusCalls(), 0);
equal(app.document.activeElement, app.target);
app.handlers.focusin(); app.flush(); equal(app.scrolls.length, 1);
app.setTarget({top:720, bottom:758, left:990, right:1000}); app.handlers.focusin(); app.flush(); equal(app.scrolls.length, 1);
app.setInside(true); app.setTarget({top:720, bottom:758, left:100, right:200}); app.handlers.focusin(); app.flush(); equal(app.scrolls.length, 1);
app.setInside(false); app.setBar({top:580, bottom:788, left:20, right:980, height:208});
app.setTarget({top:600, bottom:638, left:100, right:200}); app.resizers[0].callback(); app.flush();
equal(app.values["--pk-nca-bottom-clearance"], "236px"); equal(app.scrolls.at(-1), 70);
app.setShown(false); app.mutators[0].callback(); app.flush();
equal(app.values["--pk-nca-bottom-clearance"], "0px"); equal(app.resizers[0].observed.size, 0);
equal(app.classes["pk-nca-runbar-visible"], false);
app.setShown(true); app.mutators[0].callback(); app.flush(); equal(app.resizers[0].observed.size, 1);
app.document.hidden = true; app.handlers.visibilitychange(); app.flush();
equal(app.resizers[0].observed.size, 0); equal(app.resizers[0].disconnects, 1); equal(app.mutators[0].disconnects, 1);
app.document.hidden = false; app.handlers.visibilitychange(); app.flush(); equal(app.resizers.length, 1); equal(app.resizers[0].observed.size, 1);
app.bar.isConnected = false; app.mutators[0].callback(); app.flush(); equal(app.resizers[0].observed.size, 0);
const fallback = install(false);
fallback.document.activeElement = fallback.target; fallback.handlers.focusin(); fallback.flush();
equal(fallback.scrolls, [70]); equal(fallback.focusCalls(), 0);
fallback.setBar({top:580, bottom:788, left:20, right:980, height:208}); fallback.events.resize(); fallback.flush();
equal(fallback.values["--pk-nca-bottom-clearance"], "236px");
console.log("Action-bar JavaScript passed " + checks + " geometry and lifecycle checks.");
)---", harness_path)
  output <- system2(node, c(harness_path, script_path), stdout = TRUE, stderr = TRUE)
  expect_true(is.null(attr(output, "status")) || identical(attr(output, "status"), 0L))
  expect_match(paste(output, collapse = "\n"), "Action-bar JavaScript passed 24 geometry and lifecycle checks.", fixed = TRUE)
})

test_that("rendered results preserve unknown diagnostics and show supplemental QC provenance", {
  data <- data.frame(ID = "001", TIME = c(-.25, 0, 1, 2), CONC = c("<.2", "0", "8", "4"), BLQ = c(1, 0, 0, 0), LLOQ = .2)
  shiny::testServer(core$desktop_nca_server, args = list(engine = nca013_ui_engine()), {
    nca013_ui_configure(session, data); nca_confirm_displayed(session, input, output)
    session$setInputs(run = 1)
    nca_wait_for_job(engine, session)
    modified <- current()
    modified$value$warnings <- c(modified$value$warnings, "Future engine diagnosis <extra>")
    modified$value$trace$groups[[1]]$reason <- c(modified$value$trace$groups[[1]]$reason, "Future AUC explanation <extra>")
    modified$value$trace$groups[[1]]$predose_qc$lloq_source <- "qc_supplement:column:LIMIT"
    current(modified); session$flushReact()
    expect_match(output$results$html, "Future engine diagnosis &lt;extra&gt;", fixed = TRUE)
    expect_match(output$selected_warning$html, "Future AUC explanation &lt;extra&gt;", fixed = TRUE)
    expect_match(output$results$html, "未翻訳の診断があります（原文を確認）", fixed = TRUE)
    expect_match(output$profile_details$html, "QC専用の補完列：LIMIT", fixed = TRUE)
    expect_equal(nca013_ui_count(output$profile_details$html, "QC専用の補完列：LIMIT"), 1L)
    expect_identical(result()$value$warnings, modified$value$warnings)
  })
})
