# UI-only translation markers. Call only with fixed application-owned strings.
# Never pass data values, identifiers, column names, or derived diagnostics here.
desktop_i18n_text <- function(ja, en) {
  shiny::span(class = "pk-i18n-text", `data-pk-ja` = ja, `data-pk-en` = en, lang = "ja", ja)
}

# Application-owned options only: each entry is value -> c(Japanese, English).
# Data-driven columns, IDs, treatments, units and route values are excluded.
desktop_i18n_choice_definitions <- function() list(
  "nca-time" = stats::setNames(list(c("時間列を選択", "Select a time column")), ""),
  "nca-scope" = list(ss = c("SSNCAFL=Y（最終投与区間）", "SSNCAFL=Y (final dosing interval)"), all = c("全行", "All rows")),
  "nca-blq" = stats::setNames(list(c("方針を選択", "Select a policy"), c("除外", "Exclude"), c("0に置換", "Replace with zero"), c("記録値を使用", "Use recorded value")), c("", "exclude", "zero", "as_recorded")),
  "nca-dosing_context" = stats::setNames(list(c("投与状況を選択", "Select a dosing context"),
    c("投与継続中・未確認", "Ongoing dosing / unconfirmed"), c("単回投与", "Single dose"),
    c("独立した単回投与期", "Isolated single-dose period"), c("反復投与の最終投与後", "After the final dose of a repeated regimen")),
    c("", "ongoing", "single", "isolated_single_period", "repeated_final")),
  "nca-washout_attestation" = list(unconfirmed = c("未確認", "Unconfirmed"),
    protocol_confirmed = c("計画・投与履歴に基づき十分な休薬期間を確認した", "Confirmed adequate washout against the study plan and dosing history")),
  "nca-predose_qc_lloq_source" = stats::setNames(list(c("未指定", "Unspecified"), c("列", "Column"), c("定数", "Constant")), c("", "column", "constant")),
  "nca-body_bsa_mode" = stats::setNames(list(c("使わない", "Do not use BSA"), c("記録値", "Recorded BSA"),
    c("Mosteller式を明示的に使用（採用体重・身長が必要）", "Use the Mosteller equation (requires selected weight and height)")), c("", "recorded", "mosteller")),
  "nca-time_unit" = list(h = c("時間（h）", "Hours (h)"), min = c("分（min）", "Minutes (min)"), day = c("日（day）", "Days (day)")),
  "nca-c0_method" = stats::setNames(list(c("方針を選択", "Select a policy"), c("補完しない", "Do not impute"), c("0で補完", "Impute zero"), c("投与前値を使用", "Use predose value")), c("", "none", "zero", "predose")),
  "nca-auc_method" = stats::setNames(list(c("上昇は線形・下降は対数台形", "Linear up / log down"), c("線形台形", "Linear trapezoidal")), c("lin up/log down", "linear")),
  "nca-terminal_method" = list(none = c("計算しない", "Do not calculate"), auto = c("自動・末尾の連続点", "Automatic consecutive terminal points"), manual = c("時間区間を指定", "Specify time interval")),
  "nca-flag_convention" = list(strict = c("0/1・Y/N等（空欄はエラー）", "0/1, Y/N etc. (blank is an error)"), y_blank = c("Y/空欄（空欄・NA＝偽）", "Y / blank (blank or NA is false)")),
  "nca-all_blq_policy" = list(missing = c("欠測にする", "Set to missing"), zero = c("Cmax・AUCを0とする（解析計画による指定）", "Set Cmax and AUC to zero (per analysis plan)")),
  "nca-terminal_qc_action" = list(withhold = c("値を保留", "Withhold value"), flag = c("値と警告を表示", "Display value with warning")),
  "nca-dose_basis" = stats::setNames(list(c("基準を選択", "Select dose basis"), c("絶対量（mg等）", "Absolute dose (e.g., mg)"), c("体重当たり（mg/kg等）", "Per weight (e.g., mg/kg)"), c("体表面積当たり（mg/m2等）", "Per body surface area (e.g., mg/m2)")), c("", "absolute", "per_kg", "per_m2")),
  "nca-dose_reference" = stats::setNames(list(c("対応を確認", "Confirm dose reference"), c("EX結合で選択した基準投与", "Reference dose selected by EX matching"), c("EX結合なし・解析単位ごとに実際の基準投与1回分を明示", "No EX matching: declare one actual reference dose per analysis unit")), c("", "selected_dose", "declared_profile")),
  "nca-group-role" = stats::setNames(list(c("未指定", "Not specified"), c("投与期", "Period"), c("分析対象物", "Analyte"), c("検体", "Specimen"), c("処置・製剤", "Treatment / formulation"), c("試験", "Study"), c("その他", "Other")), c("", "period", "analyte", "specimen", "treatment", "study", "other")),
  "nca-derived_parameters" = list(
    CMAXD = c("投与量で正規化したCmax（CMAXD）", "Dose-normalized Cmax (CMAXD)"),
    AUCLSTD = c("投与量で正規化したAUClast（AUCLSTD）", "Dose-normalized AUClast (AUCLSTD)"),
    AUCIFOD = c("投与量で正規化したAUC∞（実測Clast基準）（AUCIFOD）", "Dose-normalized AUC infinity, observed Clast (AUCIFOD)"),
    CLFO = c("見かけのクリアランス（CL/F）（CLFO）", "Apparent clearance, CL/F (CLFO)"),
    VZFO = c("見かけの分布容積（Vz/F）（VZFO）", "Apparent volume of distribution, Vz/F (VZFO)"),
    CLFOWT = c("体重当たりCL/F（CLFOWT）", "CL/F per weight (CLFOWT)"), VZFOWT = c("体重当たりVz/F（VZFOWT）", "Vz/F per weight (VZFOWT)"),
    CLFOBSA = c("体表面積当たりCL/F（CLFOBSA）", "CL/F per body surface area (CLFOBSA)"), VZFOBSA = c("体表面積当たりVz/F（VZFOBSA）", "Vz/F per body surface area (VZFOBSA)"))
)

desktop_i18n_choices <- function(id) {
  choices <- desktop_i18n_choice_definitions()[[id]]
  stopifnot(!is.null(choices), all(vapply(choices, length, integer(1)) == 2L))
  stats::setNames(names(choices), vapply(choices, `[[`, character(1), 1L))
}

desktop_i18n_choice_json <- function() {
  definitions <- lapply(desktop_i18n_choice_definitions(), function(x) {
    names(x)[names(x) == ""] <- "__pk_empty__"
    x
  })
  jsonlite::toJSON(definitions, auto_unbox = TRUE)
}

# Presentation-only translations keyed by internal diagnostic codes. Unknown
# codes remain visible with the original Japanese diagnostic for review.
desktop_i18n_diagnostic <- function(original, ja) {
  labels <- c(
    ok="Calculated", review="Review required", partial="Partially missing; review required", excluded="Excluded",
    calculated="Calculated", not_calculable="Not calculable", policy_assigned="Assigned by policy",
    disabled="Not enabled", not_evaluable="Not evaluable", pass="At or below threshold",
    pass_blq="BLQ upper bound at or below threshold", exceeds="Above threshold",
    missing_time="Missing time", missing_concentration="Missing concentration",
    invalid_time="Invalid time", no_quantifiable="No original quantifiable observations",
    no_original_quantifiable_observation="No original quantifiable observations",
    auc_not_calculated="AUC not calculated", outside_analysis_interval="Outside the analysis interval",
    not_used_after_last_quantifiable="Not used after the last quantifiable time",
    fewer_than_3_positive_post_cmax="Fewer than three positive observed quantifiable concentrations after Tmax",
    duplicate_terminal_time="Duplicate observed terminal-phase times",
    no_log_concentration_variation="No variation in log concentration", terminal_fit_failed="Terminal-phase regression could not be calculated",
    nonfinite_fit="Terminal-phase regression is outside the finite numeric range", nonnegative_slope="Terminal phase is not declining",
    ongoing_repeated_dose="Terminal phase during ongoing dosing is outside scope", dose_history_unconfirmed="Completeness of dosing history is not confirmed",
    source_dose_history_unconfirmed="Source dosing history has unconfirmed items", invalid_source_dose_count="Source dose count is invalid",
    single_dose_context_conflict="Single-dose selection conflicts with dosing history", invalid_next_dose_evidence="Next-dose records are inconsistent",
    intervening_dose="An additional dose occurred within the analysis interval", final_dose_context_conflict="A dose is recorded after the selected dose",
    dose_evidence_requires_reassembly="New complete dosing history is required; reassemble EX, PC and concentrations",
    dose_evidence_time_context_mismatch="Analyze this dosing history with reference time 0 h and time unit h",
    invalid_dose_reference_evidence="Reference dose evidence is missing or inconsistent",
    inconsistent_dose_evidence="Times or counts of prior/next doses are missing or inconsistent",
    incomplete_sampling_time_evidence="Source sample times are missing or invalid; timing relative to the additional dose cannot be verified",
    washout_unconfirmed="Adequate washout since the prior dose is not confirmed",
    predose_qc_required_for_prior_dose="Passing predose QC is required when a prior dose exists",
    predose_qc_exceeded="The observed predose QC ratio exceeds the threshold",
    synthetic_point="Imputed points are not used in the terminal phase",
    nonpositive_or_invalid_original_concentration="Original observed concentration is non-positive or invalid",
    not_after_cmax="Observation is at or before Tmax", not_after_infusion_end="Observation is at or before the end of infusion",
    outside_manual_interval="Outside the specified terminal-phase interval", invalid_analysis_profile="Analysis unit cannot be calculated",
    r_squared_below_threshold="Terminal-phase R² is below the threshold",
    span_below_threshold="Terminal-phase span is below the specified number of half-lives; review required",
    aucinf_requires_single_dose="AUC infinity requires confirmed single dosing", partial_auc_start="AUC does not start at the dose reference",
    missing_auclast="AUClast could not be calculated", policy_assigned_auclast="No extrapolation from policy-assigned AUC",
    terminal_excludes_tlast="Terminal-phase end does not match the last quantifiable time",
    missing_original_clast="Observed concentration at the last quantifiable time could not be identified",
    aucinf_numeric_range="AUC infinity is outside the supported numeric range",
    extrapolation_above_threshold="AUC infinity extrapolated fraction exceeds the threshold",
    unit_mismatch="Concentration units do not match", missing_unit="Concentration unit is missing", invalid_cmax="No positive Cmax",
    missing_lloq="LLOQ not specified", invalid_lloq="Invalid LLOQ", unverified_lloq="Legacy LLOQ setting could not be verified",
    no_candidate="No predose sample within the specified time window", ambiguous_latest="The latest predose sample is duplicated",
    invalid_concentration="Latest predose concentration or BLQ flag is invalid", bound_above_threshold="BLQ upper bound exceeds the threshold; QC cannot be evaluated",
    dependency_unavailable_CMAX="No acceptable Cmax", dependency_unavailable_AUCLST="No acceptable AUClast",
    dependency_unavailable_AUCIFO="No acceptable positive AUCinf based on observed Clast", dependency_unavailable_LAMZ="No acceptable positive lambda-z",
    oral_quantifiable_time_zero="Quantifiable observed concentration at the reference time of a single oral dose; check the record and dosing time",
    numeric_range="Calculated value is outside the supported numeric range", missing_dose="Actual dose has not been selected",
    invalid_dose="Actual dose is missing or not a positive finite value", ambiguous_dose="Actual doses within the analysis unit differ",
    unsupported_route="Apparent CL/F and Vz/F are not calculated for this route", ambiguous_route="Routes within the analysis unit differ",
    missing_route="Dosing route is missing", unsupported_concentration_unit="Cannot convert concentration mass or volume units",
    missing_weight="Weight not selected", invalid_weight="Weight is missing or not positive and finite", ambiguous_weight="Weights within the analysis unit differ",
    missing_height="Height not selected", invalid_height="Height is missing or not positive and finite", ambiguous_height="Heights within the analysis unit differ",
    missing_bsa="Body surface area not selected", invalid_bsa="Body surface area is missing or not positive and finite", ambiguous_bsa="Body surface areas within the analysis unit differ",
    c0="Imputed C0", c0_zero="C0 imputed as zero", c0_predose="Predose value used for C0",
    "計算できないパラメータがあります。NCA_EXCLUDEのパラメータ別の理由を確認してください。"="Some parameters could not be calculated. Review parameter-specific reasons in NCA_EXCLUDE."
  )
  # These engine messages already originate in English. Using the same keys
  # for both routes also recovers their meaning after legacy localization.
  engine_messages <- c("No quantifiable endpoint after the interval start",
    "Missing concentration at the requested start; select an explicit C0 policy when starting at the dose reference",
    "Numeric overflow: the calculated value exceeds the supported range", "PKNCA returned a missing AUC",
    "Duplicate observation times within the analysis profile", "Ambiguous latest predose measurement",
    "No eligible predose measurement within the specified lookback", "Fewer than two eligible original observations in the analysis profile",
    "All observed non-BLQ concentrations are zero; Tmax is undefined", "No eligible non-BLQ observed concentrations",
    "AUCLST must start at the dose reference; the selected interval starts later",
    "All original eligible observations are BLQ; no quantifiable endpoint or observed peak time",
    "All-BLQ zero policy assigned CMAX/AUC; Tmax and quantifiable Tlast remain undefined",
    "Predose concentration exceeds the selected percent-of-Cmax QC threshold; no automatic exclusion", "Copied to dose reference for AUC only",
    "profile requires review")
  labels <- c(labels, stats::setNames(engine_messages, engine_messages))
  if (is.na(original) || !nzchar(original)) return(desktop_i18n_text(ja, "Not applicable"))
  en <- if (original %in% names(labels)) unname(labels[[original]]) else NULL
  # Some legacy NCA paths store an already-localized Japanese diagnostic.
  # Resolve it only when the code-to-Japanese mapping has one unambiguous
  # English equivalent; never apply this lookup to arbitrary data columns.
  if (is.null(en)) en <- desktop_i18n_reverse_diagnostic(original, labels)
  if (is.null(en) && grepl("^(.*)。(算出不可|方針による値)：[A-Za-z0-9_, ]+$", ja)) {
    reason_ja <- sub("^(.*)。(算出不可|方針による値)：[A-Za-z0-9_, ]+$", "\\1", ja)
    reason_en <- desktop_i18n_reverse_diagnostic(reason_ja, labels)
    if (!is.null(reason_en)) {
      action <- if (grepl("。算出不可：", ja, fixed = TRUE)) "Not calculable" else "Assigned by policy"
      parameters <- sub("^.*。(算出不可|方針による値)：", "", ja)
      en <- paste0(reason_en, ". ", action, ": ", parameters)
    }
  }
  if (is.null(en) && grepl("^算出不可：", original)) en <- paste0("Not calculable: ", sub("^算出不可：", "", original))
  if (is.null(en) && grepl("^方針による値：", original)) en <- paste0("Assigned by policy: ", sub("^方針による値：", "", original))
  if (is.null(en) && grepl("^時間欠測 [0-9]+ 行$", original)) en <- sub("^時間欠測 ([0-9]+) 行$", "Missing time: \\1 rows", original)
  if (is.null(en) && grepl("^濃度欠測 [0-9]+ 行$", original)) en <- sub("^濃度欠測 ([0-9]+) 行$", "Missing concentration: \\1 rows", original)
  if (is.null(en) && grepl("^[0-9]+ source rows excluded for missing (time|concentration)(; profile requires review)?$", original)) {
    count <- sub(" .*", "", original)
    field <- if (grepl("missing time", original, fixed = TRUE)) "time" else "concentration"
    en <- paste0(count, " source rows excluded for missing ", field,
      if (grepl("; profile requires review", original, fixed = TRUE)) "; review the analysis unit" else "")
  }
  if (is.null(en) && identical(original, "投与前QC：評価不可")) en <- "Predose QC: not evaluable"
  if (is.null(en) && identical(original, "投与前QC：未設定")) en <- "Predose QC: not enabled"
  if (is.null(en) && identical(original, "投与前QC：閾値超過")) en <- "Predose QC: threshold exceeded"
  if (is.null(en) && grepl("^投与前QC：閾値超過（実測 [0-9.]+%、閾値 [0-9.]+%）$", original))
    en <- sub("^投与前QC：閾値超過（実測 ([0-9.]+)%、閾値 ([0-9.]+)%）$", "Predose QC: threshold exceeded (observed \\1%, threshold \\2%)", original)
  if (is.null(en) && grepl("^[A-Za-z][A-Za-z0-9 /,.'%()-]*[ ][A-Za-z]", original) && !grepl("[ぁ-んァ-ン一-龠]", original)) en <- original
  if (is.null(en)) en <- paste0("Untranslated diagnostic; review the Japanese: ", ja)
  desktop_i18n_text(ja, en)
}

desktop_i18n_reverse_diagnostic <- local({
  index <- NULL
  function(original, labels) {
    if (is.null(index)) {
      japanese <- vapply(names(labels), desktop_nca_diagnostic_label, character(1))
      grouped <- split(unname(labels[!is.na(japanese)]), japanese[!is.na(japanese)])
      index <<- lapply(grouped, unique)
    }
    candidates <- index[[original]]
    if (length(candidates) == 1L) candidates[[1L]] else NULL
  }
})

desktop_i18n_controls <- function() {
  shiny::tagList(
    shiny::tags$style(shiny::HTML('
.pk-language-bar { display:flex; align-items:center; flex-wrap:wrap; gap:8px; padding:8px 18px; background:#f1f6f6; border-bottom:1px solid #d5e2e4; color:#23343c; }
.pk-language-bar button { border:1px solid #8ca6ad; background:white; color:#23343c; border-radius:5px; padding:4px 10px; }
.pk-language-bar button[aria-pressed="true"] { background:#176b79; border-color:#176b79; color:white; }
.pk-language-bar button:focus-visible { outline:3px solid #d1820a; outline-offset:2px; }
.pk-language-preview { color:#694800; font-size:13px; line-height:1.5; }
@media (min-width:992px) {
  body:has(.pk-language-bar) .navbar .navbar-nav { flex-wrap:nowrap; max-width:100%; overflow-x:auto; white-space:nowrap; }
  body:has(.pk-language-bar) .navbar .nav-link { white-space:nowrap; }
}
')),
    shiny::div(class = "pk-language-bar", role = "group", `aria-label` = "言語 / Language",
      shiny::span("表示言語 / Display language"),
      shiny::tags$button(type = "button", `data-pk-language` = "ja", `aria-pressed` = "true", lang = "ja", "日本語"),
      shiny::tags$button(type = "button", `data-pk-language` = "en", `aria-pressed` = "false", lang = "en", "English (preview)"),
      shiny::span(class = "pk-language-preview", hidden = "hidden", lang = "en",
        "English preview: some labels, warnings, errors, plots and exported files remain in Japanese. Confirm the Japanese original before interpreting or using results.")),
    shiny::tags$script(shiny::HTML(paste0("window.pkFixedChoices = ", desktop_i18n_choice_json(), ";"))),
    shiny::tags$script(shiny::HTML('
(function () {
  "use strict";
  if (window.pkLanguagePreview) return;
  var language = "ja";
  try {
    var stored = window.sessionStorage.getItem("pk-desktop-display-language");
    if (stored === "ja" || stored === "en") language = stored;
  } catch (e) { /* Browser storage may be unavailable; keep the default. */ }
  function notifyServer() {
    if (window.Shiny && typeof window.Shiny.setInputValue === "function")
      window.Shiny.setInputValue("pk_ui_language", language, {priority:"event"});
  }
  if (window.jQuery) window.jQuery(document).on("shiny:connected", notifyServer);
  // Only these application-defined controls and values are translated. Never
  // translate data-driven choices, selected values, table cells or free text.
  var choices = window.pkFixedChoices || {};
  function applyChoices() {
    function translateOption(option, pair) {
      if (!pair || (option.textContent !== pair[0] && option.textContent !== pair[1])) return;
      var text = pair[language === "en" ? 1 : 0];
      if (option.textContent !== text) option.textContent = text;
      if (option.lang !== language) option.lang = language;
    }
    Object.keys(choices).forEach(function(id) {
      var select = document.getElementById(id);
      if (!select) return;
      if (!select.selectize) {
        Array.from(select.options).forEach(function(option) {
          translateOption(option, choices[id][option.value || "__pk_empty__"]);
        });
        return;
      }
      var selectize = select.selectize;
      [selectize.$control && selectize.$control[0], selectize.$dropdown_content && selectize.$dropdown_content[0]].forEach(function(container) {
        if (!container) return;
        container.querySelectorAll("[data-value]").forEach(function(node) {
          translateOption(node, choices[id][node.getAttribute("data-value") || "__pk_empty__"]);
        });
      });
      if (id === "nca-c0_method" || id === "nca-time") {
        var field = selectize.$control_input && selectize.$control_input[0];
        var emptyLabel = choices[id]["__pk_empty__"];
        if (field && emptyLabel && (field.placeholder === emptyLabel[0] || field.placeholder === emptyLabel[1]))
          field.placeholder = emptyLabel[language === "en" ? 1 : 0];
      }
    });
    var roles = choices["nca-group-role"] || {};
    document.querySelectorAll(".pk-i18n-role-select select").forEach(function(select) {
      Array.prototype.forEach.call(select.options, function(option) {
        translateOption(option, roles[option.value || "__pk_empty__"]);
      });
    });
  }
  function apply(root) {
    if (!root) return;
    var nodes = [];
    if (root.nodeType === 1 && root.matches(".pk-i18n-text[data-pk-ja][data-pk-en]")) nodes.push(root);
    if (root.querySelectorAll) root.querySelectorAll(".pk-i18n-text[data-pk-ja][data-pk-en]").forEach(function(n) { nodes.push(n); });
    nodes.forEach(function(n) {
      var value = n.getAttribute(language === "en" ? "data-pk-en" : "data-pk-ja");
      if (value !== null && n.textContent !== value) n.textContent = value;
      if (n.lang !== language) n.lang = language;
    });
    var bars = [];
    if (root.nodeType === 1 && root.matches(".shiny-file-input-progress .progress-bar")) bars.push(root);
    if (root.querySelectorAll) root.querySelectorAll(".shiny-file-input-progress .progress-bar").forEach(function(n) { bars.push(n); });
    bars.forEach(function(bar) {
      if (bar.textContent === "Upload complete" || bar.textContent === "アップロード完了") {
        var text = language === "ja" ? "アップロード完了" : "Upload complete";
        if (bar.textContent !== text) bar.textContent = text;
      }
    });
    // The preview still contains Japanese, including safety messages.
    // Mark translated spans individually and retain Japanese as the page default.
    document.documentElement.lang = "ja";
    document.querySelectorAll("[data-pk-language]").forEach(function(b) {
      b.setAttribute("aria-pressed", String(b.getAttribute("data-pk-language") === language));
    });
    document.querySelectorAll(".pk-language-preview").forEach(function(b) { b.hidden = language !== "en"; });
    applyChoices();
  }
  document.addEventListener("click", function(event) {
    var b = event.target.closest("button[data-pk-language]");
    if (!b) return;
    var next = b.getAttribute("data-pk-language");
    if (next !== "ja" && next !== "en") return;
    language = next;
    try { window.sessionStorage.setItem("pk-desktop-display-language", language); } catch (e) {}
    apply(document);
    notifyServer();
  });
  var observer = new MutationObserver(function(records) {
    records.forEach(function(record) { record.addedNodes.forEach(function(n) { if (n.nodeType === 1) apply(n); else if (n.nodeType === 3 && n.parentElement && n.parentElement.matches(".shiny-file-input-progress .progress-bar, .selectize-input .item, .selectize-dropdown .option")) apply(n.parentElement); }); });
  });
  function start() { apply(document); observer.observe(document.documentElement, {childList:true, subtree:true}); }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start, {once:true}); else start();
  window.pkLanguagePreview = true;
}());
')))
}
