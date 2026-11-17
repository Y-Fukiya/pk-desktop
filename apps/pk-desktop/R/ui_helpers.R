# Presentation-only components; all selectors are scoped to the PK modules.
desktop_ui_styles <- function() shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML('
.pk-workspace { --pk-ink:#23343c; --pk-muted:#64747b; --pk-line:#dce4e6; --pk-blue:#176b79; color:var(--pk-ink); font-family:-apple-system,BlinkMacSystemFont,"Segoe UI","Hiragino Kaku Gothic ProN","Yu Gothic",sans-serif; padding:16px 18px 24px; min-width:0; }
.pk-workspace * { box-sizing:border-box; }
.pk-workspace .pk-header { margin:0 0 22px; }
.pk-workspace .pk-eyebrow { font-size:11px; font-weight:700; letter-spacing:.12em; color:var(--pk-muted); margin-bottom:6px; }
.pk-workspace h2 { font-size:27px; font-weight:700; line-height:1.35; margin:0 0 8px; }
.pk-workspace .pk-subtitle { color:var(--pk-muted); line-height:1.7; margin:0; max-width:85ch; }
.pk-workspace .pk-layout { display:grid; grid-template-columns:minmax(280px,320px) minmax(0,1fr); gap:22px; align-items:start; }
.pk-workspace .pk-controls,.pk-workspace .pk-results { min-width:0; }
.pk-workspace .pk-controls,.pk-workspace .pk-stack { display:flex; flex-direction:column; gap:16px; }
.pk-workspace .pk-results > * + * { margin-top:16px; }
.pk-workspace .pk-card { background:#fff; border:1px solid var(--pk-line); border-radius:10px; padding:18px; min-width:0; }
.pk-workspace .pk-section-title { display:flex; align-items:center; gap:9px; font-size:16px; font-weight:700; line-height:1.5; margin:0 0 16px; }
.pk-workspace .pk-step { display:inline-flex; align-items:center; justify-content:center; flex:0 0 26px; height:26px; border-radius:50%; background:#e8f2f7; color:#075b83; font-size:13px; }
.pk-workspace h3 { font-size:17px; font-weight:700; line-height:1.5; margin:0 0 12px; }
.pk-workspace h4 { font-size:15px; font-weight:700; line-height:1.5; }
.pk-workspace .shiny-input-container { width:100% !important; max-width:100%; }
.pk-workspace .form-group { margin-bottom:15px; }
.pk-workspace label { font-size:13px; font-weight:600; margin-bottom:6px; line-height:1.5; }
.pk-workspace .form-control,.pk-workspace .selectize-input { border-color:#a6bac7; border-radius:6px; font-size:14px; min-height:38px; box-shadow:none; }
.pk-workspace .selectize-control { margin-bottom:0; }
.pk-workspace .selectize-dropdown { z-index:1050; }
.pk-workspace .btn { white-space:normal; min-height:38px; border-radius:6px; font-size:13px; padding:8px 12px; line-height:1.5; }
.pk-workspace .btn-primary { background:var(--pk-blue); border-color:var(--pk-blue); color:#fff; font-weight:600; }
.pk-workspace .btn-primary:hover { background:#115560; border-color:#115560; }
.pk-workspace .btn[disabled] { opacity:.55; cursor:not-allowed; }
.pk-workspace .pk-actions { display:flex; flex-wrap:wrap; align-items:center; gap:8px; }
.pk-workspace .pk-caption { font-size:12px; color:var(--pk-muted); line-height:1.65; margin:8px 0 12px; }
.pk-workspace .pk-notice { border:1px solid #bdd6e4; border-left:4px solid #4387a8; border-radius:6px; padding:12px 14px; background:#f1f7fa; font-size:13px; line-height:1.7; overflow-wrap:anywhere; }
.pk-workspace .pk-notice-success { background:#eff8f3; border-color:#acd3bd; border-left-color:#24734b; color:#184b32; }
.pk-workspace .pk-notice-error { background:#fff3f1; border-color:#efb6ae; border-left-color:#ba4133; color:#8b2b21; }
.pk-workspace .pk-profile-warning + .pk-profile-warning { margin-top:12px; }
.pk-workspace .pk-warning-overview { padding-top:4px; padding-bottom:4px; }
.pk-workspace .pk-selected-warning-output:not(:empty) { margin-bottom:15px; }
.pk-workspace .pk-warning-group { border-top:1px solid #e7d5ad; margin-top:0; padding-top:0; }
.pk-workspace .pk-warning-group:first-child { border-top:0; }
.pk-workspace .pk-warning-group > summary { display:flex; align-items:baseline; gap:8px; padding:8px 2px; color:#604000; font-size:14px; font-weight:600; cursor:pointer; }
.pk-workspace .pk-warning-group > summary::before { content:"▸"; flex:none; }
.pk-workspace .pk-warning-group[open] > summary::before { content:"▾"; }
.pk-workspace .pk-warning-group > summary:focus-visible { outline:3px solid #825500; outline-offset:2px; }
.pk-workspace .pk-warning-category { font-size:12px; font-weight:500; }
.pk-workspace .pk-warning-count { margin-left:auto; flex:none; white-space:nowrap; font-weight:700; }
.pk-workspace .pk-warning-group > .pk-profile-warning { padding-left:20px; }
.pk-workspace .pk-warning-unit-context { padding-left:8px; }
.pk-workspace .pk-selected-warning-identity { font-weight:600; }
.pk-workspace .pk-notice .pk-original-diagnostic pre { white-space:pre-wrap; overflow-wrap:anywhere; }
.pk-workspace .pk-notice-warning { background:#fff8eb; border-color:#e3c683; border-left-color:#a56c0a; color:#704b0a; }
.pk-workspace .pk-selected-warning-output > .pk-notice-warning { background:#fffcf5; border-color:#e9d9ba; border-left-width:2px; }
.pk-workspace .pk-empty { min-height:180px; padding:38px 24px; text-align:center; background:#f7fafc; border:1px dashed #bacbd6; border-radius:8px; }
.pk-workspace .pk-empty p { color:var(--pk-muted); font-size:14px; line-height:1.8; margin:0 auto; max-width:52ch; }
.pk-workspace .pk-metrics { display:grid; grid-template-columns:repeat(auto-fit,minmax(112px,1fr)); gap:10px; margin:0; }
.pk-workspace .pk-metric { border:1px solid var(--pk-line); background:#f7fafc; border-radius:8px; padding:13px 14px; min-width:0; }
.pk-workspace .pk-metric dt { font-size:12px; color:var(--pk-muted); font-weight:600; line-height:1.5; }
.pk-workspace .pk-metric dd { margin:6px 0 0; font-size:25px; font-weight:700; font-variant-numeric:tabular-nums; line-height:1.3; overflow-wrap:anywhere; }
.pk-workspace .pk-metrics + .pk-notice { margin-top:12px; }
.pk-workspace .pk-condition-list { margin:0; padding-left:24px; }
.pk-workspace .pk-condition-list li { padding:7px 0 7px 3px; line-height:1.7; font-size:14px; overflow-wrap:anywhere; }
.pk-workspace details { border-top:1px solid var(--pk-line); padding-top:12px; margin-top:14px; }
.pk-workspace summary { cursor:pointer; font-size:12px; color:#355b70; padding:4px 0; }
.pk-workspace details pre { margin-top:10px; max-height:220px; overflow:auto; font-size:12px; }
.pk-workspace .pk-table-scroll { max-width:100%; max-height:490px; overflow:auto; border:1px solid var(--pk-line); border-radius:7px; scrollbar-gutter:stable; }
.pk-workspace .pk-table-scroll .shiny-html-output { min-width:0; }
.pk-workspace .pk-table-scroll table { min-width:100%; width:max-content; margin:0; font-size:13px; font-variant-numeric:tabular-nums; }
.pk-workspace .pk-table-scroll th,.pk-workspace .pk-table-scroll td { padding:9px 12px; white-space:nowrap; border-bottom:1px solid #e5edf2; }
.pk-workspace .pk-table-scroll td.pk-detail-text { white-space:normal; min-width:220px; max-width:380px; overflow-wrap:anywhere; }
.pk-workspace .pk-table-scroll thead th { position:sticky; top:0; background:#eaf1f5; color:#294657; z-index:1; font-size:12px; }
.pk-workspace .pk-table-scroll tbody tr:nth-child(even) { background:#f7fafc; }
.pk-workspace .pk-table-scroll tbody tr:hover { background:#edf6fa; }
.pk-workspace :focus-visible { outline:3px solid #d1820a; outline-offset:3px; }
.pk-workspace .pk-visually-hidden { position:absolute; width:1px; height:1px; margin:-1px; padding:0; overflow:hidden; clip:rect(0,0,0,0); white-space:nowrap; border:0; }
@media (max-width:1100px) { .pk-workspace .pk-layout { grid-template-columns:minmax(0,1fr); } .pk-workspace .pk-controls { display:grid; grid-template-columns:repeat(auto-fit,minmax(240px,1fr)); align-items:start; } }
@media (max-width:600px) { .pk-workspace { padding:12px 8px 20px; } .pk-workspace .pk-controls { display:flex; } .pk-workspace h2 { font-size:23px; } .pk-workspace .pk-card { padding:14px; } .pk-workspace .pk-metrics { grid-template-columns:repeat(2,minmax(0,1fr)); } }
'))))

desktop_ui_page <- function(title, subtitle, controls, body, eyebrow = "PK DESKTOP / DATA WORKSPACE", class = "") {
  shiny::tagList(desktop_ui_styles(), shiny::div(class = paste("pk-workspace", class),
    shiny::tags$header(class = "pk-header",
      shiny::div(class = "pk-eyebrow", eyebrow),
      shiny::h2(title), shiny::p(class = "pk-subtitle", subtitle)),
    shiny::div(class = "pk-layout",
      shiny::div(class = "pk-controls", controls),
      shiny::div(class = "pk-results", body))))
}
desktop_ui_section <- function(number, title, ...) {
  shiny::tags$section(class = "pk-card",
    shiny::h3(class = "pk-section-title", if (!is.null(number)) shiny::span(class = "pk-step", number), title), ...)
}
desktop_ui_empty <- function(title, message) {
  shiny::div(class = "pk-empty", role = "status", shiny::h3(title), shiny::p(message))
}
desktop_ui_notice <- function(message, tone = "info") {
  if (!length(message) || !nzchar(paste(message, collapse = ""))) return(NULL)
  tone <- match.arg(tone, c("info", "success", "error", "warning"))
  shiny::div(class = paste("pk-notice", paste0("pk-notice-", tone)),
    role = if (tone == "error") "alert" else "status", `aria-live` = if (tone == "error") "assertive" else "polite", message)
}
desktop_ui_metrics <- function(values, translated_labels = NULL) {
  shiny::tags$dl(class = "pk-metrics", lapply(seq_along(values), function(i) {
    value <- values[[i]]
    if (is.numeric(value)) value <- format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
    label <- names(values)[i]
    if (!is.null(translated_labels) && label %in% names(translated_labels))
      label <- desktop_i18n_text(label, unname(translated_labels[[label]]))
    shiny::div(class = "pk-metric", shiny::tags$dt(label), shiny::tags$dd(value))
  }))
}

desktop_display_value <- function(x) {
  if (length(x) != 1L || is.na(x)) return("—")
  if (is.numeric(x) && !inherits(x, c("Date", "POSIXt", "difftime")))
    return(format(signif(x, 4L), digits = 4L, trim = TRUE))
  as.character(x)
}

desktop_rule_label <- function(x) {
  labels <- c(verified_predose = "日時照合済み・投与前", attested_chronology_unverified = "基準時点を確認済み・日時の前後関係は未検証", postdose = "投与後測定・計算には使用不可", recorded = "記録値", existing_mdv = "元のMDVで除外", blq_excluded = "BLQ規則で除外",
    missing_time = "時間欠測", missing_concentration = "濃度欠測", not_blq = "非BLQ",
    pre = "投与前", post = "投与後", first = "先頭", middle = "途中", last = "末尾", all_blq = "全点BLQ",
    keep = "記録値を使用", zero = "0へ置換", half_lloq = "LLOQ/2へ置換", exclude = "除外",
    ok = "計算済み", review = "要確認", partial = "一部欠測・要確認", excluded = "除外", derived = "算出", invalid_datetime = "不正な日時",
    calculated = "計算値", policy_assigned = "方針による値", not_calculable = "算出不可",
    disabled = "未設定", not_evaluable = "評価不可", pass = "閾値以下", pass_blq = "BLQの上限が閾値以下", exceeds = "閾値超過",
    missing_lloq = "LLOQ未指定", invalid_lloq = "LLOQが不正", unverified_lloq = "旧形式のLLOQ設定を検証できません",
    unit_mismatch = "濃度単位が一致しません", missing_unit = "濃度単位がありません", invalid_cmax = "正のCmaxがありません",
    numeric_range = "計算値が扱える数値範囲外です",
    outside_range_weight = "体重が運用上の確認範囲外です。依存する計算を保留します",
    outside_range_height = "身長が運用上の確認範囲外です。依存する計算を保留します",
    outside_range_bsa = "体表面積が運用上の確認範囲外です。依存する計算を保留します",
    oral_quantifiable_time_zero = "単回経口投与の基準時刻に定量可能な実測濃度があります。記録と投与時刻を確認してください",
    unsupported_dose_unit_or_basis = "投与量の単位と基準が未対応、または一致しません", unsupported_route = "この経路では見かけのCL/F・Vz/Fを計算しません",
    ambiguous_route = "解析単位内の投与経路が一致しません", missing_route = "投与経路がありません", unsupported_dosing_context = "追加PKは単回投与または独立した単回投与期が対象です",
    unsupported_concentration_unit = "濃度の質量・体積単位を換算できません",
    dependency_unavailable_CMAX = "採用可能なCmaxがありません", dependency_unavailable_AUCLST = "採用可能なAUClastがありません",
    dependency_unavailable_AUCIFO = "採用可能な正のAUCinf_obsがありません", dependency_unavailable_LAMZ = "採用可能な正のλzがありません",
    missing_dose = "実投与量が採用されていません",
    invalid_dose = "実投与量が欠測または正の有限値ではありません",
    ambiguous_dose = "解析単位内の実投与量が一致しません",
    unsupported_unit_dose = "実投与量の単位を換算できません",
    numeric_range_dose = "実投与量の換算値が数値範囲外です",
    nonbaseline_dose = "実投与量は投与前の基準値として使用できません",
    missing_weight = "体重が採用されていません",
    invalid_weight = "体重が欠測または正の有限値ではありません",
    ambiguous_weight = "解析単位内の体重が一致しません",
    unsupported_unit_weight = "体重の単位を換算できません",
    numeric_range_weight = "体重の換算値が数値範囲外です",
    nonbaseline_weight = "体重は投与前の基準値として使用できません",
    missing_height = "身長が採用されていません",
    invalid_height = "身長が欠測または正の有限値ではありません",
    ambiguous_height = "解析単位内の身長が一致しません",
    unsupported_unit_height = "身長の単位を換算できません",
    numeric_range_height = "身長の換算値が数値範囲外です",
    nonbaseline_height = "身長は投与前の基準値として使用できません",
    missing_bsa = "体表面積が採用されていません",
    invalid_bsa = "体表面積が欠測または正の有限値ではありません",
    ambiguous_bsa = "解析単位内の体表面積が一致しません",
    unsupported_unit_bsa = "体表面積の単位を換算できません",
    numeric_range_bsa = "体表面積の換算値が数値範囲外です",
    nonbaseline_bsa = "体表面積は投与前の基準値として使用できません",
    no_candidate = "対象時間内に投与前採血がありません", ambiguous_latest = "最新の投与前採血が重複しています",
    invalid_concentration = "最新の投与前濃度またはBLQフラグが不正", bound_above_threshold = "BLQの上限が閾値を超えるため判定できません",
    missing_sample = "採血日時欠測", missing_dose = "投与日時欠測", missing_both = "両日時欠測",
    used = "使用", interval = "区間外", after_tlast = "最終定量時点より後", profile_excluded = "解析単位を除外",
    scope = "解析対象外", MDV = "元のMDVで除外", BLQ = "BLQとして除外",
    invalid_time = "時間が不正", no_quantifiable = "元の定量可能な観測値なし", auc_not_calculated = "AUC未算出",
    outside_analysis_interval = "解析区間外", no_original_quantifiable_observation = "元の定量可能な観測値がありません",
    not_used_after_last_quantifiable = "最終定量時点より後のため使用しません",
    fewer_than_3_positive_post_cmax = "Tmax後の正の実測定量点が3点未満です",
    duplicate_terminal_time = "終末相の実測時刻が重複しています",
    no_log_concentration_variation = "対数濃度に変動がありません", terminal_fit_failed = "終末相の回帰を計算できません",
    nonfinite_fit = "終末相の回帰値が有限範囲外です", nonnegative_slope = "終末相が下降していません",
    ongoing_repeated_dose = "投与継続中の終末相は対象外です", dose_history_unconfirmed = "投与履歴の完全性が未確認です",
    source_dose_history_unconfirmed = "結合元の投与履歴に未確認事項があります", invalid_source_dose_count = "結合元の投与回数が不正です",
    single_dose_context_conflict = "投与履歴と単回投与の指定が一致しません", invalid_next_dose_evidence = "次回投与の記録が不整合です",
    intervening_dose = "解析区間に追加投与があります", final_dose_context_conflict = "選んだ投与より後の投与記録があります",
    dose_evidence_requires_reassembly = "投与履歴の新しい完全な記録が必要です。EX・PC・濃度から再作成してください",
    dose_evidence_time_context_mismatch = "投与履歴は投与基準0 h・時間単位hで解析してください",
    invalid_dose_reference_evidence = "基準投与の記録が欠落または不整合です",
    inconsistent_dose_evidence = "前回・次回投与の時刻と回数が欠落または不整合です",
    incomplete_sampling_time_evidence = "元の採血時刻に欠測・不正があり、追加投与との前後関係を確認できません",
    washout_unconfirmed = "前回投与からの十分な休薬期間が未確認です",
    predose_qc_required_for_prior_dose = "前回投与がある解析単位には投与前QCの合格が必要です",
    predose_qc_exceeded = "投与前QCの実測比が閾値を超えています",
    isolated_single_period = "単回投与の独立した投与期（クロスオーバー等）", protocol_confirmed = "十分な休薬期間を確認済み", unconfirmed = "未確認",
    synthetic_point = "補完点は終末相に使いません", nonpositive_or_invalid_original_concentration = "元の実測濃度が非正値または不正です",
    not_after_cmax = "Tmax以前の点です", not_after_infusion_end = "点滴終了以前の点です",
    outside_manual_interval = "指定した終末相区間の外です", invalid_analysis_profile = "解析単位を計算できません",
    r_squared_below_threshold = "終末相のR²が閾値未満です", span_below_threshold = "終末相の区間幅が指定した半減期数に満たないため要確認です",
    aucinf_requires_single_dose = "AUC∞は確認済みの単回投与のみ対象です", partial_auc_start = "投与基準からのAUCではありません",
    missing_auclast = "AUClastを計算できません", policy_assigned_auclast = "方針で割り当てたAUCから外挿しません",
    terminal_excludes_tlast = "終末相の終点が最終定量時点と一致しません", missing_original_clast = "最終定量時点の実測濃度を特定できません",
    aucinf_numeric_range = "AUC∞の計算値が扱える数値範囲外です", extrapolation_above_threshold = "AUC∞の外挿率が閾値を超えています",
    selected = "終末相に選択", not_selected = "終末相の候補・未選択",
    c0 = "C0補完", c0_zero = "C0を0で補完", c0_predose = "C0に投与前値を使用",
    c0_source = "C0のコピー元", imputed_c0_zero = "C0を0で補完", imputed_c0_predose = "C0に投与前値を使用")
  if (is.na(x)) return("—")
  if (x %in% names(labels)) return(unname(labels[x]))
  if (grepl("^invalid_(time|concentration)_(non_numeric|non_finite|underflow|negative)$", x)) {
    reason <- c(non_numeric = "数値として読めません", non_finite = "有限の数値範囲を超えています", underflow = "小さすぎるため数値として保持できません", negative = "負の値です")
    return(paste0(if (startsWith(x, "invalid_time_")) "時間：" else "濃度：", reason[sub("^invalid_(time|concentration)_", "", x)]))
  }
  for (position in c("all_blq", "pre", "post", "first", "middle", "last")) {
    prefix <- paste0(position, "_")
    if (startsWith(x, prefix)) {
      action <- substring(x, nchar(prefix) + 1L)
      if (action %in% names(labels)) return(paste(labels[position], labels[action], sep = "："))
    }
  }
  x
}

desktop_nca_diagnostic_label <- function(x) {
  if (is.na(x) || !nzchar(x)) return("該当なし")
  label <- desktop_rule_label(x)
  if (!identical(label, x)) return(label)
  labels <- c(
    "No quantifiable endpoint after the interval start" = "解析開始後に定量可能な終点がありません",
    "Missing concentration at the requested start; select an explicit C0 policy when starting at the dose reference" = "解析開始時点の濃度がありません。投与基準から解析する場合はC0補完方針を選択してください",
    "Numeric overflow: the calculated value exceeds the supported range" = "計算値が扱える数値範囲を超えています",
    "PKNCA returned a missing AUC" = "PKNCAでAUCが欠測となりました",
    "Duplicate observation times within the analysis profile" = "解析単位内で観測時刻が重複しています",
    "Ambiguous latest predose measurement" = "C0補完に使う最新の投与前採血が重複しています",
    "No eligible predose measurement within the specified lookback" = "C0補完の遡及時間内に使用可能な投与前採血がありません",
    "Fewer than two eligible original observations in the analysis profile" = "解析単位内で使用可能な元の観測値が2点未満です",
    "All observed non-BLQ concentrations are zero; Tmax is undefined" = "非BLQの観測濃度がすべて0のためTmaxは定義できません",
    "No eligible non-BLQ observed concentrations" = "使用可能な非BLQの観測濃度がありません",
    "AUCLST must start at the dose reference; the selected interval starts later" = "AUCLSTは投与基準からの解析が必要です。選択区間は投与基準より後に始まります",
    "All original eligible observations are BLQ; no quantifiable endpoint or observed peak time" = "使用可能な元の観測値がすべてBLQのため、定量可能な終点と観測ピーク時刻がありません",
    "All-BLQ zero policy assigned CMAX/AUC; Tmax and quantifiable Tlast remain undefined" = "全点BLQの方針によりCmax・AUCを0としました。Tmaxと最終定量時点は定義できません",
    "Predose concentration exceeds the selected percent-of-Cmax QC threshold; no automatic exclusion" = "投与前濃度とCmaxの比がQC閾値を超えています。自動除外は行いません",
    "Copied to dose reference for AUC only" = "AUCの計算用に投与基準時点へコピーしました",
    "profile requires review" = "解析単位の確認が必要です")
  if (x %in% names(labels)) return(unname(labels[x]))
  if (grepl("^Invalid numeric data in source rows [0-9, ]+$", x))
    return(paste0("数値が不正な元行：", sub("^Invalid numeric data in source rows ", "", x)))
  if (grepl("^[0-9]+ source rows excluded for missing (time|concentration)(; profile requires review)?$", x)) {
    count <- sub(" .*", "", x)
    return(paste0(if (grepl("missing time", x, fixed = TRUE)) "時間欠測" else "濃度欠測", "により元行 ", count, " 行を除外しました",
      if (grepl("; profile requires review", x, fixed = TRUE)) "。解析単位を確認してください" else ""))
  }
  if (grepl("^Predose percent-of-Cmax QC is not evaluable \\([^()]*\\)$", x)) {
    reason <- sub("^Predose percent-of-Cmax QC is not evaluable \\((.*)\\)$", "\\1", x)
    translated <- desktop_nca_diagnostic_label(reason)
    if (!is.na(translated)) return(paste0("投与前QCを評価できません（", translated, "）"))
  }
  if (grepl("^Profile [0-9]+ terminal: ", x)) {
    prefix <- regmatches(x, regexpr("^Profile [0-9]+ terminal: ", x))
    translated <- desktop_nca_diagnostic_label(substring(x, nchar(prefix) + 1L))
    if (!is.na(translated)) return(paste0(gsub(" terminal", " 終末相", sub("Profile ", "解析単位 ", prefix, fixed = TRUE), fixed = TRUE), translated))
  }
  codes <- c("CMAX", "TMAX", "AUCLST", "AUCPART", "AUCPROC", "LAMZ", "LAMZHL", "AUCIFO", "AUCPEO", names(desktop_nca_derived_parameters()))
  prefix <- regmatches(x, regexpr(paste0("^(Profile [0-9]+|", paste(codes, collapse="|"), "): "), x))
  if (length(prefix) && nzchar(prefix)) {
    translated <- desktop_nca_diagnostic_label(substring(x, nchar(prefix) + 1L))
    if (!is.na(translated)) return(paste0(sub("Profile ", "解析単位 ", prefix, fixed = TRUE), translated))
    return(NA_character_)
  }
  protected <- x
  for (known in names(labels)[grepl("; ", names(labels), fixed = TRUE)])
    protected <- gsub(known, gsub("; ", "\uE000", known, fixed = TRUE), protected, fixed = TRUE)
  parts <- gsub("\uE000", "; ", strsplit(protected, ";[ ]*", perl = TRUE)[[1]], fixed = TRUE)
  if (length(parts) > 1L) {
    translated <- vapply(parts, desktop_nca_diagnostic_label, character(1))
    if (!anyNA(translated)) return(paste(translated, collapse = "。"))
    return(NA_character_)
  }
  japanese_warnings <- c("計算できないパラメータがあります。NCA_EXCLUDEのパラメータ別の理由を確認してください。",
    "AUCの開始濃度がありません。投与基準時刻から解析する場合は明示的なC0補完設定と投与前値の条件を確認してください。")
  if (x %in% japanese_warnings || grepl("^投与前QC：(閾値超過|評価不可|未設定)(（実測 [0-9]+\\.[0-9]%、閾値 [0-9]+\\.[0-9]%）)?$", x) ||
      grepl(paste0("^(方針による値|算出不可)：(", paste(codes, collapse="|"), ")(, (", paste(codes, collapse="|"), "))*$"), x)) return(x)
  if (grepl("[ぁ-んァ-ン一-龠]", x) && !grepl("[A-Za-z]", x)) return(x)
  NA_character_
}

desktop_nca_warning_text <- function(x) {
  translated <- desktop_nca_diagnostic_label(x)
  if (!is.na(translated)) translated else paste0("未翻訳の診断があります（原文を確認）：", x)
}

# Split only semicolon-delimited machine reason codes; preserve natural-language diagnostics verbatim.
desktop_nca_reason_parts <- function(reason) {
  if (is.na(reason) || !nzchar(reason)) return(reason)
  if (!grepl("^[a-z][A-Za-z0-9_]*(;[ ]*[a-z][A-Za-z0-9_]*)+$", reason)) return(reason)
  strsplit(reason, ";[ ]*", perl = TRUE)[[1L]]
}

desktop_nca_expand_messages <- function(messages) {
  unique(unlist(lapply(messages, function(message) {
    prefix <- regmatches(message, regexpr("^[A-Z][A-Z0-9_]*: ", message))
    if (!length(prefix) || !nzchar(prefix)) return(desktop_nca_reason_parts(message))
    paste0(prefix, desktop_nca_reason_parts(substring(message, nchar(prefix) + 1L)))
  }), use.names = FALSE))
}

# Human-readable projection; the original warnings and reason codes remain in trace.
desktop_nca_public_warnings <- function(value) {
  remaining <- as.character(value$trace$warnings %||% character())
  result <- character()
  keys <- unlist(value$trace$output_key_mapping, use.names = FALSE)
  for (group in value$trace$groups) {
    prefix <- paste0("Profile ", group$profile, ": ")
    terminal_prefix <- paste0("Profile ", group$profile, " terminal: ")
    regular <- startsWith(remaining, prefix)
    terminal <- startsWith(remaining, terminal_prefix)
    messages <- unique(as.character(group$warning %||% character()))
    # Keep unexpected diagnostics even if they are absent from the group record.
    extra <- vapply(substring(remaining[regular], nchar(prefix) + 1L), function(payload) {
      known <- messages[order(nchar(messages), decreasing = TRUE)]
      while (nzchar(payload)) {
        matched <- known[payload == known | startsWith(payload, paste0(known, "; "))]
        if (!length(matched)) break
        payload <- substring(payload, nchar(matched[[1]]) + 1L)
        if (startsWith(payload, "; ")) payload <- substring(payload, 3L)
      }
      payload
    }, character(1))
    terminal_messages <- unlist(lapply(substring(remaining[terminal], nchar(terminal_prefix) + 1L), function(payload) {
      parts <- strsplit(payload, ";[ ]*", perl = TRUE)[[1]]
      if (all(!is.na(vapply(parts, desktop_nca_diagnostic_label, character(1))))) parts else payload
    }), use.names = FALSE)
    remaining <- remaining[!regular & !terminal]
    terminal_messages <- desktop_nca_expand_messages(terminal_messages)
    messages <- desktop_nca_expand_messages(c(messages, extra[nzchar(extra)], terminal_messages))
    if (!length(messages)) next
    identity <- paste(paste(names(group$identity), vapply(group$identity, function(x) {
      if (!length(x) || is.na(x[[1]])) "<欠測>" else as.character(x[[1]])
    }, character(1)), sep = "="), collapse = " / ")
    if (!nzchar(identity)) identity <- paste0("解析単位 ", group$profile)
    parameter <- grepl("^[A-Z][A-Z0-9_]*: ", messages)
    causes <- messages
    causes[parameter] <- sub("^[A-Z][A-Z0-9_]*: ", "", messages[parameter])
    selected <- rep(TRUE, nrow(value$data))
    if (length(keys) == length(group$identity) && all(keys %in% names(value$data))) {
      for (i in seq_along(keys)) {
        column <- as.character(value$data[[keys[i]]]); target <- as.character(group$identity[[i]])
        selected <- selected & if (is.na(target)) is.na(column) else !is.na(column) & column == target
      }
    } else selected[] <- FALSE
    for (cause in unique(causes)) {
      parameters <- sub(": .*", "", messages[parameter & causes == cause])
      # Derived parameter failures record their reason separately from their name.
      matching <- vapply(value$data$NCA_EXCLUDE, function(reason) !is.na(reason) && cause %in% desktop_nca_reason_parts(reason), logical(1))
      parameters <- unique(c(parameters, as.character(value$data$PARAMCD[selected & matching])))
      context <- unique(c(parameters, if (cause %in% terminal_messages) "終末相"))
      result <- c(result, paste0(identity, "：", if (length(context)) paste0(paste(context, collapse = "・"), "：") else "", desktop_nca_warning_text(cause)))
    }
  }
  c(result, vapply(unique(remaining), desktop_nca_warning_text, character(1), USE.NAMES = FALSE))
}

desktop_nca_diagnostics <- function(x, bilingual = TRUE) {
  if (!length(x)) return(NULL)
  shiny::tagList(lapply(as.character(x), function(original) {
    if (!is.na(original) && grepl("^([A-Z][A-Z0-9_]*: )?[a-z][A-Za-z0-9_]*(;[ ]*([A-Z][A-Z0-9_]*: )?[a-z][A-Za-z0-9_]*)+$", original))
      return(desktop_nca_diagnostics(strsplit(original, ";[ ]*", perl = TRUE)[[1L]], bilingual = bilingual))
    if (!is.na(original) && grepl("^[A-Z][A-Z0-9_]*: [a-z][A-Za-z0-9_]*$", original)) {
      parameter <- sub(": .*", "", original)
      cause <- sub("^[A-Z][A-Z0-9_]*: ", "", original)
      return(shiny::div(parameter, if (bilingual) desktop_i18n_text("：", ": ") else "：", desktop_nca_diagnostics(cause, bilingual = bilingual)))
    }
    # Split machine reason codes before localization; preserve natural-language
    # messages (which can themselves contain semicolons) as a single item.
    codes <- desktop_nca_reason_parts(original)
    if (length(codes) > 1L) return(desktop_nca_diagnostics(codes, bilingual = bilingual))
    parts <- strsplit(original, ";[ ]*", perl = TRUE)[[1]]
    # A complete diagnostic may itself contain a semicolon; keep its full
    # meaning when it has a specific translation before splitting fallback parts.
    translated <- desktop_nca_diagnostic_label(original)
    if (!is.na(translated)) return(shiny::div(class = "pk-diagnostic-message", if (bilingual) desktop_i18n_diagnostic(original, translated) else translated))
    if (bilingual && grepl("[ぁ-んァ-ン一-龠]", original)) {
      known <- desktop_i18n_diagnostic(original, original)
      if (!startsWith(known$attribs$`data-pk-en`, "Untranslated diagnostic;"))
        return(shiny::div(class = "pk-diagnostic-message", known))
    }
    if (length(parts) > 1L && all(!is.na(vapply(parts, desktop_nca_diagnostic_label, character(1)))))
      return(desktop_nca_diagnostics(parts, bilingual = bilingual))
    if (length(parts) > 1L && any(!is.na(vapply(parts, desktop_nca_diagnostic_label, character(1)))))
      return(desktop_nca_diagnostics(parts, bilingual = bilingual))
    shiny::tags$details(class = "pk-original-diagnostic",
      shiny::tags$summary(desktop_i18n_text("未翻訳の診断があります（原文を確認）", "Untranslated diagnostic; review the original")), shiny::tags$pre(original))
  }))
}

desktop_nca_lloq_origin <- function(source, prepared = FALSE) {
  if (is.null(source) || is.na(source) || !nzchar(source)) return("参照元なし")
  if (identical(source, "qc_supplement:constant")) return("QC専用の補完定数")
  if (startsWith(source, "qc_supplement:column:"))
    return(paste0("QC専用の補完列：", substring(source, nchar("qc_supplement:column:") + 1L)))
  prefix <- if (prepared) "保存済みの前処理の条件の" else "指定した"
  if (identical(source, "constant")) return(paste0(prefix, "定数"))
  if (startsWith(source, "column:")) return(paste0(prefix, "列：", substring(source, 8L)))
  desktop_nca_diagnostics(source)
}

desktop_status_badge <- function(token, bilingual = FALSE) {
  token <- as.character(token)
  status_tones <- c(ok = "is-success", pass = "is-success", pass_blq = "is-success", exceeds = "is-qc-exceeds",
    not_evaluable = "is-qc-unavailable", disabled = "is-qc-disabled")
  tone <- if (!is.na(token) && token %in% names(status_tones)) unname(status_tones[token]) else if (!is.na(token) &&
    grepl("exclude|missing|mdv|partial|review|not_calculable", token)) "is-warning" else if (!is.na(token) &&
    grepl("zero|half_lloq|policy_assigned", token)) "is-replaced" else ""
  label <- desktop_rule_label(token)
  shiny::span(class = paste("pk-rule-label", tone), if (bilingual) desktop_i18n_diagnostic(token, label) else label)
}

desktop_display_table <- function(data, bilingual_system_columns = character(), diagnostic_parts = list()) {
  badge_columns <- c("NCA_BLQ_RULE", "NCA_BLQ_POSITION", "BLQ_RULE", "BLQ_POSITION", "RULE", "POSITION", "ACTION", "PROFILE_STATUS", "PARAM_STATUS", "PREDOSE_QC_STATUS", "STATUS")
  shiny::tags$table(class = "table table-striped table-sm",
    shiny::tags$thead(shiny::tags$tr(lapply(names(data), shiny::tags$th))),
    shiny::tags$tbody(lapply(seq_len(nrow(data)), function(i) shiny::tags$tr(lapply(names(data), function(column) {
      value <- data[[column]][i]
      if (column %in% badge_columns) {
        shiny::tags$td(desktop_status_badge(value, bilingual = column %in% bilingual_system_columns))
      } else if (column %in% c("NCA_WARNING", "NCA_EXCLUDE", "PREDOSE_QC_REASON", "REASON")) {
        parts <- diagnostic_parts[[column]]
        reason <- if (!is.null(parts) && length(parts) >= i) parts[[i]] else as.character(value)
        shiny::tags$td(class = "pk-detail-text", desktop_nca_diagnostics(reason, bilingual = column %in% bilingual_system_columns))
      } else if (endsWith(column, "_BASELINE_STATUS")) shiny::tags$td(desktop_rule_label(as.character(value)))
      else if (column == "PREDOSE_QC_LLOQ_SOURCE") shiny::tags$td(desktop_nca_lloq_origin(as.character(value)))
      else shiny::tags$td(desktop_display_value(value))
    })))))
}
