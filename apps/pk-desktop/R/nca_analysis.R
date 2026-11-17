desktop_pk_flag <- function(x, label, convention = "strict", active = rep(TRUE, length(x))) {
  pk_assert(convention %in% c("strict", "y_blank") && length(convention) == 1L, "フラグ形式を確認してください。")
  value <- rep(FALSE, length(x))
  ignored <- toupper(trimws(as.character(x[!active])))
  numeric_ignored <- desktop_pk_numeric(ignored)$value
  value[!active] <- !is.na(ignored) & (ignored %in% c("Y", "TRUE") | (!is.na(numeric_ignored) & numeric_ignored == 1))
  token <- as.character(x[active])
  if (convention == "y_blank") token[is.na(token) | trimws(token) == ""] <- "N"
  if (any(active)) value[active] <- pk_flag(token, label)
  value
}

desktop_pk_numeric <- function(x) {
  token <- trimws(as.character(x))
  missing <- is.na(token) | token %in% c("", "NA")
  syntax <- !missing & grepl("^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$", token, perl = TRUE)
  number <- rep(NA_real_, length(x))
  number[syntax] <- suppressWarnings(as.numeric(token[syntax]))
  error <- ifelse(missing, "missing", ifelse(syntax, "", "non_numeric"))
  error[syntax & !is.finite(number)] <- "non_finite"
  nonzero <- !missing & grepl("[1-9]", sub("[eE].*$", "", token))
  error[syntax & nonzero & !is.na(number) & number == 0] <- "underflow"
  valid <- error == ""
  if (any(valid)) number[valid] <- pk_number(token[valid], "数値")
  number[!valid] <- NA_real_
  list(value = number, error = error)
}

desktop_pk_group <- function(data, columns) {
  identities <- data[columns]
  identities[] <- lapply(identities, as.character)
  pk_assert(all(vapply(identities, function(x) !anyNA(x) && all(nzchar(trimws(x))), logical(1))), "ID・グループ列に欠測があります。")
  key <- do.call(paste0, lapply(identities, function(x) paste0(nchar(enc2utf8(x), type = "bytes"), ":", enc2utf8(x))))
  list(identities = identities, group = match(key, unique(key)))
}

desktop_nca_specification <- function(data, specification) {
  defaults <- list(id = NULL, time = NULL, concentration = NULL, groups = list(), time_unit = "h",
    concentration_unit = "", unit_column = "", blq_column = "", blq = "exclude", mdv_column = "",
    flag_convention = "strict", scope = "all", time_origin = 0, start = 0, end = NULL,
    c0_method = "none", predose_window_h = NULL, include_aucproc = FALSE,
    all_blq_policy = "missing", predose_qc_threshold_pct = NULL, predose_qc_window_h = NULL,
    predose_qc_lloq_source = "", predose_qc_lloq_column = "", predose_qc_lloq_value = NULL,
    auc_method = "lin up/log down", pknca_version = NULL)
  pk_assert(is.list(specification) && !is.null(names(specification)) && !anyDuplicated(names(specification)) &&
    all(names(specification) %in% c(names(defaults), names(desktop_nca_terminal_defaults()), "dose", "body_size", "derived_parameters", "group_roles", "period_order")), "NCAの設定項目を確認してください。")
  s <- c(defaults, desktop_nca_terminal_specification(specification))
  for (name in intersect(names(specification), names(defaults))) s[name] <- specification[name]
  scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  for (name in c("id", "time", "concentration"))
    pk_assert(scalar(s[[name]]) && s[[name]] %in% names(data), paste("列を選択してください：", name))
  pk_assert(length(unique(unlist(s[c("id", "time", "concentration")], use.names = FALSE))) == 3L, "ID・時間・濃度には別の列を選んでください。")
  groups <- unlist(s$groups, use.names = FALSE) %||% character()
  pk_assert(is.character(groups) && !anyNA(groups) && !anyDuplicated(groups) && all(groups %in% names(data)) &&
    !any(groups %in% unlist(s[c("id", "time", "concentration")], use.names = FALSE)), "追加グループ列を確認してください。")
  s$groups <- as.list(groups)
  if (!is.null(specification$group_roles)) {
    roles <- specification$group_roles
    if (!is.list(roles) || !is.null(names(roles))) desktop_public_validation("グループ列の役割は配列で指定してください。")
    ok <- vapply(roles, function(x) is.list(x) && setequal(names(x), c("column", "role")) &&
      scalar(x$column) && x$column %in% groups && scalar(x$role) &&
      x$role %in% c("period", "analyte", "specimen", "treatment", "study", "other"), logical(1))
    if (!all(ok)) desktop_public_validation("役割は追加グループ列のみに指定し、役割名を確認してください。")
    columns <- vapply(roles, `[[`, "", "column")
    if (anyDuplicated(columns)) desktop_public_validation("同じグループ列に複数の役割は指定できません。")
    s$group_roles <- roles[order(match(columns, groups))]
  }
  if (!is.null(specification$period_order)) {
    order <- specification$period_order
    if (!(is.list(order) && !anyDuplicated(names(order)) && setequal(names(order), c("column", "values")) && scalar(order$column) &&
      is.list(order$values) && is.null(names(order$values)) && length(order$values) > 0L && length(order$values) <= 100L &&
      all(vapply(order$values, function(v) scalar(v) && nzchar(trimws(v)), logical(1)))))
      desktop_public_validation("期の順序は、期の列名と重複のない値の配列で指定してください。")
    roles <- s$group_roles %||% list()
    period_columns <- vapply(Filter(function(x) identical(x$role, "period"), roles), `[[`, "", "column")
    vals <- unlist(order$values, use.names = FALSE)
    actual <- unique(as.character(data[[order$column]]))
    if (!(length(period_columns) == 1L && identical(order$column, period_columns[[1]]) && !anyDuplicated(vals) &&
        !anyNA(actual) && all(nzchar(trimws(actual))) && length(vals) == length(actual) && setequal(vals, actual)))
      desktop_public_validation("期の順序は、期役割の1列にある値を漏れ・重複なく指定してください。")
    s$period_order <- list(column = order$column, values = as.list(vals))
  }
  for (name in c("unit_column", "blq_column", "mdv_column", "predose_qc_lloq_column")) pk_assert(scalar(s[[name]]) &&
    (!nzchar(s[[name]]) || s[[name]] %in% names(data)), paste("列を確認してください：", name))
  choices <- list(predose_qc_lloq_source = c("", "column", "constant"), time_unit = c("h", "min", "day"), blq = c("exclude", "zero", "as_recorded"),
    flag_convention = c("strict", "y_blank"), scope = c("all", "ss"),
    all_blq_policy = c("missing", "zero"), c0_method = c("none", "zero", "predose"), auc_method = c("linear", "lin up/log down"))
  for (name in names(choices)) pk_assert(scalar(s[[name]]) && s[[name]] %in% choices[[name]], paste("設定を確認してください：", name))
  pk_assert(scalar(s$concentration_unit), "濃度単位を指定してください。")
  pk_assert(is.null(s$predose_qc_lloq_value) || (is.atomic(s$predose_qc_lloq_value) && length(s$predose_qc_lloq_value) == 1L), "投与前QCのLLOQ定数は単一の値です。")
  if (s$predose_qc_lloq_source == "column") {
    pk_assert(nzchar(s$predose_qc_lloq_column) && is.null(s$predose_qc_lloq_value), "投与前QCのLLOQ列を選択してください。")
  } else {
    pk_assert(!nzchar(s$predose_qc_lloq_column) && (nzchar(s$predose_qc_lloq_source) || is.null(s$predose_qc_lloq_value)),
      "投与前QCのLLOQ取得方法と列・定数の設定を確認してください。")
  }
  finite_scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  pk_assert(finite_scalar(s$time_origin), "投与基準時刻は有限な数値（h）です。")
  pk_assert(finite_scalar(s$start) && s$start >= s$time_origin, "解析開始は投与基準時刻以降の時間（h）です。")
  pk_assert(is.null(s$end) || (finite_scalar(s$end) && s$end > s$start), "解析終了は開始より後の時間（h）です。")
  pk_assert(is.null(s$predose_window_h) || (finite_scalar(s$predose_window_h) && s$predose_window_h > 0), "投与前値の最大遡及時間は正の時間（h）です。")
  pk_assert(s$c0_method != "predose" || !is.null(s$predose_window_h), "投与前値を使う場合は最大遡及時間を指定してください。")
  for (name in c("predose_qc_threshold_pct", "predose_qc_window_h"))
    pk_assert(is.null(s[[name]]) || (finite_scalar(s[[name]]) && s[[name]] > 0), "投与前QCの閾値・遡及時間は正の有限数です。")
  pk_assert(is.null(s$predose_qc_threshold_pct) || !is.null(s$predose_qc_window_h), "投与前QCには最大遡及時間を指定してください。")
  pk_assert(is.logical(s$include_aucproc) && length(s$include_aucproc) == 1L && !is.na(s$include_aucproc), "処理後AUCの出力指定はTRUE/FALSEです。")
  for (name in c("time_origin", "start", "end", "predose_window_h", "predose_qc_threshold_pct", "predose_qc_window_h")) if (!is.null(s[[name]])) s[[name]] <- as.numeric(s[[name]])
  quartet <- c("NCA_CONC", "NCA_BLQ", "NCA_MDV", "NCA_BLQ_RULE")
  protected_flags <- any(c(s$blq_column, s$mdv_column) %in% c("NCA_BLQ", "NCA_MDV"))
  pk_assert(!protected_flags || identical(s$concentration, "NCA_CONC"),
    "前処理済みフラグNCA_BLQ・NCA_MDVはNCA_CONCと組み合わせてください。改名した処理列は元データからBLQ前処理を再適用してください。")
  if ("NCA_BLQ_CONTEXT" %in% names(data)) {
    context <- desktop_pk_blq_context(data)
    if (!identical(s$concentration, "NCA_CONC"))
      pk_assert(identical(s$concentration, context$step$concentration) && identical(s$blq_column, context$step$blq_column) &&
        identical(s$mdv_column, context$step$mdv_column %||% ""),
        "BLQ前処理の条件記録と濃度・フラグ列が一致しません。元の濃度・BLQ・MDV列を選択するか、元データからBLQ前処理を再適用してください。")
  }
  if (identical(s$concentration, "NCA_CONC") && all(quartet %in% names(data)))
    pk_assert(identical(s$blq_column, "NCA_BLQ") && identical(s$mdv_column, "NCA_MDV") && identical(s$blq, "as_recorded"),
      "前処理済みNCA_CONCはNCA_BLQ・NCA_MDVと記録値使用で解析してください。BLQ規則は前処理で変更してください。")
  if (identical(s$concentration, "NCA_CONC") && any(c(quartet[-1], "NCA_BLQ_POSITION", "NCA_BLQ_CONTEXT", "NCA_BLQ_PROFILE_SIGNATURE", "NCA_BLQ_BASELINE") %in% names(data)))
    desktop_pk_blq_validate(data, s)
  desktop_assembly_validate_nca(data, s)
  desktop_covariate_context(data, c(s$id, unlist(s$groups, use.names = FALSE)))
  s <- c(s, desktop_nca_derived_specification(data, specification))
  version <- as.character(utils::packageVersion("PKNCA"))
  pk_assert(is.null(s$pknca_version) || identical(s$pknca_version, version), "保存時とPKNCAの版が異なります。保存時の版で再実行してください。")
  s$pknca_version <- version
  s
}

desktop_nca_area <- function(time, concentration, start, end, options) {
  if (!is.finite(end) || end <= start) return(list(value = NA_real_, reason = "No quantifiable endpoint after the interval start"))
  keep <- time >= start & time <= end
  time <- time[keep]; concentration <- concentration[keep]
  if (!length(time) || !any(time == start)) return(list(value = NA_real_, reason = "Missing concentration at the requested start; select an explicit C0 policy when starting at the dose reference"))
  order <- order(time)
  value <- suppressWarnings(PKNCA::pk.calc.auc.last(concentration[order], time[order], interval = c(start, end), options = options))
  reason <- attr(value, "exclude") %||% ""
  if (!is.na(value) && !is.finite(value)) return(list(value = NA_real_, reason = "Numeric overflow: the calculated value exceeds the supported range"))
  list(value = as.numeric(value), reason = if (is.na(value) && !nzchar(reason)) "PKNCA returned a missing AUC" else reason)
}

desktop_nca_predose_qc <- function(evidence, cmax, threshold_pct, window_h, lloq_source = "") {
  qc <- list(status = "disabled", source_row = NA_integer_, time_h = NA_real_, concentration = NA_real_,
    cmax = cmax, pct = NA_real_, threshold_pct = threshold_pct %||% NA_real_,
    reason = "", lloq = NA_real_, pct_upper_bound = NA_real_, lloq_source = "")
  if (is.null(threshold_pct)) return(qc)
  qc$status <- "not_evaluable"
  candidates <- evidence[evidence$time_h < 0 & evidence$time_h >= -window_h, , drop = FALSE]
  qc$reason <- "no_candidate"
  if (!nrow(candidates)) return(qc)
  latest <- candidates[candidates$time_h == max(candidates$time_h), , drop = FALSE]
  qc$reason <- "ambiguous_latest"
  if (nrow(latest) != 1L) return(qc)
  qc$source_row <- latest$row; qc$time_h <- latest$time_h
  if (latest$blq) {
    qc$lloq <- latest$lloq; qc$lloq_source <- latest$lloq_source %||% lloq_source
  } else {
    qc$reason <- "invalid_concentration"
    if (!latest$concentration_valid) return(qc)
    qc$concentration <- latest$concentration
  }
  qc$reason <- latest$unit_reason
  if (nzchar(qc$reason)) return(qc)
  if (latest$blq) {
    qc$reason <- latest$lloq_reason
    if (nzchar(qc$reason)) return(qc)
  }
  qc$reason <- "invalid_cmax"
  if (!is.finite(cmax) || cmax <= 0) return(qc)
  numerator <- if (latest$blq) latest$lloq else latest$concentration
  pct <- if (numerator <= .Machine$double.xmax / 100) (100 * numerator) / cmax else (numerator / cmax) * 100
  qc$reason <- "numeric_range"
  if (!is.finite(pct) || (numerator > 0 && pct == 0)) {
    qc$concentration <- NA_real_
    return(qc)
  }
  if (latest$blq) {
    qc$reason <- "bound_above_threshold"
    qc$pct_upper_bound <- pct
    if (pct > threshold_pct) return(qc)
    qc$status <- "pass_blq"
  } else {
    qc$pct <- pct; qc$status <- if (pct > threshold_pct) "exceeds" else "pass"
  }
  qc$reason <- ""
  qc
}

pkapp_nca <- function(data, specification) {
  pkapp_table_check(data)
  pk_assert(nrow(data) > 0L, "入力データが空です。")
  pk_assert(requireNamespace("PKNCA", quietly = TRUE), "PKNCAをinstall.Rで導入してください。")
  s <- desktop_nca_specification(data, specification)
  keys <- c(s$id, unlist(s$groups, use.names = FALSE))
  identity <- desktop_pk_group(data, keys)
  n <- nrow(data)
  scope <- rep(TRUE, n)
  if (s$scope == "ss") {
    pk_assert("SSNCAFL" %in% names(data), "SSNCAFL列がありません。")
    scope <- desktop_pk_flag(data$SSNCAFL, "SSNCAFL", s$flag_convention)
  }
  mdv <- if (nzchar(s$mdv_column)) desktop_pk_flag(data[[s$mdv_column]], s$mdv_column, s$flag_convention, scope) else rep(FALSE, n)
  blq <- if (nzchar(s$blq_column)) desktop_pk_flag(data[[s$blq_column]], s$blq_column, s$flag_convention, scope & !mdv) else rep(FALSE, n)
  parsed_time <- desktop_pk_numeric(data[[s$time]])
  source_time <- parsed_time$value * switch(s$time_unit, h = 1, min = 1/60, day = 24)
  parsed_time$error[!is.na(source_time) & !is.finite(source_time)] <- "non_finite"
  source_time[parsed_time$error != ""] <- NA_real_
  processed_text <- as.character(data[[s$concentration]])
  raw_conc <- if (identical(s$concentration, "NCA_CONC") && "NCA_ORIGINAL_CONC" %in% names(data)) as.character(data$NCA_ORIGINAL_CONC) else processed_text
  if (s$blq == "zero") processed_text[blq] <- "0"
  parsed_conc <- desktop_pk_numeric(processed_text)
  concentration <- parsed_conc$value
  parsed_conc$error[!is.na(concentration) & concentration < 0] <- "negative"
  eligible <- scope & !mdv & !(blq & s$blq == "exclude")
  reason <- ifelse(!scope, "scope", ifelse(mdv, "MDV", ifelse(blq & s$blq == "exclude", "BLQ", "")))
  reason[eligible & parsed_time$error == "missing"] <- "missing_time"
  reason[eligible & parsed_conc$error == "missing" & reason == ""] <- "missing_concentration"
  reason[eligible & !parsed_time$error %in% c("", "missing")] <- paste0("invalid_time_", parsed_time$error[eligible & !parsed_time$error %in% c("", "missing")])
  bad_conc <- eligible & !parsed_conc$error %in% c("", "missing")
  reason[bad_conc] <- paste0("invalid_concentration_", parsed_conc$error[bad_conc])
  prepared <- identical(s$concentration, "NCA_CONC") && "NCA_BLQ_CONTEXT" %in% names(data)
  original_mdv <- if (prepared) desktop_pk_flag(data$NCA_ORIGINAL_MDV, "NCA_ORIGINAL_MDV", "strict", scope) else mdv
  lloq <- if (prepared) desktop_pk_blq_lloq(data) else
    desktop_pk_lloq(data, s$predose_qc_lloq_source, s$predose_qc_lloq_column, s$predose_qc_lloq_value)
  lloq$source <- rep(lloq$source, n)
  if (prepared && nzchar(s$predose_qc_lloq_source)) {
    supplement <- desktop_pk_lloq(data, s$predose_qc_lloq_source, s$predose_qc_lloq_column, s$predose_qc_lloq_value)
    missing <- lloq$reason == "missing_lloq"
    lloq$value[missing] <- supplement$value[missing]
    lloq$reason[missing] <- supplement$reason[missing]
    lloq$source[missing] <- paste0("qc_supplement:", supplement$source)
  }
  if (prepared) {
    prep_reason <- as.character(data$NCA_BLQ_RULE)
    recover <- scope & mdv & !is.na(prep_reason) & prep_reason %in% c("missing_time", "missing_concentration", "blq_excluded", "existing_mdv")
    reason[recover] <- prep_reason[recover]
  }
  missing_time <- scope & !original_mdv & parsed_time$error == "missing"
  original_conc <- desktop_pk_numeric(raw_conc)
  missing_concentration <- scope & !original_mdv & !blq & original_conc$error == "missing"
  valid <- eligible & reason == ""
  quantifiable <- valid & !blq & concentration > 0
  unit_rows <- if (any(valid)) valid else scope & !original_mdv
  if (s$all_blq_policy == "zero") {
    for (g in unique(identity$group)) {
      originals <- which(identity$group == g & scope & !original_mdv)
      relevant <- originals[!is.na(source_time[originals]) & source_time[originals] >= s$start &
        (is.null(s$end) | source_time[originals] <= (s$end %||% Inf))]
      if (length(relevant) && all(blq[relevant]) && all(parsed_time$error[originals] == "")) unit_rows[relevant] <- TRUE
    }
  }
  if (nzchar(s$unit_column)) {
    units <- trimws(as.character(data[[s$unit_column]][unit_rows]))
    pk_assert(!anyNA(units) && all(nzchar(units)) && length(unique(desktop_nca_unit_key(units))) <= 1L, "使用する濃度の単位が欠測または混在しています。単位ごとに分けてください。")
    unit <- if (length(units)) desktop_nca_unit_key(units)[1] else trimws(s$concentration_unit)
  } else unit <- trimws(s$concentration_unit)
  pk_assert(nzchar(unit) || !any(valid), "濃度単位を入力するか単位列を選択してください。")
  options <- list(auc.method = s$auc_method, conc.blq = list(first = "keep", middle = "keep", last = "keep"), conc.na = "drop", first.tmax = TRUE, progress = FALSE)
  audit <- data.frame(ROW = seq_len(n), SOURCE_ROW = seq_len(n), PROFILE = identity$group,
    SOURCE_TIME_H = source_time, TIME_H = source_time - s$time_origin,
    CONC_ORIGINAL = raw_conc, CONC_USED = concentration, BLQ = blq, MDV = mdv,
    IMPUTED = FALSE, QUANTIFIABLE = quantifiable,
    BLQ_POSITION = if (identical(s$concentration, "NCA_CONC") && "NCA_BLQ_POSITION" %in% names(data)) as.character(data$NCA_BLQ_POSITION) else ifelse(blq, "flagged", "not_blq"),
    BLQ_RULE = if (identical(s$concentration, "NCA_CONC") && "NCA_BLQ_RULE" %in% names(data)) as.character(data$NCA_BLQ_RULE) else ifelse(blq, s$blq, "recorded"),
    USE_AUC = FALSE, USE_OBS = FALSE, USE_AUCPROC = FALSE,
    MISSING_TIME = missing_time, MISSING_CONCENTRATION = missing_concentration,
    STATUS = reason, REASON = reason, stringsAsFactors = FALSE)
  audit_reserved <- names(audit)
  params <- c(CMAX = "Max Conc", TMAX = "Time of CMAX", AUCLST = "AUC to Last Nonzero Conc")
  if (s$start != s$time_origin) params <- c(params, AUCPART = "AUC from Selected Start to Tlast")
  if (s$include_aucproc) params <- c(params, AUCPROC = "AUC of Processed Profile")
  reserved <- c(audit_reserved, "PARAMCD", "PARAM", "AVAL", "AVALU", "NCA_START_H", "NCA_END_H", "FIRST_OBS_H", "LAST_OBS_H", "TLAST_QUANT_H", "AUC_LAST_H", "PROC_LAST_H", "TIME_ORIGIN_H", "INPUT_START_H", "INPUT_END_H", "NCA_METHOD", "PKNCA_VERSION", "NCA_EXCLUDE", "PROFILE_STATUS")
  reserved <- c(reserved, names(params), "PARAM_STATUS", "POLICY_ASSIGNED", "NCA_WARNING", "N_INPUT", "N_MISSING_TIME", "N_MISSING_CONCENTRATION", "N_EXCLUDED",
    "PREDOSE_QC_STATUS", "PREDOSE_QC_SOURCE_ROW", "PREDOSE_QC_TIME_H", "PREDOSE_QC_CONC", "PREDOSE_QC_CMAX", "PREDOSE_QC_PCT", "PREDOSE_QC_THRESHOLD_PCT",
    "PREDOSE_QC_REASON", "PREDOSE_QC_LLOQ", "PREDOSE_QC_PCT_UPPER_BOUND", "PREDOSE_QC_LLOQ_SOURCE")
  if (!is.null(s$terminal_method) && s$terminal_method != "none")
    reserved <- c(reserved, "LAMZ", "LAMZHL", "AUCIFO", "AUCPEO", "TERMINAL_ELIGIBLE", "USE_TERMINAL", "TERMINAL_REASON")
  output_keys <- keys
  for (i in seq_along(keys)) {
    candidate <- keys[i]
    while (candidate %in% c(reserved, output_keys[-i])) candidate <- paste0("SOURCE_", candidate)
    output_keys[i] <- candidate
  }
  audit_identity <- identity$identities; names(audit_identity) <- output_keys
  audit <- cbind(audit_identity, audit)
  rows <- list(); summaries <- list(); warnings <- character()
  start <- s$start - s$time_origin
  for (g in unique(identity$group)) {
    all <- which(identity$group == g)
    actual <- all[valid[all] & source_time[all] >= s$start & (is.null(s$end) | source_time[all] <= (s$end %||% Inf))]
    end_input <- s$end %||% if (length(actual)) max(source_time[actual]) else NA_real_
    end <- end_input - s$time_origin
    observed <- actual[!blq[actual]]
    quantified <- actual[quantifiable[actual]]
    last_quant <- if (length(quantified)) max(source_time[quantified]) - s$time_origin else NA_real_
    proc_last <- if (any(concentration[actual] > 0)) max(source_time[actual][concentration[actual] > 0]) - s$time_origin else NA_real_
    errors <- rep("", length(params)); names(errors) <- names(params)
    values <- rep(NA_real_, length(params)); names(values) <- names(params)
    parameter_status <- stats::setNames(rep("not_calculable", length(params)), names(params))
    profile_warning <- character()
    n_missing_time <- sum(missing_time[all]); n_missing_concentration <- sum(missing_concentration[all])
    if (n_missing_time) profile_warning <- c(profile_warning, paste(n_missing_time, "source rows excluded for missing time; profile requires review"))
    if (n_missing_concentration) profile_warning <- c(profile_warning, paste(n_missing_concentration, "source rows excluded for missing concentration; profile requires review"))
    original_rows <- all[scope[all] & !original_mdv[all]]
    relevant <- original_rows[!is.na(source_time[original_rows]) & source_time[original_rows] >= s$start &
      (is.null(s$end) | source_time[original_rows] <= (s$end %||% Inf))]
    all_blq <- length(relevant) > 0L && all(blq[relevant]) && all(parsed_time$error[original_rows] == "")
    dose_review <- desktop_assembly_profile_decision(data, all, s, source_time, scope, original_mdv)
    profile_error <- dose_review$reason
    invalid <- all[eligible[all] & (grepl("^invalid_", reason[all]))]
    if (!nzchar(profile_error) && length(invalid)) profile_error <- paste("Invalid numeric data in source rows", paste(invalid, collapse = ", "))
    if (!nzchar(profile_error) && (anyDuplicated(source_time[actual]) || (all_blq && anyDuplicated(source_time[relevant]))))
      profile_error <- "Duplicate observation times within the analysis profile"
    c0_index <- NA_integer_; c0_reason <- ""; synthetic <- NULL
    if (!nzchar(profile_error) && start == 0 && !any(source_time[actual] == s$time_origin) && s$c0_method != "none") {
      c0 <- NA_real_
      if (s$c0_method == "zero") c0 <- 0 else {
        candidates <- all[valid[all] & source_time[all] < s$time_origin & s$time_origin - source_time[all] <= s$predose_window_h]
        if (length(candidates)) {
          latest <- candidates[source_time[candidates] == max(source_time[candidates])]
          if (length(latest) == 1L) { c0_index <- latest; c0 <- concentration[latest] } else c0_reason <- "Ambiguous latest predose measurement"
        } else c0_reason <- "No eligible predose measurement within the specified lookback"
      }
      if (!is.na(c0)) {
        synthetic <- audit[all[1], , drop = FALSE]
        synthetic$ROW <- NA_integer_; synthetic$SOURCE_ROW <- c0_index
        synthetic$SOURCE_TIME_H <- if (is.na(c0_index)) NA_real_ else source_time[c0_index]
        synthetic$TIME_H <- 0; synthetic$CONC_ORIGINAL <- if (is.na(c0_index)) NA_character_ else raw_conc[c0_index]
        synthetic$CONC_USED <- c0; synthetic$BLQ <- if (is.na(c0_index)) FALSE else blq[c0_index]
        synthetic$MDV <- FALSE; synthetic$IMPUTED <- TRUE; synthetic$QUANTIFIABLE <- FALSE
        synthetic$MISSING_TIME <- FALSE; synthetic$MISSING_CONCENTRATION <- FALSE
        synthetic$STATUS <- paste0("imputed_c0_", s$c0_method); synthetic$REASON <- synthetic$STATUS
        synthetic$BLQ_POSITION <- "c0"; synthetic$BLQ_RULE <- paste0("c0_", s$c0_method)
      }
    }
    contributing <- actual[!is.na(last_quant) & source_time[actual] - s$time_origin <= last_quant]
    measured_rows <- unique(c(contributing, c0_index[!is.na(c0_index)]))
    area_error <- if (length(measured_rows) < 2L) "Fewer than two eligible original observations in the analysis profile" else ""
    profile_time <- source_time[actual] - s$time_origin
    profile_conc <- concentration[actual]
    if (!is.null(synthetic)) { profile_time <- c(profile_time, 0); profile_conc <- c(profile_conc, synthetic$CONC_USED) }
    if (!nzchar(profile_error)) {
      if (length(observed)) {
        values["CMAX"] <- max(concentration[observed])
        if (values["CMAX"] > 0) values["TMAX"] <- min(source_time[observed][concentration[observed] == values["CMAX"]]) - s$time_origin
        else errors["TMAX"] <- "All observed non-BLQ concentrations are zero; Tmax is undefined"
      } else errors[c("CMAX", "TMAX")] <- "No eligible non-BLQ observed concentrations"
      area <- if (nzchar(area_error)) list(value = NA_real_, reason = area_error) else desktop_nca_area(profile_time, profile_conc, start, last_quant, options)
      if (start == 0) { values["AUCLST"] <- area$value; errors["AUCLST"] <- area$reason }
      else { errors["AUCLST"] <- "AUCLST must start at the dose reference; the selected interval starts later"; values["AUCPART"] <- area$value; errors["AUCPART"] <- area$reason }
      if (nzchar(c0_reason) && is.na(values["AUCLST"]) && start == 0) errors["AUCLST"] <- c0_reason
      if (s$include_aucproc) {
        proc <- if (nzchar(area_error)) list(value = NA_real_, reason = area_error) else desktop_nca_area(profile_time, profile_conc, start, proc_last, options)
        values["AUCPROC"] <- proc$value; errors["AUCPROC"] <- proc$reason
      }
      audit$USE_OBS[observed] <- TRUE
      area_ok <- is.finite(area$value)
      processed_ok <- s$include_aucproc && is.finite(values["AUCPROC"])
      audit$USE_AUC[actual] <- area_ok & !is.na(last_quant) & profile_time[seq_along(actual)] <= last_quant
      audit$USE_AUCPROC[actual] <- processed_ok & !is.na(proc_last) & profile_time[seq_along(actual)] <= proc_last
      if (!is.null(synthetic)) {
        synthetic$USE_AUC <- area_ok && !is.na(last_quant) && last_quant > 0
        synthetic$USE_AUCPROC <- processed_ok && !is.na(proc_last) && proc_last > 0
      }
      parameter_status[is.finite(values)] <- "calculated"
      if (all_blq) {
        values[] <- NA_real_; errors[] <- "All original eligible observations are BLQ; no quantifiable endpoint or observed peak time"
        parameter_status[] <- "not_calculable"
        audit$USE_OBS[actual] <- FALSE; audit$USE_AUC[actual] <- FALSE; audit$USE_AUCPROC[actual] <- FALSE
        if (!is.null(synthetic)) { synthetic$USE_AUC <- FALSE; synthetic$USE_AUCPROC <- FALSE }
        if (s$all_blq_policy == "zero") {
          pk_assert(nzchar(unit), "全点BLQの0割当てにも濃度単位を入力するか単位列を選択してください。")
          assigned <- c("CMAX", if (start == 0) "AUCLST" else "AUCPART", if (s$include_aucproc) "AUCPROC")
          values[assigned] <- 0; parameter_status[assigned] <- "policy_assigned"; errors[assigned] <- ""
          profile_warning <- c(profile_warning, "All-BLQ zero policy assigned CMAX/AUC; Tmax and quantifiable Tlast remain undefined")
        }
      }
    } else errors[] <- profile_error
    qc_rows <- all[scope[all] & !original_mdv[all] & parsed_time$error[all] == ""]
    qc_unit <- rep(if (nzchar(unit)) "" else "missing_unit", length(qc_rows))
    if (nzchar(s$unit_column)) {
      selected_units <- trimws(as.character(data[[s$unit_column]][qc_rows]))
      qc_unit[is.na(selected_units) | !nzchar(selected_units)] <- "missing_unit"
      qc_unit[!is.na(selected_units) & nzchar(selected_units) & desktop_nca_unit_key(selected_units) != desktop_nca_unit_key(unit)] <- "unit_mismatch"
    }
    evidence <- data.frame(row = qc_rows, time_h = source_time[qc_rows] - s$time_origin,
      blq = blq[qc_rows], concentration = original_conc$value[qc_rows],
      concentration_valid = original_conc$error[qc_rows] == "" & !is.na(original_conc$value[qc_rows]) & original_conc$value[qc_rows] >= 0,
      lloq = lloq$value[qc_rows], lloq_reason = lloq$reason[qc_rows], lloq_source = lloq$source[qc_rows], unit_reason = qc_unit,
      stringsAsFactors = FALSE)
    qc <- desktop_nca_predose_qc(evidence, unname(values["CMAX"]), s$predose_qc_threshold_pct,
      s$predose_qc_window_h)
    if (qc$status == "exceeds") profile_warning <- c(profile_warning, "投与前QC：閾値超過")
    if (qc$status == "not_evaluable" && !nzchar(dose_review$reason)) profile_warning <- c(profile_warning, "投与前QC：評価不可")
    unavailable <- names(values)[is.na(values)]
    if (all_blq && s$all_blq_policy == "zero" && !nzchar(profile_error)) unavailable <- setdiff(unavailable, "TMAX")
    if (length(unavailable)) profile_warning <- c(profile_warning, paste0(unavailable, ": ", errors[unavailable]))
    audit$STATUS[all[reason[all] == ""]] <- "interval"
    audit$REASON[all[reason[all] == ""]] <- "outside_analysis_interval"
    unused_status <- if (is.na(last_quant)) rep("no_quantifiable", length(actual)) else ifelse(audit$TIME_H[actual] > last_quant, "after_tlast", "auc_not_calculated")
    unused_reason <- ifelse(unused_status == "no_quantifiable", "no_original_quantifiable_observation",
      ifelse(unused_status == "after_tlast", "not_used_after_last_quantifiable", errors[if (start == 0) "AUCLST" else "AUCPART"]))
    audit$STATUS[actual] <- if (nzchar(profile_error)) "profile_excluded" else ifelse(audit$USE_AUC[actual] | audit$USE_OBS[actual] | audit$USE_AUCPROC[actual], "used", unused_status)
    audit$REASON[actual] <- if (nzchar(profile_error)) profile_error else ifelse(audit$STATUS[actual] == "used", "", unused_reason)
    if (nzchar(dose_review$reason)) {
      audit$STATUS[dose_review$affected_rows$source_row] <- "profile_excluded"
      audit$REASON[dose_review$affected_rows$source_row] <- dose_review$reason
    }
    if (!is.na(c0_index) && !nzchar(profile_error) && !is.null(synthetic) && (synthetic$USE_AUC || synthetic$USE_AUCPROC)) {
      audit$STATUS[c0_index] <- "c0_source"; audit$REASON[c0_index] <- "Copied to dose reference for AUC only"
    }
    if (!is.null(synthetic)) {
      if (nzchar(profile_error)) { synthetic$USE_AUC <- FALSE; synthetic$USE_AUCPROC <- FALSE; synthetic$STATUS <- "profile_excluded"; synthetic$REASON <- profile_error }
      audit <- rbind(audit, synthetic)
    }
    used_rows <- all[audit$USE_AUC[all] | audit$USE_OBS[all] | audit$USE_AUCPROC[all] | audit$STATUS[all] == "c0_source"]
    n_excluded <- length(all) - length(used_rows)
    status <- if (nzchar(profile_error) || sum(!is.na(values)) == 0L) "excluded" else if (anyNA(values)) "partial" else if (length(profile_warning)) "review" else "ok"
    prefix <- identity$identities[rep(all[1], length(params)), , drop = FALSE]; names(prefix) <- output_keys
    result <- data.frame(PARAMCD = names(params), PARAM = unname(params), AVAL = unname(values),
      AVALU = ifelse(names(params) == "CMAX", if (nzchar(unit)) unit else NA_character_, ifelse(names(params) == "TMAX", "h", if (nzchar(unit)) paste0("h*", unit) else NA_character_)),
      NCA_START_H = start, NCA_END_H = end, FIRST_OBS_H = if (length(actual)) min(source_time[actual]) - s$time_origin else NA_real_,
      LAST_OBS_H = if (length(actual)) max(source_time[actual]) - s$time_origin else NA_real_,
      TLAST_QUANT_H = if (status == "excluded") NA_real_ else last_quant, AUC_LAST_H = last_quant, PROC_LAST_H = proc_last,
      TIME_ORIGIN_H = s$time_origin, INPUT_START_H = s$start, INPUT_END_H = end_input,
      NCA_METHOD = s$auc_method, PKNCA_VERSION = s$pknca_version, PROFILE_STATUS = status,
      NCA_EXCLUDE = ifelse(nzchar(errors), errors, NA_character_), PARAM_STATUS = unname(parameter_status),
      NCA_WARNING = if (length(profile_warning)) paste(profile_warning, collapse = "; ") else NA_character_,
      N_INPUT = length(all), N_MISSING_TIME = n_missing_time, N_MISSING_CONCENTRATION = n_missing_concentration, N_EXCLUDED = n_excluded,
      PREDOSE_QC_STATUS = qc$status, PREDOSE_QC_SOURCE_ROW = qc$source_row, PREDOSE_QC_TIME_H = qc$time_h,
      PREDOSE_QC_CONC = qc$concentration, PREDOSE_QC_CMAX = qc$cmax, PREDOSE_QC_PCT = qc$pct,
      PREDOSE_QC_THRESHOLD_PCT = qc$threshold_pct, PREDOSE_QC_REASON = qc$reason,
      PREDOSE_QC_LLOQ = qc$lloq, PREDOSE_QC_PCT_UPPER_BOUND = qc$pct_upper_bound,
      PREDOSE_QC_LLOQ_SOURCE = qc$lloq_source, stringsAsFactors = FALSE)
    rows[[length(rows) + 1L]] <- cbind(prefix, result)
    summaries[[length(summaries) + 1L]] <- list(profile = g, identity = as.list(identity$identities[all[1], , drop = FALSE]),
      status = status, reason = if (nzchar(profile_error)) profile_error else unique(errors[nzchar(errors)]),
      observed_rows = length(actual), c0_source_row = c0_index, imputed = !is.null(synthetic), start = start, end = end,
      tlast_quant = last_quant, processed_last = proc_last, all_blq = all_blq,
      observation_end_h = if (length(relevant)) max(source_time[relevant]) - s$time_origin else NULL,
      observation_time_complete = all(parsed_time$error[original_rows] == ""),
      n_input = length(all), n_missing_time = n_missing_time, n_missing_concentration = n_missing_concentration,
      n_excluded = n_excluded, warning = profile_warning, predose_qc = qc, dose_review = dose_review)
    if (length(profile_warning)) warnings <- c(warnings, paste0("Profile ", g, ": ", paste(profile_warning, collapse = "; ")))
  }
  result <- do.call(rbind, rows); rownames(result) <- NULL; rownames(audit) <- NULL
  failed <- sum(vapply(summaries, function(x) identical(x$status, "excluded"), logical(1)))
  if (failed) warnings <- c(warnings, paste(failed, "解析単位を除外しました。解析単位ごとの理由を確認してください。"))
  policy_tmax <- s$all_blq_policy == "zero" & result$PARAMCD == "TMAX" &
    result$NCA_EXCLUDE %in% "All original eligible observations are BLQ; no quantifiable endpoint or observed peak time"
  if (any(is.na(result$AVAL) & !policy_tmax)) warnings <- c(warnings, "計算できないパラメータがあります。NCA_EXCLUDEのパラメータ別の理由を確認してください。")
  missing_start <- is.na(result$AVAL) & result$PARAMCD %in% c("AUCLST", "AUCPART", "AUCPROC") &
    !is.na(result$NCA_EXCLUDE) & grepl("Missing concentration at the requested start|predose measurement", result$NCA_EXCLUDE)
  if (any(missing_start)) warnings <- c(warnings, "AUCの開始濃度がありません。投与基準時刻から解析する場合は明示的なC0補完設定と投与前値の条件を確認してください。")
  original_used <- audit$ROW[!is.na(audit$ROW) & (audit$USE_AUC | audit$USE_OBS | audit$USE_AUCPROC | audit$STATUS == "c0_source")]
  counts <- c(input = n, used = length(unique(original_used)), excluded = n - length(unique(original_used)),
    curves = length(rows), failed_curves = failed, imputed = sum(audit$IMPUTED), result = nrow(result))
  value <- list(data = result, counts = counts, specification = s, warnings = warnings,
    trace = list(specification = s, pknca_options = options, pknca_version = s$pknca_version,
      R = as.character(getRversion()), concentration_unit = unit, time_basis = "All TIME_H and result endpoint columns are hours since time_origin; SOURCE_TIME_H and INPUT_* retain the input axis",
      auc_definition = "AUCLST ends at the original valid non-BLQ positive observation; explicit substitutions cannot extend that endpoint",
      blq_processing = "Explicit flags and preparation policies applied once; PKNCA keep options prevent a second zero-based BLQ pass",
      output_key_mapping = stats::setNames(as.list(output_keys), keys), groups = summaries, rows = audit, warnings = warnings))
  value <- desktop_nca_derived_apply(desktop_nca_terminal_apply(value, data, s), data, s)
  value$warnings <- desktop_nca_public_warnings(value)
  value
}
