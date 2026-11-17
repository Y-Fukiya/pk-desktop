# Exploratory displays use saved NCA tables; no AI-generated R is evaluated.
desktop_explore_init <- function(engine) {
  engine$explore <- NULL
  engine$explore_revision <- 0L
  engine$explore_session <- desktop_random_token()
  engine$explore_signal <- shiny::reactiveVal(0L)
}

desktop_explore_get <- function(engine) {
  list(selection = engine$explore, explore_revision = engine$explore_revision, explore_session = engine$explore_session,
    views = as.list(c("summary", "boxplot", "listing", "qc")))
}

desktop_explore_context <- function(engine, source) {
  path <- desktop_table_path(engine, source)
  manifest <- desktop_read(file.path(path, "manifest.json"))
  desktop_check(manifest$kind %in% c("nca", "nca_annotation"), "E_VALIDATION", "保存済みのNCA結果または背景情報付き結果を選んでください。")
  data <- desktop_table_get(engine, source)
  # desktop_table_get verifies all manifest files before the frozen metadata is read.
  trace <- desktop_read(file.path(path, "trace.json"))
  mapping <- trace$output_key_mapping
  annotated_columns <- character()
  original <- if (identical(manifest$kind, "nca")) desktop_read(file.path(path, "recipe.json"))$specification else {
    frozen <- readRDS(file.path(path, "inputs.rds"))$result
    desktop_check(is.list(frozen) && identical(desktop_hash(frozen$trace$output_key_mapping), desktop_hash(mapping)),
      "E_INPUT_CHANGED", "背景情報付き結果の解析単位の対応が一致しません。")
    annotated_columns <- setdiff(names(data), names(frozen$data))
    frozen$specification
  }
  id <- original$id
  keys <- unique(c(id, unlist(original$groups, use.names = FALSE)))
  desktop_check(is.character(id) && length(id) == 1L && nzchar(id) && is.list(mapping) &&
    all(keys %in% names(mapping)), "E_VALIDATION", "NCAの解析単位情報がありません。")
  mapped <- unname(unlist(mapping[keys], use.names = FALSE))
  desktop_check(length(mapped) == length(keys) && !anyNA(mapped) && !anyDuplicated(mapped) && all(mapped %in% names(data)),
    "E_VALIDATION", "NCAの解析単位列が結果と一致しません。")
  desktop_check(all(c("PARAMCD", "AVAL", "AVALU") %in% names(data)) && is.numeric(data$AVAL),
    "E_VALIDATION", "保存結果にパラメータ・値・単位の列がありません。")
  desktop_check(!anyDuplicated(data[c(mapped, "PARAMCD", "AVALU")]),
    "E_VALIDATION", "NCAの解析単位とパラメータが重複しています。")
  roles <- original$group_roles %||% list()
  period_original <- vapply(Filter(function(x) identical(x$role, "period"), roles), `[[`, "", "column")
  period_mapped <- if (length(period_original)) unname(unlist(mapping[period_original], use.names = FALSE)) else character()
  treatment_original <- vapply(Filter(function(x) identical(x$role, "treatment"), roles), `[[`, "", "column")
  treatment_mapped <- if (length(treatment_original)) unname(unlist(mapping[treatment_original], use.names = FALSE)) else character()
  study_original <- vapply(Filter(function(x) identical(x$role, "study"), roles), `[[`, "", "column")
  study_mapped <- if (length(study_original)) unname(unlist(mapping[study_original], use.names = FALSE)) else character()
  desktop_check(all(period_mapped %in% mapped[-1]), "E_VALIDATION", "期の役割の記録が不正です。")
  desktop_check(all(treatment_mapped %in% mapped[-1]), "E_VALIDATION", "無効な処置役割の記録です。")
  desktop_check(all(study_mapped %in% mapped[-1]), "E_VALIDATION", "無効な試験役割の記録です。")
  local_time <- function(text) {
    instant <- tryCatch(as.POSIXct(strptime(text, "%Y-%m-%dT%H:%M:%OS", tz = "UTC")), error = function(e) as.POSIXct(NA))
    if (length(instant) != 1L || is.na(instant)) "日時不明" else format(instant, "%Y/%m/%d %H:%M %Z", tz = Sys.timezone() %||% "UTC")
  }
  saved_origin <- if ("role_provenance.json" %in% names(manifest$files)) desktop_read(file.path(path, "role_provenance.json")) else NULL
  if (!is.null(saved_origin)) desktop_check(identical(saved_origin$format, "pk-nca-role-provenance-1") &&
    identical(saved_origin$roles_hash, desktop_hash(roles)), "E_INPUT_CHANGED", "保存された役割と設定記録が一致しません。")
  event <- if (!is.null(saved_origin) && identical(saved_origin$origin$status, "matched")) saved_origin$origin else NULL
  if (is.null(saved_origin)) {
    execution <- manifest$provenance$execution %||% list()
    audit <- (engine$nca %||% list())$audit %||% list()
    matching <- Filter(function(item) identical(item$action, "configure") && !is.null(execution$revision) &&
      as.numeric(item$revision) <= as.numeric(execution$revision) &&
      identical(desktop_hash(item$specification), desktop_hash(original)), audit)
    if (length(matching)) event <- tail(matching, 1L)[[1]]
  }
  role_origin <- if (!is.null(event)) paste0("設定時の申告者: ", event$actor %||% "不明", " / 経路: ", event$channel %||% "不明", " / 日時: ", local_time(event$at), "（本人認証ではありません）") else "役割設定者の変更履歴を照合できません。保存時の条件を確認してください。"
  manual_order <- original$period_order %||% NULL
  if (!is.null(manual_order)) desktop_check(length(period_original) == 1L && identical(manual_order$column, period_original[[1L]]), "E_VALIDATION", "保存された期順の設定を確認してください。")
  list(data = data, id = mapped[[1]], strata = mapped[-1], period = period_mapped, treatment = treatment_mapped, study = study_mapped, roles = roles, manual_order = manual_order,
    role_origin = role_origin, annotated = annotated_columns, name = manifest$name)
}
desktop_explore_table <- function(engine, source) desktop_explore_context(engine, source)$data
desktop_explore_group_candidates <- function(context) {
  data <- context$data
  candidates <- unique(c(context$strata, context$annotated %||% character()))
  candidates <- setdiff(candidates, context$id)
  candidates[vapply(candidates, function(name) {
    x <- data[[name]]; (is.character(x) || is.factor(x) || is.logical(x) || (name %in% context$strata && is.numeric(x))) &&
      length(unique(x[!is.na(x)])) <= 20L
  }, logical(1))]
}

desktop_explore_unit_key <- function(x) ifelse(is.na(x), "M:", paste0("V:", x))

desktop_explore_set <- function(engine, args, channel = "mcp") {
  desktop_nca_idle(engine)
  desktop_nca_revision(engine, args$base_revision, args$project_id)
  desktop_nca_identity(args)
  desktop_check(is.character(args$explore_session) && length(args$explore_session) == 1L &&
    identical(args$explore_session, engine$explore_session), "E_REVISION_CONFLICT", "表示セッションが変更されました。表示条件を読み直してください。")
  desktop_check(is.numeric(args$explore_revision) && length(args$explore_revision) == 1L &&
    identical(as.numeric(args$explore_revision), as.numeric(engine$explore_revision)), "E_REVISION_CONFLICT", "表示条件が更新されています。再読み込みして選び直してください。")
  scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nchar(x) <= 200L
  desktop_check(all(vapply(args[c("source", "view", "parameter", "unit", "group")], scalar, logical(1))) &&
    all(c("source", "view", "parameter", "unit", "group") %in% names(args)), "E_VALIDATION", "表示条件の形式が不正です。")
  desktop_check(args$view %in% c("summary", "boxplot", "listing", "qc"), "E_VALIDATION", "指定した表示方法は使えません。")
  unit_missing <- isTRUE(args$unit_missing)
  desktop_check(is.null(args$unit_missing) || (is.logical(args$unit_missing) && length(args$unit_missing) == 1L && !is.na(args$unit_missing)), "E_VALIDATION", "単位欠測の指定は真偽値で指定してください。")
  desktop_check(!unit_missing || identical(args$unit, ""), "E_VALIDATION", "単位欠測を指定するときは単位を空欄にしてください。")
  context <- desktop_explore_context(engine, args$source)
  data <- context$data
  aggregate_over <- as.character(unlist(args$aggregate_over %||% character(), use.names = FALSE))
  if (!(is.character(aggregate_over) && !anyNA(aggregate_over) && !anyDuplicated(aggregate_over) &&
    all(aggregate_over %in% context$strata))) desktop_public_validation("まとめる列には記録された解析グループ列を指定してください。")
  desktop_check(all(aggregate_over %in% context$period), "E_VALIDATION", "期として記録された列だけをまとめられます。分析対象・試験・処置などの列はまとめられません。")
  desktop_check(!nzchar(args$group) || !args$group %in% aggregate_over, "E_VALIDATION", "集計する列とまとめる列に同じ列は指定できません。")
  confirmed <- args$single_treatment_confirmed %||% FALSE
  exception_reason <- args$single_treatment_reason %||% ""
  desktop_check(is.logical(confirmed) && length(confirmed) == 1L && !is.na(confirmed) &&
    is.character(exception_reason) && length(exception_reason) == 1L && !is.na(exception_reason) && nchar(exception_reason) <= 200L,
    "E_VALIDATION", "単一処置の確認または理由の形式が不正です。")
  treatment_retained <- intersect(context$treatment, setdiff(context$strata, aggregate_over))
  exception_used <- length(aggregate_over) > 0L && !length(treatment_retained)
  if (exception_used && !(isTRUE(confirmed) && nzchar(trimws(exception_reason)))) desktop_public_validation(
    "期をまとめるには処置の役割を持つグループ列が必要です。単一処置と確認できる場合のみ、確認欄と理由を入力してください。")
  basis <- args$summary_basis %||% "analysis_unit"
  if (!(is.character(basis) && length(basis) == 1L && !is.na(basis) && basis %in% c("analysis_unit", "subject_geomean"))) desktop_public_validation("指定した要約単位は使えません。")
  if (identical(basis, "subject_geomean")) {
    desktop_check(identical(args$view, "summary") && length(aggregate_over) > 0L && !identical(args$parameter, "TMAX"), "E_VALIDATION", "被験者別の幾何平均は、期をまとめた要約表でTMAX以外のパラメータを選んでください。")
  }
  scale <- args$scale %||% "linear"
  desktop_check(is.character(scale) && length(scale) == 1L && !is.na(scale) && scale %in% c("linear", "log"),
    "E_VALIDATION", "縦軸の目盛りは通常または対数を選んでください。")
  unit_match <- if (unit_missing) is.na(data$AVALU) else !is.na(data$AVALU) & data$AVALU == args$unit
  matched <- !is.na(data$PARAMCD) & data$PARAMCD == args$parameter & unit_match
  desktop_check(any(matched), "E_VALIDATION", "保存結果にあるパラメータと単位を選んでください。")
  if (length(aggregate_over) && length(treatment_retained)) for (column in treatment_retained) {
    values <- as.character(data[[column]][matched])
    if (anyNA(values) || any(!nzchar(trimws(values)))) desktop_public_validation("処置列に欠測または空欄があるため期をまとめられません。処置を確認してください。")
  }
  if (length(aggregate_over) && length(treatment_retained)) {
    integrity <- desktop_explore_treatment_integrity(data, context$id, context$period, treatment_retained, context$study)
    if (nrow(integrity$conflicts)) desktop_public_validation("同一被験者・同一期に異なる処置が記録されているため、期をまとめられません。役割と投与情報を確認してください。")
  }
  desktop_check(identical(args$group, "") || args$group %in% desktop_explore_group_candidates(context), "E_VALIDATION", "集計列には選択可能なカテゴリ列または全体を選んでください。")
  if (identical(basis, "subject_geomean"))
    desktop_explore_subject_means(data[matched, , drop = FALSE], context$id, args$group, setdiff(context$strata, aggregate_over))
  selection <- args[c("source", "view", "parameter", "unit", "group")]
  selection$unit_missing <- unit_missing
  selection$aggregate_over <- as.list(aggregate_over)
  selection$scale <- scale
  selection$summary_basis <- basis
  if (exception_used) {
    selection$single_treatment_confirmed <- TRUE
    selection$single_treatment_reason <- trimws(exception_reason)
  }
  desktop_nca_record(engine, "explore", args, channel, list(exploration = selection))
  engine$explore <- selection
  engine$explore_revision <- engine$explore_revision + 1L
  engine$explore_signal(shiny::isolate(engine$explore_signal()) + 1L)
  desktop_explore_get(engine)
}

desktop_explore_subset <- function(data, selection) {
  unit_match <- if (isTRUE(selection$unit_missing)) is.na(data$AVALU) else !is.na(data$AVALU) & data$AVALU == selection$unit
  keep <- !is.na(data$PARAMCD) & data$PARAMCD == selection$parameter & unit_match
  data[keep, , drop = FALSE]
}

desktop_explore_groups <- function(data, columns = character()) {
  # Use data-frame equality rather than joining text, which can conflate NA with a literal label.
  if (!length(columns)) return(list(rep(TRUE, nrow(data))))
  unique_rows <- unique(data[columns])
  lapply(seq_len(nrow(unique_rows)), function(i) Reduce(`&`, lapply(columns, function(name) {
    x <- data[[name]]; value <- unique_rows[[name]][i]
    if (is.na(value)) is.na(x) else !is.na(x) & x == value
  })))
}

desktop_explore_class <- function(data) {
  status <- if ("PARAM_STATUS" %in% names(data)) as.character(data$PARAM_STATUS) else rep(NA_character_, nrow(data))
  warning <- if ("NCA_WARNING" %in% names(data)) !is.na(data$NCA_WARNING) & nzchar(data$NCA_WARNING) else rep(FALSE, nrow(data))
  ifelse(!is.finite(data$AVAL), "算出不可・欠測", ifelse(is.na(status), "状態不明", ifelse(status == "policy_assigned", "方針割当",
    ifelse(status != "calculated", "状態要確認", ifelse(warning, "算出値（警告あり）", "算出値")))))
}

desktop_explore_subject_means <- function(data, id, group = "", strata = character(), identity_columns = character()) {
  desktop_check(id %in% names(data), "E_VALIDATION", "被験者ID列がありません。")
  ids <- as.character(data[[id]])
  state <- desktop_explore_class(data)
  warning <- if ("NCA_WARNING" %in% names(data)) !is.na(data$NCA_WARNING) & nzchar(data$NCA_WARNING) else rep(FALSE, nrow(data))
  policy <- if ("PARAM_STATUS" %in% names(data)) !is.na(data$PARAM_STATUS) & data$PARAM_STATUS == "policy_assigned" else rep(FALSE, nrow(data))
  invalid_id <- is.na(ids) | !nzchar(trimws(ids))
  invalid_value <- is.finite(data$AVAL) & data$AVAL <= 0
  invalid_state <- !state %in% c("算出値", "算出不可・欠測")
  invalid <- invalid_id | invalid_value | invalid_state | warning | policy
  # Refuse the entire affected treatment/analyte group: never silently drop just the problematic subject or period.
  group_columns <- unique(c(strata, if (nzchar(group)) group))
  groups <- desktop_explore_groups(data, group_columns)
  blocked <- vapply(groups, function(index) any(invalid[index]), logical(1))
  safe_index <- rep(FALSE, nrow(data))
  for (index in groups[!blocked]) safe_index <- safe_index | index
  reasons <- vapply(seq_len(nrow(data)), function(i) paste(c(
    if (invalid_id[i]) "被験者IDが欠測", if (invalid_value[i]) "値が0以下",
    if (warning[i]) "警告付き", if (policy[i]) "方針割当",
    if (invalid_state[i] && !warning[i] && !policy[i]) "結果の状態が未確認"), collapse = "・"), "")
  issue_columns <- intersect(unique(c(id, identity_columns, strata, if (nzchar(group)) group, "PARAMCD", "AVALU", "NCA_WARNING")), names(data))
  issues <- data[invalid, issue_columns, drop = FALSE]
  reason_name <- "保留理由"
  while (reason_name %in% names(issues)) reason_name <- paste0(reason_name, "_")
  issues[[reason_name]] <- reasons[invalid]
  counts_name <- ".pk_subject_n_valid"
  while (counts_name %in% names(data)) counts_name <- paste0(counts_name, "_")
  columns <- unique(c(group_columns, id))
  good <- data[safe_index, , drop = FALSE]
  if (nrow(good)) {
    parts <- desktop_explore_groups(good, columns)
    values <- do.call(rbind, lapply(parts, function(index) {
      rows <- which(index); x <- good$AVAL[rows]; finite <- x[is.finite(x)]
      first <- good[rows[1], , drop = FALSE]
      first$AVAL <- if (length(finite)) exp(mean(log(finite))) else NA_real_
      first$PARAM_STATUS <- if (length(finite)) "calculated" else "missing"
      first$NCA_WARNING <- NA_character_
      first[[counts_name]] <- length(finite)
      first
    }))
  } else { values <- data[FALSE, , drop = FALSE]; values[[counts_name]] <- integer() }
  rownames(values) <- NULL
  list(data = values, input_units = nrow(data), missing_units = sum(!is.finite(data$AVAL)),
    subject_rows = nrow(values), blocked_groups = sum(blocked), blocked_units = sum(!safe_index),
    issues = issues, counts_column = counts_name)
}

desktop_explore_treatment_integrity <- function(data, id, period, treatment, study = character()) {
  keys <- unique(c(study, id, period)); columns <- unique(c(keys, treatment))
  desktop_check(all(columns %in% names(data)), "E_VALIDATION", "被験者・期・処置の列を確認してください。")
  if (!length(treatment) || !nrow(data)) return(list(conflicts = data.frame(), unassessable = 0L))
  complete <- Reduce(`&`, lapply(columns, function(column) {
    v <- as.character(data[[column]]); !is.na(v) & nzchar(trimws(v))
  }))
  observations <- unique(data[complete, columns, drop = FALSE])
  parts <- if (nrow(observations)) desktop_explore_groups(observations, keys) else list()
  problematic <- Filter(function(index) nrow(unique(observations[index, treatment, drop = FALSE])) > 1L, parts)
  issues <- if (length(problematic)) do.call(rbind, lapply(problematic, function(index) {
    row <- observations[which(index)[1], keys, drop = FALSE]
    count_name <- "異なる処置の組合せ数"
    while (count_name %in% names(row)) count_name <- paste0(count_name, "_")
    combinations <- unique(observations[index, treatment, drop = FALSE])
    row[[count_name]] <- nrow(combinations)
    value_name <- "処置の組合せ"
    while (value_name %in% names(row)) value_name <- paste0(value_name, "_")
    row[[value_name]] <- paste(vapply(seq_len(nrow(combinations)), function(i) {
      paste(paste0(treatment, "=", vapply(combinations[i, , drop = FALSE], function(v) as.character(v[1]), "")), collapse = " / ")
    }, ""), collapse = "； ")
    row
  })) else observations[FALSE, keys, drop = FALSE]
  list(conflicts = issues, unassessable = sum(!complete))
}

desktop_explore_allocation <- function(data, id, period, treatment, study = character()) {
  columns <- unique(c(study, period, treatment))
  desktop_check(id %in% names(data) && all(columns %in% names(data)), "E_VALIDATION", "期・処置・被験者IDの列を確認してください。")
  if (!nrow(data)) return(list(table = data.frame(), one_level = character(), no_treatment = !length(treatment)))
  parts <- desktop_explore_groups(data, columns)
  result <- do.call(rbind, lapply(parts, function(index) {
    keys <- if (length(columns)) data[which(index)[1], columns, drop = FALSE] else data.frame(集計 = "全体")
    for (name in names(keys)) if (is.character(keys[[name]])) keys[[name]] <- ifelse(is.na(keys[[name]]), "欠測（NA）", paste0("値：", keys[[name]]))
    ids <- as.character(data[[id]][index]); present <- !is.na(ids) & nzchar(trimws(ids))
    out <- cbind(keys, data.frame(`被験者数（異なるID）` = length(unique(ids[present])), `ID欠測の解析単位数` = sum(!present), check.names = FALSE))
    names(out) <- make.unique(names(out)); out
  }))
  one_level <- treatment[vapply(treatment, function(column) length(unique(data[[column]][!is.na(data[[column]])])) <= 1L, logical(1))]
  list(table = result, one_level = one_level, no_treatment = !length(treatment))
}

desktop_explore_sort_indices <- function(data) {
  if (!nrow(data) || !ncol(data)) return(seq_len(nrow(data)))
  args <- lapply(data, function(x) {
    if (is.numeric(x)) return(x)
    y <- enc2utf8(as.character(x)); Encoding(y) <- "UTF-8"; n <- suppressWarnings(as.numeric(y))
    if (all(is.na(y) | (!is.na(n) & nzchar(trimws(y))))) n else y
  })
  do.call(order, c(unname(args), list(na.last = TRUE, method = "radix")))
}

# Only infer labelled periods when all labels share one unambiguous pattern.
desktop_explore_period_order <- function(vals, explicit = NULL) {
  vals <- enc2utf8(as.character(vals)); Encoding(vals) <- "UTF-8"
  present <- !is.na(vals) & nzchar(trimws(vals))
  labels <- unique(vals[present])
  if (!is.null(explicit)) {
    chosen <- unlist(explicit, use.names = FALSE)
    if (!is.character(chosen) || anyNA(chosen) || anyDuplicated(chosen) || !all(labels %in% chosen))
      return(list(ok = FALSE, note = "保存された期順と結果の期が一致しません。NCA条件を確認してください。"))
    return(list(ok = TRUE, labels = chosen, number = seq_along(chosen), inferred = TRUE, manual = TRUE))
  }
  # Normalize digits only for detection; original labels remain analysis keys and display values.
  normalized <- chartr("０１２３４５６７８９", "0123456789", labels)
  numeric <- suppressWarnings(as.numeric(normalized))
  if (!length(labels)) return(list(ok = FALSE, note = "期の記録がないため順序を判定できません。"))
  inferred <- FALSE
  if (!all(!is.na(numeric) & is.finite(numeric))) {
    trimmed <- trimws(normalized)
    matches <- gregexpr("[0-9]+", trimmed, perl = TRUE)
    if (any(vapply(matches, function(x) length(x) != 1L || x[1] < 0L, logical(1))))
      return(list(ok = FALSE, note = "期の順序を数値で判定できないため投与順を表示できません。"))
    prefix <- vapply(seq_along(trimmed), function(i) substr(trimmed[i], 1L, matches[[i]][1] - 1L), "")
    suffix <- vapply(seq_along(trimmed), function(i) substring(trimmed[i], matches[[i]][1] + attr(matches[[i]], "match.length")[1]), "")
    if (length(unique(prefix)) != 1L || length(unique(suffix)) != 1L)
      return(list(ok = FALSE, note = "数字以外の部分でも期が区別されている可能性があるため、自動判定しません。"))
    if (any(grepl("[+＋\u2212−－-]$", prefix)) || any(grepl("^[.,．，]", suffix)))
      return(list(ok = FALSE, note = "数字の符号や小数点が曖昧なため、期の順序を自動判定しません。"))
    numeric <- suppressWarnings(as.numeric(vapply(seq_along(trimmed), function(i) regmatches(trimmed[i], matches[[i]])[1], "")))
    if (any(!is.finite(numeric)) || any(numeric > 9007199254740991))
      return(list(ok = FALSE, note = "期番号を安全に判定できないため投与順を表示できません。"))
    inferred <- TRUE
  }
  if (anyDuplicated(numeric)) return(list(ok = FALSE, note = "同じ数値に異なる期の表記があるため投与順を判定できません。"))
  list(ok = TRUE, labels = labels, number = numeric, inferred = inferred)
}

desktop_explore_allocation_matrix <- function(data, id, period, treatment, study = character(), explicit = NULL) {
  if (length(treatment) != 1L || !nrow(data)) return(NULL)
  columns <- unique(c(study, period)); if (!length(columns)) return(NULL)
  rows <- unique(data[columns]); parts <- desktop_explore_groups(data, columns)
  sort_rows <- rows
  if (length(period) == 1L) {
    ordering <- desktop_explore_period_order(data[[period]], explicit)
    if (isTRUE(ordering$ok)) sort_rows[[period]] <- ordering$number[match(as.character(rows[[period]]), ordering$labels)]
  }
  ord <- desktop_explore_sort_indices(sort_rows); rows <- rows[ord, , drop = FALSE]; parts <- parts[ord]
  values <- unique(enc2utf8(as.character(data[[treatment]]))); Encoding(values) <- "UTF-8"
  values <- values[order(values, na.last = TRUE, method = "radix")]
  labels <- ifelse(is.na(values), "（処置欠測）", ifelse(values == "（処置欠測）", "（処置欠測）［入力値］", values))
  labels <- make.unique(c(names(rows), paste0("処置：", labels), "ID欠測の解析単位数"))
  result <- rows
  for (i in seq_along(values)) {
    selected <- if (is.na(values[i])) is.na(data[[treatment]]) else !is.na(data[[treatment]]) & as.character(data[[treatment]]) == values[i]
    result[[labels[length(columns)+i]]] <- vapply(parts, function(index) {
      ids <- as.character(data[[id]][index & selected]); length(unique(ids[!is.na(ids) & nzchar(trimws(ids))]))
    }, integer(1))
  }
  result[[tail(labels, 1)]] <- vapply(parts, function(index) {
    ids <- as.character(data[[id]][index]); sum(is.na(ids) | !nzchar(trimws(ids)))
  }, integer(1))
  for (name in columns) if (is.character(result[[name]])) result[[name]] <- ifelse(is.na(result[[name]]), "欠測（NA）", paste0("値：", result[[name]]))
  result
}

desktop_explore_sequences <- function(data, id, period, treatment, study = character(), explicit = NULL) {
  if (!length(treatment)) return(list(table = NULL, note = "処置役割の列がないため投与順は作れません。"))
  if (length(period) != 1L) return(list(table = NULL, note = "期の列が1つではないため投与順を自動判定できません。"))
  ordering <- desktop_explore_period_order(data[[period]], explicit)
  if (!isTRUE(ordering$ok)) return(list(table = NULL, note = ordering$note))
  identity <- unique(c(study, id)); allkeys <- unique(c(identity, period, treatment))
  good <- Reduce(`&`, lapply(allkeys, function(name) { x <- as.character(data[[name]]); !is.na(x) & nzchar(trimws(x)) }))
  excluded_rows <- sum(!good); observations <- unique(data[good, allkeys, drop = FALSE])
  if (!nrow(observations)) return(list(table = NULL, note = "投与順を判定できる被験者・期・処置の記録がありません。"))
  people <- desktop_explore_groups(observations, identity)
  per_person <- lapply(people, function(index) {
    person <- observations[index, , drop = FALSE]
    study_mask <- if (length(study)) Reduce(`&`, lapply(study, function(name) as.character(observations[[name]]) == as.character(person[[name]][1]))) else rep(TRUE, nrow(observations))
    all_periods <- unique(as.character(observations[[period]][study_mask])); all_periods <- all_periods[order(ordering$number[match(all_periods, ordering$labels)])]
    assignments <- vapply(all_periods, function(p) {
      x <- unique(person[as.character(person[[period]]) == p, treatment, drop = FALSE])
      if (!nrow(x)) return("未記録")
      if (nrow(x) > 1L) return("複数処置（要確認）")
      if (length(treatment) == 1L) {
        value <- as.character(x[[1]][1])
        return(if (value %in% c("未記録", "複数処置（要確認）")) paste0(value, "［処置の値］") else value)
      }
      paste(paste0(treatment, "=", vapply(x, function(v) as.character(v[1]), "")), collapse = " / ")
    }, "")
    raw_assignments <- lapply(all_periods, function(p) {
      x <- unique(person[as.character(person[[period]]) == p, treatment, drop = FALSE])
      if (!nrow(x)) list(period = p, status = "absent") else list(period = p, status = "recorded", values = lapply(x, as.character))
    })
    list(signature = desktop_json(raw_assignments), label = paste(paste0(if (ordering$inferred) "" else "期", all_periods, ": ", assignments), collapse = " → "))
  })
  keys <- vapply(seq_along(per_person), function(i) desktop_json(list(study = as.list(observations[which(people[[i]])[1], study, drop = FALSE]), sequence = per_person[[i]]$signature)), "")
  groups <- split(seq_along(per_person), factor(keys, levels = unique(keys)))
  result <- do.call(rbind, lapply(groups, function(ix) {
    row <- if (length(study)) observations[which(people[[ix[1]]])[1], study, drop = FALSE] else data.frame(row.names = 1L)
    out <- cbind(row, data.frame(`期順の処置` = per_person[[ix[1]]]$label, `被験者数` = length(ix), check.names = FALSE))
    names(out) <- make.unique(names(out)); out
  }))
  result <- result[desktop_explore_sort_indices(result), , drop = FALSE]
  list(table = result, note = paste0(if (isTRUE(ordering$manual)) "NCA設定で利用者が指定した期順です。元の割付・投与情報と照合してください。" else if (ordering$inferred) "期の表記から抽出した数字順に表示します。実際の投与順は元データで確認してください。" else "期の数値順に表示します。", "未記録は選択したNCA結果に行がない期です。ID・期・処置などが欠測の行 ", excluded_rows, " 件は順序集計に含めていません。"))
}

desktop_explore_has_repeated <- function(data, group, strata, id) {
  if (is.null(id) || !id %in% names(data) || !nrow(data)) return(FALSE)
  state <- ".探索状態"; while (state %in% names(data)) state <- paste0(state, "_")
  data[[state]] <- desktop_explore_class(data)
  columns <- unique(c(strata, if (nzchar(group)) group, state))
  any(vapply(desktop_explore_groups(data, columns), function(index) {
    ids <- as.character(data[[id]][index & is.finite(data$AVAL)])
    ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
    anyDuplicated(ids) > 0L
  }, logical(1)))
}

desktop_explore_summary <- function(data, group = "", strata = character(), id = NULL, subject_counts = NULL) {
  if (!nrow(data)) return(data.frame())
  state_column <- ".探索状態"
  while (state_column %in% names(data)) state_column <- paste0(state_column, "_")
  data[[state_column]] <- desktop_explore_class(data)
  columns <- unique(c(strata, if (nzchar(group)) group, state_column))
  partitions <- desktop_explore_groups(data, columns)
  do.call(rbind, lapply(partitions, function(index) {
    keys <- data[which(index)[1], columns, drop = FALSE]
    status_label <- "結果の状態"
    while (status_label %in% setdiff(names(keys), state_column)) status_label <- paste0(status_label, "_")
    names(keys)[names(keys) == state_column] <- status_label
    for (name in setdiff(names(keys), status_label)) if (is.character(keys[[name]])) keys[[name]] <- ifelse(is.na(keys[[name]]), "欠測（NA）", paste0("値：", keys[[name]]))
    x <- data$AVAL[index]
    good <- x[is.finite(x)]
    ids <- if (!is.null(id) && id %in% names(data)) as.character(data[[id]][index][is.finite(x)]) else character()
    ids <- ids[!is.na(ids) & nzchar(ids)]
    nonpositive <- sum(good <= 0)
    is_tmax <- "PARAMCD" %in% names(data) && all(as.character(data$PARAMCD[index]) == "TMAX")
    geometric <- length(good) > 0L && nonpositive == 0L && !is_tmax
    log_sd <- if (geometric && length(good) > 1L) stats::sd(log(good)) else NA_real_
    stats <- data.frame(`解析単位数` = length(x), N = length(good), `有限値の被験者ID数` = if (is.null(id)) NA_integer_ else length(unique(ids)),
      `同一IDの複数値` = desktop_explore_repeat_label(ids), `幾何計算対象外（0以下）` = nonpositive, `幾何平均` = if (geometric) exp(mean(log(good))) else NA_real_,
      `幾何CV%` = if (is.finite(log_sd)) sqrt(expm1(log_sd^2)) * 100 else NA_real_,
      `算術CV%` = if (!is_tmax && length(good) > 1L && mean(good) != 0) stats::sd(good) / mean(good) * 100 else NA_real_,
      `欠測・非有限` = length(x) - length(good), 平均 = if (!is_tmax && length(good)) mean(good) else NA_real_,
      SD = if (!is_tmax && length(good) > 1L) stats::sd(good) else NA_real_, 中央値 = if (length(good)) stats::median(good) else NA_real_,
      最小 = if (length(good)) min(good) else NA_real_, 最大 = if (length(good)) max(good) else NA_real_, check.names = FALSE)
    if (!is.null(subject_counts)) {
      valid_counts <- data[[subject_counts]][index][is.finite(x)]
      stats[["同一IDの複数値"]] <- NULL
      stats[["使用期・解析単位数"]] <- if (length(valid_counts)) paste0(min(valid_counts), "–", max(valid_counts)) else "—"
    }
    out <- cbind(keys, stats)
    names(out) <- make.unique(names(out))
    metric <- c("使用期・解析単位数", "解析単位数", "N", "有限値の被験者ID数", "同一IDの複数値", "平均", "SD", "算術CV%", "幾何平均", "幾何CV%", "中央値", "最小", "最大", "欠測・非有限", "幾何計算対象外（0以下）")
    out[c(setdiff(names(out), metric), intersect(metric, names(out)))]
  }))
}

desktop_explore_display_summary <- function(data, table_label = "この表", hide_empty_repeat = FALSE) {
  if (!nrow(data)) return(desktop_display_table(data))
  repeat_column <- "同一IDの複数値"
  if (isTRUE(hide_empty_repeat) && repeat_column %in% names(data) && is.character(data[[repeat_column]]) &&
      all(!is.na(data[[repeat_column]]) & !nzchar(data[[repeat_column]]))) data[[repeat_column]] <- NULL
  hidden <- intersect(c("欠測・非有限", "幾何計算対象外（0以下）"), names(data))
  hidden <- hidden[vapply(hidden, function(name) is.numeric(data[[name]]) && all(!is.na(data[[name]]) & data[[name]] == 0), logical(1))]
  data <- data[setdiff(names(data), hidden)]
  value_cols <- intersect(c("平均", "SD", "幾何平均", "中央値", "最小", "最大"), names(data))
  cv_cols <- intersect(c("算術CV%", "幾何CV%"), names(data))
  format_value <- function(x) {
    if (!is.finite(x)) return("—")
    if (x == 0) return("0")
    magnitude <- abs(signif(x, 4L))
    if (magnitude < 1e-4 || magnitude >= 1e6) return(formatC(x, format = "e", digits = 3))
    sub("\\.$", "", trimws(formatC(x, format = "fg", digits = 4, flag = "#")))
  }
  formatted <- data
  for (name in names(formatted)) if (is.character(formatted[[name]])) {
    literal_missing <- !is.na(formatted[[name]]) & formatted[[name]] == "値：欠測（NA）"
    formatted[[name]] <- sub("^値：", "", formatted[[name]])
    formatted[[name]][literal_missing] <- "欠測（NA）［入力値］"
  }
  for (name in value_cols) formatted[[name]] <- vapply(data[[name]], format_value, "")
  for (name in cv_cols) {
    x <- data[[name]]; formatted[[name]] <- ifelse(is.finite(x), formatC(x, format = "f", digits = 1), "—")
  }
  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  table <- shiny::tags$table(class = "table table-striped table-sm pk-explore-summary-table",
    shiny::tags$thead(shiny::tags$tr(lapply(names(data), function(name) shiny::tags$th(class = if (name %in% numeric_cols) "pk-number" else NULL, name)))),
    shiny::tags$tbody(lapply(seq_len(nrow(data)), function(i) shiny::tags$tr(lapply(names(data), function(name)
      shiny::tags$td(class = if (name %in% numeric_cols) "pk-number" else NULL,
        if (is.na(formatted[[name]][i])) "—" else as.character(formatted[[name]][i])))))))
  shiny::div(class = "pk-explore-table-block", shiny::div(class = "pk-table-scroll", tabindex = "0", role = "region",
    `aria-label` = "要約表。横方向にスクロールできます。", table),
    if (length(hidden)) shiny::tags$details(shiny::tags$summary(paste0(table_label, "で0の項目")),
      shiny::p(paste(paste0(hidden, "：0"), collapse = "、"))))
}

desktop_explore_repeat_label <- function(ids) {
  counts <- table(ids)
  repeated <- counts[counts > 1L]
  if (!length(repeated)) return("")
  if (length(unique(repeated)) == 1L)
    sprintf("要確認（%d IDで%d件ずつ）", length(repeated), unname(repeated[[1]]))
  else sprintf("要確認（%d IDに複数値）", length(repeated))
}

desktop_explore_overview_has_repeated <- function(data, group = "", strata = character(), id = NULL) {
  if (is.null(id) || !id %in% names(data) || !nrow(data)) return(FALSE)
  columns <- unique(c(strata, if (nzchar(group)) group))
  any(vapply(desktop_explore_groups(data, columns), function(index) {
    ids <- as.character(data[[id]][index & is.finite(data$AVAL)])
    ids <- ids[!is.na(ids) & nzchar(ids)]
    anyDuplicated(ids) > 0L
  }, logical(1)))
}

desktop_explore_overview <- function(data, group = "", strata = character(), id = NULL) {
  if (!nrow(data)) return(data.frame())
  columns <- unique(c(strata, if (nzchar(group)) group))
  parts <- desktop_explore_groups(data, columns)
  do.call(rbind, lapply(parts, function(index) {
    keys <- if (length(columns)) data[which(index)[1], columns, drop = FALSE] else data.frame(集計 = "全体")
    for (name in names(keys)) if (is.character(keys[[name]])) keys[[name]] <- ifelse(is.na(keys[[name]]), "欠測（NA）", paste0("値：", keys[[name]]))
    x <- data$AVAL[index]; good <- is.finite(x); classes <- desktop_explore_class(data[index, , drop = FALSE])
    ids <- if (!is.null(id) && id %in% names(data)) as.character(data[[id]][index][good]) else character()
    ids <- ids[!is.na(ids) & nzchar(ids)]
    out <- cbind(keys, data.frame(`解析単位数` = length(x), `有限値N` = sum(good), `欠測・非有限` = sum(!good),
      `有限値の被験者ID数` = if (is.null(id)) NA_integer_ else length(unique(ids)),
      `同一IDの複数値` = desktop_explore_repeat_label(ids),
      `警告付き` = sum(classes == "算出値（警告あり）"), `方針割当` = sum(classes == "方針割当"), check.names = FALSE))
    names(out) <- make.unique(names(out)); out
  }))
}

desktop_explore_plot_groups <- function(data, group = "", strata = character()) {
  data <- data[is.finite(data$AVAL), , drop = FALSE]
  if (!nrow(data)) return(list(values = list(), legend = data.frame()))
  state_column <- ".探索状態"
  while (state_column %in% names(data)) state_column <- paste0(state_column, "_")
  data[[state_column]] <- desktop_explore_class(data)
  columns <- unique(c(strata, if (nzchar(group)) group, state_column))
  parts <- desktop_explore_groups(data, columns)
  codes <- paste0("G", seq_along(parts))
  values <- lapply(parts, function(index) data$AVAL[index]); names(values) <- codes
  legend <- do.call(rbind, lapply(seq_along(parts), function(i) {
    keys <- data[which(parts[[i]])[1], columns, drop = FALSE]
    state_label <- "結果の状態"
    while (state_label %in% setdiff(names(keys), state_column)) state_label <- paste0(state_label, "_")
    names(keys)[names(keys) == state_column] <- state_label
    for (name in setdiff(names(keys), state_label)) if (is.character(keys[[name]]))
      keys[[name]] <- ifelse(is.na(keys[[name]]), "欠測（NA）", paste0("値：", keys[[name]]))
    out <- cbind(data.frame(`図の群` = codes[i], check.names = FALSE), keys,
      data.frame(`有限値N` = sum(parts[[i]]), check.names = FALSE))
    names(out) <- make.unique(names(out)); out
  }))
  offsets <- lapply(parts, function(index) {
    rows <- which(index)
    if (length(rows) == 1L) return(0)
    vapply(rows, function(i) {
      row <- data[i, , drop = FALSE]; rownames(row) <- NULL
      hash <- digest::digest(row, algo = "sha256", serialize = TRUE)
      (strtoi(substr(hash, 1L, 7L), base = 16L) / 268435455 - 0.5) * 0.16
    }, numeric(1))
  })
  labels <- vapply(seq_along(parts), function(i) {
    row <- legend[i, setdiff(names(legend), c("図の群", "有限値N", "結果の状態")), drop = FALSE]
    parts_text <- if (ncol(row)) paste(sub("^値：", "", as.character(row[1, ])), collapse = " / ") else "全体"
    state <- as.character(legend[["結果の状態"]][i])
    if (state != "算出値") parts_text <- paste0(parts_text, "/", state)
    paste0(parts_text, "\nn=", length(values[[i]]))
  }, "")
  list(values = values, legend = legend, labels = labels, offsets = offsets)
}

desktop_explore_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "pk-data-page pk-workspace",
    shiny::h2(desktop_i18n_text("保存した NCA 結果を探索", "Explore saved NCA results")),
    shiny::p("パラメータ・単位・解析単位のグループ・結果状態を分けて表示します。まとめる列は明示的に指定してください。Nは選択した要約単位の有限値数です。警告の採否は自動判定しません。"),
    shiny::div(class = "pk-card", shiny::uiOutput(ns("choices")),
      shiny::div(class = "pk-explore-pooling", shiny::uiOutput(ns("pooling_control")),
        shiny::conditionalPanel("input.aggregate_over && input.aggregate_over.length > 0", ns = ns, shiny::uiOutput(ns("single_controls")))),
      shiny::uiOutput(ns("integrity_preview")), shiny::uiOutput(ns("apply_control")),
      shiny::textOutput(ns("notice"))),
    shiny::div(class = "pk-card", shiny::uiOutput(ns("active")), shiny::uiOutput(ns("allocation")), shiny::uiOutput(ns("table")),
      shiny::uiOutput(ns("plot_area")), shiny::uiOutput(ns("legend"))))
}

desktop_explore_ui_message <- function(e) {
  message <- conditionMessage(e)
  if (grepl("[ぁ-んァ-ン一-龥]", message)) return(message)
  if (inherits(e, "pkapp_error") && e$code %in% c("E_REVISION_CONFLICT", "E_PROJECT_CONFLICT")) return("表示条件またはプロジェクトが変更されています。読み直して選び直してください。")
  if (inherits(e, "pkapp_error") && e$code %in% c("E_INPUT_CHANGED", "E_CODE_CHANGED")) return("保存結果またはプログラムの整合性を確認してください。")
  "指定した条件または保存結果を確認してください。"
}

desktop_explore_server <- function(id, engine) shiny::moduleServer(id, function(input, output, session) {
  notice <- shiny::reactiveVal("保存結果と表示条件を選び、「表示を更新」を押してください。")
  initial_selection <- shiny::isolate(engine$explore)
  confirmed_source <- shiny::reactiveVal(if (!is.null(initial_selection) && isTRUE(initial_selection$single_treatment_confirmed)) initial_selection$source else NULL)
  may_restore <- shiny::reactiveVal(TRUE)
  last_source <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$single_treatment_confirmed, {
    confirmed_source(if (isTRUE(input$single_treatment_confirmed)) input$source else NULL)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$source, {
    previous <- shiny::isolate(last_source())
    changed <- if (is.null(previous)) !is.null(initial_selection) && !identical(initial_selection$source, input$source) else !identical(previous, input$source)
    if (changed) {
      if (!is.null(shiny::isolate(confirmed_source())) || isTRUE(shiny::isolate(input$single_treatment_confirmed)))
        notice("保存結果を変更したため単一処置の確認と理由を解除しました。表示を更新する前に再確認してください。")
      may_restore(FALSE)
      confirmed_source(NULL)
      shiny::updateCheckboxInput(session, "single_treatment_confirmed", value = FALSE)
      shiny::updateTextInput(session, "single_treatment_reason", value = "")
    }
    last_source(input$source)
  }, ignoreInit = FALSE, ignoreNULL = TRUE, priority = 100)
  draft_scale <- shiny::reactiveVal(if (!is.null(engine$explore)) engine$explore$scale %||% "linear" else "linear")
  shiny::observeEvent(input$scale, { if (input$scale %in% c("linear", "log")) draft_scale(input$scale) }, ignoreNULL = TRUE)
  draft_revision <- shiny::reactiveVal(engine$explore_revision)
  shiny::observeEvent(engine$explore_signal(), {
    if (!identical(as.numeric(draft_revision()), as.numeric(engine$explore_revision)))
      notice("別の操作で共有表示が更新されました。入力欄を使って変更する前に、ページを再読み込みしてください。")
  }, ignoreInit = TRUE)
  output$choices <- shiny::renderUI({
    engine$tables_signal(); engine$explore_signal()
    tables <- Filter(function(x) x$kind %in% c("nca", "nca_annotation"), desktop_tables(engine))
    if (!length(tables)) return(shiny::p(desktop_i18n_text("保存済みの NCA 結果がありません。", "There are no saved NCA results.")))
    ids <- vapply(tables, `[[`, "", "id")
    labels <- vapply(tables, function(x) {
      instant <- tryCatch(as.POSIXct(strptime(x$created_at, "%Y-%m-%dT%H:%M:%OS", tz = "UTC")), error = function(e) as.POSIXct(NA))
      stamp <- if (is.na(instant)) "日時不明" else format(instant, "%Y/%m/%d %H:%M %Z", tz = Sys.timezone() %||% "UTC")
      paste0(x$name, " · ", stamp)
    }, "")
    selection <- engine$explore
    source <- if (!is.null(input$source) && input$source %in% ids) input$source else if (!is.null(selection) && selection$source %in% ids) selection$source else ids[1]
    context <- tryCatch(desktop_explore_context(engine, source), error = function(e) NULL)
    data <- if (is.null(context)) NULL else context$data
    if (is.null(data)) return(shiny::p("保存結果を読み込めません。整合性を確認してください。"))
    params <- unique(as.character(data$PARAMCD[!is.na(data$PARAMCD)]))
    parameter <- if (!is.null(input$parameter) && input$parameter %in% params) input$parameter else if (!is.null(selection) && identical(selection$source, source) && selection$parameter %in% params) selection$parameter else params[1]
    units <- unique(as.character(data$AVALU[!is.na(data$PARAMCD) & data$PARAMCD == parameter]))
    unit_keys <- desktop_explore_unit_key(units)
    saved_unit <- if (!is.null(selection) && isTRUE(selection$unit_missing)) "M:" else if (!is.null(selection)) desktop_explore_unit_key(selection$unit) else NULL
    unit <- if (!is.null(input$unit) && input$unit %in% unit_keys) input$unit else if (!is.null(selection) && identical(selection$source, source) && identical(selection$parameter, parameter) && saved_unit %in% unit_keys) saved_unit else unit_keys[1]
    ns <- session$ns
    candidates <- desktop_explore_group_candidates(context)
    shiny::div(class = "pk-explore-form", shiny::selectInput(ns("source"), desktop_i18n_text("保存結果", "Saved result"), stats::setNames(ids, labels), selected = source),
      shiny::selectInput(ns("view"), desktop_i18n_text("表示", "Display"), c("要約表" = "summary", "箱ひげ図" = "boxplot", "結果一覧" = "listing", "警告・算出不可" = "qc"), selected = if (!is.null(input$view)) input$view else if (!is.null(selection)) selection$view else "summary"),
      shiny::selectInput(ns("parameter"), desktop_i18n_text("パラメータ", "Parameter"), params, selected = parameter),
      shiny::selectInput(ns("unit"), desktop_i18n_text("単位", "Unit"), stats::setNames(unit_keys, ifelse(is.na(units), "（単位欠測）", ifelse(nzchar(units), units, "（単位なし）"))), selected = unit, selectize = FALSE),
      shiny::selectInput(ns("group"), desktop_i18n_text("集計・色分けする列", "Column for grouping / colours"), c("全体" = "", candidates), selected = if (!is.null(input$group) && input$group %in% c("", candidates)) input$group else if (!is.null(selection) && selection$group %in% candidates) selection$group else "", selectize = FALSE),
      if (identical(input$view %||% if (!is.null(selection)) selection$view else "summary", "summary"))
        shiny::selectInput(ns("summary_basis"), desktop_i18n_text("要約する単位", "Summary basis"), choices = c("解析単位（各期）" = "analysis_unit", "被験者ごとの幾何平均" = "subject_geomean"),
          selected = input$summary_basis %||% if (!is.null(selection)) selection$summary_basis %||% "analysis_unit" else "analysis_unit", selectize = FALSE),
      if (identical(input$view %||% if (!is.null(selection)) selection$view else "summary", "boxplot"))
        shiny::selectInput(ns("scale"), desktop_i18n_text("縦軸の目盛り", "Y-axis scale"), choices = c("通常" = "linear", "対数（正の値のみ）" = "log"),
          selected = input$scale %||% draft_scale(), selectize = FALSE),
      shiny::p(class = "pk-explore-note", "NCA設定で「期」と明示した列だけ統合できます。旧結果や未指定の列は統合できません。同じ被験者の複数解析単位が入る場合があります。"))
  })
  output$pooling_control <- shiny::renderUI({
    source <- input$source
    if (is.null(source) || !nzchar(source)) return(NULL)
    context <- tryCatch(desktop_explore_context(engine, source), error = function(e) NULL)
    if (is.null(context)) return(NULL)
    restore <- shiny::isolate(may_restore()) && !is.null(initial_selection) && identical(initial_selection$source, source)
    shiny::checkboxGroupInput(session$ns("aggregate_over"), "明示的にまとめる解析グループ列", choices = context$period,
      selected = if (restore) intersect(unlist(initial_selection$aggregate_over, use.names = FALSE), context$period) else character(), inline = TRUE)
  })
  # These controls depend on the source only; changing parameter, view or applying a selection does not recreate them.
  output$single_controls <- shiny::renderUI({
    source <- input$source
    if (is.null(source) || !nzchar(source)) return(NULL)
    context <- tryCatch(desktop_explore_context(engine, source), error = function(e) NULL)
    if (is.null(context) || length(context$treatment)) return(NULL)
    restore <- shiny::isolate(may_restore()) && !is.null(initial_selection) && identical(initial_selection$source, source) && isTRUE(initial_selection$single_treatment_confirmed)
    shiny::div(class = "pk-explore-single-controls",
      shiny::checkboxInput(session$ns("single_treatment_confirmed"), desktop_i18n_text("全期が同一処置であることを確認した", "I confirm that all periods have the same treatment."), value = restore),
      shiny::textInput(session$ns("single_treatment_reason"), desktop_i18n_text("処置列なしで期をまとめる理由（必須）", "Reason for pooling periods without a treatment column (required)"), value = if (restore) initial_selection$single_treatment_reason else "", placeholder = "例：全期で同一製剤を投与"))
  })
  integrity_check <- shiny::reactive({
    source <- input$source
    if (is.null(source) || !length(input$aggregate_over %||% character())) return(NULL)
    context <- tryCatch(desktop_explore_context(engine, source), error = function(e) NULL)
    if (is.null(context) || !length(context$treatment)) return(NULL)
    desktop_explore_treatment_integrity(context$data, context$id, context$period, context$treatment, context$study)
  })
  output$notice <- shiny::renderText({
    check <- integrity_check()
    initial <- "保存結果と表示条件を選び、「表示を更新」を押してください。"
    if (!is.null(check) && nrow(check$conflicts) > 0L && identical(notice(), initial)) return("")
    notice()
  })
  output$integrity_preview <- shiny::renderUI({
    check <- integrity_check()
    if (is.null(check) || (!nrow(check$conflicts) && !check$unassessable)) return(NULL)
    shiny::div(class = "pk-explore-section",
      if (nrow(check$conflicts)) shiny::tagList(shiny::p(class = "text-danger", paste0("同一被験者・同一期に複数処置の組合せがあるため、期をまとめられません（", nrow(check$conflicts), "組）。役割と投与情報を確認してください。")),
        shiny::div(class = "pk-table-scroll", style = "max-height:200px;min-height:0;overflow:auto", tabindex = "0", role = "region", `aria-label` = "矛盾の一覧。スクロールできます。", desktop_display_table(head(check$conflicts, 100L))),
        if (nrow(check$conflicts) > 100L) shiny::p("先頭100組を表示しています。")),
      if (check$unassessable) shiny::p(class = "text-danger", paste0("ID・期・処置・試験の欠測があるNCA結果 ", check$unassessable, "行は矛盾を確認できません。")))
  })
  output$apply_control <- shiny::renderUI({
    check <- integrity_check(); blocked <- !is.null(check) && nrow(check$conflicts) > 0L
    shiny::tagList(shiny::tags$button(id = session$ns("apply"), type = "button", class = "btn btn-default action-button btn-primary", disabled = if (blocked) "disabled" else NULL, shiny::span(class = "action-label", "表示を更新")),
      if (blocked) shiny::span(class = "text-danger", " 期・処置の矛盾を確認してください。"))
  })
  shiny::observeEvent(input$apply, tryCatch({
    pk_assert(is.character(input$unit) && length(input$unit) == 1L && (input$unit == "M:" || startsWith(input$unit, "V:")), "単位を選択してください。")
    applied <- desktop_explore_set(engine, list(source = input$source, view = input$view, parameter = input$parameter,
      unit = if (input$unit == "M:") "" else substring(input$unit, 3), unit_missing = input$unit == "M:",
      explore_revision = draft_revision(), explore_session = engine$explore_session,
      group = input$group, aggregate_over = input$aggregate_over %||% character(), scale = input$scale %||% draft_scale(),
      single_treatment_confirmed = isTRUE(input$single_treatment_confirmed) && identical(confirmed_source(), input$source), single_treatment_reason = input$single_treatment_reason %||% "",
      summary_basis = if (identical(input$view, "summary")) input$summary_basis %||% "analysis_unit" else "analysis_unit", base_revision = engine$nca$revision, project_id = engine$nca$project_id,
      actor = "local user", reason = "結果を画面で探索"), channel = "ui")
    draft_revision(applied$explore_revision)
    notice("表示を更新しました。元の NCA 結果は変更していません。")
  }, error = function(e) notice(paste("表示できません：", desktop_explore_ui_message(e)))))
  selected <- shiny::reactive({
    engine$explore_signal(); x <- engine$explore
    if (is.null(x)) return(NULL)
    tryCatch({ context <- desktop_explore_context(engine, x$source)
      list(selection = x, data = desktop_explore_subset(context$data, x), strata = setdiff(context$strata, as.character(unlist(x$aggregate_over %||% character(), use.names = FALSE))), all_strata = context$strata, period = context$period, treatment = context$treatment, study = context$study, id = context$id, name = context$name, roles = context$roles, role_origin = context$role_origin, manual_order = context$manual_order)
    }, error = function(e) NULL)
  })
  output$active <- shiny::renderUI({
    x <- selected(); if (is.null(x)) return(shiny::p(desktop_i18n_text("表示する結果を選んでください。", "Select a result to display.")))
    shiny::tagList(shiny::p(paste("対象：", x$name, "／", x$selection$parameter, "／", if (isTRUE(x$selection$unit_missing)) "単位欠測" else x$selection$unit,
      "／", if (nzchar(x$selection$group)) x$selection$group else "全体", "／", c(summary = "要約表", boxplot = "箱ひげ図", listing = "結果一覧", qc = "警告・算出不可")[[x$selection$view]], "／ まとめる列:", if (length(x$selection$aggregate_over)) paste(unlist(x$selection$aggregate_over, use.names = FALSE), collapse = ", ") else "なし")),
      if (length(x$selection$aggregate_over)) shiny::p(class = "pk-caption", paste("役割の出どころ：", x$role_origin)),
      if (isTRUE(x$selection$single_treatment_confirmed)) shiny::p(class = "pk-caption", paste("単一処置の申告による例外：", x$selection$single_treatment_reason, "（自己申告）")))
  })
  output$allocation <- shiny::renderUI({
    x <- selected(); if (is.null(x) || !length(x$selection$aggregate_over)) return(NULL)
    cross <- desktop_explore_allocation(x$data, x$id, x$period, x$treatment, x$study)
    matrix <- desktop_explore_allocation_matrix(x$data, x$id, x$period, x$treatment, x$study, if (!is.null(x$manual_order)) x$manual_order$values else NULL)
    sequences <- desktop_explore_sequences(x$data, x$id, x$period, x$treatment, x$study, if (!is.null(x$manual_order)) x$manual_order$values else NULL)
    fallback <- cross$table
    if (is.data.frame(fallback) && nrow(fallback)) {
      columns <- intersect(unique(c(x$study, x$period, x$treatment)), names(fallback))
      fallback <- fallback[desktop_explore_sort_indices(fallback[columns]), , drop = FALSE]
    }
    shiny::div(class = "pk-explore-section",
      shiny::h3(desktop_i18n_text("期×処置の確認（被験者数）", "Period by treatment review (subject counts)")),
      shiny::p("NCA結果にある各期・処置の被験者数（異なるID）を数えています。欠測値の行も含みます。割付表そのものではないため、処置列の役割と値を元の割付・投与情報に照合してください。試験列が未設定ならIDは試験をまたいで一意と仮定します。"),
      if (length(x$treatment)) shiny::p(paste0("処置役割の列：", paste(x$treatment, collapse = "、"))),
      if (cross$no_treatment) shiny::p(class = "text-danger", "処置役割の列がないため、処置別の内訳は表示できません。単一処置の申告と投与情報を確認してください。"),
      if (length(cross$one_level)) shiny::p(class = "text-danger", paste0("処置役割の列で水準が1種類以下です：", paste(cross$one_level, collapse = "、"), "。試験設計と照合してください。")),
      if (length(x$treatment) > 1L) shiny::p("処置役割が複数列のため、期・処置の組合せを行ごとに表示します。"),
      desktop_explore_display_summary(if (is.null(matrix)) fallback else matrix, "期×処置の確認表"),
      shiny::h3(desktop_i18n_text("期順の処置の組合せ（被験者数）", "Treatment sequences in period order (subject counts)")),
      shiny::p(sequences$note),
      if (!is.null(sequences$table)) desktop_explore_display_summary(sequences$table, "期順の処置の表"))
  })
  output$table <- shiny::renderUI({
    x <- selected(); if (is.null(x)) return(NULL)
    d <- x$data; view <- x$selection$view
    if (identical(view, "summary")) {
      subject_mode <- identical(x$selection$summary_basis %||% "analysis_unit", "subject_geomean")
      transformed <- if (subject_mode) tryCatch(desktop_explore_subject_means(d, x$id, x$selection$group, x$strata, x$all_strata), error = identity) else NULL
      if (inherits(transformed, "error")) return(shiny::p(class = "text-danger", paste("被験者別要約を表示できません：", desktop_explore_ui_message(transformed))))
      summarized <- if (subject_mode) transformed$data else d
      return(shiny::tagList(
      shiny::h3(desktop_i18n_text("解析単位ベースの件数", "Counts by analysis unit")),
      shiny::p(paste0("欠測を含む群の件数：値の状態別の内訳は下の表で確認してください。被験者ID数はID列の異なる値の数です。", if (desktop_explore_overview_has_repeated(d, x$selection$group, x$strata, x$id)) "同一IDの複数値がある群は反復測定を含む可能性があり、標本数とばらつきの解釈に注意してください。" else "")),
      desktop_explore_display_summary(desktop_explore_overview(d, x$selection$group, x$strata, x$id), "解析単位の件数表", hide_empty_repeat = !"同一IDの複数値" %in% c(x$strata, x$selection$group)),
      shiny::p(desktop_i18n_text("結果の状態別の統計量：方針割当と警告付き算出値は分けます。0以下を含む区分の幾何統計量は表示しません。TMAXは中央値・範囲を確認してください。", "Statistics by result status: policy-assigned and warning-bearing values are separated. Geometric statistics are not shown for groups containing values at or below zero. Review the median and range for TMAX.")),
      if (subject_mode) shiny::p(paste0("被験者ごとの正の有限値の幾何平均を作り、それらを要約しています。元の解析単位 ", transformed$input_units, "、欠測・非有限 ", transformed$missing_units, "。欠測は各被験者の計算から除きます。下の表の解析単位数・Nは被験者別平均の件数です。得られるCV%は被験者別平均の記述統計で、モデルによる被験者間CVの推定ではありません。")) else if (desktop_explore_has_repeated(d, x$selection$group, x$strata, x$id)) shiny::p("反復測定を含む場合、この幾何CV%は観測値全体の記述統計であり、被験者間CVではありません。"),
      if (subject_mode) shiny::p("使用期・解析単位数が1–1の群では、有限値の統計量は解析単位ベースと同じです。"),
      if (subject_mode && transformed$blocked_groups) shiny::tagList(
        shiny::p(class = "text-danger", paste0("要確認の行を含む ", transformed$blocked_groups, " 群（", transformed$blocked_units, " 解析単位）は群全体を保留しました。該当群の一部だけを除いて計算していません。")),
        shiny::p("処置間の比較には両群の結果が必要です。NCA画面で該当プロファイルを確認し、必要に応じて理由付きで除外して再計算してください。"),
        shiny::h4(desktop_i18n_text("保留の原因となった行（先頭100行）", "Rows causing withholding (first 100 rows)")),
        shiny::div(class = "pk-table-scroll", desktop_display_table(head(transformed$issues, 100L)))),
      shiny::h3(if (subject_mode) "被験者ごとの平均ベースの統計量" else "解析単位ベースの統計量"),
      if (nrow(summarized)) desktop_explore_display_summary(desktop_explore_summary(summarized, x$selection$group, x$strata, x$id,
        if (subject_mode) transformed$counts_column else NULL), "統計量の表", hide_empty_repeat = !"同一IDの複数値" %in% c(x$strata, x$selection$group)) else shiny::p("要約できる群はありません。保留理由を確認してください。")))
    }
    if (identical(view, "boxplot")) return(shiny::p("有限の PK 値を点で表示し、Nが5以上の群に箱を重ねます。対数目盛りでは0以下の値を含む群を表示できません。"))
    if (identical(view, "qc")) {
      warning <- if ("NCA_WARNING" %in% names(d)) !is.na(d$NCA_WARNING) & nzchar(d$NCA_WARNING) else rep(FALSE, nrow(d))
      excluded <- if ("NCA_EXCLUDE" %in% names(d)) !is.na(d$NCA_EXCLUDE) & nzchar(d$NCA_EXCLUDE) else rep(FALSE, nrow(d))
      parameter_problem <- if ("PARAM_STATUS" %in% names(d)) is.na(d$PARAM_STATUS) | d$PARAM_STATUS != "calculated" else rep(FALSE, nrow(d))
      profile_problem <- if ("PROFILE_STATUS" %in% names(d)) is.na(d$PROFILE_STATUS) | d$PROFILE_STATUS != "ok" else rep(FALSE, nrow(d))
      predose_problem <- if ("PREDOSE_QC_STATUS" %in% names(d)) !is.na(d$PREDOSE_QC_STATUS) & !d$PREDOSE_QC_STATUS %in% c("disabled", "pass", "pass_blq") else rep(FALSE, nrow(d))
      d <- d[warning | excluded | parameter_problem | profile_problem | predose_problem, , drop = FALSE]
      if (!nrow(d)) return(shiny::p(desktop_i18n_text("選択したパラメータに警告・算出不可の記録はありません。", "No warning or non-calculable records for the selected parameter.")))
    }
    shiny::tagList(shiny::p(paste("表示：先頭", min(nrow(d), 100L), "行／全", nrow(d), "行")), desktop_display_table(head(d, 100L)))
  })
  output$plot_area <- shiny::renderUI({
    x <- selected(); if (is.null(x) || !identical(x$selection$view, "boxplot")) return(NULL)
    shiny::plotOutput(session$ns("plot"), height = "420px")
  })
  output$plot <- shiny::renderPlot({
    x <- selected(); shiny::req(!is.null(x), identical(x$selection$view, "boxplot"))
    grouped <- desktop_explore_plot_groups(x$data, x$selection$group, x$strata)
    shiny::validate(shiny::need(length(grouped$values) > 0L, "表示できる有限の値がありません。"))
    scale <- x$selection$scale %||% "linear"
    values <- unlist(grouped$values, use.names = FALSE)
    shiny::validate(shiny::need(scale != "log" || all(values > 0), "対数目盛りは0以下の値を含むため表示できません。通常目盛りを選んでください。"))
    old <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old), add = TRUE)
    graphics::par(mar = c(6, 4.5, 2, 1), xpd = FALSE, las = 1)
    limits <- range(values); if (diff(limits) == 0) limits <- if (scale == "log") limits * c(0.8, 1.2) else limits + c(-1, 1) * max(1, abs(limits[1]) * 0.1)
    graphics::plot(NA, xlim = c(0.5, length(grouped$values) + 0.5), ylim = limits,
      log = if (scale == "log") "y" else "", xaxt = "n", xlab = "", ylab = "", main = paste0(x$selection$parameter, " (", x$selection$unit, ")", if (scale == "log") " — 対数目盛り" else ""))
    for (i in seq_along(grouped$values)) {
      v <- grouped$values[[i]]
      if (length(v) >= 5L) graphics::boxplot(v, at = i, add = TRUE, axes = FALSE, outline = FALSE, boxwex = 0.55)
      offset <- grouped$offsets[[i]]
      state <- as.character(grouped$legend[["結果の状態"]][i])
      color <- if (state == "算出値（警告あり）") "#b44b2b" else if (state == "方針割当") "#87611c" else "#286d80"
      graphics::points(rep(i, length(v)) + offset, v, pch = 19, col = color)
    }
    graphics::axis(1, at = seq_along(grouped$values), labels = grouped$labels, las = 1, cex.axis = 1)
  })
  output$legend <- shiny::renderUI({
    x <- selected(); if (is.null(x) || !identical(x$selection$view, "boxplot")) return(NULL)
    grouped <- desktop_explore_plot_groups(x$data, x$selection$group, x$strata)
    if (!length(grouped$values)) return(NULL)
    shiny::tagList(shiny::p("解析単位・結果状態の対応（青：算出値、赤：警告付き、茶：方針割当）"), desktop_explore_display_summary(grouped$legend, "図の対応表"))
  })
})
