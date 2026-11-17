desktop_assembly_profile_columns <- function(data) {
  protected <- c("NCA_ASSEMBLY_PROFILE_COLUMNS", "NCA_ASSEMBLY_PROFILE")
  if (!any(protected %in% names(data))) return(NULL)
  pk_assert(all(protected %in% names(data)), "結合時の解析単位の記録が欠けています。EX・PC・濃度から再作成してください。")
  tokens <- unique(as.character(data$NCA_ASSEMBLY_PROFILE_COLUMNS))
  pk_assert(length(tokens) == 1L && !is.na(tokens), "結合条件が異なる表を混在させることはできません。")
  columns <- tryCatch(jsonlite::fromJSON(tokens), error = function(e) NULL)
  pk_assert(is.character(columns) && length(columns) > 0L && !anyNA(columns) &&
    !anyDuplicated(columns) && all(columns %in% names(data)), "結合時の解析単位の列が欠けています。元の結合結果を使ってください。")
  columns
}

desktop_assembly_reference_columns <- function(data) {
  if (!"NCA_DOSE_REFERENCE_COLUMNS" %in% names(data)) return(NULL)
  fail <- "結合時の基準投与キーの記録が欠落または不正です。EX・PC・濃度から再作成してください。"
  tokens <- unique(as.character(data$NCA_DOSE_REFERENCE_COLUMNS))
  pk_assert(length(tokens) == 1L && !is.na(tokens), fail)
  mapping <- tryCatch(jsonlite::fromJSON(tokens, simplifyVector = FALSE), error = function(e) NULL)
  pk_assert(is.list(mapping) && !is.null(names(mapping)) && !anyDuplicated(names(mapping)) &&
    all(c("ID", "DOSE_ID") %in% names(mapping)) && all(grepl("^[A-Z][A-Z0-9_]*$", names(mapping))) &&
    all(vapply(mapping, function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x), logical(1))), fail)
  columns <- unlist(mapping, use.names = FALSE)
  pk_assert(!anyDuplicated(columns) && all(columns %in% names(data)), fail)
  mapping
}

desktop_assembly_profile_matches <- function(data, columns, profile) {
  remaining <- rep(enc2utf8(profile), nrow(data))
  for (column in columns) {
    tag <- substr(remaining, 1L, 1L)
    if (any(!tag %in% c("c", "n"))) return(FALSE)
    token <- enc2utf8(as.character(data[[column]]))
    numeric_rows <- tag == "n"
    if (any(numeric_rows)) {
      if (is.numeric(data[[column]]) && !inherits(data[[column]], c("POSIXt", "Date"))) {
        value <- data[[column]][numeric_rows]
        if (any(!is.finite(value))) return(FALSE)
      } else {
        parsed <- desktop_pk_numeric(token[numeric_rows])
        if (any(parsed$error != "")) return(FALSE)
        value <- parsed$value
      }
      token[numeric_rows] <- sprintf("%.17g", ifelse(value == 0, 0, value))
    }
    if (anyNA(token) || any(!nzchar(trimws(token)))) return(FALSE)
    expected <- paste0(tag, nchar(token, type = "bytes"), ":", token)
    if (!all(startsWith(remaining, expected))) return(FALSE)
    remaining <- substring(remaining, nchar(expected) + 1L)
  }
  all(!nzchar(remaining))
}

desktop_assembly_validate_context <- function(data, profiles, time, origin = 0) {
  columns <- desktop_assembly_profile_columns(data)
  if (is.null(columns)) return(invisible(NULL))
  pk_assert(setequal(columns, unlist(profiles, use.names = FALSE)),
    paste0("結合時と解析単位が異なります。次の列をすべて指定してください：", paste(columns, collapse = ", ")))
  pk_assert(identical(time, "NCA_TIME_H") && is.numeric(origin) && length(origin) == 1L &&
    is.finite(origin) && origin == 0, "結合済みデータはNCA_TIME_H・投与基準0 hで解析してください。")
  invisible(TRUE)
}

desktop_assembly_validate_nca <- function(data, specification) {
  s <- specification
  columns <- desktop_assembly_profile_columns(data)
  if (is.null(columns)) return(invisible(NULL))
  desktop_assembly_validate_context(data, c(s$id, unlist(s$groups)), s$time, s$time_origin)
  pk_assert(identical(s$time_unit, "h"), "結合済みのNCA_TIME_Hは時間（h）として指定してください。")
  required <- c("NCA_NEXT_DOSE_H", "NCA_DOSE_REF_ID", "NCA_ASSEMBLY_PROFILE", "NCA_ASSEMBLY_STATUS")
  pk_assert(all(required %in% names(data)), "結合時の投与記録が欠けています。EX・PC・濃度から再作成してください。")
  reference_columns <- desktop_assembly_reference_columns(data)
  evidence_v2 <- "NCA_DOSE_EVIDENCE_VERSION" %in% names(data) &&
    any(desktop_pk_numeric(data$NCA_DOSE_EVIDENCE_VERSION)$value == 2, na.rm = TRUE)
  scope <- rep(TRUE, nrow(data))
  if (s$scope == "ss") {
    pk_assert("SSNCAFL" %in% names(data), "SSNCAFL列がありません。")
    scope <- desktop_pk_flag(data$SSNCAFL, "SSNCAFL", s$flag_convention)
  }
  prepared <- identical(s$concentration, "NCA_CONC") && "NCA_BLQ_CONTEXT" %in% names(data)
  if (prepared) scope <- scope & !desktop_pk_flag(data$NCA_ORIGINAL_MDV, "NCA_ORIGINAL_MDV", "strict", scope)
  else if (nzchar(s$mdv_column)) scope <- scope & !desktop_pk_flag(data[[s$mdv_column]], s$mdv_column, s$flag_convention, scope)
  time <- desktop_pk_numeric(data$NCA_TIME_H)$value
  next_dose <- desktop_pk_numeric(data$NCA_NEXT_DOSE_H)
  provenance <- c("NCA_NEXT_EX_ROW", "NCA_NEXT_DOSE_REF_ID")
  pk_assert(!any(provenance %in% names(data)) || all(provenance %in% names(data)),
    "結合時の次回投与の記録が欠けています。EX・PC・濃度から再作成してください。")
  for (profile in unique(data$NCA_ASSEMBLY_PROFILE[!is.na(data$NCA_ASSEMBLY_PROFILE)])) {
    rows <- which(!is.na(data$NCA_ASSEMBLY_PROFILE) & data$NCA_ASSEMBLY_PROFILE == profile)
    pk_assert(desktop_assembly_profile_matches(data[rows, , drop = FALSE], columns, as.character(profile)),
      "結合時の解析単位と現在のID・試験・投与期等の値が一致しません。元の結合結果を使うか、EX・PC・濃度から再作成してください。")
    references <- unique(na.omit(as.character(data$NCA_DOSE_REF_ID[rows])))
    pk_assert(length(references) <= 1L, "同じ解析単位に異なる基準投与があります。投与期・分析対象物の対応を確認してください。")
    pk_assert(all(next_dose$error[rows] %in% c("", "missing")) &&
      length(unique(next_dose$value[rows])) == 1L &&
      all(is.na(next_dose$value[rows]) | next_dose$value[rows] > 0),
      "結合時の次回投与時刻の記録が不正または不一致です。EX・PC・濃度から再作成してください。")
    active <- rows[scope[rows] & is.finite(time[rows]) & time[rows] >= s$start & time[rows] <= (s$end %||% Inf)]
    pk_assert(all(!is.na(data$NCA_ASSEMBLY_STATUS[active]) & data$NCA_ASSEMBLY_STATUS[active] == "ok"),
      "解析に使う行の結合・投与履歴に未解決事項があります。EX・PC・濃度の問題一覧を確認し、不明な日時や同時刻の別投与を整理して再作成してください。")
    if (evidence_v2) pk_assert(!is.null(reference_columns) && length(references) == 1L &&
      !anyNA(data$NCA_DOSE_REF_ID[rows]) &&
      desktop_assembly_profile_matches(data[rows, , drop = FALSE], unlist(reference_columns, use.names = FALSE), references),
      "結合時の基準投与とEXのキー列が一致しないか、記録が欠けています。EX・PC・濃度から再作成してください。")
    if (all(provenance %in% names(data))) {
      next_row <- desktop_pk_numeric(data$NCA_NEXT_EX_ROW[rows])
      next_key <- as.character(data$NCA_NEXT_DOSE_REF_ID[rows])
      missing_key <- is.na(next_key) | !nzchar(trimws(next_key))
      absent <- all(is.na(next_dose$value[rows]))
      pk_assert(length(unique(next_row$value)) == 1L &&
        (if (absent) all(next_row$error == "missing") && all(missing_key) else
          all(next_row$error == "" & next_row$value >= 1 & next_row$value <= .Machine$integer.max & next_row$value == floor(next_row$value)) &&
          !any(missing_key) && length(unique(next_key)) == 1L && !any(next_key %in% references)),
        "結合時の次回投与のEX行・投与キーの記録が不正または不一致です。EX・PC・濃度から再作成してください。")
    }
  }
  invisible(TRUE)
}

desktop_assembly_profile_decision <- function(data, rows, s, time_h, scope, original_mdv) {
  decision <- list(reason = "", next_h = NULL, reference = NULL,
    next_dose = list(ex_row = NULL, key = NULL),
    affected_rows = data.frame(source_row = integer(), conc_row = integer(), pc_row = integer(), time_h = numeric()))
  if (!"NCA_ASSEMBLY_PROFILE" %in% names(data)) return(decision)
  reference <- as.character(data$NCA_DOSE_REF_ID[rows[1L]])
  if (!is.na(reference) && nzchar(reference)) decision$reference <- reference
  next_h <- desktop_pk_numeric(data$NCA_NEXT_DOSE_H[rows[1L]])$value
  if (is.na(next_h)) return(decision)
  decision$next_h <- next_h
  if ("NCA_NEXT_EX_ROW" %in% names(data)) {
    decision$next_dose$ex_row <- desktop_pk_numeric(data$NCA_NEXT_EX_ROW[rows[1L]])$value
    decision$next_dose$key <- as.character(data$NCA_NEXT_DOSE_REF_ID[rows[1L]])
  }
  affected <- rows[scope[rows] & !original_mdv[rows] & is.finite(time_h[rows]) &
    time_h[rows] >= s$start & time_h[rows] <= (s$end %||% Inf) & time_h[rows] >= next_h]
  if (length(affected)) {
    decision$reason <- "intervening_dose"
    source_rows <- function(column) if (column %in% names(data)) desktop_pk_numeric(data[[column]][affected])$value else rep(NA_real_, length(affected))
    decision$affected_rows <- data.frame(source_row = affected, conc_row = source_rows("NCA_CONC_ROW"),
      pc_row = source_rows("NCA_PC_ROW"), time_h = time_h[affected])
  }
  decision
}

desktop_nca_dose_evidence_columns <- function() c("NCA_DOSE_EVIDENCE_VERSION", "NCA_DOSE_REF_ID",
  "NCA_DOSE_COUNT", "NCA_PREV_DOSE_H", "NCA_NEXT_DOSE_H", "NCA_PRIOR_DOSE_COUNT",
  "NCA_LATER_DOSE_COUNT", "NCA_DOSE_HISTORY_CONFIRMED")

desktop_nca_dose_evidence <- function(data, rows, settings) {
  evidence <- list(version = NULL, reference = NULL, count = NULL, previous_h = NULL, next_h = NULL,
    prior_count = NULL, later_count = NULL, source_complete = FALSE,
    provenance = if ("NCA_ASSEMBLY_PROFILE" %in% names(data)) "assembly" else "declared_raw", reason = "")
  fail <- function(reason) { evidence$reason <- reason; evidence }
  required <- desktop_nca_dose_evidence_columns()
  if (!all(required %in% names(data)) || !length(rows)) return(fail("dose_evidence_requires_reassembly"))
  if (!identical(settings$time_unit, "h") || settings$time_origin != 0)
    return(fail("dose_evidence_time_context_mismatch"))
  if (evidence$provenance == "assembly") {
    profiles <- unique(as.character(data$NCA_ASSEMBLY_PROFILE[rows]))
    if (length(profiles) != 1L || is.na(profiles) || !nzchar(profiles) ||
        !setequal(rows, which(!is.na(data$NCA_ASSEMBLY_PROFILE) & data$NCA_ASSEMBLY_PROFILE == profiles)) ||
        !desktop_assembly_profile_matches(data[rows, , drop = FALSE], desktop_assembly_profile_columns(data), profiles))
      return(fail("invalid_dose_reference_evidence"))
  }
  reference <- unique(as.character(data$NCA_DOSE_REF_ID[rows]))
  if (length(reference) != 1L || is.na(reference) || !nzchar(trimws(reference)))
    return(fail("invalid_dose_reference_evidence"))
  evidence$reference <- reference
  fields <- c(version = "NCA_DOSE_EVIDENCE_VERSION", count = "NCA_DOSE_COUNT", previous_h = "NCA_PREV_DOSE_H",
    next_h = "NCA_NEXT_DOSE_H", prior_count = "NCA_PRIOR_DOSE_COUNT", later_count = "NCA_LATER_DOSE_COUNT")
  for (name in names(fields)) {
    parsed <- desktop_pk_numeric(data[[fields[[name]]]][rows])
    optional <- name %in% c("previous_h", "next_h")
    if (any(!parsed$error %in% if (optional) c("", "missing") else "") || length(unique(parsed$value)) != 1L)
      return(fail("inconsistent_dose_evidence"))
    evidence[name] <- list(if (is.na(parsed$value[1])) NULL else parsed$value[1])
  }
  if (!identical(evidence$version, 2)) return(fail("dose_evidence_requires_reassembly"))
  if (evidence$provenance == "assembly") {
    reference_columns <- desktop_assembly_reference_columns(data)
    if (is.null(reference_columns)) return(fail("dose_evidence_requires_reassembly"))
    if (!desktop_assembly_profile_matches(data[rows, , drop = FALSE], unlist(reference_columns, use.names = FALSE), reference))
      return(fail("invalid_dose_reference_evidence"))
  }
  counts <- unlist(evidence[c("count", "prior_count", "later_count")], use.names = FALSE)
  if (any(!is.finite(counts) | counts != floor(counts) | counts < 0 | counts > .Machine$integer.max) || evidence$count < 1 ||
      evidence$count != 1 + evidence$prior_count + evidence$later_count)
    return(fail("inconsistent_dose_evidence"))
  if ((evidence$prior_count == 0) != is.null(evidence$previous_h) ||
      (evidence$later_count == 0) != is.null(evidence$next_h) ||
      (!is.null(evidence$previous_h) && evidence$previous_h >= 0) ||
      (!is.null(evidence$next_h) && evidence$next_h <= 0)) return(fail("inconsistent_dose_evidence"))
  confirmed <- tryCatch(desktop_pk_flag(data$NCA_DOSE_HISTORY_CONFIRMED[rows], "NCA_DOSE_HISTORY_CONFIRMED"), error = function(e) NULL)
  if (is.null(confirmed) || !all(confirmed)) return(fail("source_dose_history_unconfirmed"))
  if ("NCA_ASSEMBLY_STATUS" %in% names(data) && any(is.na(data$NCA_ASSEMBLY_STATUS[rows]) | data$NCA_ASSEMBLY_STATUS[rows] != "ok"))
    return(fail("source_dose_history_unconfirmed"))
  evidence$source_complete <- TRUE
  evidence
}
