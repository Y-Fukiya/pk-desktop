desktop_prepare_column <- function(data, column, label, optional = FALSE) {
  pk_assert(is.character(column) && length(column) == 1L && !is.na(column) &&
    ((optional && !nzchar(column)) || column %in% names(data)), paste0(label, "の列を選択してください。"))
  column
}

desktop_fixed_offset <- function(x) {
  pk_assert(is.character(x) && !anyNA(x) && all(grepl("^[+-](0[0-9]|1[0-4]):[0-5][0-9]$", x)),
    "時差は+09:00などの固定UTCオフセットで指定してください。")
  hours <- as.integer(substr(x, 2L, 3L)); minutes <- as.integer(substr(x, 5L, 6L))
  pk_assert(all(hours < 14L | minutes == 0L), "UTCオフセットは±14:00以内です。")
  (hours * 3600 + minutes * 60) * ifelse(substr(x, 1L, 1L) == "-", -1, 1)
}

desktop_timestamp_parse <- function(x, offset, mode = "iso") {
  pk_assert(is.character(mode) && length(mode) == 1L && mode %in% c("iso", "sas_seconds", "unix_seconds"), "日時形式はiso・sas_seconds・unix_secondsから選択してください。")
  assumed_offset <- desktop_fixed_offset(offset)
  n <- length(x)
  value <- rep(NA_real_, n); reason <- rep("", n); assumed <- rep(FALSE, n)
  precision <- rep(NA_character_, n); type <- rep(mode, n)
  if (inherits(x, "POSIXt")) {
    value <- as.numeric(as.POSIXct(x)); type[] <- "POSIXct"; precision[] <- "seconds"
    reason[is.na(value)] <- "missing"; reason[!is.na(value) & !is.finite(value)] <- "invalid_numeric"
    sas_format <- attr(x, "format.sas", exact = TRUE) %||% ""
    sas_datetime <- length(sas_format) == 1L && !is.na(sas_format) && nzchar(trimws(sas_format))
    if (mode == "sas_seconds") {
      if (sas_datetime) {
        # haven has already converted the epoch, but SAS carries no time zone.
        value <- value - assumed_offset
        type[] <- "SAS_DATETIME"; assumed[reason == ""] <- TRUE
      } else reason[reason == ""] <- "sas_mode_requires_sas_datetime_metadata"
    } else if (sas_datetime && mode == "iso") {
      reason[reason == ""] <- "sas_datetime_requires_clock_mode"
    } else if (sas_datetime) type[] <- "SAS_DATETIME_as_UTC"
  } else if (mode != "iso") {
    parsed <- desktop_pk_numeric(x)
    value <- parsed$value; reason <- parsed$error; precision[] <- "seconds"
    if (mode == "sas_seconds") {
      value <- value + as.numeric(as.Date("1960-01-01")) * 86400 - assumed_offset
      assumed[reason == ""] <- TRUE
    }
  } else {
    text <- trimws(as.character(x))
    missing <- is.na(text) | text %in% c("", "NA")
    reason[missing] <- "missing"
    reason[!missing] <- "invalid_format"
    # Date-only/partial SDTM values stay missing. Minute precision has explicit zero seconds.
    pattern <- "^([0-9]{4}-[0-9]{2}-[0-9]{2})[T ]([0-9]{2}):([0-9]{2})(?::([0-9]{2}(?:\\.[0-9]+)?))?(Z|[+-][0-9]{2}:[0-9]{2})?$"
    indices <- which(!missing)
    parts <- regmatches(text[indices], regexec(pattern, text[indices], perl = TRUE))
    matched <- lengths(parts) == 6L
    if (any(matched)) {
      rows <- indices[matched]; fields <- do.call(rbind, parts[matched])
      dates <- suppressWarnings(as.Date(fields[, 2L], format = "%Y-%m-%d"))
      h <- as.numeric(fields[, 3L]); m <- as.numeric(fields[, 4L])
      sec <- ifelse(fields[, 5L] == "", 0, as.numeric(fields[, 5L]))
      date_ok <- !is.na(dates) & format(dates, "%Y-%m-%d") == fields[, 2L] & substr(fields[, 2L], 1L, 4L) != "0000"
      valid <- date_ok & h < 24 & m < 60 & sec < 60
      offsets <- fields[, 6L]; no_offset <- offsets == ""
      offsets[no_offset] <- offset; offsets[offsets == "Z"] <- "+00:00"
      offset_ok <- grepl("^[+-](0[0-9]|1[0-4]):[0-5][0-9]$", offsets) &
        (substr(offsets, 2L, 3L) != "14" | substr(offsets, 5L, 6L) == "00")
      reason[rows] <- ifelse(!offset_ok, "invalid_offset", "invalid_datetime")
      use <- valid & offset_ok
      if (any(use)) {
        good <- rows[use]
        value[good] <- as.numeric(dates[use]) * 86400 + h[use] * 3600 + m[use] * 60 + sec[use] - desktop_fixed_offset(offsets[use])
        reason[good] <- ""; assumed[good] <- no_offset[use]
        precision[good] <- ifelse(fields[use, 5L] == "", "minute_seconds_zero", ifelse(grepl("\\.", fields[use, 5L]), "fractional_seconds", "seconds"))
      }
    }
  }
  reason[!is.na(value) & !is.finite(value)] <- "invalid_numeric"
  value[reason != ""] <- NA_real_
  list(value = value, reason = reason, offset_assumed = assumed, precision = precision, type = type)
}

desktop_timestamp_seconds <- function(x, offset, label) {
  parsed <- desktop_timestamp_parse(x, offset)
  pk_assert(!any(!parsed$reason %in% c("", "missing")), paste0(label, "に不正な日時があります。"))
  parsed$value
}

desktop_prepare_elapsed <- function(data, step) {
  sample <- desktop_prepare_column(data, step$sample_column, "採血日時")
  dose <- desktop_prepare_column(data, step$dose_column, "投与日時")
  name <- step$output %||% "NCA_TIME_H"
  pk_assert(is.character(name) && length(name) == 1L && !is.na(name) && nzchar(name) &&
    identical(trimws(name), name) && !name %in% names(data), "経過時間の新しい列名が空欄・重複しています。元の列には上書きできません。")
  offset <- step$utc_offset %||% "+09:00"
  pk_assert(length(offset) == 1L, "日時の固定UTCオフセットは1つ指定してください。")
  desktop_fixed_offset(offset)
  invalid <- step$invalid_datetime %||% "missing"
  pk_assert(is.character(invalid) && length(invalid) == 1L && invalid %in% c("missing", "error"), "不正な日時の処置はmissingまたはerrorです。")
  sample_time <- desktop_timestamp_parse(data[[sample]], offset, step$sample_datetime_type %||% "iso")
  dose_time <- desktop_timestamp_parse(data[[dose]], offset, step$dose_datetime_type %||% "iso")
  bad <- !sample_time$reason %in% c("", "missing") | !dose_time$reason %in% c("", "missing")
  pk_assert(invalid != "error" || !any(bad), paste0("不正な日時があります（行 ", paste(which(bad), collapse = ", "), "）。"))
  data[[name]] <- (sample_time$value - dose_time$value) / 3600
  status <- ifelse(is.na(sample_time$value) & is.na(dose_time$value), "missing_both",
    ifelse(is.na(sample_time$value), "missing_sample", ifelse(is.na(dose_time$value), "missing_dose", "derived")))
  status[bad] <- "invalid_datetime"
  rows <- data.frame(ROW = seq_len(nrow(data)), STATUS = status,
    SAMPLE_UTC_SECONDS = sample_time$value, DOSE_UTC_SECONDS = dose_time$value, TIME_H = data[[name]],
    SAMPLE_OFFSET_ASSUMED = sample_time$offset_assumed, DOSE_OFFSET_ASSUMED = dose_time$offset_assumed,
    SAMPLE_PRECISION = sample_time$precision, DOSE_PRECISION = dose_time$precision,
    SAMPLE_TYPE = sample_time$type, DOSE_TYPE = dose_time$type,
    SAMPLE_REASON = sample_time$reason, DOSE_REASON = dose_time$reason, stringsAsFactors = FALSE)
  list(data = data, rows = rows, summary = as.data.frame(table(STATUS = status), stringsAsFactors = FALSE))
}

desktop_pk_default_groups <- function(columns, id = NULL) {
  setdiff(intersect(c("STUDYID", "ANALYTE", "PERIOD", "APERIOD"), columns), id)
}

desktop_pk_time_hint <- function(data, column, language = "ja") {
  if (length(column) != 1L || !column %in% names(data)) return(if (identical(language, "en")) "Select a time column. Only columns convertible to numbers are shown." else "時間列を選択してください。数値に変換できる列だけを表示します。")
  parsed <- desktop_pk_numeric(data[[column]])
  numbers <- parsed$value[is.finite(parsed$value)]
  range <- if (length(numbers)) paste(format(range(numbers), digits = 7, trim = TRUE), collapse = " ～ ") else if (identical(language, "en")) "no valid numbers" else "有効な数値なし"
  if (identical(language, "en")) paste0(column, " [", paste(class(data[[column]]), collapse = "/"), "] range ", range, " / missing ", sum(parsed$error == "missing"), " rows")
  else paste0(column, " [", paste(class(data[[column]]), collapse = "/"), "] 範囲 ", range, " / 欠測 ", sum(parsed$error == "missing"), " 行")
}

desktop_pk_time_choices <- function(data, excluded = character()) {
  if (!is.data.frame(data)) return(character())
  excluded <- unique(c(excluded, "ID", "USUBJID", "SUBJID", "Subject", desktop_pk_default_groups(names(data)),
    names(data)[toupper(names(data)) %in% c("BLQ", "BLQFL", "MDV", "LLOQ",
      "NCA_CONC", "NCA_BLQ", "NCA_MDV", "NCA_LLOQ_USED", "NCA_ORIGINAL_CONC", "NCA_ORIGINAL_MDV",
      "NCA_BLQ_RULE", "NCA_BLQ_POSITION", "NCA_BLQ_CONTEXT", "NCA_BLQ_PROFILE_SIGNATURE", "NCA_BLQ_BASELINE")]))
  columns <- setdiff(names(data), excluded)
  columns <- columns[vapply(data[columns], function(x) {
    parsed <- desktop_pk_numeric(x)
    all(parsed$error %in% c("", "missing")) && (any(parsed$error == "") || is.numeric(x)) && !is.logical(x)
  }, logical(1))]
  stats::setNames(columns, columns)
}

desktop_pk_blq_json <- function(value) {
  ordered <- function(x) {
    if (!is.list(x)) return(x)
    if (!is.null(names(x))) x <- x[order(names(x))]
    lapply(x, ordered)
  }
  as.character(jsonlite::toJSON(ordered(value), auto_unbox = TRUE, na = "null", null = "null", digits = NA))
}

desktop_pk_blq_json_atoms <- function(x) {
  encoded <- as.character(jsonlite::toJSON(x, auto_unbox = FALSE, na = "null", null = "null"))
  tokens <- regmatches(encoded, gregexpr('"(?:[^"\\\\]|\\\\.)*"|null', encoded, perl = TRUE))[[1L]]
  pk_assert(length(tokens) == length(x), "BLQ前処理のJSON値を符号化できません。")
  tokens
}

desktop_pk_blq_rows <- function(canonical, named = FALSE, n = if (length(canonical)) length(canonical[[1L]]) else 0L) {
  if (!n) return(character())
  rows <- rep("", n)
  names_json <- if (named) desktop_pk_blq_json_atoms(names(canonical)) else NULL
  for (j in seq_along(canonical)) {
    tokens <- desktop_pk_blq_json_atoms(canonical[[j]])
    if (named) tokens <- paste0(names_json[j], ":", tokens)
    rows <- paste0(rows, if (j > 1L) "," else "", tokens)
  }
  if (named) paste0("{", rows, "}") else paste0("[", rows, "]")
}

desktop_pk_blq_baseline_json <- function(canonical, i, n) {
  if (!length(i)) return(character())
  paste0('{"i":', sprintf("%.0f", i), ',"n":', sprintf("%.0f", n), ',"v":', desktop_pk_blq_rows(canonical), '}')
}

desktop_pk_blq_profile_signatures <- function(row_hash, group, context_json = NULL) {
  signature <- character(length(group))
  for (g in unique(group)) {
    rows <- which(group == g)
    signature[rows] <- digest::digest(paste(c(context_json, sort(row_hash[rows])), collapse = "\n"), algo = "sha256", serialize = FALSE)
  }
  signature
}

desktop_pk_blq_columns <- function(context) {
  step <- context$step
  columns <- c(unlist(context$profile_columns, use.names = FALSE), step$concentration, step$blq_column,
    step$time, step$mdv_column, step$predose_column, if (identical(step$lloq_source, "column")) step$lloq_column,
    "NCA_CONC", "NCA_BLQ", "NCA_MDV", "NCA_BLQ_RULE", "NCA_BLQ_POSITION", "NCA_LLOQ_USED", "NCA_ORIGINAL_CONC", "NCA_ORIGINAL_MDV",
    unlist(context$dose_columns, use.names = FALSE))
  sort(unique(columns[!is.na(columns) & nzchar(columns)]))
}

desktop_pk_blq_context <- function(data) {
  fail <- "BLQ前処理の条件記録がありません、または不正です。元データからBLQ前処理を再適用してください。"
  pk_assert("NCA_BLQ_CONTEXT" %in% names(data) && nrow(data) > 0L, fail)
  texts <- unique(as.character(data$NCA_BLQ_CONTEXT))
  pk_assert(!anyNA(texts) && all(nzchar(texts)), fail)
  contexts <- lapply(texts, function(text) tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL))
  scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  for (context in contexts) {
    pk_assert(is.list(context) && is.numeric(context$version) && length(context$version) == 1L &&
      !is.na(context$version) && context$version %in% c(1, 2), fail)
    fields <- c("version", "mode", "profile_columns", "time_column", "time_origin_h", "step", if (context$version == 2) "columns", if (!is.null(context$dose_columns)) "dose_columns")
    pk_assert(identical(sort(names(context)), sort(fields)), fail)
    keys <- unlist(context$profile_columns, use.names = FALSE) %||% character()
    pk_assert(scalar(context$mode) && context$mode %in% c("legacy", "positional") &&
      is.list(context$profile_columns) && is.null(names(context$profile_columns)) &&
      all(vapply(context$profile_columns, scalar, logical(1))) && is.character(keys) && !anyNA(keys) && !anyDuplicated(keys) &&
      all(nzchar(keys)) && identical(keys, sort(keys)) && scalar(context$time_column) &&
      is.numeric(context$time_origin_h) && length(context$time_origin_h) == 1L && is.finite(context$time_origin_h) && is.list(context$step) &&
      !is.null(names(context$step)) && !anyDuplicated(names(context$step)) && all(nzchar(names(context$step))), fail)
    pk_assert((context$mode == "legacy" && !length(keys)) || (context$mode == "positional" && length(keys) > 0L), fail)
    step <- context$step
    optional <- c("mdv_column", "predose_column", "lloq_source", "lloq_column")
    pk_assert(all(vapply(step[intersect(names(step), optional)], function(x) is.null(x) || (is.character(x) && length(x) == 1L && !is.na(x)), logical(1))) &&
      scalar(step$concentration) && scalar(step$blq_column) && identical(step$op, "blq_rules") &&
      identical(step$mode %||% "legacy", context$mode) && identical(step$time, context$time_column) &&
      is.numeric(step$time_origin %||% 0) && identical(as.numeric(step$time_origin %||% 0), as.numeric(context$time_origin_h)) &&
      (context$mode != "positional" || identical(sort(unlist(step$profile_columns, use.names = FALSE)), keys)), fail)
    if (!is.null(context$dose_columns)) pk_assert(context$version == 2 && is.list(context$dose_columns) &&
      is.null(names(context$dose_columns)) && all(vapply(context$dose_columns, scalar, logical(1))) &&
      !anyDuplicated(unlist(context$dose_columns)) && all(unlist(context$dose_columns) %in% c(desktop_nca_dose_evidence_columns(),
        "NCA_ASSEMBLY_PROFILE", "NCA_ASSEMBLY_PROFILE_COLUMNS", "NCA_ASSEMBLY_STATUS", "NCA_ASSEMBLY_REASON",
        "NCA_DOSE_REFERENCE_COLUMNS", "NCA_NEXT_EX_ROW", "NCA_NEXT_DOSE_REF_ID", unlist(desktop_assembly_reference_columns(data), use.names = FALSE))), fail)
    if (context$version == 2) pk_assert(is.list(context$columns) && is.null(names(context$columns)) &&
      all(vapply(context$columns, scalar, logical(1))) && identical(unlist(context$columns, use.names = FALSE), desktop_pk_blq_columns(context)), fail)
  }
  pk_assert(length(unique(vapply(contexts, desktop_pk_blq_json, character(1)))) == 1L, fail)
  contexts[[1L]]
}

desktop_pk_blq_canonical <- function(data, columns, keys) {
  missing <- setdiff(columns, names(data))
  pk_assert(!length(missing), paste0("BLQ前処理に使用した元の列・処理列がありません: ", paste(missing, collapse = ", "), "。元データからBLQ前処理を再適用してください。"))
  canonical <- lapply(columns, function(column) {
    source <- data[[column]]
    token <- if (is.numeric(source)) ifelse(is.na(source), NA_character_, sprintf("%.17g", source)) else as.character(source)
    if (!column %in% keys) {
      parsed <- desktop_pk_numeric(token)
      valid <- parsed$error == ""
      token[valid] <- sprintf("%.17g", parsed$value[valid])
      token[parsed$error == "missing"] <- NA_character_
    }
    token
  })
  stats::setNames(canonical, columns)
}

desktop_pk_blq_baselines <- function(data, context) {
  fail <- "BLQ前処理のNCA_BLQ_BASELINE記録が欠落・不正です（元の値は未検証）。元データからBLQ前処理を再適用してください。"
  pk_assert("NCA_BLQ_BASELINE" %in% names(data), fail)
  integer_value <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 1 && x <= .Machine$integer.max && x == floor(x)
  lapply(as.character(data$NCA_BLQ_BASELINE), function(text) {
    value <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
    pk_assert(is.list(value) && identical(sort(names(value)), c("i", "n", "v")) &&
      integer_value(value$i) && integer_value(value$n) && value$i <= value$n &&
      is.list(value$v) && is.null(names(value$v)) && length(value$v) == length(context$columns) &&
      all(vapply(value$v, function(x) is.null(x) || (is.character(x) && length(x) == 1L && !is.na(x)), logical(1))), fail)
    value
  })
}

desktop_pk_blq_signatures <- function(data, context) {
  keys <- unlist(context$profile_columns, use.names = FALSE)
  columns <- desktop_pk_blq_columns(context)
  canonical <- desktop_pk_blq_canonical(data, columns, keys)
  if (context$version == 1) {
    group <- if (length(keys)) desktop_pk_group(data, keys)$group else rep(1L, nrow(data))
    row_hash <- vapply(desktop_pk_blq_rows(canonical, named = TRUE), digest::digest, character(1), algo = "sha256", serialize = FALSE)
    context_json <- NULL
    baselines <- NULL
  } else {
    baselines <- desktop_pk_blq_baselines(data, context)
    original <- lapply(seq_along(columns), function(j) vapply(baselines, function(x) x$v[[j]] %||% NA_character_, character(1)))
    names(original) <- columns
    identities <- desktop_pk_blq_rows(original[keys], n = nrow(data))
    group <- match(identities, unique(identities))
    row_hash <- desktop_pk_blq_baseline_json(original, vapply(baselines, `[[`, numeric(1), "i"), vapply(baselines, `[[`, numeric(1), "n"))
    context_json <- desktop_pk_blq_json(context)
  }
  signature <- desktop_pk_blq_profile_signatures(row_hash, group, context_json)
  list(signature = signature, group = group, baselines = baselines, canonical = canonical)
}

desktop_pk_blq_validate <- function(data, s, scope = NULL) {
  context <- desktop_pk_blq_context(data)
  reapply <- "元データから、NCAと同じ解析単位・時間設定でBLQ前処理を再適用してください。"
  keys <- sort(c(s$id, unlist(s$groups, use.names = FALSE)))
  expected_keys <- unlist(context$profile_columns, use.names = FALSE)
  if (context$mode == "positional") pk_assert(identical(keys, expected_keys), paste0("BLQ前処理とNCAの解析単位が異なります。記録: ", paste(expected_keys, collapse = ", "), " / 選択: ", paste(keys, collapse = ", "), "。", reapply))
  pk_assert(identical(s$time, context$time_column) && identical(s$time_unit %||% "h", "h") &&
    identical(as.numeric(s$time_origin %||% 0), as.numeric(context$time_origin_h)), paste0("BLQ前処理とNCAの時間設定が異なります。記録: ", context$time_column, " / h / ", context$time_origin_h, "、選択: ", s$time, " / ", s$time_unit %||% "h", " / ", s$time_origin %||% 0, "。", reapply))
  pk_assert("NCA_BLQ_PROFILE_SIGNATURE" %in% names(data), paste("BLQ前処理の解析単位記録がありません。", reapply))
  current <- desktop_pk_blq_signatures(data, context)
  supplied <- as.character(data$NCA_BLQ_PROFILE_SIGNATURE)
  if (is.null(scope) && identical(s$scope, "ss")) {
    pk_assert("SSNCAFL" %in% names(data), "SSNCAFL列がありません。")
    scope <- desktop_pk_flag(data$SSNCAFL, "SSNCAFL", s$flag_convention %||% "strict")
  }
  if (!is.null(scope)) pk_assert(is.logical(scope) && length(scope) == nrow(data) && !anyNA(scope), "NCA対象範囲を確認してください。")
  issues <- list()
  add <- function(kind, identity, detail) { issues[[length(issues) + 1L]] <<- list(kind = kind, identity = identity, detail = detail) }
  bounded <- function(x) paste(c(head(x, 12L), if (length(x) > 12L) paste0("ほか", length(x) - 12L, "件")), collapse = ", ")
  for (g in unique(current$group)) {
    rows <- which(current$group == g)
    if (context$version == 1) {
      identity <- if (length(expected_keys)) paste(paste0(expected_keys, "=", vapply(data[rows[1], expected_keys, drop = FALSE], as.character, character(1))), collapse = ", ") else "全行"
      if (anyNA(supplied[rows]) || !identical(supplied[rows], current$signature[rows])) add("legacy", identity, "行・元データ・処理値が変わっています。旧形式v1のため変更列と元の解析単位は特定できません")
    } else {
      baselines <- current$baselines[rows]
      original <- baselines[[1]]$v[match(expected_keys, unlist(context$columns))]
      identity <- if (length(expected_keys)) paste(paste0(expected_keys, "=", vapply(original, function(x) x %||% "<欠測>", character(1))), collapse = ", ") else "全行"
      n <- unique(vapply(baselines, function(x) x$n, numeric(1)))
      ordinals <- vapply(baselines, function(x) x$i, numeric(1))
      complete <- length(n) == 1L && length(rows) == n && !anyDuplicated(ordinals)
      if (!complete) {
        duplicates <- unique(ordinals[duplicated(ordinals)])
        detail <- paste0("行構成が変更されています: 記録行数=", bounded(n), " / 現在行数=", length(rows))
        if (length(n) == 1L && n <= 1000000) detail <- paste0(detail, " / 欠落行番号=", bounded(setdiff(seq_len(n), ordinals)))
        if (length(duplicates)) detail <- paste0(detail, " / 重複行番号=", bounded(duplicates))
        add("membership", identity, paste0(detail, "（記録上の元の解析単位。完全な整合性は未検証）"))
      } else if (anyNA(supplied[rows]) || !identical(supplied[rows], current$signature[rows])) {
        add("metadata", identity, "条件・ベースライン・署名の記録が破損しています（元の値は未検証）")
      } else {
        changed <- lapply(seq_along(rows), function(j) {
          baseline <- vapply(baselines[[j]]$v, function(x) x %||% NA_character_, character(1))
          now <- vapply(current$canonical, `[`, character(1), rows[j])
          names(now)[!(is.na(now) & is.na(baseline)) & (is.na(now) != is.na(baseline) | (!is.na(now) & !is.na(baseline) & now != baseline))]
        })
        columns <- unique(unlist(changed, use.names = FALSE))
        if (length(columns)) {
          detail <- paste0("変更列: ", bounded(columns), " / 現在の行: ", bounded(rows[lengths(changed) > 0L]))
          changed_keys <- intersect(columns, expected_keys)
          if (length(changed_keys)) {
            moved_rows <- rows[vapply(changed, function(x) any(x %in% expected_keys), logical(1))]
            destinations <- lapply(moved_rows, function(i) vapply(current$canonical[expected_keys], `[`, character(1), i))
            unique_destinations <- unique(destinations)
            movements <- vapply(unique_destinations, function(destination) {
              count <- sum(vapply(destinations, identical, logical(1), destination))
              label <- paste(paste0(expected_keys, "=", ifelse(is.na(destination), "<欠測>", destination)), collapse = ", ")
              paste0(identity, " → ", label, "（", count, "行）")
            }, character(1))
            detail <- paste0(detail, " / 解析単位の移動先: ", bounded(movements))
          }
          add("changed_columns", identity, detail)
        }
      }
    }
    if (!is.null(scope) && any(scope[rows]) && !all(scope[rows]))
      add("scope", identity, paste0("NCA対象範囲が解析単位の一部だけです: 選択 ", sum(scope[rows]), " / 全 ", length(rows), " 行"))
  }
  if (length(issues)) {
    messages <- vapply(head(issues, 8L), function(x) paste0("[", substr(x$identity, 1L, 500L), "] ", substr(x$detail, 1L, 1800L)), character(1))
    pk_assert(FALSE, paste(c("BLQ前処理後の確認が必要です。", messages, if (length(issues) > 8L) paste0("ほか ", length(issues) - 8L, " 件"), reapply), collapse = "\n"))
  }
  invisible(context)
}

desktop_pk_lloq <- function(data, source = "", column = "", value = NULL) {
  pk_assert(is.character(source) && length(source) == 1L && !is.na(source) && source %in% c("", "column", "constant"), "LLOQの参照元を選択してください。")
  if (!nzchar(source)) return(list(value = rep(NA_real_, nrow(data)), reason = rep("missing_lloq", nrow(data)), source = ""))
  if (source == "column") {
    column <- desktop_prepare_column(data, column, "LLOQ")
    input <- data[[column]]
    label <- paste0("column:", column)
  } else {
    pk_assert(is.null(value) || length(value) <= 1L, "LLOQの定数を1つ指定してください。")
    input <- rep(value %||% NA_real_, length.out = nrow(data))
    label <- "constant"
  }
  parsed <- desktop_pk_numeric(input)
  reason <- ifelse(parsed$error == "missing", "missing_lloq", ifelse(parsed$error != "" | !is.finite(parsed$value) | parsed$value <= 0, "invalid_lloq", ""))
  list(value = ifelse(reason == "", parsed$value, NA_real_), reason = reason, source = label)
}

desktop_pk_blq_lloq <- function(data) {
  context <- desktop_pk_blq_context(data)
  step <- context$step
  source <- step$lloq_source %||% "constant"
  evidence <- desktop_pk_lloq(data, source, step$lloq_column %||% "", step$lloq_value)
  non_lloq_context <- context
  non_lloq_context$step$lloq_source <- "constant"
  if (context$version == 1 && (source == "constant" ||
      (source == "column" && step$lloq_column %in% desktop_pk_blq_columns(non_lloq_context)))) {
    evidence$value[] <- NA_real_
    evidence$reason[] <- "unverified_lloq"
    evidence$source <- ""
  }
  evidence
}

desktop_prepare_blq <- function(data, step) {
  desktop_assembly_validate_context(data, step$profile_columns, step$time, step$time_origin %||% 0)
  concentration <- desktop_prepare_column(data, step$concentration, "濃度")
  blq_column <- desktop_prepare_column(data, step$blq_column, "BLQフラグ")
  time_column <- desktop_prepare_column(data, step$time, "経過時間")
  mdv_column <- desktop_prepare_column(data, step$mdv_column %||% "", "既存のMDV", TRUE)
  predose_column <- desktop_prepare_column(data, step$predose_column %||% "", "投与前フラグ", TRUE)
  forbidden_time <- unique(c("ID", "USUBJID", "SUBJID", "Subject", desktop_pk_default_groups(names(data)),
    unlist(step$profile_columns, use.names = FALSE), concentration, blq_column, mdv_column, predose_column,
    names(data)[toupper(names(data)) %in% c("BLQ", "BLQFL", "MDV", "LLOQ")],
    if (identical(step$lloq_source, "column")) step$lloq_column))
  pk_assert(!time_column %in% forbidden_time, "時間列にはID・解析単位・濃度・フラグ列を指定できません。経過時間（h）の列を選択してください。")
  outputs <- c("NCA_CONC", "NCA_BLQ", "NCA_MDV", "NCA_BLQ_RULE", "NCA_BLQ_POSITION", "NCA_LLOQ_USED", "NCA_ORIGINAL_CONC", "NCA_ORIGINAL_MDV", "NCA_BLQ_CONTEXT", "NCA_BLQ_PROFILE_SIGNATURE", "NCA_BLQ_BASELINE")
  pk_assert(!any(outputs %in% names(data)), "NCAの前処理列は新規作成用です。既存の列には上書きできません。")
  mode <- step$mode %||% "legacy"
  pk_assert(is.character(mode) && length(mode) == 1L && mode %in% c("legacy", "positional"), "BLQ分類はlegacyまたはpositionalです。")
  actions <- c("keep", "zero", "half_lloq", "exclude")
  rule_names <- if (mode == "legacy") c("pre_rule", "post_rule") else c("pre_rule", "first_rule", "middle_rule", "last_rule")
  rules <- step[rule_names]
  if (mode == "positional") rules$all_blq_rule <- step$all_blq_rule %||% step$first_rule
  for (rule in rules) pk_assert(is.character(rule) && length(rule) == 1L && !is.na(rule) && rule %in% actions, "BLQ規則を選択してください。")
  zero <- step$zero_time %||% "error"
  pk_assert(is.character(zero) && length(zero) == 1L && !is.na(zero) && zero %in% c("error", "pre", "post"), "時刻0の扱いを選択してください。")
  origin <- step$time_origin %||% 0
  pk_assert(is.numeric(origin) && length(origin) == 1L && is.finite(origin), "投与基準時刻は有限の時間（h）です。")
  convention <- step$flag_convention %||% "strict"
  mdv <- if (nzchar(mdv_column)) desktop_pk_flag(data[[mdv_column]], mdv_column, convention) else rep(FALSE, nrow(data))
  blq <- desktop_pk_flag(data[[blq_column]], blq_column, convention, !mdv)
  numeric_values <- function(x, use, label) {
    x <- as.character(x); x[is.na(x) | trimws(x) %in% c("", "NA")] <- NA_character_
    answer <- rep(NA_real_, length(x))
    if (any(use)) answer[use] <- pk_number(x[use], label, allow_na = TRUE)
    answer
  }
  time <- numeric_values(data[[time_column]], !mdv, "経過時間")
  original <- numeric_values(data[[concentration]], !mdv & !blq, "濃度")
  pk_assert(all(is.na(original) | original >= 0), "負の濃度は使用できません。")
  period <- rep(NA_character_, nrow(data))
  if (nzchar(predose_column)) {
    period[!mdv] <- ifelse(desktop_pk_flag(data[[predose_column]], predose_column, convention, !mdv)[!mdv], "pre", "post")
  } else {
    period[!is.na(time) & time < origin] <- "pre"
    period[!is.na(time) & time > origin] <- "post"
    at_zero <- !mdv & blq & !is.na(time) & time == origin
    pk_assert(zero != "error" || !any(at_zero), "BLQ行の時刻が0です。投与前フラグ列または時刻0の扱いを指定してください。")
    if (zero != "error") period[blq & !is.na(time) & time == origin] <- zero
  }
  position <- ifelse(blq, period, "not_blq")
  identity <- NULL
  if (mode == "positional") {
    keys <- unlist(step$profile_columns, use.names = FALSE)
    pk_assert(is.character(keys) && length(keys) > 0L && !anyNA(keys) && !anyDuplicated(keys) && all(keys %in% names(data)), "位置別BLQ規則には被験者・分析対象・投与期などの解析単位列を指定してください。")
    identity <- desktop_pk_group(data, keys)
    for (g in unique(identity$group)) {
      indices <- which(identity$group == g)
      quant <- indices[!mdv[indices] & !blq[indices] & !is.na(time[indices]) & !is.na(original[indices]) & original[indices] > 0 & time[indices] >= origin & (is.na(period[indices]) | period[indices] != "pre")]
      post <- indices[!mdv[indices] & blq[indices] & !is.na(period[indices]) & period[indices] == "post" & !is.na(time[indices])]
      if (!length(quant)) position[post] <- "all_blq" else {
        first <- min(time[quant]); last <- max(time[quant])
        position[post] <- ifelse(time[post] < first, "first", ifelse(time[post] > last, "last", "middle"))
      }
    }
  }
  rule <- rep("recorded", nrow(data)); action <- rep("keep", nrow(data))
  for (name in names(rules)) {
    category <- sub("_rule$", "", name)
    selected <- !mdv & blq & !is.na(position) & position == category
    action[selected] <- rules[[name]]; rule[selected] <- paste0(category, "_", rules[[name]])
  }
  reason <- rep("", nrow(data)); reason[mdv] <- "existing_mdv"
  reason[!mdv & is.na(time)] <- "missing_time"
  reason[reason == "" & blq & action == "exclude"] <- "blq_excluded"
  used <- reason == ""
  concentration_used <- rep(NA_real_, nrow(data))
  recorded <- used & (!blq | action == "keep")
  concentration_used[recorded] <- numeric_values(data[[concentration]], recorded, "濃度")[recorded]
  concentration_used[used & blq & action == "zero"] <- 0
  half <- used & blq & action == "half_lloq"
  lloq <- rep(NA_real_, nrow(data)); lloq_source <- step$lloq_source %||% "constant"
  pk_assert(is.character(lloq_source) && length(lloq_source) == 1L && lloq_source %in% c("constant", "column"), "LLOQの参照元を選択してください。")
  if (any(half)) {
    if (lloq_source == "column") {
      column <- desktop_prepare_column(data, step$lloq_column, "LLOQ")
      lloq[half] <- pk_number(as.character(data[[column]][half]), "LLOQ")
    } else {
      pk_assert(length(step$lloq_value) == 1L, "LLOQの定数を1つ指定してください。")
      lloq[half] <- pk_number(as.character(step$lloq_value), "LLOQ")
    }
    pk_assert(all(lloq[half] > 0), "LLOQは0より大きい有限数です。")
    concentration_used[half] <- lloq[half] / 2
    pk_assert(all(concentration_used[half] > 0), "LLOQ/2が数値の表現範囲を下回ります。")
  }
  pk_assert(all(is.na(concentration_used) | concentration_used >= 0), "負の濃度は使用できません。")
  reason[reason == "" & is.na(concentration_used)] <- "missing_concentration"
  rule[nzchar(reason)] <- reason[nzchar(reason)]
  data$NCA_CONC <- concentration_used; data$NCA_BLQ <- as.integer(blq); data$NCA_MDV <- as.integer(nzchar(reason))
  data$NCA_BLQ_RULE <- rule; data$NCA_BLQ_POSITION <- position; data$NCA_LLOQ_USED <- lloq
  data$NCA_ORIGINAL_CONC <- as.character(data[[concentration]])
  data$NCA_ORIGINAL_MDV <- as.integer(mdv)
  context <- list(version = 2L, mode = mode, profile_columns = as.list(if (mode == "positional") sort(keys) else character()),
    time_column = time_column, time_origin_h = origin, step = step)
  dose_columns <- intersect(c(desktop_nca_dose_evidence_columns(), "NCA_ASSEMBLY_PROFILE", "NCA_ASSEMBLY_PROFILE_COLUMNS",
    "NCA_ASSEMBLY_STATUS", "NCA_ASSEMBLY_REASON", "NCA_DOSE_REFERENCE_COLUMNS", "NCA_NEXT_EX_ROW", "NCA_NEXT_DOSE_REF_ID",
    unlist(desktop_assembly_reference_columns(data), use.names = FALSE)), names(data))
  if (length(dose_columns)) context$dose_columns <- as.list(sort(dose_columns))
  context$columns <- as.list(desktop_pk_blq_columns(context))
  canonical <- desktop_pk_blq_canonical(data, unlist(context$columns), unlist(context$profile_columns))
  groups <- if (mode == "positional") identity$group else rep(1L, nrow(data))
  ordinal <- count <- integer(nrow(data))
  for (g in unique(groups)) {
    indices <- which(groups == g)
    ordinal[indices] <- seq_along(indices); count[indices] <- length(indices)
  }
  data$NCA_BLQ_BASELINE <- desktop_pk_blq_baseline_json(canonical, ordinal, count)
  context_json <- desktop_pk_blq_json(context)
  data$NCA_BLQ_CONTEXT <- rep(context_json, nrow(data))
  identities <- desktop_pk_blq_rows(canonical[unlist(context$profile_columns, use.names = FALSE)], n = nrow(data))
  signature_groups <- match(identities, unique(identities))
  data$NCA_BLQ_PROFILE_SIGNATURE <- desktop_pk_blq_profile_signatures(data$NCA_BLQ_BASELINE, signature_groups, context_json)
  rows <- data.frame(ROW = seq_len(nrow(data)), PERIOD = period, POSITION = position,
    RULE = rule, ACTION = action, ORIGINAL_CONC = as.character(data[[concentration]]), LLOQ_USED = lloq,
    CONC_USED = concentration_used, BLQ = blq, MDV = data$NCA_MDV, REASON = reason, stringsAsFactors = FALSE)
  if (!is.null(identity)) rows <- cbind(PROFILE = identity$group, rows)
  summary <- as.data.frame(table(POSITION = position, RULE = rule, useNA = "ifany"), stringsAsFactors = FALSE)
  summary <- summary[summary$Freq > 0L, , drop = FALSE]; rownames(summary) <- NULL
  list(data = data, rows = rows, summary = summary)
}

desktop_pk_prepare_controls <- function(ns) {
  rules <- c("記録値" = "keep", "0" = "zero", "LLOQ/2" = "half_lloq", "除外" = "exclude")
  formats <- c("ISO日時・絶対時刻POSIXct" = "iso", "SAS日時（1960年秒・XPT現地時刻）" = "sas_seconds", "Unix秒・XPTのUTC時刻" = "unix_seconds")
  rule_row <- function(id, title) shiny::div(class = "pk-blq-rule-row",
    shiny::div(class = "pk-blq-rule-label", shiny::strong(title),
      shiny::span(class = "pk-blq-rule-count", shiny::span(class = "pk-blq-count-caption", "対象行："),
        shiny::textOutput(ns(paste0(id, "_count")), inline = TRUE))),
    shiny::radioButtons(ns(id), shiny::span(class = "pk-visually-hidden", paste(title, "の処置")), rules, selected = character(0), inline = TRUE))
  shiny::tagList(
    shiny::conditionalPanel("input.op === 'elapsed_time'", ns = ns,
      shiny::div(class = "pk-form-grid",
        desktop_workbench_select(ns("sample_column"), "採血日時列", character()),
        desktop_workbench_select(ns("dose_column"), "対応する投与日時列", character()),
        desktop_workbench_select(ns("sample_datetime_type"), "採血日時の形式", formats),
        desktop_workbench_select(ns("dose_datetime_type"), "投与日時の形式", formats),
        shiny::textInput(ns("utc_offset"), "時差のない日時に使うUTCオフセット", "+09:00"),
        shiny::textInput(ns("elapsed_output"), "経過時間（h）の新しい列名", "NCA_TIME_H"),
        desktop_workbench_select(ns("invalid_datetime"), "不完全・不正な日時", c("欠測にして行ごとに記録" = "missing", "処理を停止して確認" = "error"))),
      shiny::p(class = "pk-caption", "ISO日時は分・秒・小数秒、Z/±HH:MMに対応します。分精度は秒を0として記録します。時差の仮定・形式・不正理由を行別に残します。SAS数値・XPTの現地日時には指定オフセットを適用します。XPTのSAS日時は現地時刻かUTCかを明示してください。通常のPOSIXctは絶対時刻を保持します。"),
      shiny::p(class = "pk-caption", "採血日時−同じ行の投与日時をhで作成します。別の投与表は被験者・投与期などで先に結合してください。対応する投与は自動推定しません。投与前の負値は保持します。")),
    shiny::conditionalPanel("input.op === 'blq_rules'", ns = ns,
      shiny::div(class = "pk-form-grid",
        desktop_workbench_select(ns("blq_concentration"), "元の濃度列", character()),
        desktop_workbench_select(ns("prep_blq_column"), "BLQフラグ列", character()),
        shiny::div(class = "pk-time-field", desktop_workbench_select(ns("blq_time"), "時間列（h）", c("時間列を選択" = "")),
          shiny::textOutput(ns("blq_time_hint"))),
        desktop_workbench_select(ns("prep_mdv_column"), "既存のMDVフラグ列", c("使わない" = "")),
        desktop_workbench_select(ns("predose_column"), "投与前フラグ列（Y/1＝投与前）", c("使わない" = "")),
        desktop_workbench_select(ns("prep_flag_convention"), "フラグの規約", c("0/1・Y/N等（空欄はエラー）" = "strict", "Y/空欄（空欄・NA＝偽）" = "y_blank")),
        shiny::numericInput(ns("prep_time_origin"), "投与基準時刻（入力時間軸のh）", 0),
        desktop_workbench_select(ns("zero_time"), "基準時刻のBLQの分類", c("停止して確認" = "error", "投与前" = "pre", "投与後" = "post")),
        desktop_workbench_select(ns("blq_mode"), "BLQ規則の区分", c("投与前・先頭・途中・末尾" = "positional", "投与前・投与後（旧形式）" = "legacy"))),
      shiny::conditionalPanel("input.blq_mode === 'positional'", ns = ns,
        desktop_workbench_select(ns("profile_columns"), "解析単位の列（被験者・分析対象物・投与期など）", character(), multiple = TRUE)),
      shiny::div(class = "pk-blq-matrix",
        shiny::div(class = "pk-blq-rule-row pk-blq-rule-heading", shiny::strong("区分"), shiny::strong("処置"), shiny::strong("対象行")),
        rule_row("pre_rule", "投与前"),
        shiny::conditionalPanel("input.blq_mode === 'positional'", ns = ns,
          rule_row("first_rule", "先頭"), rule_row("middle_rule", "途中"), rule_row("last_rule", "末尾"), rule_row("all_blq_rule", "全点BLQ")),
        shiny::conditionalPanel("input.blq_mode === 'legacy'", ns = ns, rule_row("post_rule", "投与後"))),
      shiny::div(class = "pk-lloq-controls",
        shiny::p(class = "pk-caption", "LLOQはLLOQ/2処理または投与前QCで使用します。濃度と同じ単位で指定してください。LLOQ/2処理には指定が必要です。未指定の場合、BLQの投与前値のQCは評価できません。"),
        shiny::div(class = "pk-form-grid",
          desktop_workbench_select(ns("lloq_source"), "LLOQの参照元", c("定数" = "constant", "列" = "column")),
          shiny::conditionalPanel("input.lloq_source === 'constant'", ns = ns, shiny::numericInput(ns("lloq_value"), "LLOQ（濃度と同じ単位）", NA, min = 0)),
          shiny::conditionalPanel("input.lloq_source === 'column'", ns = ns, desktop_workbench_select(ns("lloq_column"), "LLOQ列", c("列を選択" = ""))))),
      shiny::p(class = "pk-caption", "先頭・途中・末尾は同じ解析単位の全行にある元の非BLQ正値の位置で分類します。NCAの開始・終了範囲では再分類しません。規則は解析計画に従って選択してください。投与前フラグを指定した場合は時間の符号より優先します。"),
      shiny::p(class = "pk-caption", "元の濃度と除外行を保持し、NCA用の列を新規作成します。LLOQは濃度と同じ単位です。全点BLQから最終定量時点は作りません。")))
}

desktop_pk_prepare_form <- function(input) {
  switch(input$op %||% "",
    elapsed_time = list(op = "elapsed_time", sample_column = input$sample_column, dose_column = input$dose_column,
      utc_offset = input$utc_offset %||% "+09:00", output = input$elapsed_output %||% "NCA_TIME_H",
      sample_datetime_type = input$sample_datetime_type %||% "iso", dose_datetime_type = input$dose_datetime_type %||% "iso",
      invalid_datetime = input$invalid_datetime %||% "missing"),
    blq_rules = {
      mode <- input$blq_mode %||% "legacy"
      step <- list(op = "blq_rules", concentration = input$blq_concentration, blq_column = input$prep_blq_column,
        time = input$blq_time, mdv_column = input$prep_mdv_column %||% "", predose_column = input$predose_column %||% "",
        mode = mode, time_origin = input$prep_time_origin %||% 0, flag_convention = input$prep_flag_convention %||% "strict",
        zero_time = input$zero_time %||% "error", pre_rule = input$pre_rule,
        lloq_source = input$lloq_source %||% "constant", lloq_value = input$lloq_value, lloq_column = input$lloq_column)
      if (mode == "positional") c(step, list(profile_columns = as.list(input$profile_columns),
        first_rule = input$first_rule, middle_rule = input$middle_rule, last_rule = input$last_rule,
        all_blq_rule = input$all_blq_rule %||% input$first_rule)) else c(step, list(post_rule = input$post_rule))
    }, NULL)
}

desktop_pk_prepare_update_columns <- function(session, input, columns, step = NULL, data = NULL, preferred_time = NULL, initialize_lloq = FALSE) {
  candidates <- list(sample_column = c("PCDTC", "SAMPLE_DTM", "SAMPLE_DTC"), dose_column = c("EXSTDTC", "DOSE_DTM", "DOSE_DTC"),
    blq_concentration = c("AVAL", "CONC"), prep_blq_column = c("BLQ", "BLQFL"), blq_time = c(preferred_time, "NCA_TIME_H", "TAD_H", "TIME_H", "TIME"),
    prep_mdv_column = "MDV", predose_column = character(), lloq_column = "LLOQ")
  saved <- switch(step$op %||% "",
    elapsed_time = list(sample_column = step$sample_column, dose_column = step$dose_column),
    blq_rules = list(blq_concentration = step$concentration, prep_blq_column = step$blq_column,
      blq_time = step$time, prep_mdv_column = step$mdv_column %||% "", predose_column = step$predose_column %||% "",
      lloq_column = step$lloq_column), list())
  if (initialize_lloq && is.null(step)) {
    detected <- intersect(candidates$lloq_column, columns)
    shiny::updateSelectInput(session, "lloq_source", selected = if (length(detected)) "column" else "constant")
    shiny::updateNumericInput(session, "lloq_value", value = NA_real_)
  }
  for (id in names(candidates)) {
    optional <- id %in% c("prep_mdv_column", "predose_column")
    values <- columns
    if (id == "blq_time") {
      values <- if (is.data.frame(data)) desktop_pk_time_choices(data, shiny::isolate(c(input$profile_columns, input$blq_concentration, input$prep_blq_column, input$prep_mdv_column, input$predose_column, if (identical(input$lloq_source, "column")) input$lloq_column))) else setdiff(columns, c("ID", "USUBJID", "SUBJID", desktop_pk_default_groups(columns)))
    }
    choices <- if (optional) c("使わない" = "", stats::setNames(columns, columns)) else if (id == "blq_time") c("時間列を選択" = "", values) else if (id == "lloq_column") c("列を選択" = "", columns) else columns
    current <- if (id %in% names(saved)) saved[[id]] else if (id == "lloq_column" && initialize_lloq) NULL else shiny::isolate(input[[id]])
    preferred <- intersect(candidates[[id]], unname(values))
    fresh_time <- id == "blq_time" && !id %in% names(saved) && length(preferred_time) == 1L && preferred_time %in% unname(values)
    selected <- if (fresh_time) preferred_time else if (length(current) == 1L && current %in% unname(choices) && (nzchar(current) || optional)) current else if (length(preferred)) preferred[1] else if (optional || id == "blq_time") "" else columns[1]
    if (id == "lloq_column") selected <- if (is.null(current)) {
      if (length(preferred)) preferred[1] else ""
    } else if (length(current) == 1L && current %in% columns) current else ""
    shiny::updateSelectInput(session, id, choices = choices, selected = selected)
  }
  selected <- if (identical(step$op, "blq_rules")) unlist(step$profile_columns) else shiny::isolate(input$profile_columns)
  selected <- intersect(selected, columns)
  if (!length(selected)) {
    id <- head(intersect(c("USUBJID", "ID", "SUBJID"), columns), 1L)
    selected <- c(id, desktop_pk_default_groups(columns, id))
  }
  if (is.data.frame(data) && "NCA_ASSEMBLY_PROFILE_COLUMNS" %in% names(data) && !identical(step$op, "blq_rules"))
    selected <- desktop_assembly_profile_columns(data)
  shiny::updateSelectizeInput(session, "profile_columns", choices = columns, selected = selected)
}

desktop_pk_prepare_restore <- function(session, step) {
  if (identical(step$op, "elapsed_time")) {
    shiny::updateSelectInput(session, "sample_column", selected = step$sample_column)
    shiny::updateSelectInput(session, "dose_column", selected = step$dose_column)
    shiny::updateTextInput(session, "utc_offset", value = step$utc_offset %||% "+09:00")
    shiny::updateTextInput(session, "elapsed_output", value = step$output %||% "NCA_TIME_H")
    shiny::updateSelectInput(session, "sample_datetime_type", selected = step$sample_datetime_type %||% "iso")
    shiny::updateSelectInput(session, "dose_datetime_type", selected = step$dose_datetime_type %||% "iso")
    shiny::updateSelectInput(session, "invalid_datetime", selected = step$invalid_datetime %||% "missing")
  }
  if (identical(step$op, "blq_rules")) {
    mapping <- c(blq_concentration = "concentration", prep_blq_column = "blq_column", blq_time = "time",
      prep_mdv_column = "mdv_column", predose_column = "predose_column", zero_time = "zero_time",
      pre_rule = "pre_rule", post_rule = "post_rule", first_rule = "first_rule", middle_rule = "middle_rule", last_rule = "last_rule",
      lloq_source = "lloq_source", lloq_column = "lloq_column")
    for (id in names(mapping)) {
      if (grepl("_rule$", id)) shiny::updateRadioButtons(session, id, selected = step[[mapping[[id]]]] %||% character(0))
      else shiny::updateSelectInput(session, id, selected = step[[mapping[[id]]]] %||% "")
    }
    shiny::updateRadioButtons(session, "all_blq_rule", selected = step$all_blq_rule %||% step$first_rule %||% character(0))
    shiny::updateSelectInput(session, "blq_mode", selected = step$mode %||% "legacy")
    shiny::updateSelectInput(session, "prep_flag_convention", selected = step$flag_convention %||% "strict")
    shiny::updateNumericInput(session, "prep_time_origin", value = step$time_origin %||% 0)
    shiny::updateSelectizeInput(session, "profile_columns", selected = unlist(step$profile_columns))
    shiny::updateNumericInput(session, "lloq_value", value = step$lloq_value %||% NA_real_)
  }
}
