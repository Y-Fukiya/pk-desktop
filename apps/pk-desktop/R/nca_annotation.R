desktop_annotation_error <- function(message) desktop_public_validation(message)

desktop_validate_annotation <- function(specification) {
  schema <- desktop_nca_contract$annotation_specification
  if (!desktop_schema_valid(specification, schema, schema[["$defs"]]))
    desktop_annotation_error("背景情報の条件が不正です。キー・列・VSの採用条件を公開スキーマに合わせて指定してください。")
  invisible(TRUE)
}

desktop_annotation_result <- function(engine, id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl("^table-[a-f0-9]{24}$", id))
    desktop_annotation_error("追加元には保存済みNCA結果の表IDを指定してください。")
  data <- desktop_table_get(engine, id)
  folder <- desktop_table_path(engine, id)
  manifest <- desktop_read(file.path(folder, "manifest.json"))
  if (!identical(manifest$kind, "nca")) desktop_annotation_error("背景情報の追加元には保存済みNCA結果を選択してください。")
  list(data = data, specification = desktop_read(file.path(folder, "recipe.json"))$specification,
    trace = list(output_key_mapping = desktop_read(file.path(folder, "trace.json"))$output_key_mapping))
}

desktop_covariate_keys <- function(data, mapping, id, source, keys, scope, study_column = "STUDYID", period_columns = c("PERIOD", "APERIOD")) {
  mapping <- unlist(mapping)
  subject <- unname(mapping[[id]])
  study <- unname(mapping[intersect(names(mapping), study_column)])
  columns <- unlist(keys, use.names = FALSE)
  target <- names(keys)
  if (!length(subject) || !subject %in% target || !all(target %in% mapping) ||
      anyDuplicated(columns) || !all(columns %in% names(source)))
    desktop_annotation_error("照合キーにはNCAの被験者IDと、存在する対応列を指定してください。")
  if ((length(study) && !all(study %in% target)) || ("STUDYID" %in% names(source) && !length(study)) ||
      (length(study) && "STUDYID" %in% names(source) && !identical(unname(unlist(keys[study])), "STUDYID")))
    desktop_annotation_error("試験をまたぐ照合を防ぐため、NCAと背景情報のSTUDYIDを照合キーに含めてください。")
  if (scope == "subject" && !setequal(target, c(subject, study)))
    desktop_annotation_error("被験者単位では被験者ID・STUDYIDだけを照合キーに指定してください。")
  period <- unname(mapping[intersect(names(mapping), period_columns)])
  if (scope == "keys" && length(period) && !all(period %in% target))
    desktop_annotation_error("解析単位別のVS照合にはNCAの投与期（PERIOD・APERIOD）を含めてください。全投与期で共通に使う場合は被験者単位を明示してください。")
  if (scope == "keys" && length(setdiff(target, c(subject, study))) == 0L)
    desktop_annotation_error("解析単位別では投与期などの追加キーを指定してください。")
  right <- source[columns]; names(right) <- target
  for (key in target) {
    a <- data[[key]]; b <- right[[key]]
    if (key %in% c(subject, study) && (!is.character(a) || !is.character(b)))
      desktop_annotation_error("被験者ID・STUDYIDは文字型のまま照合してください。数値への変換は行いません。")
    if (!key %in% c(subject, study) && is.character(a) && is.numeric(b) && !inherits(b, c("Date", "POSIXt"))) {
      right[[key]] <- as.character(b); b <- right[[key]]
    }
    if (!(is.character(a) && is.character(b)) && !(is.numeric(a) && is.numeric(b) &&
        !inherits(a, c("Date", "POSIXt")) && !inherits(b, c("Date", "POSIXt"))))
      desktop_annotation_error("照合キーの型が一致しません。文字列と数値を自動変換しません。")
  }
  list(left = desktop_assembly_tuple(data, target), right = desktop_assembly_tuple(right, target))
}

pkapp_annotate_nca <- function(result, dm = NULL, vs = NULL, specification) {
  s <- jsonlite::fromJSON(desktop_json(specification), simplifyVector = FALSE)
  desktop_validate_annotation(s)
  if (is.null(s$dm) && is.null(s$vs)) desktop_annotation_error("DMまたはVSを少なくとも1つ選択してください。")
  if (is.null(s$dm) != is.null(dm) || is.null(s$vs) != is.null(vs))
    desktop_annotation_error("DM・VSの入力と条件を一致させてください。")
  if (!is.list(result) || !is.data.frame(result$data) || is.null(result$trace$output_key_mapping) || is.null(result$specification$id))
    desktop_annotation_error("NCAの結果・条件・解析単位の列対応が必要です。")
  if (!all(c("PARAMCD", "AVAL", "AVALU") %in% names(result$data)) || !nrow(result$data))
    desktop_annotation_error("NCAパラメータを含む結果が必要です。")
  desktop_covariates_join(result$data, result$trace$output_key_mapping, result$specification$id, dm, vs, s)
}

desktop_annotation_keys <- function(result, source, keys, scope) {
  desktop_covariate_keys(result$data, result$trace$output_key_mapping, result$specification$id, source, keys, scope)
}

desktop_covariates_join <- function(data, mapping, id, dm, vs, s, study = "STUDYID", period = c("PERIOD", "APERIOD"), input_mode = FALSE) {
  original <- data
  pkapp_table_check(data)
  identity_columns <- unname(unlist(mapping))
  profiles <- !duplicated(desktop_assembly_tuple(data, identity_columns))
  added <- list(); records <- list(); issues <- list()
  append <- function(name, value) {
    if (name %in% c(names(data), names(added))) desktop_annotation_error("追加列または付随する記録列が既存列と重複します。追加名を変更してください。")
    added[[name]] <<- value
  }
  issue <- function(field, reason, rows) {
    rows <- which(rows & profiles)
    if (length(rows)) issues[[length(issues) + 1L]] <<- cbind(data[rows, identity_columns, drop = FALSE],
      data.frame(field = field, reason = reason, stringsAsFactors = FALSE))
  }
  if (!is.null(dm)) {
    pkapp_table_check(dm)
    keys <- desktop_covariate_keys(data, mapping, id, dm, s$dm$keys, "subject", study, period)
    if (anyDuplicated(keys$right)) desktop_annotation_error("DMの照合キーが重複しています。1被験者・1試験につき1行に整理してください。")
    selected <- match(keys$left, keys$right)
    columns <- unlist(s$dm$columns)
    if (!all(columns %in% names(dm))) desktop_annotation_error("DMに選択した列がありません。")
    for (name in names(columns)) {
      value <- dm[[columns[[name]]]][selected]
      append(name, value)
      issue(name, "missing_value", is.na(value) | if (is.character(value)) !nzchar(trimws(value)) else FALSE)
    }
    append("DM_SOURCE_ROW", selected)
    issue("DM", "missing_match", is.na(selected))
    records$dm <- cbind(data[profiles, identity_columns, drop = FALSE], DM_SOURCE_ROW = selected[profiles])
  }
  if (!is.null(vs)) {
    pkapp_table_check(vs)
    keys <- desktop_covariate_keys(data, mapping, id, vs, s$vs$keys, s$vs$scope, study, period)
    fields <- unlist(s$vs[intersect(c("test_column", "value_column", "unit_column", "datetime_column", "visit_column"), names(s$vs))])
    if (!all(fields %in% names(vs))) desktop_annotation_error("VSに選択した検査・値・単位・日時・来院の列がありません。")
    keep <- rep(TRUE, nrow(vs))
    if (s$vs$selection$mode == "filter") {
      column <- s$vs$selection$column
      if (!column %in% names(vs)) desktop_annotation_error("VSの採用条件の列がありません。")
      x <- vs[[column]]; values <- unlist(s$vs$selection$values)
      if (is.numeric(x)) values <- pk_number(values, "VS採用条件") else if (!is.character(x))
        desktop_annotation_error("VSの採用条件には文字列または数値の列を選択してください。")
      keep <- !is.na(x) & x %in% values
    }
    tests <- vs[[s$vs$test_column]]
    if (!is.character(tests)) desktop_annotation_error("VSの検査コードは文字型で指定してください。")
    for (m in s$vs$measurements) {
      rows <- which(keep & !is.na(tests) & tests == m$test)
      if (anyDuplicated(keys$right[rows])) desktop_annotation_error("VSの採用候補が重複しています。検査ごとに照合キーが1行になる採用条件を指定してください。")
      selected <- rows[match(keys$left, keys$right[rows])]
      value <- if (input_mode) desktop_pk_numeric(vs[[s$vs$value_column]][selected])$value else pk_number(vs[[s$vs$value_column]][selected], "VS測定値", allow_na = TRUE)
      unit <- as.character(vs[[s$vs$unit_column]][selected])
      dt <- if (!is.null(s$vs$datetime_column)) vs[[s$vs$datetime_column]][selected] else rep(NA_character_, nrow(data))
      visit <- if (!is.null(s$vs$visit_column)) vs[[s$vs$visit_column]][selected] else rep(NA_character_, nrow(data))
      append(m$name, value); append(paste0(m$name, "_UNIT"), unit)
      append(paste0(m$name, "_DTC"), dt); append(paste0(m$name, "_VISIT"), visit)
      append(paste0(m$name, "_SOURCE_ROW"), selected)
      issue(m$name, "missing_match", is.na(selected)); issue(m$name, "missing_value", !is.na(selected) & is.na(value))
      issue(m$name, "missing_unit", !is.na(selected) & (is.na(unit) | !nzchar(trimws(unit))))
      comparable_unit <- !is.na(selected) & is.finite(value) & !is.na(unit) & nzchar(trimws(unit))
      if (length(unique(trimws(unit[profiles & comparable_unit]))) > 1L)
        issue(m$name, "mixed_units", comparable_unit)
      issue(m$name, "missing_datetime", !is.na(selected) & (is.na(dt) | !nzchar(trimws(as.character(dt)))))
      records[[m$name]] <- cbind(data[profiles, identity_columns, drop = FALSE],
        data.frame(SOURCE_ROW = selected[profiles], TEST = m$test, VALUE = value[profiles], UNIT = unit[profiles],
          DTC = as.character(dt[profiles]), VISIT = as.character(visit[profiles]), stringsAsFactors = FALSE))
    }
  }
  for (name in names(added)) data[[name]] <- added[[name]]
  problem_rows <- if (length(issues)) do.call(rbind, issues) else data.frame()
  list(data = data, counts = c(input = nrow(original), result = nrow(data), profiles = sum(profiles),
      added_columns = length(added), issues = nrow(problem_rows)), specification = s,
    trace = list(output_key_mapping = mapping, records = records, issues = problem_rows,
      key_matching = list(subject_and_study = "exact character", other_keys = "numeric source keys use NCA-compatible as.character text when NCA output keys are character; leading-zero text remains distinct"),
      source_result_sha256 = desktop_hash(original)))
}

desktop_annotation_save <- function(engine, name, inputs, value, source_ids, channel = "ui", actor = "local user", reason = "背景情報を追加", args = NULL) {
  nca <- identical(engine$mode, "nca")
  if (is.null(args)) args <- list(project_id = engine$nca$project_id, base_revision = engine$nca$revision, actor = actor, reason = reason)
  if (nca) {
    desktop_nca_idle(engine); desktop_nca_revision(engine, args$base_revision, args$project_id); desktop_nca_identity(args)
  }
  provenance <- list(source_ids = source_ids, source_hashes = lapply(inputs, desktop_hash), reason = reason)
  if (nca) provenance$identity <- desktop_nca_identity_metadata(args, channel)
  saved <- desktop_table_save(engine, name, inputs, list(kind = "nca_annotation", specification = value$specification),
    channel, actor, frozen = value, provenance = provenance)
  if (nca) {
    before <- engine$nca; engine$nca$revision <- engine$nca$revision + 1L
    tryCatch(desktop_nca_record(engine, "nca_table_operation", args, channel,
      list(operation = "annotate_nca", table_id = saved$id, source_ids = source_ids)),
      error = function(e) { engine$nca <- before; stop(e) })
    desktop_nca_notify(engine)
    saved$revision <- engine$nca$revision
  }
  saved
}

desktop_annotate_nca <- function(engine, args, channel = "mcp") {
  desktop_nca_idle(engine); desktop_nca_revision(engine, args$base_revision, args$project_id); desktop_nca_identity(args)
  desktop_validate_annotation(args$specification)
  for (id in args[intersect(c("result_source", "dm", "vs"), names(args))]) {
    if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl("^table-[a-f0-9]{24}$", id))
      desktop_annotation_error("背景情報の入力には保存済みの表IDを指定してください。")
  }
  inputs <- list(result = desktop_annotation_result(engine, args$result_source))
  for (name in c("dm", "vs")) if (!is.null(args[[name]])) inputs[[name]] <- desktop_table_get(engine, args[[name]])
  value <- tryCatch(pkapp_annotate_nca(inputs$result, inputs$dm, inputs$vs, args$specification),
    pkapp_public_error = function(e) stop(e), error = function(e) desktop_annotation_error("背景情報の照合に失敗しました。列・キー・型・採用条件をローカル画面で確認してください。"))
  source_ids <- args[intersect(c("result_source", "dm", "vs"), names(args))]
  desktop_annotation_save(engine, args$name, inputs, value, source_ids, channel, args$actor, args$reason, args)
}
