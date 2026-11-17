desktop_validate_covariates <- function(specification) {
  schema <- desktop_nca_contract$covariate_specification
  if (!desktop_schema_valid(specification, schema, schema[["$defs"]]))
    desktop_public_validation("NCA入力のDM・VS条件が不正です。解析単位・採用条件・基準時点の確認を指定してください。")
  invisible(TRUE)
}

desktop_covariate_signature <- function(data, context) {
  columns <- unlist(context$columns, use.names = FALSE)
  keys <- c(context$profile$id, unlist(context$profile$groups, use.names = FALSE))
  canonical <- desktop_pk_blq_canonical(data, columns, keys)
  groups <- desktop_pk_group(data, keys)$group
  desktop_pk_blq_profile_signatures(desktop_pk_blq_rows(canonical, named = TRUE), groups, desktop_pk_blq_json(context))
}

desktop_covariate_context <- function(data, profile = NULL) {
  markers <- c("NCA_COV_CONTEXT", "NCA_COV_SIGNATURE")
  if (!any(markers %in% names(data))) return(NULL)
  fail <- "DM・VS入力の条件・採用値・解析単位が変更されています。元の入力から作成し直してください。"
  pk_assert(all(markers %in% names(data)) && nrow(data) > 0, fail)
  texts <- unique(as.character(data$NCA_COV_CONTEXT))
  pk_assert(length(texts) == 1L && !is.na(texts), fail)
  context <- tryCatch(jsonlite::fromJSON(texts, simplifyVector = FALSE), error = function(e) NULL)
  pk_assert(is.list(context) && identical(context$version, 1L) && is.list(context$profile) && is.list(context$columns), fail)
  keys <- c(context$profile$id, unlist(context$profile$groups, use.names = FALSE))
  pk_assert(all(c(keys, unlist(context$columns)) %in% names(data)), fail)
  if (!is.null(profile)) pk_assert(setequal(profile, keys), fail)
  pk_assert(identical(as.character(data$NCA_COV_SIGNATURE), desktop_covariate_signature(data, context)), fail)
  context
}

pkapp_prepare_nca_covariates <- function(data, dm = NULL, vs = NULL, specification) {
  s <- jsonlite::fromJSON(desktop_json(specification), simplifyVector = FALSE)
  desktop_validate_covariates(s)
  pkapp_table_check(data)
  pk_assert(nrow(data) > 0 && !any(grepl("^NCA_COV_", names(data))), "元の濃度表を選択してください。DM・VSを重ねて追加できません。")
  pk_assert(!is.null(s$dm) || !is.null(s$vs), "DMまたはVSを選択してください。")
  pk_assert(is.null(s$dm) == is.null(dm) && is.null(s$vs) == is.null(vs), "DM・VSの入力と条件を一致させてください。")
  keys <- c(s$profile$id, unlist(s$profile$groups, use.names = FALSE))
  pk_assert(!anyDuplicated(keys) && all(keys %in% names(data)), "解析単位のID・グループ列を確認してください。")
  pk_assert(is.character(data[[s$profile$id]]), "被験者IDは文字型のまま照合してください。")
  for (role in c("study", "period")) if (!is.null(s$profile[[role]]))
    pk_assert(s$profile[[role]] %in% keys, "試験・投与期は解析単位に含めてください。")
  if ("STUDYID" %in% names(data)) pk_assert(identical(s$profile$study, "STUDYID"), "試験をまたぐ照合を防ぐためSTUDYIDを試験キーに指定してください。")
  assembled <- desktop_assembly_profile_columns(data)
  if (!is.null(assembled)) pk_assert(setequal(keys, assembled), "結合時と同じ解析単位を選択してください。")
  if ("NCA_BLQ_CONTEXT" %in% names(data)) {
    blq <- desktop_pk_blq_context(data)
    pk_assert(setequal(keys, unlist(blq$profile_columns)), "BLQ前処理と同じ解析単位を選択してください。")
  }
  if (!is.null(vs)) {
    baseline <- s$vs$baseline
    pk_assert(isTRUE(baseline$confirmed) &&
      ((baseline$mode == "period" && s$vs$scope == "keys" && !is.null(s$profile$period) && s$profile$period %in% names(s$vs$keys)) ||
       (baseline$mode == "subject_confirmed" && s$vs$scope == "subject")), "VSの投与期別基準時点、または全投与期で共有する基準時点を明示的に確認してください。")
    roles <- vapply(s$vs$measurements, `[[`, character(1), "role")
    pk_assert(!anyDuplicated(roles[roles != "carry"]), "体重・身長・体表面積はそれぞれ1つの採用値に対応させてください。")
  }
  mapping <- stats::setNames(as.list(keys), keys)
  value <- desktop_covariates_join(data, mapping, s$profile$id, dm, vs, s,
    study = s$profile$study %||% character(), period = s$profile$period %||% character(), input_mode = TRUE)
  roles <- list()
  for (m in s$vs$measurements %||% list()) {
    field <- paste0(m$name, "_BASELINE_STATUS")
    status <- rep("attested_chronology_unverified", nrow(data))
    if ("NCA_DOSE_EPOCH_S" %in% names(data) && !is.null(s$vs$datetime_column)) {
      text <- as.character(value$data[[paste0(m$name, "_DTC")]])
      absolute <- !is.na(text) & grepl("(Z|[+-][0-9]{2}:[0-9]{2})$", text)
      parsed <- desktop_timestamp_parse(ifelse(absolute, text, NA_character_), "+00:00", "iso")
      dose <- desktop_pk_numeric(data$NCA_DOSE_EPOCH_S)$value
      known <- is.finite(parsed$value) & is.finite(dose)
      status[known & parsed$value <= dose] <- "verified_predose"
      status[known & parsed$value > dose] <- "postdose"
    }
    pk_assert(!field %in% names(value$data), "追加列名が重複しています。")
    value$data[[field]] <- status
    if (m$role != "carry") roles[[m$role]] <- list(value_column = m$name, unit_column = paste0(m$name, "_UNIT"),
      status_column = field, source_row_column = paste0(m$name, "_SOURCE_ROW"), datetime_column = paste0(m$name, "_DTC"),
      visit_column = paste0(m$name, "_VISIT"), test = m$test)
    flagged <- !duplicated(desktop_assembly_tuple(data, keys)) & status == "postdose"
    if (any(flagged)) value$trace$issues <- rbind(value$trace$issues,
      cbind(data[flagged, keys, drop = FALSE], field = m$name, reason = "postdose"))
  }
  added <- setdiff(names(value$data), names(data))
  context <- list(version = 1L, profile = s$profile, roles = roles, specification = s,
    source_hashes = lapply(Filter(Negate(is.null), list(source = data, dm = dm, vs = vs)), desktop_hash),
    columns = as.list(unique(c(keys, added, intersect(c("NCA_EX_ROW", "NCA_DOSE_REF_ID", "NCA_DOSE_EPOCH_S"), names(data))))))
  value$data$NCA_COV_CONTEXT <- rep(desktop_pk_blq_json(context), nrow(data))
  value$data$NCA_COV_SIGNATURE <- desktop_covariate_signature(value$data, context)
  value$counts[c("added_columns", "issues")] <- c(ncol(value$data) - ncol(data), nrow(value$trace$issues))
  value$trace$context <- context
  value$trace$baseline <- "Explicit user baseline attestation. Only full ISO timestamps with offsets and a known reference dose clock are chronologically verified; postdose measurements cannot be computational inputs."
  value
}

desktop_covariates_save <- function(engine, name, inputs, value, source_ids, channel = "ui", actor = "local user", reason = "NCA入力にDM・VSを追加", args = NULL) {
  if (is.null(args)) args <- list(project_id = engine$nca$project_id, base_revision = engine$nca$revision, actor = actor, reason = reason)
  if (identical(engine$mode, "nca")) {
    desktop_nca_idle(engine); desktop_nca_revision(engine, args$base_revision, args$project_id); desktop_nca_identity(args)
  }
  saved <- desktop_table_save(engine, name, inputs, list(kind = "nca_covariates", specification = value$specification),
    channel, actor, frozen = value, provenance = list(source_ids = source_ids, source_hashes = lapply(inputs, desktop_hash), reason = reason))
  if (identical(engine$mode, "nca")) {
    engine$nca$revision <- engine$nca$revision + 1L
    desktop_nca_record(engine, "nca_table_operation", args, channel,
      list(operation = "prepare_nca_covariates", table_id = saved$id, source_ids = source_ids))
    engine$nca_disclosure <- NULL; desktop_nca_notify(engine); saved$revision <- engine$nca$revision
  }
  saved
}

desktop_prepare_nca_covariates <- function(engine, args, channel = "mcp") {
  desktop_nca_idle(engine); desktop_nca_revision(engine, args$base_revision, args$project_id); desktop_nca_identity(args)
  desktop_validate_covariates(args$specification)
  ids <- args[intersect(c("source", "dm", "vs"), names(args))]
  if (!all(vapply(ids, function(id) is.character(id) && length(id) == 1L && !is.na(id) && grepl("^table-[a-f0-9]{24}$", id), logical(1))))
    desktop_public_validation("NCA入力には保存済みの表IDを指定してください。")
  inputs <- lapply(ids, function(id) desktop_table_get(engine, id))
  value <- tryCatch(pkapp_prepare_nca_covariates(inputs$source, inputs$dm, inputs$vs, args$specification),
    error = function(e) desktop_public_validation("DM・VS入力の照合に失敗しました。キー・採用行・基準時点をローカル画面で確認してください。"))
  desktop_covariates_save(engine, args$name, inputs, value, ids, channel, args$actor, args$reason, args)
}
