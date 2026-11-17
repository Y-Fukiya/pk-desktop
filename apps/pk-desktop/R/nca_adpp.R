# A review-only projection of an existing NCA result; never derives analysis flags.
pkapp_adpp_draft <- function(value, source_input = NULL, provenance = NULL, analyte_order = NULL, parameter_mapping = NULL, mapping_inputs = NULL) {
  pk_assert(is.list(value) && is.data.frame(value$data) && is.list(value$specification) && is.list(value$trace),
    'NCAの結果と計算条件・処理記録を確認してください。')
  s <- value$specification; d <- value$data; trace <- value$trace
  groups <- unlist(s$groups, use.names = FALSE)
  pk_assert(identical(s$id, 'USUBJID') && 'STUDYID' %in% groups,
    'ADPP候補には、被験者ID列としてUSUBJID、追加グループとしてSTUDYIDを指定してNCAを実行してください。')
  keys <- c(s$id, groups)
  reserved_adam <- c('DTYPE','ASEQ','SRCDOM','SRCVAR','SRCSEQ')
  pk_assert(!any(grepl('^ANL[0-9][0-9]FL$',groups)) && !any(groups %in% reserved_adam),
    '解析フラグ・導出種別・由来用の列をADPP候補のグループとして自動転記できません。試験仕様を確認してください。')
  mapping <- trace$output_key_mapping
  pk_assert(is.list(mapping) && all(keys %in% names(mapping)), 'NCA結果の解析単位キーを確認してください。')
  output_keys <- unlist(mapping[keys], use.names = FALSE)
  pk_assert(is.character(output_keys) && length(output_keys) == length(keys) && !anyDuplicated(output_keys) &&
    all(output_keys %in% names(d)), 'NCA結果の解析単位キーを確認してください。')
  pk_assert(identical(unname(output_keys[match('USUBJID',keys)]), 'USUBJID') &&
    identical(unname(output_keys[match('STUDYID',keys)]), 'STUDYID'), 'STUDYIDまたはUSUBJIDがNCA結果の列名と競合しています。')
  fields <- c('PARAMCD','PARAM','AVAL','AVALU')
  pk_assert(nrow(d)>0L && all(fields %in% names(d)) && is.numeric(d$AVAL) &&
    all(is.na(d$AVAL) | is.finite(d$AVAL)), 'NCAパラメータと数値を確認してください。')
  key_fields <- c('STUDYID','USUBJID',setdiff(output_keys,c('STUDYID','USUBJID')),'PARAMCD')
  pk_assert(!anyDuplicated(key_fields) && all(vapply(d[key_fields], function(x) !anyNA(x) && all(nzchar(trimws(as.character(x)))), logical(1))),
    '解析単位キーとPARAMCDに欠測があります。')
  identity <- desktop_pk_group(d,key_fields)
  pk_assert(!anyDuplicated(identity$group), '解析単位キーとPARAMCDが重複しています。')
  # Status and warning columns stay with every value, even if the CSV is separated from the ZIP.
  status_map <- c(NCAPSTAT='PARAM_STATUS',NCAEXCL='NCA_EXCLUDE',NCAWARN='NCA_WARNING',PROFSTAT='PROFILE_STATUS')
  pk_assert(all(unname(status_map) %in% names(d)) && !any(names(status_map) %in% names(d)),
    'NCAの状態・警告列を安全に候補へ転記できません。')
  pk_assert(!any('NCAPCD' %in% names(d)), 'NCA元コードの列名が入力と競合しています。')
  roles <- s$group_roles %||% list()
  pk_assert(is.list(roles) && all(vapply(roles, function(x) is.list(x) &&
    is.character(x$column) && length(x$column) == 1L && !is.na(x$column) &&
    is.character(x$role) && length(x$role) == 1L && !is.na(x$role) &&
    x$role %in% c('study','period','treatment','analyte','specimen','other'), logical(1))),
    'グループ列の役割を確認してください。')
  role_cols <- vapply(roles, `[[`, '', 'column')
  missing_roles <- setdiff(groups, role_cols)
  pk_assert(!anyDuplicated(role_cols) && setequal(role_cols, groups),
    paste0('ADPP候補には全グループ列の役割が必要です。未指定：', paste(missing_roles, collapse='、'), '。NCA設定で役割を指定して再実行してください。'))
  pk_assert(identical(roles[[match('STUDYID', role_cols)]]$role, 'study'),
    'STUDYIDの役割に「試験」を指定してNCAを再実行してください。')
  identity_cols <- vapply(Filter(function(x) x$role %in% c('study','period','treatment','analyte','specimen'),roles), `[[`, '', 'column')
  base_keys <- unique(c('STUDYID','USUBJID',unlist(mapping[identity_cols],use.names=FALSE)))
  other_cols <- vapply(Filter(function(x) identical(x$role,'other'),roles), `[[`, '', 'column')
  for (column in other_cols) {
    key <- unname(unlist(mapping[column],use.names=FALSE))
    observed <- unique(d[c(base_keys,key)])
    if (anyDuplicated(desktop_pk_group(observed,base_keys)$group))
      pk_assert(FALSE, paste0('「',column,'」は同じ試験・被験者・期・処置・分析対象・検体で複数の値があります。「その他」の役割を見直し、分析対象物・検体などを正しく指定してNCAを再実行してください。'))
  }
  analyte_cols <- vapply(Filter(function(x) identical(x$role,'analyte'),roles), `[[`, '', 'column')
  specimen_cols <- vapply(Filter(function(x) identical(x$role,'specimen'),roles), `[[`, '', 'column')
  parameter_cols <- c(analyte_cols,specimen_cols)
  parameter_keys <- unname(unlist(mapping[parameter_cols],use.names=FALSE))
  pk_assert(length(parameter_keys)==length(parameter_cols) && all(parameter_keys %in% output_keys), '分析対象・検体の役割と解析単位が一致しません。')
  if (!is.null(analyte_order)) {
    pk_assert(is.list(analyte_order) && !anyDuplicated(names(analyte_order)) && setequal(names(analyte_order),c('column','values')) &&
      is.character(analyte_order$column) && length(analyte_order$column)==1L && !is.na(analyte_order$column) && analyte_order$column %in% analyte_cols,
      '分析対象の順序は「分析対象物」の役割を付けた列で指定してください。')
    vals <- unlist(analyte_order$values,use.names=FALSE)
    order_key <- unname(unlist(mapping[analyte_order$column],use.names=FALSE))
    actual <- unique(as.character(d[[order_key]]))
    pk_assert(is.character(vals) && !anyNA(vals) && !anyDuplicated(vals) && length(vals)==length(actual) && setequal(vals,actual),
      '分析対象の順序には結果に含まれる値を重複なくすべて指定してください。')
    analyte_order <- list(column=analyte_order$column,values=as.list(vals))
  }
  candidate <- d[c(key_fields,'PARAM','AVAL','AVALU')]
  candidate$NCAPCD <- as.character(d$PARAMCD)
  for (name in names(status_map)) candidate[[name]] <- d[[status_map[[name]]]]
  if (length(parameter_cols)) {
    # Assign analyte combinations in UTF-8 sorted order, independently of row order,
    # unit and PARAM labels. Codes are provisional and scoped to this export.
    analyte_values <- as.data.frame(lapply(d[parameter_keys], function(x) enc2utf8(as.character(x))), stringsAsFactors=FALSE)
    combinations <- unique(analyte_values)
    order_parts <- unname(combinations)
    if (!is.null(analyte_order)) order_parts <- c(list(match(combinations[[order_key]],vals)),order_parts)
    ord <- do.call(order, c(order_parts, list(method='radix')))
    combinations <- combinations[ord,,drop=FALSE]
    joined <- rbind(combinations, analyte_values)
    group <- desktop_pk_group(joined, names(combinations))$group
    analyte_number <- match(group[nrow(combinations) + seq_len(nrow(d))], group[seq_len(nrow(combinations))])
    candidate$PARAM <- vapply(seq_len(nrow(d)), function(i) {
      names_values <- vapply(parameter_keys,function(k) as.character(d[[k]][i]),'')
      unit <- as.character(d$AVALU[i]); if (is.na(unit)) unit <- ''
      paste0(as.character(d$PARAM[i]),', ',paste(names_values,collapse=', '),if (nzchar(unit)) paste0(' (',unit,')') else '')
    }, '')
    if (is.null(parameter_mapping) && nrow(combinations) > 1L) candidate$PARAMCD <- vapply(seq_len(nrow(d)), function(i) {
      suffix <- as.character(analyte_number[i])
      pk_assert(nchar(suffix) < 8L, '分析対象が多すぎます。試験固有の対応表が必要です。')
      paste0(substr(as.character(d$PARAMCD[i]),1L,8L-nchar(suffix)),suffix)
    }, '')
  }
  if (!is.null(parameter_mapping)) {
    pk_assert(is.data.frame(parameter_mapping) && !anyDuplicated(names(parameter_mapping)), '対応表の列名を確認してください。')
    map_keys <- c('NCAPCD',parameter_keys)
    pk_assert(!anyDuplicated(c(map_keys,'PARAMCD')) && setequal(names(parameter_mapping),c(map_keys,'PARAMCD')),
      paste0('対応表の列は ',paste(c(map_keys,'PARAMCD'),collapse='、'),' としてください。'))
    pm <- as.data.frame(lapply(parameter_mapping,as.character),stringsAsFactors=FALSE,check.names=FALSE)
    pk_assert(nrow(pm)>0L && all(vapply(pm,function(x) !anyNA(x) && all(nzchar(trimws(x))),logical(1))), '対応表のキーとPARAMCDに空欄があります。')
    pk_assert(all(grepl('^[A-Z][A-Z0-9_]{0,7}$',pm$PARAMCD)), '対応表のPARAMCDは英大文字で始まる8文字以内の英大文字・数字・アンダースコアにしてください。')
    expected <- unique(candidate[map_keys]); joined <- rbind(expected,pm[map_keys])
    ids <- desktop_pk_group(joined,map_keys)$group
    expected_ids <- ids[seq_len(nrow(expected))]; supplied_ids <- ids[nrow(expected)+seq_len(nrow(pm))]
    pk_assert(!anyDuplicated(supplied_ids) && length(supplied_ids)==length(expected_ids) && setequal(supplied_ids,expected_ids),
      '対応表の元コード・分析対象・検体の組合せに重複、不足、または余分な行があります。ひな形を作り直して確認してください。')
    pk_assert(!anyDuplicated(pm$PARAMCD), '対応表のPARAMCDが重複しています。組合せごとに一意のコードを指定してください。')
    all_joined <- rbind(expected,candidate[map_keys]); all_ids <- desktop_pk_group(all_joined,map_keys)$group
    candidate$PARAMCD <- pm$PARAMCD[match(all_ids[nrow(expected)+seq_len(nrow(candidate))],supplied_ids)]
  }
  # Compare source identities too: equal labels must not conceal a code collision.
  code_sources <- unique(candidate[c('PARAMCD','NCAPCD',parameter_keys)])
  pk_assert(!anyDuplicated(code_sources$PARAMCD), '仮PARAMCDが衝突し、PARAMとの対応が一対一になりません。分析対象・検体の数や8文字制限を確認し、試験固有の対応表を用意してください。')
  # Never silently emit ambiguous, colliding or malformed PARAM/PARAMCD pairs.
  pairs <- unique(candidate[c('PARAMCD','PARAM')])
  pk_assert(!anyDuplicated(pairs$PARAMCD), '仮PARAMCDが衝突し、PARAMとの対応が一対一になりません。分析対象・検体の数や8文字制限を確認し、試験固有の対応表を用意してください。')
  pk_assert(!anyNA(pairs$PARAM) && all(nzchar(trimws(pairs$PARAM))) && !anyDuplicated(pairs$PARAM) &&
    all(grepl('^[A-Za-z][A-Za-z0-9_]{0,7}$',pairs$PARAMCD)),
    'PARAMCDとPARAMの対応が一対一ではないか、コードの形式が不正です。試験のパラメータ仕様を確認してください。')
  # Exact values, units, statuses, and the original NCA codes are retained.
  fields_meta <- names(candidate)
  labels <- c(STUDYID='Study Identifier', USUBJID='Unique Subject Identifier', PARAMCD='Parameter Code',
    PARAM='Parameter', AVAL='Analysis Value', AVALU='Analysis Value Unit',
    NCAPCD='Original NCA parameter code', NCAPSTAT='NCA parameter state', NCAEXCL='NCA exclusion reason', NCAWARN='NCA profile warnings', PROFSTAT='NCA profile state')
  metadata <- data.frame(DATASET='ADPP_CANDIDATE', VARIABLE=fields_meta,
    LABEL=vapply(fields_meta,function(x) if (x %in% names(labels)) unname(labels[[x]]) else '', ''),
    TYPE=vapply(candidate,function(x) if (is.numeric(x)) 'numeric' else 'character',''),
    ORIGIN=vapply(fields_meta,function(x) if (x %in% c('STUDYID','USUBJID',output_keys)) 'NCA input analysis key' else 'NCA result',''),
    METHOD=vapply(fields_meta,function(x) if (x %in% c('PARAMCD','PARAM','AVAL','AVALU')) 'Copied unchanged from NCA result' else 'Copied unchanged from NCA grouping key',''),
    REVIEW='Study specification and controlled terminology not verified',stringsAsFactors=FALSE)
  metadata$METHOD[metadata$VARIABLE %in% names(status_map)] <- 'Copied unchanged from NCA status, reason or warning'
  metadata$METHOD[metadata$VARIABLE=='NCAPCD'] <- 'Original PARAMCD copied unchanged from NCA result'
  if (length(parameter_cols)) {
    metadata$METHOD[metadata$VARIABLE=='PARAMCD'] <- 'NCA code retained for one analyte/specimen combination; otherwise NCA code plus provisional combination number'
    metadata$METHOD[metadata$VARIABLE=='PARAM'] <- 'NCA parameter label combined with recorded analyte, specimen and unit'
  }
  if (!is.null(parameter_mapping)) metadata$METHOD[metadata$VARIABLE=='PARAMCD'] <- 'Study-specific code supplied by user mapping; independently review'
  metadata$REVIEW[metadata$VARIABLE=='NCAPCD'] <- 'Original NCA code; provisional codes require study-specific mapping and CT review'
  metadata$REVIEW[metadata$VARIABLE=='PARAM'] <- 'NCA label with analyte, specimen and unit if roles are present; CDISC CT version and study meaning not verified'
  metadata$REVIEW[metadata$VARIABLE=='PARAM'] <- paste(metadata$REVIEW[metadata$VARIABLE=='PARAM'], 'Applicability of parameters to each specimen has not been verified.')
  if (any(d$PARAMCD %in% 'AUCPROC')) metadata$REVIEW[metadata$VARIABLE=='PARAM'] <- paste(
    metadata$REVIEW[metadata$VARIABLE=='PARAM'], 'AUCPROC is an NCA processing-defined candidate; controlled terminology not verified.')
  if (any(grepl('D$',as.character(d$PARAMCD)))) metadata$REVIEW[metadata$VARIABLE=='AVALU'] <- paste(
    metadata$REVIEW[metadata$VARIABLE=='AVALU'], 'Dose-normalized unit copied from NCA; e.g. 1/L or h/L may differ from concentration/dose presentation; verify conversion.')
  metadata$REVIEW[metadata$VARIABLE=='PARAMCD'] <- if (!is.null(parameter_mapping)) 'User-supplied code; study approval, controlled terminology and suitability not verified' else if (length(parameter_cols)) 'Code is provisional only; numbering depends on analyte/specimen combinations and chosen order; assign and validate per study' else 'NCA code copied; CT version and study meaning not verified'
  # We do not infer whether an analyte is a metabolite. The dose belongs to the
  # administered compound, so all such rows need a study-specific interpretation.
  if (length(analyte_cols) && any(as.character(d$PARAMCD) %in% names(desktop_nca_derived_parameters()))) {
    dose_note <- 'Dose-derived parameters use the administered compound dose; for metabolite analytes, review dose basis and interpretation, including whether CL/F and Vz/F are meaningful.'
    idx <- metadata$VARIABLE %in% c('PARAMCD','PARAM','AVAL','AVALU')
    metadata$REVIEW[idx] <- paste(metadata$REVIEW[idx],dose_note)
  }
  metadata$REVIEW[metadata$VARIABLE %in% c('NCAPSTAT','NCAEXCL','NCAWARN','PROFSTAT')] <- 'Copied NCA state/reason/warning; review study criteria; no analysis flag assigned'
  for (entry in roles) {
    target <- switch(entry$role,period='APERIOD / APERIODC candidate',treatment='TRTA candidate',analyte='PARCAT1 candidate (study design dependent)',specimen='Specimen category candidate (study design dependent)',study='STUDYID candidate',other='Study-specific', 'Study-specific')
    outcol <- unlist(mapping[entry$column],use.names=FALSE)
    metadata$REVIEW[metadata$VARIABLE %in% outcol] <- paste(target,'- mapping not applied; confirm with study specification')
  }
  param_map <- unique(candidate[c('NCAPCD','PARAMCD','PARAM','AVALU',parameter_keys)])
  review <- d
  if (!is.null(source_input)) pk_assert(is.data.frame(source_input),'入力データを確認してください。')
  if (is.null(provenance)) provenance <- list(status='not_supplied')
  pk_assert(is.list(provenance),'出力の由来を確認してください。')
  mapping_audit <- desktop_adpp_mapping_audit(value, parameter_mapping, mapping_inputs)
  record <- list(status='review_only_not_submission_ready', source='PK Desktop NCA result',
    software=list(pk_desktop=desktop_version, pknca=trace$pknca_version, r=trace$R),
    independent_pknca_comparison='not_performed',
    logical_content_hashes=list(algorithm='desktop_hash (SHA-256 of canonical JSON; not a CSV file hash)',
      candidate=desktop_hash(candidate), nca_result=desktop_hash(d), row_trace=desktop_hash(trace$rows),
      source_input=if (is.null(source_input)) NULL else desktop_hash(source_input)),
    source_input_hash_status=if (is.null(source_input)) 'not_supplied' else 'provided_not_independently_verified',
    output_provenance=provenance, parameter_mapping=param_map, analyte_order=analyte_order,
    parameter_mapping_source=if (is.null(parameter_mapping)) 'generated_provisional' else 'user_supplied',
    supplied_mapping_hash=if (is.null(parameter_mapping)) NULL else desktop_hash(parameter_mapping),
    parameter_mapping_method=mapping_audit$method, parameter_mapping_input_hashes=mapping_audit$input_hashes,
    terminology=list(version='not_verified', auto_mapping='not_applied', aucproc='nonstandard_nca_processing_measure_review'),
    nca_key_mapping=mapping,
    nca_specification=s, pknca_options=trace$pknca_options, nca_warnings=value$warnings,
    limitations=c('ADaM compliance and controlled terminology not verified',
      'Generated PARAMCD numbering depends on analyte/specimen combinations and selected order; supplied or generated codes require study review',
      'Other-group checks compare within study, subject, period, treatment, analyte and specimen; correlated misclassification and missing pairings can remain undetected',
      'Applicability of parameters to each specimen has not been verified',
      'No ANL01FL, ADSL join, treatment, period or visit derivations assigned',
      'Source input hash is caller-provided; verify against saved source and study records',
      'Dataset identifiers and keys require source and study-specification review',
      'Candidate values and warning/exclusion states must be reviewed together',
      'No Define-XML or XPT generated; no independent comparator was run'))
  list(data=candidate, metadata=metadata, review=review, trace=trace, record=record)
}

# Write a local review package. Existing destinations are never overwritten.
pkapp_write_adpp_draft <- function(value, destination, source_input = NULL, provenance = NULL, analyte_order = NULL, parameter_mapping = NULL, mapping_inputs = NULL) {
  pk_assert(is.character(destination) && length(destination)==1L && !is.na(destination) && nzchar(destination), '出力先を確認してください。')
  pk_assert(requireNamespace('zip',quietly=TRUE), 'レビュー用ZIPの作成にはRパッケージ zip が必要です。')
  link <- Sys.readlink(destination)
  pk_assert(!file.exists(destination) && (is.na(link) || !nzchar(link)) && dir.exists(dirname(destination)), '出力先が存在するか、フォルダがありません。')
  draft <- pkapp_adpp_draft(value,source_input=source_input,provenance=provenance,analyte_order=analyte_order,parameter_mapping=parameter_mapping,mapping_inputs=mapping_inputs)
  stage <- tempfile('pk-adpp-review-'); dir.create(stage,mode='0700'); on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  paths <- c('ADPP_CANDIDATE.csv','ADPP_CANDIDATE_METADATA.csv','ADPP_CANDIDATE_REVIEW.csv','NCA_ROW_HANDLING.csv','NCA_TRACE.json','ADPP_CANDIDATE_RECORD.json','PARAMETER_MAPPING.csv')
  pk_write_csv(draft$data,file.path(stage,paths[1])); pk_write_csv(draft$metadata,file.path(stage,paths[2]))
  pk_write_csv(draft$review,file.path(stage,paths[3])); pk_write_csv(draft$trace$rows,file.path(stage,paths[4]))
  desktop_atomic(draft$trace,file.path(stage,paths[5])); desktop_atomic(draft$record,file.path(stage,paths[6]))
  pk_write_csv(draft$record$parameter_mapping,file.path(stage,paths[7]))
  info <- lapply(paths,function(name) { p<-file.path(stage,name); list(name=name,bytes=unname(file.info(p)$size),sha256=digest::digest(file=p,algo='sha256',serialize=FALSE)) })
  desktop_atomic(list(format='pk-adpp-review-manifest-1',status='review_only_not_submission_ready',files=info),file.path(stage,'MANIFEST.json'))
  paths <- c(paths,'MANIFEST.json'); Sys.chmod(file.path(stage,paths),'0600')
  temp_zip <- tempfile('.pk-adpp-',tmpdir=dirname(destination),fileext='.zip'); on.exit(unlink(temp_zip),add=TRUE)
  zip::zipr(zipfile=temp_zip,files=paths,root=stage,include_directories=FALSE)
  pk_assert(file.exists(temp_zip) && file.info(temp_zip)$size>0, 'ZIPの作成に失敗しました。')
  # A hard link in the same directory fails atomically if destination exists.
  pk_assert(isTRUE(file.link(temp_zip,destination)), '出力先を作成できません。既存ファイルとファイルシステムを確認してください。')
  Sys.chmod(destination,'0600'); invisible(destination)
}

# A blank mapping can be created even when provisional codes would collide.
pkapp_adpp_mapping_template <- function(value) {
  pk_assert(is.list(value) && is.data.frame(value$data) && is.list(value$specification) && is.list(value$trace), 'NCAの結果を確認してください。')
  roles <- value$specification$group_roles %||% list()
  cols <- vapply(Filter(function(x) x$role %in% c('analyte','specimen'),roles), `[[`, '', 'column')
  keys <- unname(unlist(value$trace$output_key_mapping[cols],use.names=FALSE))
  pk_assert(length(keys)==length(cols) && all(keys %in% names(value$data)) && 'PARAMCD' %in% names(value$data) &&
    !anyDuplicated(c('NCAPCD',keys,'PARAMCD')), '分析対象・検体の列とNCA元コードを確認してください。')
  template <- unique(value$data[c('PARAMCD',keys)])
  names(template)[1] <- 'NCAPCD'
  template$PARAMCD <- rep('',nrow(template))
  template
}


# CSV decoding must fail before parsing; read.csv may otherwise return partial rows.
desktop_adpp_read_mapping_csv <- function(path) {
  msg <- '対応表CSVを読み込めません。UTF-8で保存してください（Excelでは「CSV UTF-8」を選択）。'
  tryCatch({
    size <- file.info(path)$size
    pk_assert(length(size)==1L && !is.na(size) && size >= 0 && size <= .Machine$integer.max, msg)
    bytes <- readBin(path, 'raw', n=as.integer(size))
    if (length(bytes)>=3L && identical(bytes[1:3],as.raw(c(239,187,191)))) bytes <- bytes[-(1:3)]
    pk_assert(!any(bytes==as.raw(0)), msg)
    input <- rawToChar(bytes)
    input <- iconv(input,from='UTF-8',to='UTF-8',sub=NA_character_)
    pk_assert(length(input)==1L && !is.na(input),msg)
    con <- textConnection(input,open='r',local=TRUE); on.exit(close(con),add=TRUE)
    withCallingHandlers(utils::read.csv(con,colClasses='character',check.names=FALSE,
      na.strings=character(),stringsAsFactors=FALSE),
      warning=function(w) stop('対応表CSVの形式を確認してください。',call.=FALSE))
  }, error=function(e) {
    if (identical(conditionMessage(e),'対応表CSVの形式を確認してください。')) stop(conditionMessage(e),call.=FALSE)
    stop(msg,call.=FALSE)
  })
}

pkapp_adpp_suffix_template <- function(value) {
  full <- pkapp_adpp_mapping_template(value)
  keys <- setdiff(names(full),c('NCAPCD','PARAMCD'))
  pk_assert(length(keys)>0L && !'SUFFIX' %in% keys, '接尾辞方式には分析対象または検体の役割が必要です。')
  out <- unique(full[keys]); out$SUFFIX <- rep('',nrow(out)); out
}

# Expand to the existing full mapping. Never truncate or invent a code.
desktop_adpp_expand_suffix_mapping <- function(value, suffix_mapping, overrides=NULL, validate_codes=TRUE) {
  full <- pkapp_adpp_mapping_template(value)
  simple <- pkapp_adpp_suffix_template(value)
  keys <- setdiff(names(simple),'SUFFIX')
  pk_assert(is.data.frame(suffix_mapping) && !anyDuplicated(names(suffix_mapping)) &&
    setequal(names(suffix_mapping),names(simple)), '接尾辞対応表の列をひな形に合わせてください。')
  sm <- as.data.frame(lapply(suffix_mapping,as.character),stringsAsFactors=FALSE,check.names=FALSE)
  pk_assert(nrow(sm)>0L && all(vapply(sm[keys],function(x) !anyNA(x) && all(nzchar(trimws(x))),logical(1))) && !anyNA(sm$SUFFIX), '接尾辞対応表の分析対象・検体を確認してください。')
  pk_assert(sum(sm$SUFFIX=='')<=1L, '接尾辞なしは分析対象・検体の1組だけ指定できます。')
  pk_assert(all(grepl('^[A-Z0-9_]*$',sm$SUFFIX)), '接尾辞は英大文字・数字・アンダースコアで指定してください。')
  groups <- desktop_pk_group(rbind(simple[keys],sm[keys]),keys)$group
  expected <- groups[seq_len(nrow(simple))]; supplied <- groups[nrow(simple)+seq_len(nrow(sm))]
  pk_assert(!anyDuplicated(supplied) && length(supplied)==length(expected) && setequal(supplied,expected),
    '接尾辞対応表の分析対象・検体に重複、不足、または余分な行があります。')
  full_groups <- desktop_pk_group(rbind(simple[keys],full[keys]),keys)$group
  full$PARAMCD <- paste0(as.character(full$NCAPCD),sm$SUFFIX[match(full_groups[nrow(simple)+seq_len(nrow(full))],supplied)])
  if (!is.null(overrides)) {
    pk_assert(is.data.frame(overrides) && !anyDuplicated(names(overrides)) && setequal(names(overrides),names(full)),
      '個別指定の列を対応表ひな形に合わせてください。')
    ov <- as.data.frame(lapply(overrides,as.character),stringsAsFactors=FALSE,check.names=FALSE)
    if (nrow(ov)) {
      pk_assert(all(vapply(ov,function(x) !anyNA(x) && all(nzchar(trimws(x))),logical(1))), '個別指定に空欄があります。')
      map_keys <- setdiff(names(full),'PARAMCD')
      groups <- desktop_pk_group(rbind(full[map_keys],ov[map_keys]),map_keys)$group
      valid <- groups[seq_len(nrow(full))]; supplied <- groups[nrow(full)+seq_len(nrow(ov))]
      pk_assert(!anyDuplicated(supplied) && all(supplied %in% valid), '個別指定に重複または余分な組合せがあります。')
      full$PARAMCD[match(supplied,valid)] <- ov$PARAMCD
    }
  }
  if (validate_codes) {
    pk_assert(all(grepl('^[A-Z][A-Z0-9_]{0,7}$',full$PARAMCD)),
      '接尾辞を付けたPARAMCDの形式または8文字制限を確認し、必要な行を個別指定してください。')
    pk_assert(!anyDuplicated(full$PARAMCD), '接尾辞または個別指定によるPARAMCDが重複しています。')
  }
  full
}

pkapp_adpp_expand_suffix_mapping <- function(value, suffix_mapping, overrides=NULL) {
  desktop_adpp_expand_suffix_mapping(value,suffix_mapping,overrides,validate_codes=TRUE)
}

pkapp_adpp_suffix_override_template <- function(value, suffix_mapping) {
  full <- desktop_adpp_expand_suffix_mapping(value,suffix_mapping,validate_codes=FALSE)
  full <- full[nchar(full$PARAMCD,type='chars')>8L,,drop=FALSE]
  full$PARAMCD <- rep('',nrow(full))
  full
}

# Provenance is derived from the actual supplied tables and checked against the
# mapping used for export. Hashes identify canonical data-frame content, not CSV bytes.
desktop_adpp_mapping_audit <- function(value, parameter_mapping, mapping_inputs=NULL) {
  algorithm <- 'desktop_hash (SHA-256 of canonical JSON; not a CSV file hash)'
  if (is.null(mapping_inputs)) return(list(method=if (is.null(parameter_mapping)) 'generated_provisional' else 'user_supplied_unspecified', input_hashes=NULL))
  pk_assert(!is.null(parameter_mapping) && is.list(mapping_inputs) && !anyDuplicated(names(mapping_inputs)) &&
    is.character(mapping_inputs$method) && length(mapping_inputs$method)==1L && !is.na(mapping_inputs$method), '対応表の記録を確認してください。')
  if (identical(mapping_inputs$method,'individual')) {
    pk_assert(setequal(names(mapping_inputs),c('method','mapping')) && is.data.frame(mapping_inputs$mapping), '個別対応表の記録を確認してください。')
    pk_assert(identical(desktop_hash(parameter_mapping),desktop_hash(mapping_inputs$mapping)), '対応表と記録対象が一致しません。')
    return(list(method='individual', input_hashes=list(algorithm=algorithm,individual=desktop_hash(mapping_inputs$mapping))))
  }
  pk_assert(identical(mapping_inputs$method,'suffix') && setequal(names(mapping_inputs),c('method','suffix_mapping','overrides')) &&
    is.data.frame(mapping_inputs$suffix_mapping) && (is.null(mapping_inputs$overrides) || is.data.frame(mapping_inputs$overrides)), '接尾辞対応表の記録を確認してください。')
  expanded <- pkapp_adpp_expand_suffix_mapping(value,mapping_inputs$suffix_mapping,mapping_inputs$overrides)
  pk_assert(identical(desktop_hash(parameter_mapping),desktop_hash(expanded)), '対応表と記録対象が一致しません。')
  list(method='suffix',input_hashes=list(algorithm=algorithm,suffix=desktop_hash(mapping_inputs$suffix_mapping),
    overrides=if (is.null(mapping_inputs$overrides)) NULL else desktop_hash(mapping_inputs$overrides)))
}

# For local display only: never put submitted values in MCP/API error messages.
desktop_adpp_mapping_diagnostics <- function(value, mapping) {
  expected <- pkapp_adpp_mapping_template(value)
  keys <- setdiff(names(expected),'PARAMCD')
  if (!is.data.frame(mapping) || anyDuplicated(names(mapping)) || !all(keys %in% names(mapping))) return(NULL)
  x <- as.data.frame(lapply(mapping[keys],as.character),stringsAsFactors=FALSE,check.names=FALSE)
  e <- as.data.frame(lapply(expected[keys],as.character),stringsAsFactors=FALSE,check.names=FALSE)
  if (anyNA(x) || anyNA(e)) return(NULL)
  g <- desktop_pk_group(rbind(e,x),keys)$group
  a <- g[seq_len(nrow(e))]; b <- g[nrow(e)+seq_len(nrow(x))]
  visible <- function(v) { v <- gsub(' ', '␠',v,fixed=TRUE); v <- gsub('\t','\\t',v,fixed=TRUE); v <- gsub('\n','\\n',v,fixed=TRUE); v }
  describe <- function(z) if (!nrow(z)) character() else apply(head(z,3L),1L,function(row) paste(paste0(names(z),'=',visible(row)),collapse='、'))
  list(missing=describe(e[!a %in% b,,drop=FALSE]), extra=describe(x[!b %in% a,,drop=FALSE]),
    duplicate=describe(x[duplicated(b),,drop=FALSE]))
}
