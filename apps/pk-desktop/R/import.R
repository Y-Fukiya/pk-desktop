# Import to the established strict bundle contract; source hashes and mappings are retained.
pkapp_map_columns <- function(data, mapping = list()) {
  pkapp_table_check(data)
  if (!length(mapping)) return(data)
  pk_assert(!is.null(names(mapping)) && !anyDuplicated(names(mapping)) && all(nzchar(names(mapping))) &&
    all(lengths(mapping) == 1), "マッピングは変換先名 = 元の列名で指定してください。")
  sources <- unlist(mapping, use.names = FALSE)
  pk_assert(is.character(sources) && !anyNA(sources) && !anyDuplicated(sources) && all(sources %in% names(data)), "マッピング元の列が存在しないか重複しています。")
  names(data)[match(sources, names(data))] <- names(mapping)
  pkapp_table_check(data)
  data
}
pkapp_import_bundle <- function(adnca, adpp, destination, mapping = list(ADNCA = list(), ADPP = list()), time_unit = "h") {
  pk_assert(!dir.exists(destination) && !file.exists(destination), "新しい出力フォルダーを指定してください。")
  pk_assert(length(time_unit) == 1 && time_unit %in% c("h", "min", "day"), "時間の単位をh/min/dayから指定してください。")
  paths <- list(ADNCA = normalizePath(adnca, mustWork = TRUE), ADPP = normalizePath(adpp, mustWork = TRUE))
  hashes <- lapply(paths, pk_hash)
  data <- lapply(names(paths), function(name) pkapp_map_columns(pkapp_read_table(paths[[name]]), mapping[[name]] %||% list()))
  names(data) <- names(paths)
  for (name in intersect(c("TIME_H", "TAD_H", "ANALYSIS_DOSE_TIME_H"), names(data$ADNCA))) {
    data$ADNCA[[name]] <- pk_number(data$ADNCA[[name]], name, allow_na = name != "TIME_H") * switch(time_unit, h = 1, min = 1/60, day = 24)
  }
  parent <- dirname(destination)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(".import-", parent); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  for (name in names(data)) pk_write_csv(data[[name]], file.path(stage, paste0(name, ".csv")))
  validated <- pk_load_bundle(stage)
  pk_assert(identical(hashes, lapply(paths, pk_hash)), "読み込み中に入力ファイルが変わりました。")
  provenance <- list(version = 2L, files = validated$hashes, import = list(source_hashes = hashes,
    source_formats = lapply(paths, tools::file_ext), mapping = mapping, input_time_unit = time_unit, output_time_unit = "h"))
  desktop_atomic(provenance, file.path(stage, "BUNDLE.json"))
  pk_load_bundle(stage)
  pk_assert(file.rename(stage, destination), "入力セットを保存できません。")
  normalizePath(destination)
}
desktop_load_bundle <- function(engine, bundle, base_revision, project_id, channel, actor, reason) {
  desktop_revision(engine, base_revision, project_id)
  pk_assert(channel %in% c("ui", "mcp", "r") && is.character(actor) && length(actor) == 1 && nzchar(actor) &&
    is.character(reason) && length(reason) == 1 && nzchar(reason), "操作元と変更理由を指定してください。")
  # Reuse the creation path for full validation, defaults and a content-addressed snapshot.
  stage <- tempfile(".candidate-"); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  candidate <- desktop_new(stage, bundle, channel, actor)
  on.exit(desktop_jobs_close(candidate), add = TRUE)
  destination <- file.path(engine$project, "data", desktop_hash(candidate$data$hashes))
  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
    for (f in names(candidate$data$hashes)) pk_assert(file.copy(file.path(candidate$spec$data$bundle, f), file.path(destination, f)), "入力の保存に失敗しました。")
  }
  data <- pk_load_bundle(destination)
  pk_assert(identical(data$hashes, candidate$data$hashes), "入力スナップショットが一致しません。")
  spec <- candidate$spec
  spec$project_id <- engine$spec$project_id
  spec$revision <- engine$spec$revision + 1L
  spec$data$bundle <- normalizePath(destination)
  spec$report <- engine$spec$report
  audit <- c(engine$audit, list(list(at = desktop_time(), channel = channel, actor = actor, reason = reason, before = engine$spec, after = spec)))
  desktop_atomic(list(spec = spec, audit = audit), file.path(engine$project, "state.json"))
  engine$data <- data; engine$info <- candidate$info; engine$info$bundle <- normalizePath(destination)
  engine$spec <- spec; engine$audit <- audit
  engine$data_signal(shiny::isolate(engine$data_signal()) + 1L)
  engine$signal(spec$revision)
  list(revision = spec$revision, analysis_hash = desktop_analysis_hash(spec))
}
pkapp_load_data <- function(project, bundle, base_revision = pkapp_spec(project)$revision, reason, actor = "R user") {
  engine <- desktop_r_engine(project)
  desktop_load_bundle(engine, bundle, base_revision, engine$spec$project_id, "r", actor, reason)
}
desktop_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML('
.pk-import .pk-mapping-row { display:grid; grid-template-columns:minmax(120px,.8fr) minmax(160px,1.2fr); gap:10px; padding:12px 0; border-bottom:1px solid var(--pk-line); align-items:start; }
.pk-import .pk-mapping-label { font-size:13px; line-height:1.6; padding-top:6px; }
.pk-import .pk-mapping-label code { display:block; background:none; padding:0; color:var(--pk-muted); font-size:11px; }
.pk-import .pk-mapping-row .form-group { margin-bottom:3px; }
.pk-import .pk-mapping-sample { font-size:12px; color:var(--pk-muted); overflow-wrap:anywhere; line-height:1.6; }
.pk-import .pk-mapping-required { font-size:11px; color:#9a3e25; margin-left:5px; }
.pk-import .pk-import-file { font-size:14px; font-weight:600; overflow-wrap:anywhere; }
.pk-import .pk-import-staged { margin-top:14px; }
.pk-import .pk-import-actions { display:flex; flex-direction:column; gap:10px; }
@media(max-width:600px) { .pk-import .pk-mapping-row { grid-template-columns:minmax(0,1fr); gap:2px; } }
')))), shiny::div(class = "pk-import", desktop_ui_page(
    "解析データの取込", "ファイルや保存した表から濃度とPKパラメータを選び、列の対応を指定します。検証した内容を確認してから解析へ適用できます。",
    controls = shiny::tagList(
      desktop_ui_section("1", "入力データを選ぶ",
        shiny::selectInput(ns("adnca_source"), "濃度データ（ADNCA）の入力元", c("ファイル" = "FILE", "現在の解析：ADNCA" = "ADNCA", "現在の解析：ADPP" = "ADPP")),
        shiny::conditionalPanel("input.adnca_source === 'FILE'", ns = ns,
          shiny::fileInput(ns("adnca"), "濃度データのファイル", buttonLabel = desktop_i18n_text("ファイルを選択", "Choose file"), placeholder = "未選択",
            accept = c(".csv", ".xpt", ".sas7bdat", ".rds", ".parquet"))),
        shiny::selectInput(ns("adpp_source"), "PKパラメータ（ADPP）の入力元", c("ファイル" = "FILE", "現在の解析：ADNCA" = "ADNCA", "現在の解析：ADPP" = "ADPP")),
        shiny::conditionalPanel("input.adpp_source === 'FILE'", ns = ns,
          shiny::fileInput(ns("adpp"), "PKパラメータのファイル", buttonLabel = desktop_i18n_text("ファイルを選択", "Choose file"), placeholder = "未選択",
            accept = c(".csv", ".xpt", ".sas7bdat", ".rds", ".parquet"))),
        shiny::p(class = "pk-caption", "保存した前処理・結合の表を直接選べます。ファイルはCSV / XPT / SAS7BDAT / RDS / Parquetに対応。元のデータは変更しません。")),
      desktop_ui_section("3", "検証して解析へ適用",
        shiny::selectInput(ns("unit"), "濃度データの時間単位（必須）",
          c("選択してください" = "", "時間（h）" = "h", "分（min）" = "min", "日（day）" = "day")),
        shiny::p(class = "pk-caption", "TIME_H・TAD_H・ANALYSIS_DOSE_TIME_Hを時間に換算します。濃度・PK値の単位は各表のAVALU列から読み込みます。"),
        shiny::textInput(ns("reason"), "変更理由（必須・履歴に保存）", "解析データの取込"),
        shiny::div(class = "pk-import-actions", shiny::uiOutput(ns("validate_action")), shiny::uiOutput(ns("apply_action"))),
        shiny::uiOutput(ns("status")),
        shiny::tags$details(shiny::tags$summary("適用時に変わるもの・保持されるもの"),
          shiny::p(class = "pk-caption", "解析用の入力データを置き換え、対象者・描画条件・層別条件を新しいデータの初期値に戻します。保存済みRun・帳票と変更履歴は保持されます。")))),
    body = shiny::tagList(
      desktop_ui_section("2", "列の対応を確認",
        shiny::radioButtons(ns("mapping_mode"), "指定方法", c("列を選択" = "fields", "JSONで指定（詳細）" = "json"), inline = TRUE),
        shiny::conditionalPanel("input.mapping_mode === 'fields'", ns = ns,
          shiny::p(class = "pk-caption", "同じ列名だけを自動選択します。元の値と読込型を確かめて、対応する列を選んでください。その他の列も保持されます。")),
        shiny::conditionalPanel("input.mapping_mode === 'json'", ns = ns,
          desktop_ui_notice("現在はJSONの対応を使用します。下の「JSONを編集」を開いて指定してください。列選択の設定は使われません。"),
          shiny::tags$details(shiny::tags$summary("JSONを編集"),
            shiny::textAreaInput(ns("mapping"), "変換先の列名: 元の列名", '{"ADNCA":{},"ADPP":{}}', rows = 7, width = "100%"),
            shiny::p(class = "pk-caption", '例：{"ADNCA":{"USUBJID":"ID","TIME_H":"TIME","AVAL":"CONC"},"ADPP":{"USUBJID":"ID"}}'))),
        shiny::uiOutput(ns("mapping_summary"))),
      desktop_ui_section(NULL, "濃度データ · ADNCA", shiny::uiOutput(ns("source_adnca")),
        shiny::conditionalPanel("input.mapping_mode === 'fields'", ns = ns, shiny::uiOutput(ns("map_adnca")))),
      desktop_ui_section(NULL, "PKパラメータ · ADPP", shiny::uiOutput(ns("source_adpp")),
        shiny::conditionalPanel("input.mapping_mode === 'fields'", ns = ns, shiny::uiOutput(ns("map_adpp")))),
      shiny::uiOutput(ns("validation_result"))))))
}

# These helpers describe presentation fields only. Validation remains in pkapp_import_bundle.
desktop_import_fields <- function(name) {
  required <- if (name == "ADNCA") c("USUBJID", "TIME_H", "AVAL", "AVALU") else c("USUBJID", "PARAMCD", "AVAL", "AVALU")
  optional <- if (name == "ADNCA") c("STUDYID", "PARAMCD", "TAD_H", "ANALYSIS_DOSE_TIME_H", "DOSE_MG", "ARM", "SEX",
    "ANALYTE", "MDV", "BLQ", "LLOQ", "SSNCAFL", "TROUGHFL", "ANL01FL", "AGE", "WT", "ROUTE") else
    c("STUDYID", "PARAM", "DOSE_MG", "ARM", "SEX", "ANALYTE", "AGE", "WT", "ROUTE")
  list(required = required, optional = optional)
}
desktop_import_label <- function(target) {
  labels <- c(USUBJID = "被験者ID", TIME_H = "採血時刻", AVAL = "測定値・PK値", AVALU = "値の単位", PARAMCD = "パラメータコード",
    STUDYID = "試験ID", TAD_H = "投与後時間", ANALYSIS_DOSE_TIME_H = "解析基準の投与時刻", DOSE_MG = "投与量", ARM = "投与群",
    SEX = "性別", ANALYTE = "測定対象", MDV = "欠測フラグ", BLQ = "定量限界未満フラグ", LLOQ = "定量下限",
    SSNCAFL = "定常状態フラグ", TROUGHFL = "トラフフラグ", ANL01FL = "解析対象フラグ", AGE = "年齢", WT = "体重", ROUTE = "投与経路", PARAM = "パラメータ名")
  unname(labels[[target]])
}
desktop_import_sample <- function(x) {
  kind <- if (is.numeric(x)) "数値" else if (is.logical(x)) "論理値" else "文字列"
  values <- utils::head(x, 3L)
  values <- ifelse(is.na(values), "欠損", as.character(values))
  values[values == ""] <- "空欄"
  values <- ifelse(nchar(values) > 45L, paste0(substr(values, 1L, 45L), "…"), values)
  paste0("読込型：", kind, " · 先頭の値：", if (length(values)) paste(values, collapse = " / ") else "行がありません")
}
desktop_import_server <- function(id, engine) shiny::moduleServer(id, function(input, output, session) {
  notice <- shiny::reactiveVal("")
  notice_tone <- shiny::reactiveVal("info")
  staged <- shiny::reactiveVal(NULL)
  mode <- shiny::reactive(input$mapping_mode %||% "fields")
  datasets <- c("ADNCA", "ADPP")
  map_id <- function(name, target) paste("map", tolower(name), target, sep = "_")
  upload <- function(name) input[[tolower(name)]]
  source_id <- function(name) input[[paste0(tolower(name), "_source")]] %||% "FILE"
  shiny::observeEvent(engine$tables_signal(), {
    tables <- desktop_tables(engine)
    choices <- c("ファイル" = "FILE", "現在の解析：ADNCA" = "ADNCA", "現在の解析：ADPP" = "ADPP",
      stats::setNames(vapply(tables, function(x) x$id, character(1)),
        vapply(tables, function(x) paste0("保存表：", x$name, " · ", substr(x$id, 7, 12)), character(1))))
    for (name in datasets) shiny::updateSelectInput(session, paste0(tolower(name), "_source"), choices = choices,
      selected = if (source_id(name) %in% choices) source_id(name) else "FILE")
  }, ignoreNULL = FALSE)
  fingerprint <- function(name) {
    selected <- source_id(name)
    if (selected %in% datasets) {
      engine$data_signal()
      return(list(kind = "analysis", source = selected, name = paste0("現在の解析：", selected),
        hash = digest::digest(engine$data[[selected]], algo = "sha256"), bundle_hashes = engine$data$hashes))
    }
    if (selected != "FILE") {
      desktop_table_get(engine, selected) # Verifies the saved immutable snapshot, including its recipe.
      folder <- desktop_table_path(engine, selected)
      manifest <- desktop_read(file.path(folder, "manifest.json"))
      return(list(kind = "saved_table", source = selected, name = paste0("保存表：", manifest$name),
        hash = pk_hash(file.path(folder, "result.rds")), manifest_hash = pk_hash(file.path(folder, "manifest.json"))))
    }
    x <- upload(name)
    pk_assert(!is.null(x) && length(x$name) == 1L && length(x$datapath) == 1L && file.exists(x$datapath),
      paste(name, "のファイルを選択してください。"))
    list(kind = "file", source = "FILE", name = x$name, path = x$datapath, hash = pk_hash(x$datapath))
  }
  read_upload <- function(name) shiny::reactive({
    selected <- source_id(name)
    x <- upload(name)
    if (selected == "FILE" && is.null(x)) return(list(data = NULL, error = NULL))
    tryCatch({
      before <- fingerprint(name)
      data <- if (selected == "FILE") pkapp_read_table(x$datapath, tolower(tools::file_ext(x$name))) else desktop_source(engine, selected)
      pkapp_table_check(data)
      pk_assert(identical(before, fingerprint(name)), "読み込み中に入力データが変わりました。選択し直してください。")
      list(data = data, fingerprint = before, error = NULL)
    },
      error = function(e) list(data = NULL, error = conditionMessage(e)))
  })
  sources <- stats::setNames(lapply(datasets, read_upload), datasets)
  selected_source <- function(name, target, data) {
    value <- input[[map_id(name, target)]]
    if (is.null(value)) if (target %in% names(data)) target else "" else value
  }
  mapping <- shiny::reactive({
    pk_assert(mode() %in% c("fields", "json"), "列の指定方法を選択してください。")
    if (mode() == "json") {
      value <- jsonlite::fromJSON(input$mapping %||% "", simplifyVector = FALSE)
      pk_assert(is.list(value) && !is.null(names(value)) && !anyDuplicated(names(value)) &&
        all(names(value) %in% datasets) && all(vapply(value, is.list, logical(1))),
        "JSONはADNCA・ADPPごとに列の対応を指定してください。")
      return(value)
    }
    stats::setNames(lapply(datasets, function(name) {
      source <- sources[[name]]()
      pk_assert(is.null(source$error), paste(name, source$error))
      pk_assert(!is.null(source$data), paste(name, "のファイルを選択してください。"))
      fields <- desktop_import_fields(name)
      targets <- c(fields$required, fields$optional)
      values <- stats::setNames(lapply(targets, function(target) selected_source(name, target, source$data)), targets)
      missing <- fields$required[!vapply(values[fields$required], function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x), logical(1))]
      pk_assert(!length(missing), paste(name, "の必須列を選択してください：", paste(missing, collapse = "、")))
      values[vapply(values, function(x) length(x) == 1L && !is.na(x) && nzchar(x), logical(1))]
    }), datasets)
  })
  # Raw input state invalidates a preview even when the new mapping is invalid.
  input_state <- shiny::reactive({
    list(sources = lapply(datasets, function(name) list(id = source_id(name),
      upload = if (source_id(name) == "FILE") upload(name), fingerprint = sources[[name]]()$fingerprint)), mode = mode(), unit = input$unit,
      mapping = if (mode() == "json") input$mapping else lapply(datasets, function(name) {
        data <- sources[[name]]()$data
        lapply(unlist(desktop_import_fields(name), use.names = FALSE), function(target) selected_source(name, target, data))
      }))
  })
  clear_stage <- function() {
    value <- shiny::isolate(staged())
    if (!is.null(value)) unlink(value$folder, recursive = TRUE)
    staged(NULL)
  }
  session$onSessionEnded(clear_stage)
  shiny::observeEvent(input_state(), {
    if (!is.null(staged())) {
      clear_stage()
      notice("入力データ・列の対応・時間単位が変わりました。もう一度検証してください。")
      notice_tone("warning")
    }
  }, ignoreInit = TRUE, priority = 100)
  is_current <- shiny::reactive({
    engine$signal()
    value <- staged()
    !is.null(value) && identical(value$state, input_state()) && identical(value$revision, engine$spec$revision)
  })
  source_ready <- shiny::reactive(all(vapply(sources, function(x) !is.null(x()$data), logical(1))))
  output$validate_action <- shiny::renderUI({
    button <- shiny::actionButton(session$ns("validate_import"), "入力内容を検証", class = "btn-default")
    if (!source_ready()) button$attribs$disabled <- "disabled"
    button
  })
  output$apply_action <- shiny::renderUI({
    button <- shiny::actionButton(session$ns("load"), "解析に適用", class = "btn-primary")
    if (!is_current() || !nzchar(trimws(input$reason %||% ""))) button$attribs$disabled <- "disabled"
    button
  })
  for (name in datasets) local({
    dataset <- name
    output[[paste0("source_", tolower(dataset))]] <- shiny::renderUI({
      source <- sources[[dataset]]()
      if (!is.null(source$error)) return(desktop_ui_notice(paste("入力データを読み込めません：", source$error), "error"))
      if (is.null(source$data)) return(desktop_ui_empty("ファイルを選択してください", paste(dataset, "を読み込むと、列の対応と先頭5行が表示されます。")))
      shiny::tagList(shiny::p(class = "pk-import-file", source$fingerprint$name),
        shiny::p(class = "pk-caption", paste(format(nrow(source$data), big.mark = ","), "行 ·", ncol(source$data), "列")),
        shiny::tags$details(shiny::tags$summary("元データの先頭5行を見る"),
          shiny::div(class = "pk-table-scroll", shiny::tableOutput(session$ns(paste0("preview_", tolower(dataset)))))))
    })
    output[[paste0("preview_", tolower(dataset))]] <- shiny::renderTable({
      shiny::req(sources[[dataset]]()$data)
      utils::head(sources[[dataset]]()$data, 5L)
    }, rownames = FALSE)
    output[[paste0("map_", tolower(dataset))]] <- shiny::renderUI({
      data <- sources[[dataset]]()$data
      if (is.null(data)) return(NULL)
      fields <- desktop_import_fields(dataset)
      row <- function(target, required) shiny::div(class = "pk-mapping-row",
        shiny::div(class = "pk-mapping-label", desktop_import_label(target), if (required) shiny::span(class = "pk-mapping-required", "必須"), shiny::tags$code(target)),
        shiny::div(shiny::selectInput(session$ns(map_id(dataset, target)), paste0(target, " に対応する元の列"),
          choices = c(stats::setNames("", if (required) "元の列を選択" else "列名を変更しない"), stats::setNames(names(data), names(data))),
          selected = if (target %in% names(data)) target else ""),
          shiny::div(class = "pk-mapping-sample", shiny::textOutput(session$ns(paste0("sample_", map_id(dataset, target)))))))
      shiny::tagList(lapply(fields$required, row, required = TRUE),
        shiny::tags$details(shiny::tags$summary("任意列の対応を変更"),
          shiny::p(class = "pk-caption", "同名の列はそのまま使用します。任意列に別名の列を対応させる場合に変更してください。"),
          lapply(fields$optional, row, required = FALSE)))
    })
    for (target in unlist(desktop_import_fields(dataset), use.names = FALSE)) local({
      field <- target
      output[[paste0("sample_", map_id(dataset, field))]] <- shiny::renderText({
        data <- sources[[dataset]]()$data
        column <- selected_source(dataset, field, data)
        if (is.null(data) || !length(column) || !nzchar(column) || !(column %in% names(data))) return("対応する列を選ぶと、元の値を確認できます。")
        desktop_import_sample(data[[column]])
      })
    })
  })
  output$mapping_summary <- shiny::renderUI({
    if (!source_ready()) return(NULL)
    tryCatch({
      value <- mapping()
      for (name in datasets) pkapp_map_columns(sources[[name]]()$data, value[[name]] %||% list())
      shiny::p(class = "pk-caption", "列の対応を指定済みです。「入力内容を検証」で単位・キーの重複・被験者の一致も確認します。")
    }, error = function(e) desktop_ui_notice(conditionMessage(e), "warning"))
  })
  shiny::observeEvent(input$validate_import, {
    clear_stage()
    tryCatch({
      originals <- stats::setNames(lapply(datasets, fingerprint), datasets)
      for (name in datasets) {
        source <- sources[[name]]()
        pk_assert(is.null(source$error), paste(name, source$error))
        pk_assert(identical(originals[[name]], source$fingerprint), paste(name, "の入力データが変更されています。選択し直してください。"))
      }
      selected_mapping <- mapping()
      pk_assert(length(input$unit) == 1L && input$unit %in% c("h", "min", "day"), "入力の時間単位を選択してください。")
      folder <- tempfile(".upload-"); dir.create(folder)
      keep <- FALSE
      on.exit(if (!keep) unlink(folder, recursive = TRUE), add = TRUE)
      paths <- vapply(datasets, function(name) {
        if (source_id(name) != "FILE") {
          path <- file.path(folder, paste0(name, ".rds"))
          saveRDS(sources[[name]]()$data, path, version = 3L)
          return(path)
        }
        x <- upload(name)
        path <- file.path(folder, paste0(name, ".", tolower(tools::file_ext(x$name))))
        pk_assert(file.copy(x$datapath, path), "アップロードを読み取れません。")
        path
      }, character(1))
      bundle <- pkapp_import_bundle(paths[1], paths[2], file.path(folder, "bundle"), selected_mapping, input$unit)
      manifest <- desktop_read(file.path(bundle, "BUNDLE.json"))
      manifest$import$workspace_sources <- lapply(originals, function(x) x[setdiff(names(x), "path")])
      manifest$import$base_revision <- engine$spec$revision
      desktop_atomic(manifest, file.path(bundle, "BUNDLE.json"))
      data <- pk_load_bundle(bundle)
      # Detect file replacement during validation as well as during application.
      pk_assert(identical(originals, stats::setNames(lapply(datasets, fingerprint), datasets)), "読み込み中に入力ファイルが変わりました。もう一度検証してください。")
      staged(list(folder = folder, bundle = bundle, originals = originals, state = input_state(),
        revision = engine$spec$revision, project_id = engine$spec$project_id, unit = input$unit,
        counts = c("濃度データの行数" = nrow(data$ADNCA), "PK値の行数" = nrow(data$ADPP), "被験者数" = length(unique(data$ADNCA$USUBJID)))))
      keep <- TRUE
      notice("検証できました。解析データはまだ変更していません。結果を確認して「解析に適用」を押してください。")
      notice_tone("success")
    }, error = function(e) {
      notice(paste("検証できません：", conditionMessage(e)))
      notice_tone("error")
    })
  })
  shiny::observeEvent(input$load, {
    tryCatch({
      value <- staged()
      pk_assert(!is.null(value) && is_current(), "先に現在の入力内容を検証してください。解析データは変更していません。")
      pk_assert(is.character(input$reason) && length(input$reason) == 1L && nzchar(trimws(input$reason)), "変更理由を入力してください。")
      pk_assert(identical(value$originals, stats::setNames(lapply(datasets, fingerprint), datasets)), "検証後に入力ファイルが変わりました。もう一度検証してください。")
      result <- desktop_load_bundle(engine, value$bundle, value$revision, value$project_id, "ui", "local user", input$reason)
      clear_stage()
      notice(paste("取込完了：", engine$info$n_subjects, "例 / revision", result$revision,
        "。解析条件を初期化しました。保存済みRun・帳票は保持されています。"))
      notice_tone("success")
    }, error = function(e) {
      clear_stage()
      notice(conditionMessage(e))
      notice_tone("error")
    })
  })
  output$validation_result <- shiny::renderUI({
    value <- staged()
    if (is.null(value)) return(NULL)
    if (!is_current()) return(desktop_ui_notice("検証後に解析条件が更新されています。最新の状態でもう一度検証してください。", "warning"))
    desktop_ui_section(NULL, "検証結果 · 適用待ち", desktop_ui_metrics(value$counts),
      shiny::div(class = "pk-import-staged", desktop_ui_notice("必須列・値の単位・キーの重複・2表の被験者の一致を確認しました。", "success")),
      shiny::p(class = "pk-caption", paste0("時間の換算：", switch(value$unit, h = "時間 → 時間（変更なし）", min = "分 ÷ 60 → 時間", day = "日 × 24 → 時間"),
        "。適用すると対象者・描画・層別の条件が初期化されます。保存済みRun・帳票は保持されます。")))
  })
  output$status <- shiny::renderUI({
    if (!is.null(staged()) && !is_current()) return(desktop_ui_notice("解析条件が更新されました。最新の状態でもう一度検証してください。", "warning"))
    desktop_ui_notice(notice(), notice_tone())
  })
  output$notice <- shiny::renderText(notice())
})
