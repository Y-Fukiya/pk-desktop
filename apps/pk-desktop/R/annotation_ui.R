desktop_annotation_ui <- function(id, input_mode = FALSE) {
  ns <- shiny::NS(id)
  desktop_ui_page(if (input_mode) "NCA入力：DM・VS（任意）" else "結果表示用DM・VS追加（再計算なし）",
    if (input_mode) "計算前の濃度表にDM・VSを照合します。体重・体表面積はNCAの投与量換算やCL/F・Vz/Fの体格正規化で使えます。" else "保存済みNCA結果へDM・VSの記録値を追加します。PK値は再計算しません。",
    eyebrow = if (input_mode) "PK DESKTOP / NCA INPUT" else "PK DESKTOP / BACKGROUND", controls = shiny::tagList(
      desktop_ui_section(1, if (input_mode) "計算前の濃度表" else "保存済みNCA結果", desktop_workbench_select(ns("result_source"), "追加元", stats::setNames("", if (input_mode) "濃度表を保存して選択" else "NCA結果を保存して選択")),
        if (input_mode) shiny::uiOutput(ns("source_profile")),
        shiny::p(class = "pk-caption", if (input_mode) "EX・PC・濃度の保存表を選択し、NCAと同じ解析単位で照合します。濃度行の数・順序は保持します。DM・VSなしでも通常のNCAは実行できます。" else "NCA画面で結果を保存してから選択してください。DM・VSにSTUDYIDがある場合は、NCAの追加グループにもSTUDYIDを含めます。PK値・行数・計算状態は変更しません。")),
      lapply(c("dm", "vs"), function(source) desktop_ui_section(NULL, paste0(toupper(source), "（任意）"),
        desktop_workbench_select(ns(paste0(source, "_source")), "入力元", c("使わない" = "NONE", "ファイル" = "FILE")),
        shiny::conditionalPanel(sprintf("input.%s_source === 'FILE'", source), ns = ns,
          shiny::fileInput(ns(paste0(source, "_file")), "CSV / XPT / SAS7BDAT / RDS / Parquet",
            accept = c(".csv", ".xpt", ".sas7bdat", ".rds", ".parquet"), buttonLabel = desktop_i18n_text("ファイルを選択", "Choose file"), placeholder = "未選択")),
        shiny::uiOutput(ns(paste0(source, "_mapping")))))),
    body = shiny::tagList(shiny::uiOutput(ns("notice")),
      desktop_ui_section(NULL, "採用値と照合の確認", shiny::actionButton(ns("preview"), if (input_mode) "NCA入力をプレビュー" else "背景情報をプレビュー", class = "btn-primary"),
        shiny::div(style = "margin-top:16px", shiny::uiOutput(ns("preview_result")))),
      desktop_ui_section(NULL, if (input_mode) "NCA入力の保存" else "背景情報付き結果の保存", shiny::textInput(ns("name"), desktop_i18n_text("保存名", "Save name"), if (input_mode) "NCA入力・DMVS" else "NCA結果・背景情報付き"),
        shiny::uiOutput(ns("save_actions")), shiny::p(class = "pk-caption", if (input_mode) "保存した入力をBLQ前処理またはNCAで選択してください。元の濃度表とDM・VS、採用条件、測定役割・元行・単位・日時を保存します。単位換算・明示したMosteller体表面積とPK計算はNCAで実施します。" else "元のNCA結果とDM・VS、採用条件、元行・単位・日時を保存します。BMI・体表面積の算出、単位換算、体格によるPK補正は行いません。"))))
}

desktop_annotation_server <- function(id, engine, input_mode = FALSE) shiny::moduleServer(id, function(input, output, session) {
  current <- shiny::reactiveVal(NULL)
  saved <- shiny::reactiveVal(NULL)
  notice <- shiny::reactiveVal(if (input_mode) "計算前の濃度表を保存・選択し、必要なDM・VSと基準時点を確認してください。" else "NCA結果を保存・選択し、必要なDM・VSだけを追加してください。")
  shiny::observeEvent(list(input$result_source, input$vs_source, input$vs_file$datapath), {
    if (input_mode) shiny::updateCheckboxInput(session, "vs_baseline_confirmed", value = FALSE)
  }, ignoreInit = TRUE)
  source_input <- shiny::reactive({
    engine$tables_signal()
    if (!input_mode || !nzchar(input$result_source %||% "")) return(NULL)
    tryCatch(desktop_table_get(engine, input$result_source), error = function(e) NULL)
  })
  output$source_profile <- shiny::renderUI({
    data <- source_input(); if (is.null(data)) return(NULL)
    keys <- tryCatch(desktop_assembly_profile_columns(data), error = function(e) NULL)
    if (is.null(keys) && "NCA_BLQ_CONTEXT" %in% names(data)) keys <- unlist(desktop_pk_blq_context(data)$profile_columns)
    id <- head(intersect(c("USUBJID", "ID", "SUBJID"), keys %||% names(data)), 1L)
    if (!length(id)) id <- names(data)[1L]
    groups <- if (length(keys)) setdiff(keys, id) else intersect(c("STUDYID", "PERIOD", "APERIOD", "ANALYTE"), names(data))
    shiny::tagList(
      desktop_workbench_select(session$ns("profile_id"), desktop_i18n_text("被験者ID列", "Subject ID column"), names(data), shiny::isolate(input$profile_id) %||% id),
      desktop_workbench_select(session$ns("profile_groups"), "NCAと同じ追加グループ", names(data), shiny::isolate(input$profile_groups) %||% groups, multiple = TRUE),
      desktop_workbench_select(session$ns("profile_study"), "試験キー", c("なし" = "", names(data)), shiny::isolate(input$profile_study) %||% if ("STUDYID" %in% names(data)) "STUDYID" else ""),
      desktop_workbench_select(session$ns("profile_period"), "投与期キー", c("なし" = "", names(data)), shiny::isolate(input$profile_period) %||% (head(intersect(c("PERIOD", "APERIOD"), names(data)), 1) %||% "")))
  })
  source_result <- shiny::reactive({
    engine$tables_signal()
    if (!nzchar(input$result_source %||% "")) return(NULL)
    if (input_mode) {
      data <- source_input(); if (is.null(data) || !nzchar(input$profile_id %||% "")) return(NULL)
      keys <- c(input$profile_id, input$profile_groups %||% character())
      return(list(data = data, specification = list(id = input$profile_id), trace = list(output_key_mapping = stats::setNames(as.list(keys), keys))))
    }
    tryCatch(desktop_annotation_result(engine, input$result_source), error = function(e) NULL)
  })
  source_data <- stats::setNames(lapply(c("dm", "vs"), function(source) shiny::reactive({
    engine$tables_signal()
    selected <- input[[paste0(source, "_source")]] %||% "NONE"
    if (selected == "NONE") return(NULL)
    tryCatch({
      if (selected == "FILE") {
        file <- input[[paste0(source, "_file")]]
        if (is.null(file)) return(NULL)
        pkapp_read_table(file$datapath, tolower(tools::file_ext(file$name)))
      } else desktop_table_get(engine, selected)
    }, error = function(e) structure(conditionMessage(e), class = "annotation_input_error"))
  })), c("dm", "vs"))
  shiny::observe({
    engine$tables_signal(); tables <- desktop_tables(engine)
    nca <- if (input_mode) Filter(function(x) !x$kind %in% c("nca", "nca_annotation"), tables) else Filter(function(x) identical(x$kind, "nca"), tables)
    choices <- c(stats::setNames("", if (input_mode) "濃度表を保存して選択" else "NCA結果を保存して選択"), stats::setNames(vapply(nca, `[[`, "", "id"), vapply(nca, `[[`, "", "name")))
    selected <- shiny::isolate(input$result_source) %||% ""
    shiny::updateSelectInput(session, "result_source", choices = choices, selected = if (selected %in% choices) selected else "")
    choices <- c("使わない" = "NONE", "ファイル" = "FILE", stats::setNames(vapply(tables, `[[`, "", "id"), vapply(tables, `[[`, "", "name")))
    for (source in c("dm", "vs")) {
      field <- paste0(source, "_source"); selected <- shiny::isolate(input[[field]]) %||% "NONE"
      shiny::updateSelectInput(session, field, choices = choices, selected = if (selected %in% choices) selected else "NONE")
    }
  })
  column_input <- function(field, label, data, preferred = character(), optional = FALSE) {
    choices <- c(stats::setNames(names(data), names(data)))
    if (optional) choices <- c("使わない" = "", choices) else choices <- c("列を選択" = "", choices)
    preferred <- intersect(preferred, names(data))
    selected <- shiny::isolate(input[[field]]) %||% if (length(preferred)) preferred[1L] else ""
    desktop_workbench_select(session$ns(field), label, choices, selected = if (selected %in% choices) selected else "")
  }
  identity <- shiny::reactive({
    r <- source_result(); if (is.null(r)) return(NULL)
    mapping <- unlist(r$trace$output_key_mapping)
    list(mapping = mapping, subject = unname(mapping[[r$specification$id]]), study = unname(mapping[intersect(names(mapping), if (input_mode) input$profile_study %||% "" else "STUDYID")]))
  })
  key_controls <- function(source, data) {
    i <- identity(); if (is.null(i)) return(shiny::p(class = "pk-caption", if (input_mode) "先に濃度表と解析単位を選択してください。" else "先に保存済みNCA結果を選択してください。"))
    keys <- if (source == "dm" || !identical(input$vs_scope, "keys")) c(i$subject, i$study) else unname(i$mapping)
    shiny::tagList(lapply(keys, function(key) {
      original <- names(i$mapping)[match(key, i$mapping)]
      column_input(paste0(source, "_key_", key), paste0("NCA ", key, " に対応する列"), data,
        c(original, key, if (key == i$subject) c("USUBJID", "ID", "SUBJID")), optional = !key %in% c(i$subject, i$study))
    }))
  }
  output$dm_mapping <- shiny::renderUI({
    data <- source_data$dm()
    if (inherits(data, "annotation_input_error")) return(desktop_ui_notice(as.character(data), "error"))
    if (!is.data.frame(data)) return(NULL)
    selected <- shiny::isolate(input$dm_columns) %||% intersect(c("SEX", "AGE", "AGEU"), names(data))
    shiny::tagList(key_controls("dm", data), desktop_workbench_select(session$ns("dm_columns"), "追加するDM項目（出力名はDM_列名）",
      stats::setNames(names(data), names(data)), selected = intersect(selected, names(data)), multiple = TRUE),
      shiny::p(class = "pk-caption", "被験者・試験ごとに1行で照合します。性別等の記録値をそのまま追加します。"))
  })
  output$vs_mapping <- shiny::renderUI({
    data <- source_data$vs()
    if (inherits(data, "annotation_input_error")) return(desktop_ui_notice(as.character(data), "error"))
    if (!is.data.frame(data)) return(NULL)
    shiny::tagList(key_controls("vs", data),
      desktop_workbench_select(session$ns("vs_scope"), "VSの採用範囲（選択必須）",
        c("範囲を選択" = "", "被験者単位・全投与期で共通" = "subject", "投与期など追加キー別" = "keys"), selected = shiny::isolate(input$vs_scope) %||% ""),
      if (input_mode) shiny::checkboxInput(session$ns("vs_baseline_confirmed"), "採用値は投与前の基準値であることを確認した（被験者単位を選ぶ場合は全投与期への共用も確認）", shiny::isolate(input$vs_baseline_confirmed) %||% FALSE),
      if (input_mode) shiny::p(class = "pk-caption", "自動の最新値選択は行いません。日時が比較できる投与後測定値は計算に使えません。日時不明・時差なしの記録は基準時点の確認に依存し、時系列は未検証として保存されます。"),
      column_input("vs_test", "検査コード列", data, c("VSTESTCD", "TEST")),
      column_input("vs_value", "数値列", data, c("VSSTRESN", "VALUE")),
      column_input("vs_unit", "単位列", data, c("VSSTRESU", "UNIT")),
      column_input("vs_datetime", "測定日時列（記録の保持）", data, c("VSDTC", "DATETIME"), TRUE),
      column_input("vs_visit", "来院列（記録の保持）", data, c("VISIT", "VISITNUM"), TRUE),
      shiny::uiOutput(session$ns("vs_tests")),
      desktop_workbench_select(session$ns("vs_selection"), "VSの採用条件（選択必須）",
        c("採用条件を選択" = "", "列の値が一致する行" = "filter", "この表は採用済み・検査ごとに一意" = "all_unique"),
        selected = shiny::isolate(input$vs_selection) %||% ""),
      shiny::conditionalPanel("input.vs_selection === 'filter'", ns = session$ns,
        column_input("vs_filter_column", "採用条件の列", data, c("VSBLFL", "VISIT")), shiny::uiOutput(session$ns("vs_filter_values_ui"))),
      shiny::p(class = "pk-caption", "基準時点フラグ・来院などで採用行を明示してください。候補が複数なら停止します。身長・体重・BMI・体表面積は記録値だけを使い、最新値や平均値を自動で選びません。"))
  })
  output$vs_tests <- shiny::renderUI({
    data <- source_data$vs(); column <- input$vs_test
    if (!is.data.frame(data) || is.null(column) || !column %in% names(data)) return(NULL)
    tests <- unique(as.character(data[[column]])); tests <- tests[!is.na(tests) & nzchar(tests)]
    shiny::tagList(lapply(c("HEIGHT", "WEIGHT", "BMI", "BSA"), function(role) desktop_workbench_select(session$ns(paste0("vs_measure_", role)),
        paste0(c(HEIGHT = "身長", WEIGHT = "体重", BMI = "BMI", BSA = "体表面積")[[role]], "の検査コード → VS_", role),
        c("追加しない" = "", stats::setNames(tests, tests)), selected = shiny::isolate(input[[paste0("vs_measure_", role)]]) %||% "")))
  })
  output$vs_filter_values_ui <- shiny::renderUI({
    data <- source_data$vs(); column <- input$vs_filter_column
    if (!is.data.frame(data) || is.null(column) || !column %in% names(data)) return(NULL)
    values <- unique(as.character(data[[column]])); values <- values[!is.na(values) & nzchar(values)]
    desktop_workbench_select(session$ns("vs_filter_values"), "採用する値", stats::setNames(values, values),
      selected = intersect(shiny::isolate(input$vs_filter_values) %||% character(), values), multiple = TRUE)
  })
  request <- shiny::reactive({
    result <- source_result(); pk_assert(!is.null(result), "保存表と解析単位を選択してください。")
    i <- identity(); inputs <- if (input_mode) list(source = result$data) else list(result = result)
    specification <- list(version = 1L); ids <- if (input_mode) list(source = input$result_source) else list(result_source = input$result_source)
    if (input_mode) {
      specification$profile <- list(id = input$profile_id, groups = as.list(input$profile_groups %||% character()))
      if (nzchar(input$profile_study %||% "")) specification$profile$study <- input$profile_study
      if (nzchar(input$profile_period %||% "")) specification$profile$period <- input$profile_period
    }
    for (source in c("dm", "vs")) {
      selected <- input[[paste0(source, "_source")]] %||% "NONE"
      if (selected == "NONE") next
      data <- source_data[[source]](); pk_assert(is.data.frame(data), paste(toupper(source), "を読み込んでください。"))
      inputs[[source]] <- data
      ids[[source]] <- if (selected == "FILE") paste0("file:", input[[paste0(source, "_file")]]$name) else selected
      keys <- stats::setNames(lapply(unname(i$mapping), function(key) input[[paste0(source, "_key_", key)]] %||% ""), unname(i$mapping))
      keys <- keys[vapply(keys, nzchar, logical(1))]
      if (source == "dm") {
        columns <- input$dm_columns %||% character()
        specification$dm <- list(keys = keys, columns = stats::setNames(as.list(columns), paste0("DM_", columns)))
      } else {
        scope <- input$vs_scope %||% ""
        if (scope == "subject") keys <- keys[intersect(names(keys), c(i$subject, i$study))]
        selection <- list(mode = input$vs_selection %||% "")
        if (selection$mode == "filter") selection <- c(selection, list(column = input$vs_filter_column, values = as.list(input$vs_filter_values %||% character())))
        measurements <- lapply(c("HEIGHT", "WEIGHT", "BMI", "BSA"), function(role) {
          test <- input[[paste0("vs_measure_", role)]] %||% ""
          if (nzchar(test)) c(list(name = paste0("VS_", role), test = test), if (input_mode) list(role = if (role == "BMI") "carry" else tolower(role))) else NULL
        })
        specification$vs <- list(keys = keys, scope = scope, test_column = input$vs_test, value_column = input$vs_value,
          unit_column = input$vs_unit, selection = selection, measurements = Filter(Negate(is.null), measurements))
        if (input_mode) specification$vs$baseline <- list(mode = if (scope == "keys") "period" else "subject_confirmed", confirmed = isTRUE(input$vs_baseline_confirmed))
        if (nzchar(input$vs_datetime %||% "")) specification$vs$datetime_column <- input$vs_datetime
        if (nzchar(input$vs_visit %||% "")) specification$vs$visit_column <- input$vs_visit
      }
    }
    list(inputs = inputs, specification = specification, source_ids = ids)
  })
  result <- shiny::reactive({
    value <- current()
    if (is.null(value)) return(NULL)
    matches <- tryCatch(identical(value$hash, desktop_hash(request())), error = function(e) FALSE)
    if (matches) value else NULL
  })
  shiny::observeEvent(input$preview, {
    tryCatch({
      req <- request()
      value <- if (input_mode) pkapp_prepare_nca_covariates(req$inputs$source, req$inputs$dm, req$inputs$vs, req$specification) else pkapp_annotate_nca(req$inputs$result, req$inputs$dm, req$inputs$vs, req$specification)
      current(list(value = value, request = req, hash = desktop_hash(req),
        revision = if (identical(engine$mode, "nca")) engine$nca$revision else NULL))
      saved(NULL); notice(if (input_mode) "NCA入力を照合しました。採用値・元行・単位・日時と基準時点を確認して保存してください。" else "背景情報を照合しました。採用値・元行・単位・日時と欠測を確認して保存してください。")
    }, error = function(e) { current(NULL); saved(NULL); notice(conditionMessage(e)) })
  })
  output$notice <- shiny::renderUI(desktop_ui_notice(notice()))
  output$preview_result <- shiny::renderUI({
    r <- result(); if (is.null(r)) return(shiny::p(class = "pk-caption", "条件を選択・変更した後にプレビューしてください。"))
    value <- r$value
    issue_table <- value$trace$issues
    if (nrow(issue_table)) {
      labels <- c(missing_match = "照合なし", missing_value = "記録値なし", missing_unit = "単位なし", missing_datetime = "日時なし", mixed_units = "単位が混在", postdose = "投与後測定・計算には使用不可")
      translated <- unname(labels[issue_table$reason])
      issue_table$reason <- ifelse(is.na(translated), issue_table$reason, translated)
      names(issue_table)[names(issue_table) == "field"] <- "項目"
      names(issue_table)[names(issue_table) == "reason"] <- "確認事項"
    }
    keys <- unname(unlist(value$trace$output_key_mapping))
    extra <- setdiff(setdiff(names(value$data), c("NCA_COV_CONTEXT", "NCA_COV_SIGNATURE")), names(if (input_mode) r$request$inputs$source else r$request$inputs$result$data))
    profiles <- !duplicated(desktop_assembly_tuple(value$data, keys))
    shiny::tagList(desktop_ui_metrics(stats::setNames(c(value$counts[["result"]], value$counts[["profiles"]], value$counts[["issues"]]), c(if (input_mode) "濃度入力行" else "NCA結果行", "解析単位", "背景情報の確認事項"))),
      shiny::p(class = "pk-caption", if (input_mode) "採用した測定役割・値・単位・日時・元行を確認してください。単位換算はNCAで実施し、欠測・投与後値は依存する追加パラメータだけを保留します。" else "確認事項は背景情報の欠測や単位の混在です。NCAの計算状態・値は変わりません。"),
      if (any(value$trace$issues$reason == "mixed_units")) desktop_ui_notice("同じ項目に複数の単位があります。異なる単位の値をそのまままとめて集計しないでください。元の値と単位を保持しています。", "warning"),
      shiny::div(class = "pk-table-scroll", desktop_display_table(head(value$data[profiles, c(keys, extra), drop = FALSE], 100L))),
      if (nrow(value$trace$issues)) shiny::tags$details(shiny::tags$summary("背景情報の確認事項"), shiny::div(class = "pk-table-scroll", desktop_display_table(head(issue_table, 100L)))),
      shiny::tags$details(shiny::tags$summary(if (input_mode) "濃度を含む入力表" else "PK値を含む結果"), shiny::div(class = "pk-table-scroll", desktop_display_table(head(value$data, 100L)))))
  })
  output$save_actions <- shiny::renderUI({
    if (is.null(result())) return(shiny::p(class = "pk-caption", "プレビューが確定すると保存・CSV出力できます。"))
    shiny::tagList(shiny::actionButton(session$ns("save"), if (input_mode) "NCA入力を保存" else "背景情報付き結果を保存", class = "btn-primary"),
      shiny::downloadButton(session$ns("csv"), desktop_i18n_text("結果CSV", "Results CSV")), if (!is.null(saved())) shiny::p(class = "pk-caption", "保存しました。"))
  })
  shiny::observeEvent(input$save, {
    tryCatch({
      r <- result(); pk_assert(!is.null(r), "変更後にプレビューしてください。")
      if (identical(engine$mode, "nca")) desktop_nca_revision(engine, r$revision, engine$nca$project_id)
      receipt <- if (input_mode) desktop_covariates_save(engine, input$name, r$request$inputs, r$value, r$request$source_ids) else desktop_annotation_save(engine, input$name, r$request$inputs, r$value, r$request$source_ids)
      saved(receipt); notice(if (input_mode) "NCA入力を保存しました。BLQ前処理またはNCAの入力元でこの保存名を選択してください。" else "背景情報付き結果を新しい作業表として保存しました。元のNCA結果は保持しています。")
    }, error = function(e) notice(conditionMessage(e)))
  })
  output$csv <- shiny::downloadHandler(filename = function() "nca_with_background.csv", content = function(file) {
    shiny::req(result()); pk_write_csv(result()$value$data, file)
  })
})
