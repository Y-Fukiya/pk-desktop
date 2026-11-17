desktop_assembly_roles <- function() list(
  CONC = c(ID = "被験者ID", STUDYID = "試験", SAMPLE_ID = "検体ID", ANALYTE = "分析対象物", PERIOD = "投与期・機会", CONC = "濃度", UNIT = "濃度単位", BLQ = "BLQ", MDV = "MDV", LLOQ = "LLOQ"),
  PC = c(ID = "被験者ID", STUDYID = "試験", SAMPLE_ID = "検体ID", ANALYTE = "分析対象物", PERIOD = "投与期・機会", SAMPLE_DTM = "実採血日時", PREDOSE = "投与前フラグ"),
  EX = c(ID = "被験者ID", STUDYID = "試験", PERIOD = "投与期・機会", DOSE_ID = "投与記録ID", DOSE_DTM = "実投与日時", DOSE_AMOUNT = "投与量（任意）", DOSE_UNIT = "投与量単位（任意）", ROUTE = "投与経路（任意）"))

desktop_assembly_ui <- function(id) {
  ns <- shiny::NS(id)
  source_card <- function(source, number, label) desktop_ui_section(number, label,
    desktop_workbench_select(ns(paste0(source, "_source")), "入力元", c("ファイル" = "FILE")),
    shiny::conditionalPanel(sprintf("input.%s_source === 'FILE'", source), ns = ns,
      shiny::fileInput(ns(paste0(source, "_file")), "CSV / XPT / SAS7BDAT / RDS / Parquet", accept = c(".csv", ".xpt", ".sas7bdat", ".rds", ".parquet"), buttonLabel = desktop_i18n_text("ファイルを選択", "Choose file"), placeholder = "未選択")),
    shiny::uiOutput(ns(paste0(source, "_mapping"))))
  desktop_ui_page(desktop_i18n_text("EX・PC・濃度", "EX / PC / concentrations"), "濃度表を起点に検体を照合し、明示した基準投与からの実時間を作ります。",
    eyebrow = "PK DESKTOP / ASSEMBLY", class = "pk-assembly",
    controls = shiny::tagList(
      shiny::tags$style(shiny::HTML(".pk-assembly .pk-layout { display:block; } .pk-assembly .pk-controls { display:flex; flex-direction:column; align-items:stretch; } .pk-assembly .pk-source-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:16px; align-items:start; } .pk-assembly .pk-results { margin-top:16px; } .pk-assembly .pk-key-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(min(100%,240px),1fr)); gap:0 16px; } .pk-assembly .pk-key-field { display:grid; grid-template-rows:subgrid; grid-row:span 3; min-width:0; } .pk-assembly .pk-key-field > label { align-self:end; } @media (max-width:800px) { .pk-assembly .pk-source-grid { grid-template-columns:minmax(0,1fr); } }")),
      shiny::div(class = "pk-source-grid", source_card("conc", 1, "濃度表（全行を保持）"), source_card("pc", 2, "PC・採血記録"), source_card("ex", 3, "EX・投与記録")),
      desktop_ui_section(4, desktop_i18n_text("照合キーと解析単位", "Matching keys and analysis units"), shiny::uiOutput(ns("keys")),
        shiny::p(class = "pk-caption", "入力表・列の対応を変更すると候補を再設定します。検体キーはPCで一意、観測キーは濃度表で一意である必要があります。分析対象物・投与期を解析単位に含めてください。IDは文字型で照合します。")),
      desktop_ui_section(5, desktop_i18n_text("基準投与と実日時", "Reference dose and actual date/time"),
        shiny::checkboxInput(ns("dose_history_confirmed"), desktop_i18n_text("EXに観測期間中だけでなく、それ以前の投与を含む完全な投与履歴が記録されていることを確認した", "I confirm that EX contains the complete dosing history, including doses before and during the observation period."), FALSE),
        desktop_workbench_select(ns("reference_mode"), desktop_i18n_text("全採血で共通に使う基準投与（選択必須）", "Reference dose used for all samples (required)"), c("基準の方針を選択" = "", "投与履歴の組合せ内で最初の実投与" = "first", "投与履歴の組合せ内で最後の実投与" = "last", "投与記録の値で指定" = "selected", "解析単位ごとに投与記録を選択" = "per_profile"), selected = ""),
        shiny::conditionalPanel("input.reference_mode === 'selected'", ns = ns,
          shiny::uiOutput(ns("reference_field")), shiny::textInput(ns("reference_value"), desktop_i18n_text("一致させる投与記録の値", "Dose-record value to match"), "")),
        shiny::conditionalPanel("input.reference_mode === 'per_profile'", ns = ns, shiny::uiOutput(ns("profile_assignments"))),
        shiny::p(class = "pk-caption", "投与前検体も同じ基準投与に割り当てます。最初・最後の選択は被験者・投与期などの組合せ単位に適用し、同時刻の候補は保留します。採血直前の投与を自動選択しません。"),
        shiny::div(class = "pk-form-grid", desktop_workbench_select(ns("sample_type"), desktop_i18n_text("実採血日時の形式", "Actual sampling date/time format"), c("形式を選択" = "", "ISO日時・絶対時刻POSIXct" = "iso", "SAS日時（1960年秒・現地時刻）" = "sas_seconds", "Unix秒・UTC時刻" = "unix_seconds"), selected = ""),
          desktop_workbench_select(ns("dose_type"), desktop_i18n_text("実投与日時の形式", "Actual dosing date/time format"), c("形式を選択" = "", "ISO日時・絶対時刻POSIXct" = "iso", "SAS日時（1960年秒・現地時刻）" = "sas_seconds", "Unix秒・UTC時刻" = "unix_seconds"), selected = ""),
          shiny::textInput(ns("utc_offset"), desktop_i18n_text("オフセットのない現地日時のUTC差（例 +09:00）", "UTC offset for local date/times without an offset (e.g., +09:00)"), ""),
          desktop_workbench_select(ns("unmatched"), desktop_i18n_text("照合できない濃度行", "Unmatched concentration rows"), c("行を保持・実時間を欠測" = "missing", "停止して確認" = "error")),
          desktop_workbench_select(ns("missing_datetime"), desktop_i18n_text("不完全・不正な実日時", "Incomplete or invalid actual date/time"), c("行を保持・実時間を欠測" = "missing", "停止して確認" = "error"))))),
    body = shiny::tagList(shiny::uiOutput(ns("notice")),
      desktop_ui_section(NULL, desktop_i18n_text("プレビュー", "Preview"), shiny::actionButton(ns("preview"), desktop_i18n_text("照合と実時間をプレビュー", "Preview matching and actual times"), class = "btn-primary"), shiny::uiOutput(ns("preview_result"))),
      desktop_ui_section(NULL, desktop_i18n_text("保存してBLQ・NCAへ", "Save for BLQ processing and NCA"), shiny::textInput(ns("name"), desktop_i18n_text("保存名", "Save name"), "EX・PC・濃度 実時間"), shiny::uiOutput(ns("save_action")),
        shiny::p(class = "pk-caption", "体格を使う場合は保存した表を「NCA入力：DM・VS」で選択・照合します。その後「データ準備」の入力元で選び、BLQ規則を適用して保存します。その表をNCAで選択し、IDと追加グループ、NCA_TIME_H（h）、投与基準0を確認してください。元の3表・照合条件・行別記録も保存します。"))))
}

desktop_assembly_server <- function(id, engine) shiny::moduleServer(id, function(input, output, session) {
  current <- shiny::reactiveVal(NULL)
  saved <- shiny::reactiveVal(NULL)
  notice <- shiny::reactiveVal("3表の列・キー・基準投与・時計の解釈を選択してください。")
  roles <- desktop_assembly_roles()
  extra_keys <- shiny::reactiveValues(conc = 0L, pc = 0L, ex = 0L)
  candidates <- list(ID = c("USUBJID", "ID", "SUBJID"), SAMPLE_ID = c("SAMPLE_ID", "PCREFID", "SAMPLEID"),
    CONC = c("CONC", "AVAL", "CONCENTRATION"), UNIT = c("UNIT", "AVALU", "PCSTRESU"), ANALYTE = c("ANALYTE", "PCTESTCD"),
    PERIOD = c("PERIOD", "APERIOD"), SAMPLE_DTM = c("SAMPLE_DTM", "PCDTC"), DOSE_ID = c("DOSE_ID", "EXSEQ"), DOSE_DTM = c("DOSE_DTM", "EXSTDTC"), DOSE_AMOUNT = c("EXDOSE", "DOSE"), DOSE_UNIT = c("EXDOSU", "DOSE_UNIT"), ROUTE = c("EXROUTE", "ROUTE"))
  source_data <- lapply(c("conc", "pc", "ex"), function(source) shiny::reactive({
    engine$tables_signal()
    selected <- input[[paste0(source, "_source")]] %||% "FILE"
    tryCatch({
      if (identical(selected, "FILE")) {
        file <- input[[paste0(source, "_file")]]
        if (is.null(file)) return(NULL)
        pkapp_read_table(file$datapath, tolower(tools::file_ext(file$name)))
      } else desktop_table_get(engine, selected)
    }, error = function(e) structure(conditionMessage(e), class = "assembly_input_error"))
  }))
  names(source_data) <- c("conc", "pc", "ex")
  for (source in names(source_data)) local({
    key <- source
    shiny::observeEvent(input[[paste0(key, "_add_key")]], {
      extra_keys[[key]] <- extra_keys[[key]] + 1L
    })
    shiny::observe({
      engine$tables_signal()
      tables <- desktop_tables(engine)
      choices <- c("ファイル" = "FILE", stats::setNames(vapply(tables, `[[`, "", "id"), vapply(tables, `[[`, "", "name")))
      selected <- shiny::isolate(input[[paste0(key, "_source")]]) %||% "FILE"
      shiny::updateSelectInput(session, paste0(key, "_source"), choices = choices, selected = if (selected %in% choices) selected else "FILE")
    })
    output[[paste0(key, "_mapping")]] <- shiny::renderUI({
      data <- source_data[[key]]()
      if (inherits(data, "assembly_input_error")) return(desktop_ui_notice(as.character(data), "error"))
      if (!is.data.frame(data)) return(NULL)
      shiny::tagList(shiny::p(class = "pk-caption", paste(nrow(data), "行・", ncol(data), "列")),
        shiny::div(class = "pk-form-grid", lapply(names(roles[[toupper(key)]]), function(role) {
          field <- paste(key, role, sep = "_")
          preferred <- intersect(candidates[[role]] %||% role, names(data))
          selected <- shiny::isolate(input[[field]]) %||% if (length(preferred)) preferred[1L] else ""
          desktop_workbench_select(session$ns(field), paste0(roles[[toupper(key)]][[role]], " → ", role), c("使わない・列を選択" = "", stats::setNames(names(data), names(data))), selected = if (selected %in% names(data)) selected else "")
        })),
        shiny::tags$details(shiny::tags$summary(desktop_i18n_text("追加の照合項目（検体の反復・測定法など）", "Additional matching keys (replicates, assay method, etc.)")),
          shiny::actionButton(session$ns(paste0(key, "_add_key")), "照合項目を追加"),
          lapply(seq_len(extra_keys[[key]]), function(index) {
            field <- paste0(key, "_extra_", index)
            shiny::div(class = "pk-form-grid",
              shiny::textInput(session$ns(paste0(field, "_role")), "共通項目名（大文字、例 REPLICATE）", shiny::isolate(input[[paste0(field, "_role")]]) %||% ""),
              desktop_workbench_select(session$ns(paste0(field, "_column")), "対応する元の列", c("使わない" = "", stats::setNames(names(data), names(data))),
                selected = shiny::isolate(input[[paste0(field, "_column")]]) %||% ""))
          })),
        shiny::tags$details(shiny::tags$summary(desktop_i18n_text("元表の先頭5行", "First five rows of source data")), shiny::div(class = "pk-table-scroll", desktop_display_table(head(data, 5L)))))
    })
  })
  mapping <- shiny::reactive({
    stats::setNames(lapply(names(roles), function(source) {
      fields <- names(roles[[source]])
      values <- lapply(fields, function(role) input[[paste(tolower(source), role, sep = "_")]] %||% "")
      names(values) <- fields
      values <- values[vapply(values, nzchar, logical(1))]
      for (index in seq_len(extra_keys[[tolower(source)]])) {
        field <- paste0(tolower(source), "_extra_", index)
        role <- input[[paste0(field, "_role")]] %||% ""
        column <- input[[paste0(field, "_column")]] %||% ""
        if (nzchar(role) && nzchar(column)) {
          pk_assert(grepl("^[A-Z][A-Z0-9_]*$", role) && !role %in% names(values), "追加の共通項目名は重複しない大文字で指定してください。")
          values[[role]] <- column
        }
      }
      values
    }), names(roles))
  })
  last_key_context <- NULL
  output$keys <- shiny::renderUI({
    mapped <- mapping()
    context <- list(mapping = mapped, sources = lapply(names(source_data), function(source) {
      selected <- input[[paste0(source, "_source")]] %||% "FILE"
      list(source = selected, file = if (selected == "FILE") input[[paste0(source, "_file")]]$datapath else NULL)
    }))
    changed <- !identical(context, last_key_context)
    last_key_context <<- context
    both <- union(names(mapped$CONC), names(mapped$PC))
    definitions <- list(sample_keys = list("濃度とPCの検体キー", intersect(names(mapped$CONC), names(mapped$PC)), c("STUDYID", "ID", "SAMPLE_ID")),
      profile_keys = list("解析単位のキー", both, c("STUDYID", "ID", "PERIOD", "ANALYTE")),
      dose_group_keys = list("投与履歴をまとめるキー（被験者・投与期など）", intersect(both, names(mapped$EX)), c("STUDYID", "ID", "PERIOD")),
      dose_record_keys = list("EX投与記録の一意キー", names(mapped$EX), c("STUDYID", "ID", "PERIOD", "DOSE_ID")),
      observation_keys = list("濃度表の観測一意キー", names(mapped$CONC), c("STUDYID", "ID", "SAMPLE_ID", "ANALYTE")))
    descriptions <- c(sample_keys = "濃度行と採血記録を結ぶ項目。例：試験、被験者、検体ID。",
      profile_keys = "1つの濃度推移を区別する項目。例：試験、被験者、投与期、分析対象物。",
      dose_group_keys = "同じ投与履歴を共有する範囲。例：試験、被験者、投与期。",
      dose_record_keys = "EXの各投与記録を一意にする項目。例：試験、被験者、投与期、投与記録ID。",
      observation_keys = "濃度表の各観測を一意にする項目。例：試験、被験者、検体ID、分析対象物。")
    shiny::div(class = "pk-key-grid", lapply(names(definitions), function(field) {
      d <- definitions[[field]]
      selected <- if (changed) intersect(d[[3]], d[[2]]) else shiny::isolate(input[[field]]) %||% intersect(d[[3]], d[[2]])
      shiny::div(class = "pk-key-field", shiny::tags$label(`for` = session$ns(field), d[[1]]),
        desktop_workbench_select(session$ns(field), NULL, stats::setNames(d[[2]], d[[2]]), selected = intersect(selected, d[[2]]), multiple = TRUE),
        shiny::p(class = "pk-caption", descriptions[[field]]))
    }))
  })
  output$reference_field <- shiny::renderUI({
    fields <- names(mapping()$EX)
    desktop_workbench_select(session$ns("reference_field"), "基準投与を指定するEXの項目", fields,
      selected = shiny::isolate(input$reference_field) %||% if ("DOSE_ID" %in% fields) "DOSE_ID" else fields[1])
  })
  reference_context <- shiny::reactive({
    snapshot <- source_snapshot()
    mapped <- mapping()
    keys <- lapply(c("sample_keys", "profile_keys", "dose_group_keys", "dose_record_keys"), function(field)
      desktop_assembly_keys(input[[field]] %||% character(), field))
    names(keys) <- c("sample_keys", "profile_keys", "dose_group_keys", "dose_record_keys")
    data <- lapply(names(mapped), function(source) desktop_assembly_map(snapshot$inputs[[tolower(source)]], mapped[[source]], source))
    names(data) <- names(mapped)
    sample_conc <- desktop_assembly_tuple(data$CONC, keys$sample_keys)
    sample_pc <- desktop_assembly_tuple(data$PC, keys$sample_keys)
    pk_assert(!anyDuplicated(sample_pc), "PCの検体キーが重複しています。列・キーを確認してください。")
    matched <- match(sample_conc, sample_pc)
    combined <- data$CONC
    for (field in names(data$PC)) {
      attached <- data$PC[[field]][matched]
      if (field %in% names(combined)) {
        both <- !is.na(combined[[field]]) & !is.na(attached)
        pk_assert(!any(both & as.character(combined[[field]]) != as.character(attached)), paste("濃度とPCの値が一致しません：", field))
        missing <- is.na(combined[[field]])
        combined[[field]][missing] <- attached[missing]
      } else combined[[field]] <- attached
    }
    profile <- desktop_assembly_tuple(combined, keys$profile_keys, missing = TRUE)
    history <- desktop_assembly_tuple(combined, keys$dose_group_keys, missing = TRUE)
    dose_history <- desktop_assembly_tuple(data$EX, keys$dose_group_keys)
    dose_record <- desktop_assembly_tuple(data$EX, keys$dose_record_keys)
    pk_assert(!anyDuplicated(dose_record), "EXの投与記録キーが重複しています。列・キーを確認してください。")
    namespace <- desktop_hash(list(sources = snapshot$sources, mapping = mapped, keys = keys))
    profiles <- lapply(unique(profile[!is.na(profile)]), function(token) {
      rows <- which(!is.na(profile) & profile == token)
      histories <- unique(history[rows])
      pk_assert(length(histories) == 1L && !is.na(histories), "解析単位が複数の投与履歴にまたがっています。キーを確認してください。")
      candidates <- which(dose_history == histories)
      identity <- as.list(combined[rows[1L], keys$profile_keys, drop = FALSE])
      label <- paste(paste(names(identity), unlist(identity), sep = "="), collapse = " / ")
      choices <- stats::setNames(dose_record[candidates], vapply(candidates, function(row) {
        record <- data$EX[row, unique(c(keys$dose_record_keys, "DOSE_DTM")), drop = FALSE]
        paste0("EX元行 ", row, " / ", paste(paste(names(record), unlist(record), sep = "="), collapse = " / "))
      }, character(1)))
      list(identity = identity, label = label, candidates = candidates, choices = choices,
        field = paste0("profile_dose_", substr(desktop_hash(list(namespace, token)), 1L, 20L)))
    })
    pk_assert(length(profiles) > 0L, "列・キーを設定して解析単位を作成してください。")
    list(profiles = profiles, ex = data$EX, dose_record = dose_record, keys = keys)
  })
  output$profile_assignments <- shiny::renderUI({
    if (!identical(input$reference_mode, "per_profile")) return(NULL)
    tryCatch({
      context <- reference_context()
      shiny::tagList(shiny::p(class = "pk-caption", "各解析単位について基準投与を明示して選択してください。同じ投与履歴をまとめるキーに一致するEX記録だけを表示します。入力元・列・キーを変更した場合は再選択が必要です。"),
        lapply(context$profiles, function(profile) {
          selected <- shiny::isolate(input[[profile$field]]) %||% ""
          desktop_workbench_select(session$ns(profile$field), profile$label,
            c("基準投与を選択" = "", profile$choices), selected = if (selected %in% profile$choices) selected else "")
        }))
    }, error = function(e) desktop_ui_notice(conditionMessage(e), "warning"))
  })
  specification <- shiny::reactive({
    mode <- input$reference_mode %||% ""
    pk_assert(mode %in% c("first", "last", "selected", "per_profile"), "基準投与の方針を明示して選択してください。")
    pk_assert((input$sample_type %||% "") %in% c("iso", "sas_seconds", "unix_seconds"), "実採血日時の形式を選択してください。")
    pk_assert((input$dose_type %||% "") %in% c("iso", "sas_seconds", "unix_seconds"), "実投与日時の形式を選択してください。")
    reference <- list(mode = mode)
    if (mode == "selected") reference$predicate <- list(op = "==", args = list(list(column = input$reference_field), list(value = input$reference_value)))
    if (mode == "per_profile") {
      context <- reference_context()
      reference$assignments <- lapply(context$profiles, function(profile) {
        choice <- input[[profile$field]] %||% ""
        pk_assert(length(choice) == 1L && nzchar(choice) && choice %in% profile$choices,
          paste0("基準投与を選択してください：", profile$label))
        row <- match(choice, context$dose_record)
        list(profile = profile$identity, dose = as.list(context$ex[row, context$keys$dose_record_keys, drop = FALSE]))
      })
    }
    spec <- list(version = 1L, mapping = mapping(), dose_reference = reference,
      datetime = list(sample_type = input$sample_type, dose_type = input$dose_type, utc_offset = input$utc_offset),
      unmatched = input$unmatched %||% "missing", missing_datetime = input$missing_datetime %||% "missing", dose_history_confirmed = isTRUE(input$dose_history_confirmed))
    for (field in c("sample_keys", "profile_keys", "dose_group_keys", "dose_record_keys", "observation_keys")) spec[[field]] <- as.list(input[[field]] %||% character())
    spec
  })
  source_snapshot <- shiny::reactive({
    inputs <- lapply(source_data, function(read) read())
    pk_assert(all(vapply(inputs, is.data.frame, logical(1))), "濃度・PC・EXの3表を読み込んでください。")
    sources <- stats::setNames(lapply(names(source_data), function(source) {
      selection <- input[[paste0(source, "_source")]] %||% "FILE"
      list(source = selection, filename = if (selection == "FILE") input[[paste0(source, "_file")]]$name else NULL,
        hash = desktop_hash(inputs[[source]]))
    }), names(source_data))
    list(inputs = inputs, sources = sources)
  })
  draft <- shiny::reactive({
    snapshot <- source_snapshot()
    list(inputs = snapshot$inputs, specification = specification(), sources = snapshot$sources)
  })
  fingerprint <- shiny::reactive(tryCatch(desktop_hash(draft()), error = function(e) NULL))
  result <- shiny::reactive({
    value <- current()
    if (is.null(value) || !identical(value$fingerprint, fingerprint())) return(NULL)
    value
  })
  shiny::observeEvent(input$preview, {
    current(NULL); saved(NULL)
    tryCatch({
      request <- draft()
      value <- pkapp_assemble_nca(request$inputs$conc, request$inputs$pc, request$inputs$ex, request$specification)
      current(list(value = value, inputs = request$inputs, sources = request$sources, fingerprint = fingerprint()))
      notice("実時間を作成しました。未解決行・基準投与・負の時間を確認して保存してください。")
    }, error = function(e) notice(paste("プレビューできませんでした：", conditionMessage(e))))
  })
  output$notice <- shiny::renderUI({
    if (!is.null(current()) && is.null(result())) return(desktop_ui_notice("入力または条件が変わりました。再プレビューしてから保存してください。", "warning"))
    desktop_ui_notice(notice(), if (grepl("できませんでした", notice())) "error" else "info")
  })
  output$preview_result <- shiny::renderUI({
    preview <- result()
    if (is.null(preview)) return(desktop_ui_empty("最新のプレビューがありません", "3表と照合条件を設定してプレビューしてください。"))
    value <- preview$value
    columns <- intersect(c("NCA_CONC_ROW", "NCA_PC_ROW", "NCA_EX_ROW", unlist(value$specification$profile_keys), "NCA_DOSE_REF_ID", "SAMPLE_DTM", "DOSE_DTM", "NCA_TIME_H", "NCA_ASSEMBLY_STATUS", "NCA_ASSEMBLY_REASON"), names(value$data))
    shiny::tagList(desktop_ui_metrics(c("濃度入力" = value$counts[["conc"]], "結果行" = value$counts[["result"]], "PC未照合" = value$counts[["unmatched_conc"]], "基準未解決" = value$counts[["unresolved_dose"]], "時間欠測" = value$counts[["missing_time"]])),
      shiny::p(class = "pk-caption", paste("未使用PC行", value$counts[["unused_pc"]], "・負の実時間", sum(value$data$NCA_TIME_H < 0, na.rm = TRUE), "行。表示は先頭100行です。")),
      shiny::div(class = "pk-table-scroll", desktop_display_table(head(value$data[columns], 100L))),
      shiny::tags$details(shiny::tags$summary("照合・日時の問題"), shiny::div(class = "pk-table-scroll", desktop_display_table(head(value$issues, 100L)))),
      shiny::tags$details(shiny::tags$summary("基準投与の候補と選択記録"), shiny::uiOutput(session$ns("dose_references"))))
  })
  output$dose_references <- shiny::renderUI({
    shiny::req(result())
    refs <- result()$value$trace$dose_references
    rows <- lapply(refs, function(ref) data.frame(解析単位 = as.character(ref$profile), 方針 = ref$mode,
      候補EX元行 = paste(unlist(ref$candidates), collapse = ", "), 選択EX元行 = paste(ref$selected %||% "未解決", collapse = ", "),
      確認理由 = ref$reason %||% "", check.names = FALSE))
    if (length(rows)) shiny::div(class = "pk-table-scroll", desktop_display_table(do.call(rbind, rows)))
  })
  output$save_action <- shiny::renderUI({
    button <- shiny::actionButton(session$ns("save"), desktop_i18n_text("最新プレビューを作業表に保存", "Save latest preview as a working table"))
    if (is.null(result())) button$attribs$disabled <- "disabled"
    button
  })
  shiny::observeEvent(input$save, {
    tryCatch({
      preview <- result()
      pk_assert(!is.null(preview), "入力・条件が変わりました。再プレビューしてください。")
      receipt <- desktop_table_save(engine, input$name, preview$inputs,
        list(kind = "nca_assembly", specification = preview$value$specification), frozen = preview$value,
        provenance = list(sources = preview$sources, reason = "Assemble EX, PC and concentration using actual datetimes"))
      saved(receipt)
      notice(paste("保存しました：", receipt$name, "。データ準備の入力元から選択してBLQ処理へ進めます。"))
    }, error = function(e) notice(paste("保存できませんでした：", conditionMessage(e))))
  })
})
