# A staged data workspace. Applied results and immutable saved snapshots are distinct.
desktop_workbench_select <- function(...) shiny::selectizeInput(...,
  options = list(dropdownParent = "body", dropdownClass = "selectize-dropdown pk-wb-dropdown",
    onInitialize = I('function() {
      var originalPosition = this.positionDropdown;
      this.positionDropdown = function() {
        originalPosition.call(this);
        var box = this.$control[0].getBoundingClientRect();
        var below = window.innerHeight - box.bottom - 12;
        var above = box.top - 12;
        var width = Math.min(box.width, window.innerWidth - 16);
        this.$dropdown.css({width:width, minWidth:width,
          left:window.scrollX + Math.max(8, Math.min(box.left, window.innerWidth - width - 8))});
        var up = below < Math.min(220, this.$dropdown_content[0].scrollHeight) && above > below;
        this.$dropdown_content.css("max-height", Math.max(80, Math.min(220, up ? above : below)));
        this.$dropdown.css("top", window.scrollY + (up ? box.top - this.$dropdown.outerHeight() - 4 : box.bottom + 4));
      };
    }')))

desktop_workbench_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(id = ns("root"), class = "pk-workbench",
    shiny::tags$aside(class = "pk-wb-sidebar", `aria-label` = "データ操作",
      shiny::div(class = "pk-wb-sidebar-heading", shiny::h2(desktop_i18n_text("データ操作", "Data operations")),
        shiny::p(desktop_i18n_text("表を選び、条件を適用します。", "Select a table and apply transformations."))),
      shiny::uiOutput(ns("sidebar_status")),
      shiny::tabsetPanel(id = ns("mode"), type = "pills", selected = "catalog",
        shiny::tabPanel(desktop_i18n_text("データ一覧", "Data"), value = "catalog",
          shiny::div(class = "pk-wb-catalog",
            shiny::p(class = "pk-caption", desktop_i18n_text("元データと保存済みの表", "Source data and saved tables")),
            shiny::uiOutput(ns("catalog")),
            shiny::conditionalPanel("input.source === 'FILE'", ns = ns,
              shiny::fileInput(ns("file"), desktop_i18n_text("ファイルを読み込む", "Import a file"), accept = c(".csv", ".xpt", ".sas7bdat", ".rds", ".parquet"),
                buttonLabel = desktop_i18n_text("ファイルを選択", "Choose file"), placeholder = "未選択", width = "100%"),
              shiny::p(class = "pk-caption", "CSV / XPT / SAS7BDAT / RDS / Parquet")),
            shiny::actionButton(ns("load_source"), desktop_i18n_text("選んだ表を開く", "Open selected table"), class = "btn-primary", width = "100%"),
            shiny::p(class = "pk-caption", desktop_i18n_text("表を開くと「前処理」に移ります。保存した表も入力に使えます。", "Opening a table takes you to Preprocessing. Saved tables can also be used as input.")))),
      shiny::tabPanel(desktop_i18n_text("前処理", "Preprocessing"), value = "prepare",
        shiny::uiOutput(ns("editor_heading")),
        desktop_workbench_select(ns("op"), desktop_i18n_text("加工方法", "Operation"), c("加工を選択" = "", "行を絞り込む" = "filter", "残す列を選ぶ" = "select", "列名を変える" = "rename", "列の型を変える" = "convert", "行を並べ替える" = "sort", "重複行を取り除く" = "distinct", "採血・投与日時から経過時間を作る" = "elapsed_time", "位置別のBLQ規則を適用する" = "blq_rules")),
        shiny::conditionalPanel("['filter','rename','convert'].indexOf(input.op) >= 0", ns = ns,
          desktop_workbench_select(ns("column"), desktop_i18n_text("対象列", "Column"), character())),
        shiny::conditionalPanel("input.op === 'filter'", ns = ns,
          desktop_workbench_select(ns("operator"), desktop_i18n_text("比較", "Comparison"), c("等しい" = "eq", "等しくない" = "ne", "いずれかに一致（改行区切り）" = "in", "以上" = "ge", "以下" = "le", "より大きい" = "gt", "より小さい" = "lt", "欠測" = "missing", "非欠測" = "present")),
          shiny::conditionalPanel("input.operator !== 'missing' && input.operator !== 'present'", ns = ns,
            shiny::textAreaInput(ns("value"), "比較値", rows = 2, width = "100%")),
          shiny::p(class = "pk-caption", desktop_i18n_text("IDの先頭ゼロを保持します。欠測行は「欠測」で指定します。", "Leading zeros in IDs are retained. Select Missing to identify missing rows."))),
        shiny::conditionalPanel("['select','sort','distinct'].indexOf(input.op) >= 0", ns = ns,
          desktop_workbench_select(ns("columns"), desktop_i18n_text("対象列（順序も使用）", "Columns (order is used)"), character(), multiple = TRUE)),
        shiny::conditionalPanel("input.op === 'sort'", ns = ns, shiny::checkboxInput(ns("decreasing"), desktop_i18n_text("降順にする", "Sort descending"), FALSE)),
        shiny::conditionalPanel("input.op === 'distinct'", ns = ns, shiny::p(class = "pk-caption", "対象列が同じ行は、先頭の1行だけを残します。その他の列の値は比較しません。")),
        shiny::conditionalPanel("input.op === 'rename'", ns = ns, shiny::textInput(ns("new_name"), desktop_i18n_text("新しい列名", "New column name"))),
        shiny::conditionalPanel("input.op === 'convert'", ns = ns,
          desktop_workbench_select(ns("type"), desktop_i18n_text("変換先", "Convert to"), c("文字" = "character", "数値" = "numeric"))),
        shiny::conditionalPanel("['elapsed_time','blq_rules'].indexOf(input.op) >= 0", ns = ns,
          shiny::p(class = "pk-caption", "設定と適用前の件数は本文のカードで確認してください。"))),
      shiny::tabPanel(desktop_i18n_text("結合", "Join"), value = "join",
        shiny::p(class = "pk-caption", "現在の前処理結果を左表にして、右表から列を追加します。"),
        shiny::uiOutput(ns("right_source_ui")),
        shiny::conditionalPanel("input.right_source === 'FILE'", ns = ns,
          shiny::uiOutput(ns("right_file_input"))),
        shiny::uiOutput(ns("right_param_ui")),
        desktop_workbench_select(ns("keys"), desktop_i18n_text("結合キー", "Join keys"), character(), multiple = TRUE),
        desktop_workbench_select(ns("join_columns"), desktop_i18n_text("右表から追加する列", "Columns to add from right table"), character(), multiple = TRUE),
        desktop_workbench_select(ns("method"), desktop_i18n_text("結合方法", "Join type"), c("左表の全行を残す" = "left", "一致した行だけ残す" = "inner")),
        shiny::p(class = "pk-caption", "右表はキーごとに1行が必要です。重複は自動でまとめません。"))),
      shiny::conditionalPanel("input.mode === 'join' || (input.mode === 'prepare' && ['elapsed_time','blq_rules'].indexOf(input.op) < 0)", ns = ns, class = "pk-wb-sidebar-footer-host",
        shiny::div(class = "pk-wb-sidebar-footer",
          shiny::conditionalPanel("input.mode === 'prepare' && ['elapsed_time','blq_rules'].indexOf(input.op) < 0", ns = ns,
            shiny::actionButton(ns("apply_step"), desktop_i18n_text("条件を適用", "Apply"), class = "btn-primary", width = "100%")),
          shiny::conditionalPanel("input.mode === 'join'", ns = ns,
            shiny::actionButton(ns("apply_join"), desktop_i18n_text("結合を適用", "Apply join"), class = "btn-primary", width = "100%")),
          shiny::conditionalPanel("input.mode === 'join' || (input.mode === 'prepare' && ['elapsed_time','blq_rules'].indexOf(input.op) < 0)", ns = ns,
            shiny::actionButton(ns("discard_draft"), desktop_i18n_text("未適用の変更を破棄", "Discard unapplied changes"), width = "100%")),
          shiny::p(class = "pk-caption", "タブを切り替えても入力内容は保持されます。")))),
    shiny::tags$main(class = "pk-wb-main",
      shiny::div(class = "pk-wb-toolbar", shiny::uiOutput(ns("heading"))),
      shiny::uiOutput(ns("notice_ui")), shiny::uiOutput(ns("state_ui")),
      shiny::conditionalPanel("input.mode === 'prepare' && ['elapsed_time','blq_rules'].indexOf(input.op) >= 0", ns = ns,
        shiny::div(class = "pk-card pk-preparation-card", shiny::h2(desktop_i18n_text("前処理の設定", "Preprocessing settings")),
          desktop_pk_prepare_controls(ns), shiny::uiOutput(ns("draft_preview")),
          shiny::div(class = "pk-preparation-apply", shiny::actionButton(ns("apply_step_body"), desktop_i18n_text("条件を適用", "Apply"), class = "btn-primary"),
            shiny::actionButton(ns("discard_draft_body"), desktop_i18n_text("未適用の変更を破棄", "Discard unapplied changes"))))),
      shiny::tabsetPanel(id = ns("view"),
        shiny::tabPanel(desktop_i18n_text("データ", "Data"), value = "data", desktop_grid_ui(ns("grid"))),
        shiny::tabPanel(desktop_i18n_text("条件と履歴", "Settings and history"), value = "history", shiny::uiOutput(ns("history")),
          shiny::selectInput(ns("edit_step_index"), desktop_i18n_text("操作する条件", "Select a step"), character()),
          shiny::div(class = "pk-actions", shiny::actionButton(ns("edit_step"), desktop_i18n_text("条件を編集", "Edit step")),
            shiny::actionButton(ns("remove_step"), desktop_i18n_text("条件を削除", "Remove step")), shiny::actionButton(ns("undo_step"), desktop_i18n_text("1つ戻す", "Undo"))),
          shiny::uiOutput(ns("source_history"))),
        shiny::tabPanel(desktop_i18n_text("結合の確認", "Join checks"), value = "qc", shiny::uiOutput(ns("qc_summary")),
          shiny::selectInput(ns("qc_view"), desktop_i18n_text("確認する行", "Rows to review"), c("右表のキー重複" = "duplicate_right",
            "左表のキー欠測・空欄" = "missing_left", "右表のキー欠測・空欄" = "missing_right", "左表の不一致行" = "unmatched_left")),
          desktop_grid_ui(ns("qc_grid")))),
      shiny::conditionalPanel("output.save_available === 'true'", ns = ns, shiny::div(class = "pk-wb-savebar", shiny::div(class = "pk-wb-save-name",
        shiny::textInput(ns("save_name"), desktop_i18n_text("作業表の保存名", "Saved table name"), "前処理済みデータ", width = "100%")),
        shiny::div(class = "pk-wb-save-actions", shiny::uiOutput(ns("save_actions")), shiny::uiOutput(ns("save_status")))))))
}

desktop_workbench_server <- function(id, engine) shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns
  select_panel <- function(value, focus = TRUE) {
    shiny::updateTabsetPanel(session, "mode", selected = value)
    if (focus) session$sendCustomMessage("pk-workbench-focus-tab", list(id = ns("mode"), value = value))
  }
  base <- shiny::reactiveVal(NULL)
  originals <- intersect(c("ADNCA", "ADPP"), names(engine$data))
  initial_source <- if ("ADNCA" %in% originals) "ADNCA" else "FILE"
  initial_right <- if ("ADPP" %in% originals) "ADPP" else "FILE"
  base_id <- shiny::reactiveVal(initial_source)
  base_name <- shiny::reactiveVal(if (initial_source == "FILE") "データを読み込んでください" else initial_source)
  steps <- shiny::reactiveVal(list())
  undo <- shiny::reactiveVal(list())
  joined <- shiny::reactiveVal(NULL)
  editing <- shiny::reactiveVal(NULL)
  form_ack <- shiny::reactiveVal(NULL)
  draft_generation <- shiny::reactiveVal(0L)
  lloq_initialized_generation <- NULL
  active_mode <- shiny::reactiveVal("prepare")
  receipt <- shiny::reactiveVal(NULL)
  notice <- shiny::reactiveVal(NULL)
  source_pending <- shiny::reactiveVal(NULL)
  active_right_file <- shiny::reactiveVal(NULL)
  right_file_generation <- shiny::reactiveVal(0L)
  shiny::observeEvent(input$right_file, active_right_file(input$right_file), priority = 100)
  output$right_file_input <- shiny::renderUI({
    right_file_generation()
    selected <- shiny::isolate(active_right_file())
    shiny::tagList(shiny::fileInput(ns("right_file"), "右表のファイル", accept = c(".csv", ".xpt", ".sas7bdat", ".rds", ".parquet"),
      buttonLabel = desktop_i18n_text("ファイルを選択", "Choose file"), placeholder = if (is.null(selected)) "未選択" else paste0("保持中：", selected$name), width = "100%"),
      if (!is.null(selected)) shiny::p(class = "pk-caption", paste0("適用済みのファイルを保持：", selected$name, "。変更するときは別のファイルを選択してください。")))
  })
  say <- function(message, tone = "info") notice(list(message = message, tone = tone))
  safe <- function(expr) tryCatch(expr, error = function(e) { say(conditionMessage(e), "error"); NULL })
  tables <- shiny::reactive({ engine$tables_signal(); desktop_tables(engine) })
  table_name <- function(id) {
    if (id %in% c("ADNCA", "ADPP")) return(id)
    if (identical(id, "FILE")) return(input$file$name %||% "読み込んだ表")
    found <- Filter(function(x) identical(x$id, id), tables())
    if (length(found)) found[[1]]$name else id
  }
  source_choices <- shiny::reactive({
    saved <- tables()
    choices <- if (length(originals)) list("元データ" = originals) else list()
    if (length(saved)) choices[["保存済みの表"]] <- stats::setNames(vapply(saved, `[[`, character(1), "id"),
      vapply(saved, function(x) paste0(x$name, " · ", x$counts$result, " 行 · ", substr(x$id, 7L, 12L)), character(1)))
    choices[["ファイル"]] <- c("ファイルを読み込む" = "FILE")
    choices
  })
  output$catalog <- shiny::renderUI({
    choices <- source_choices()
    values <- unlist(choices, use.names = FALSE)
    labels <- c(originals, if (length(tables())) vapply(tables(), function(x) paste0(x$name, " · ", x$counts$result, " 行"), character(1)), "ファイルを読み込む")
    selected <- shiny::isolate(input$source) %||% base_id()
    shiny::radioButtons(ns("source"), NULL, choiceNames = lapply(labels, function(x) shiny::span(class = "pk-data-label", x)),
      choiceValues = values, selected = if (selected %in% values) selected else initial_source)
  })
  output$right_source_ui <- shiny::renderUI(desktop_workbench_select(ns("right_source"), "右表（追加元）", source_choices(),
    selected = shiny::isolate(input$right_source) %||% initial_right))
  read_source <- function(id, file = NULL) {
    pk_assert(length(id) == 1L, "データを選択してください。")
    if (identical(id, "FILE")) {
      pk_assert(!is.null(file), "ファイルを選択してください。")
      return(pkapp_read_table(file$datapath, tolower(tools::file_ext(file$name))))
    }
    if (id %in% c("ADNCA", "ADPP")) return(engine$data[[id]])
    desktop_table_get(engine, id)
  }
  reset_editor <- function() {
    editing(NULL); form_ack(NULL)
    draft_generation(draft_generation() + 1L)
    shiny::updateSelectInput(session, "op", selected = "")
  }
  open_source <- function(id, file = NULL, silent = FALSE) {
    value <- read_source(id, file); pkapp_table_check(value)
    base(value); base_id(id); base_name(if (identical(id, "FILE")) file$name else table_name(id))
    steps(list()); undo(list()); joined(NULL); receipt(NULL); reset_editor(); active_mode("prepare")
    if (!silent) select_panel("prepare")
    shiny::updateTabsetPanel(session, "view", selected = "data")
    say(if (silent) "" else paste0(base_name(), " を開きました。"))
  }
  shiny::observeEvent(TRUE, {
    if (length(originals)) open_source(initial_source, silent = TRUE) else say("ファイルを読み込むか、保存済みの表を選んで開いてください。")
  }, once = TRUE)
  prepared <- shiny::reactive({ shiny::req(base()); pkapp_prepare(base(), steps()) })
  form <- shiny::reactive({
    op <- input$op %||% ""
    switch(op, filter = list(op = op, column = input$column, operator = input$operator %||% "eq",
      values = if ((input$operator %||% "eq") %in% c("missing", "present")) list() else
        as.list(if (identical(input$operator, "in")) strsplit(input$value %||% "", "\n", fixed = TRUE)[[1]] else input$value %||% "")),
      select = list(op = op, columns = as.list(input$columns)),
      sort = list(op = op, columns = as.list(input$columns), decreasing = isTRUE(input$decreasing)),
      distinct = list(op = op, columns = as.list(input$columns)),
      rename = list(op = op, column = input$column, name = input$new_name %||% ""),
      convert = list(op = op, column = input$column, type = input$type %||% "character"), desktop_pk_prepare_form(input))
  })
  shiny::observeEvent(input$op, {
    if (identical(input$op, "") && is.null(editing())) form_ack(NULL)
  })
  step_dirty <- shiny::reactive(!is.null(editing()) || (!is.null(form()) && !identical(form(), form_ack())))
  draft_preview <- shiny::reactive({
    if (is.null(base()) || !step_dirty() || !(input$op %||% "") %in% c("elapsed_time", "blq_rules")) return(NULL)
    tryCatch({
      index <- editing()
      source <- if (is.null(index)) prepared()$data else pkapp_prepare(base(), if (index > 1L) steps()[seq_len(index - 1L)] else list())$data
      pkapp_prepare(source, list(form()))
    }, error = function(e) structure(list(message = conditionMessage(e)), class = "pk_draft_error"))
  })
  output$draft_preview <- shiny::renderUI({
    value <- draft_preview()
    if (is.null(value)) return(NULL)
    if (inherits(value, "pk_draft_error")) return(shiny::p(class = "pk-caption", paste("適用前の確認：", value$message)))
    trace <- value$trace[[1]]
    shiny::div(class = "pk-draft-preview", shiny::h3("適用前の件数"),
      shiny::p(class = "pk-caption", "下のデータ表は適用済みの結果です。「条件を適用」でこの設定を反映します。"),
      shiny::div(class = "pk-table-scroll", desktop_display_table(trace$summary)))
  })
  for (rule in c("pre", "post", "first", "middle", "last", "all_blq")) local({
    position <- rule
    output[[paste0(position, "_rule_count")]] <- shiny::renderText({
      value <- draft_preview()
      if (is.null(value) || inherits(value, "pk_draft_error") || !identical(input$op, "blq_rules")) return("—")
      rows <- value$trace[[1]]$rows
      paste0(sum(!is.na(rows$POSITION) & rows$POSITION == position & rows$BLQ), " 行")
    })
  })
  right <- shiny::reactive({
    tryCatch(read_source(input$right_source %||% initial_right, active_right_file()), error = function(e) structure(list(message = conditionMessage(e)), class = "pk_wb_error"))
  })
  output$right_param_ui <- shiny::renderUI({
    data <- right()
    if (!is.data.frame(data) || !"PARAMCD" %in% names(data)) return(NULL)
    codes <- unique(as.character(data$PARAMCD[!is.na(data$PARAMCD)]))
    preferred <- intersect(c("CMAXSS", "CMAX"), codes)
    selected <- shiny::isolate(input$right_param)
    if (is.null(selected) || !selected %in% c("__ALL__", codes)) selected <- if (length(preferred)) preferred[[1]] else "__ALL__"
    desktop_workbench_select(ns("right_param"), "右表のPARAMCD", c("すべて（絞り込みなし）" = "__ALL__", stats::setNames(codes, codes)), selected = selected)
  })
  # Keep column choices stable when applying filters; only schema changes reset choices.
  editor_columns <- shiny::reactive({
    index <- editing()
    if (is.null(index)) names(prepared()$data) else names(pkapp_prepare(base(), if (index > 1L) steps()[seq_len(index - 1L)] else list())$data)
  })
  shiny::observeEvent(list(editor_columns(), draft_generation()), {
    choices <- editor_columns()
    step <- if (is.null(editing())) NULL else steps()[[editing()]]
    data <- if (is.null(editing())) prepared()$data else pkapp_prepare(base(), if (editing() > 1L) steps()[seq_len(editing() - 1L)] else list())$data
    elapsed <- Filter(function(x) identical(x$op, "elapsed_time"), steps())
    latest <- if (length(elapsed)) tail(elapsed, 1L)[[1]]$output %||% "NCA_TIME_H" else NULL
    initialize_lloq <- is.null(step) && !identical(lloq_initialized_generation, draft_generation())
    desktop_pk_prepare_update_columns(session, input, choices, step, data = data, preferred_time = latest,
      initialize_lloq = initialize_lloq)
    if (initialize_lloq) lloq_initialized_generation <<- draft_generation()
    shiny::updateSelectInput(session, "column", choices = choices, selected = if (length(intersect(shiny::isolate(input$column), choices))) intersect(shiny::isolate(input$column), choices)[1] else choices[1])
    shiny::updateSelectizeInput(session, "columns", choices = choices,
      selected = if (is.null(shiny::isolate(input$columns))) choices else intersect(shiny::isolate(input$columns), choices))
  })
  shiny::observeEvent(list(input$profile_columns, input$blq_concentration, input$prep_blq_column,
    input$prep_mdv_column, input$predose_column, input$lloq_source, input$lloq_column), {
    if (!identical(input$op, "blq_rules")) return()
    data <- if (is.null(editing())) prepared()$data else pkapp_prepare(base(), if (editing() > 1L) steps()[seq_len(editing() - 1L)] else list())$data
    choices <- c("時間列を選択" = "", desktop_pk_time_choices(data, c(input$profile_columns, input$blq_concentration, input$prep_blq_column, input$prep_mdv_column, input$predose_column,
      if (identical(input$lloq_source, "column")) input$lloq_column)))
    current <- shiny::isolate(input$blq_time) %||% ""
    shiny::updateSelectInput(session, "blq_time", choices = choices, selected = if (current %in% unname(choices)) current else "")
  })
  output$blq_time_hint <- shiny::renderText({
    desktop_pk_time_hint(prepared()$data, input$blq_time)
  })
  shiny::observeEvent(list(names(prepared()$data), names(right())), {
    choices <- intersect(names(prepared()$data), names(right()))
    selected <- intersect(shiny::isolate(input$keys), choices)
    if (!length(selected)) selected <- intersect(c("STUDYID", "USUBJID"), choices)
    shiny::updateSelectizeInput(session, "keys", choices = choices, selected = selected)
  })
  shiny::observeEvent(list(names(right()), input$keys), {
    choices <- setdiff(names(right()), input$keys)
    current <- intersect(shiny::isolate(input$join_columns), choices)
    if (!length(current)) current <- intersect(c("PARAMCD", "AVAL", "AVALU"), choices)
    if (!length(current)) current <- choices
    shiny::updateSelectizeInput(session, "join_columns", choices = choices, selected = current)
  })
  join_spec <- shiny::reactive(list(source = input$right_source %||% initial_right, file = active_right_file(),
    param = if (is.data.frame(right()) && "PARAMCD" %in% names(right())) input$right_param else NULL,
    by = input$keys, columns = input$join_columns, join = input$method %||% "left"))
  join_inputs <- shiny::reactive({
    rhs <- right(); pk_assert(is.data.frame(rhs), rhs$message %||% "右表を選択してください。")
    spec <- join_spec()
    if ("PARAMCD" %in% names(rhs)) {
      pk_assert(length(spec$param) == 1L, "右表のPARAMCDを選択してください。")
      if (!identical(spec$param, "__ALL__")) rhs <- rhs[!is.na(rhs$PARAMCD) & rhs$PARAMCD == spec$param, , drop = FALSE]
    }
    lhs <- prepared()$data
    if (identical(base_id(), "FILE") && !identical(spec$source, "FILE")) lhs <- desktop_merge_csv_keys(lhs, rhs, spec$by)
    if (identical(spec$source, "FILE") && !identical(base_id(), "FILE")) rhs <- desktop_merge_csv_keys(rhs, lhs, spec$by)
    list(left = lhs, right = rhs)
  })
  join_dirty <- shiny::reactive(identical(active_mode(), "join") && (is.null(joined()) || !identical(join_spec(), joined()$spec)))
  dirty <- shiny::reactive(step_dirty() || join_dirty())
  result <- shiny::reactive({
    if (!is.null(joined())) return(joined())
    value <- prepared()
    value$inputs <- list(source = base())
    value$recipe <- list(kind = "prepare", steps = steps(), source = list(id = base_id(), name = base_name()))
    value
  })
  result_signature <- shiny::reactive(desktop_hash(list(result()$inputs, result()$recipe, input$save_name)))
  is_saved <- shiny::reactive(!is.null(receipt()) && identical(receipt()$signature, result_signature()) && !dirty())
  shiny::observeEvent(input$load_source, {
    safe({
      id <- input$source %||% initial_source
      # Validate the proposed input before offering to leave a valid workspace.
      pkapp_table_check(read_source(id, input$file))
      if ((length(steps()) || !is.null(joined()) || dirty()) && !is_saved()) {
        source_pending(list(id = id, file = input$file))
        shiny::showModal(shiny::modalDialog(title = "現在の作業表は未保存です", "別の表を開くと現在の加工条件はリセットされます。保存済みの表は残ります。",
          footer = shiny::tagList(shiny::modalButton("作業に戻る"), shiny::actionButton(ns("confirm_source"), "保存せず表を開く"))))
      } else open_source(id, input$file)
    })
  })
  shiny::observeEvent(input$confirm_source, {
    pending <- source_pending(); shiny::req(pending)
    safe(open_source(pending$id, pending$file)); source_pending(NULL); shiny::removeModal()
  })
  shiny::observeEvent(input$mode, {
    # Catalog navigation must not alter the applied mode or discard a draft.
    if (!input$mode %in% c("prepare", "join")) return()
    if (identical(input$mode, active_mode())) return()
    if (dirty()) {
      select_panel(active_mode())
      say("未適用の変更があります。「適用」または「未適用の変更を破棄」で確定してください。", "warning")
    } else active_mode(input$mode)
  }, ignoreInit = TRUE)
  apply_steps <- function(candidate) {
    value <- pkapp_prepare(base(), candidate) # validate the entire chain atomically
    undo(c(undo(), list(steps()))); steps(candidate)
    had_join <- !is.null(joined()); joined(NULL); receipt(NULL)
    say(paste0("適用しました。", nrow(value$data), " 行・", ncol(value$data), " 列", if (had_join) "。前処理が変わったため、結合は再適用してください。" else "。"), "success")
  }
  apply_step <- function() {
    safe({
      pk_assert(identical(active_mode(), "prepare"), "前処理を選択してください。")
      step <- form(); pk_assert(!is.null(step), "加工方法を選択してください。")
      if (identical(step$op, "filter") && !step$operator %in% c("missing", "present"))
        pk_assert(length(step$values) > 0L && all(nzchar(unlist(step$values))), "比較値を入力してください。")
      candidate <- steps(); index <- editing()
      if (is.null(index)) candidate[[length(candidate) + 1L]] <- step else candidate[[index]] <- step
      apply_steps(candidate); form_ack(step); editing(NULL)
      draft_generation(draft_generation() + 1L)
      shiny::updateSelectInput(session, "op", selected = "")
    })
  }
  shiny::observeEvent(input$apply_step, apply_step())
  shiny::observeEvent(input$apply_step_body, apply_step())
  shiny::observeEvent(input$edit_step, {
    safe({
      pk_assert(!dirty(), "未適用の変更を適用または破棄してください。")
      index <- suppressWarnings(as.integer(input$edit_step_index))
      pk_assert(length(index) == 1L && !is.na(index) && index >= 1L && index <= length(steps()), "編集する条件を選択してください。")
      step <- steps()[[index]]; editing(index); active_mode("prepare")
      select_panel("prepare")
      shiny::updateSelectInput(session, "op", selected = step$op)
      shiny::updateSelectInput(session, "column", choices = editor_columns(), selected = step$column)
      shiny::updateSelectizeInput(session, "columns", choices = editor_columns(), selected = unlist(step$columns))
      shiny::updateSelectInput(session, "operator", selected = step$operator %||% "eq")
      shiny::updateTextAreaInput(session, "value", value = paste(unlist(step$values), collapse = "\n"))
      shiny::updateTextInput(session, "new_name", value = step$name %||% "")
      shiny::updateSelectInput(session, "type", selected = step$type %||% "character")
      shiny::updateCheckboxInput(session, "decreasing", value = isTRUE(step$decreasing))
      desktop_pk_prepare_restore(session, step)
      say(paste0("条件 ", index, " を編集中です。結果は「条件を適用」で更新されます。"))
    })
  })
  shiny::observeEvent(input$remove_step, {
    safe({
      pk_assert(!dirty(), "未適用の変更を適用または破棄してください。")
      index <- suppressWarnings(as.integer(input$edit_step_index))
      pk_assert(length(index) == 1L && !is.na(index) && index >= 1L && index <= length(steps()), "削除する条件を選択してください。")
      apply_steps(steps()[-index]); reset_editor()
    })
  })
  shiny::observeEvent(input$undo_step, {
    safe({
      pk_assert(!dirty(), "未適用の変更を適用または破棄してください。")
      history <- undo(); pk_assert(length(history) > 0L, "戻せる操作がありません。")
      candidate <- history[[length(history)]]; pkapp_prepare(base(), candidate)
      steps(candidate); undo(head(history, -1L)); joined(NULL); receipt(NULL); reset_editor()
      say("前処理を1つ前の状態に戻しました。", "success")
    })
  })
  shiny::observeEvent(input$apply_join, {
    safe({
      pk_assert(!step_dirty(), "前処理の未適用の変更を適用または破棄してください。")
      values <- join_inputs(); spec <- join_spec()
      prefix <- if (spec$source %in% c("ADNCA", "ADPP")) spec$source else "RIGHT"
      value <- pkapp_merge(values$left, values$right, spec$by, spec$columns, spec$join, prefix)
      value$inputs <- c(values, list(left_source = base(), right_source = right()))
      value$recipe <- list(kind = "join", by = as.list(spec$by), columns = as.list(spec$columns), join = spec$join, right_name = prefix,
        left_steps = steps(), source = list(id = base_id(), name = base_name()),
        right_source = list(id = spec$source, name = if (identical(spec$source, "FILE")) active_right_file()$name else table_name(spec$source)),
        right_param = spec$param,
        left_csv_keys = identical(base_id(), "FILE") && !identical(spec$source, "FILE"),
        right_csv_keys = identical(spec$source, "FILE") && !identical(base_id(), "FILE"))
      value$spec <- spec; joined(value); receipt(NULL)
      say(paste0("結合を適用しました。", value$counts[["result"]], " 行。左表の一致 ", value$counts[["matched"]], " 行・不一致 ", value$counts[["unmatched"]], " 行。"), "success")
    })
  })
  discard_draft <- function() {
    form_ack(form()); editing(NULL); shiny::updateSelectInput(session, "op", selected = "")
    draft_generation(draft_generation() + 1L)
    current <- joined()
    if (is.null(current)) {
      active_mode("prepare"); select_panel("prepare")
    } else {
      spec <- current$spec
      shiny::updateSelectInput(session, "right_source", selected = spec$source)
      shiny::updateSelectInput(session, "right_param", selected = spec$param)
      shiny::updateSelectizeInput(session, "keys", selected = spec$by)
      shiny::updateSelectizeInput(session, "join_columns", selected = spec$columns)
      shiny::updateSelectInput(session, "method", selected = spec$join)
      active_right_file(spec$file)
      right_file_generation(right_file_generation() + 1L)
      active_mode("prepare"); select_panel("prepare")
    }
    say("未適用の変更を破棄しました。適用済みの結果は保持されています。")
  }
  shiny::observeEvent(input$discard_draft, discard_draft())
  shiny::observeEvent(input$discard_draft_body, discard_draft())
  shiny::observeEvent(input$save_result, {
    safe({
      pk_assert(!dirty(), "未適用の変更があります。適用または破棄してから保存してください。")
      value <- result()
      saved <- desktop_table_save(engine, input$save_name, value$inputs, value$recipe)
      receipt(list(signature = result_signature(), id = saved$id, name = saved$name))
      say(paste0("「", saved$name, "」を保存しました。データ一覧から選択できます。"), "success")
    })
  })
  output$csv <- shiny::downloadHandler(filename = function() paste0(gsub("[^[:alnum:]一-龠ぁ-んァ-ヶー_-]", "_", input$save_name %||% "data"), ".csv"),
    content = function(file) { pk_assert(!dirty(), "未適用の変更を適用または破棄してください。"); pk_write_csv(result()$data, file) }, contentType = "text/csv; charset=utf-8")
  diagnostics <- shiny::reactive({
    if (!identical(active_mode(), "join") && is.null(joined())) return(NULL)
    tryCatch({ values <- if (identical(active_mode(), "join")) join_inputs() else joined()$inputs
      desktop_join_diagnostics(values$left, values$right, if (identical(active_mode(), "join")) input$keys else unlist(joined()$recipe$by))
    }, error = function(e) list(error = conditionMessage(e)))
  })
  desktop_grid_server("grid", data = shiny::reactive(result()$data),
    preferred = shiny::reactive(unique(c("STUDYID", "USUBJID", "ID", "NCA_TIME_H", "NCA_CONC", "NCA_BLQ", "NCA_MDV", "NCA_BLQ_RULE", "NCA_BLQ_POSITION", "CONC", "BLQ", "BLQFL", "SEX", "TIME_H", "TIME", "PARAMCD", "AVAL", "AVALU", unlist(lapply(Filter(function(x) identical(x$op, "elapsed_time"), steps()), function(x) x$output %||% "NCA_TIME_H")), unname(joined()$added_columns)))),
    highlight = shiny::reactive(unname(joined()$added_columns) %||% character()))
  desktop_grid_server("qc_grid", data = shiny::reactive({
    value <- diagnostics(); frame <- if (!is.null(value)) value[[input$qc_view %||% "duplicate_right"]] else NULL
    if (is.data.frame(frame)) frame else NULL
  }), preferred = shiny::reactive(input$keys %||% c("STUDYID", "USUBJID")))
  output$sidebar_status <- shiny::renderUI({
    shiny::div(class = "pk-wb-sidebar-state", shiny::strong(base_name()))
  })
  output$heading <- shiny::renderUI(shiny::div(shiny::h1(base_name()),
    shiny::p(class = "pk-caption", if (!is.null(joined())) "結合を適用した作業表" else if (length(steps())) "前処理を適用した作業表" else "入力データのプレビュー")))
  output$notice_ui <- shiny::renderUI({
    value <- notice()
    if (!is.null(value) && nzchar(value$message) && !(dirty() && identical(value$tone, "success")))
      desktop_ui_notice(value$message, value$tone)
  })
  output$state_ui <- shiny::renderUI({
    if (is.null(base())) return(desktop_ui_empty("データはまだありません", "データ一覧からファイルを選んで開いてください。"))
    value <- result(); summary <- paste0(format(nrow(value$data), big.mark = ","), " 行 × ", ncol(value$data), " 列")
    shiny::div(class = "pk-wb-state", shiny::strong(summary), shiny::span(class = paste("pk-wb-status", if (dirty()) "is-draft" else if (is_saved()) "is-saved" else "is-applied"),
      if (dirty()) "未適用の変更あり" else if (is_saved()) "保存済み" else if (length(steps()) || !is.null(joined())) "適用済み・未保存" else "元データ"),
      if (dirty()) shiny::p(class = "pk-caption", "プレビューは直前に適用した結果です。変更を適用または破棄するまで保存・CSV出力できません。"),
      if (nrow(value$data) == 0L) desktop_ui_notice("結果は0行です。絞り込み条件と結合方法を確認してください。", "warning"))
  })
  output$editor_heading <- shiny::renderUI(shiny::p(class = "pk-caption", if (is.null(editing())) "条件を追加してから結果を確認できます。" else paste0("条件 ", editing(), " を編集しています。")))
  output$history <- shiny::renderUI({
    trace <- prepared()$trace
    shiny::tagList(
      if (!length(trace)) desktop_ui_empty("加工条件はまだありません", "左の「前処理」から行の絞り込みや列の変更を追加できます。") else
        shiny::tags$ol(class = "pk-wb-steps", lapply(seq_along(trace), function(i) shiny::tags$li(shiny::strong(desktop_prepare_step_label(trace[[i]]$step)),
          shiny::span(class = "pk-caption", paste0(trace[[i]]$before, " → ", trace[[i]]$after, " 行"))))),
      if (!is.null(joined())) {
        recipe <- joined()$recipe
        shiny::div(class = "pk-card", shiny::h3("適用済みの結合"),
          shiny::p(paste0("右表：", recipe$right_source$name, if (!is.null(recipe$right_param) && recipe$right_param != "__ALL__") paste0(" / PARAMCD = ", recipe$right_param) else "")),
          shiny::p(paste0("キー：", paste(unlist(recipe$by), collapse = "・"), " / ", if (recipe$join == "left") "左表の全行を残す" else "一致した行だけ残す")),
          shiny::p(paste0("追加列：", paste(unname(joined()$added_columns), collapse = "・"))))
      })
  })
  shiny::observeEvent(steps(), {
    choices <- stats::setNames(as.character(seq_along(steps())), vapply(seq_along(steps()), function(i) paste(i, desktop_prepare_step_label(steps()[[i]])), character(1)))
    shiny::updateSelectInput(session, "edit_step_index", choices = choices)
  })
  output$source_history <- shiny::renderUI({
    if (!startsWith(base_id(), "table-")) return(NULL)
    metadata <- desktop_read(file.path(desktop_table_path(engine, base_id()), "manifest.json"))
    recipe <- desktop_read(file.path(desktop_table_path(engine, base_id()), "recipe.json"))
    inherited_steps <- if (identical(recipe$kind, "prepare")) recipe$steps else recipe$left_steps
    shiny::tags$details(shiny::tags$summary("入力にした保存表の履歴（変更されません）"),
      shiny::p(paste(metadata$name, metadata$created_at)),
      if (!is.null(recipe$source$name)) shiny::p(paste0("入力表：", recipe$source$name)),
      if (length(inherited_steps)) shiny::tags$ol(lapply(inherited_steps, function(step) shiny::tags$li(desktop_prepare_step_label(step)))) else
        shiny::p("記録された前処理条件はありません。"),
      if (identical(recipe$kind, "join")) shiny::div(
        shiny::p(paste0("右表：", recipe$right_source$name %||% recipe$right_name,
          if (!is.null(recipe$right_param) && recipe$right_param != "__ALL__") paste0(" / PARAMCD = ", recipe$right_param) else "")),
        shiny::p(paste0("結合キー：", paste(unlist(recipe$by), collapse = "・"))),
        shiny::p(paste0("追加する列：", paste(unlist(recipe$columns), collapse = "・"))),
        shiny::p(if (identical(recipe$join, "left")) "左表の全行を残す結合" else "一致した行だけ残す結合")),
      shiny::tags$details(shiny::tags$summary("再実行条件の詳細"), shiny::tags$pre(desktop_json(recipe))))
  })
  output$qc_summary <- shiny::renderUI({
    value <- diagnostics()
    if (is.null(value)) return(desktop_ui_empty("結合条件を指定してください", "左の「結合」で右表とキーを選ぶと、重複・欠測・不一致の行を確認できます。"))
    shiny::tagList(if (!is.null(value$error)) desktop_ui_notice(value$error, "warning"),
      if (!is.null(value$counts)) {
        counts <- value$counts
        count_label <- function(x, unit = "行") if (!length(x) || anyNA(x)) "未計算" else paste(x, unit)
        shiny::div(class = "pk-wb-qc-counts", shiny::p(paste0("左表 ", count_label(counts$left), " / 右表 ", count_label(counts$right))),
          shiny::p(paste0("右キー重複 ", count_label(counts$duplicate_right_keys, "組"), "（", count_label(counts$duplicate_right_rows), "）・キー欠測 左 ", count_label(counts$missing_left), " / 右 ", count_label(counts$missing_right), "・左不一致 ", count_label(counts$unmatched_left))))
      }, shiny::p(class = "pk-caption", "ここでの確認は選択した結合キーに限ります。PK値や解析対象の妥当性を保証するものではありません。"))
  })
  output$save_available <- shiny::renderText(if (!is.null(base()) && !dirty()) "true" else "false")
  shiny::outputOptions(output, "save_available", suspendWhenHidden = FALSE)
  output$save_actions <- shiny::renderUI({
    disabled <- is.null(base()) || dirty()
    save_button <- shiny::actionButton(ns("save_result"), "作業表と履歴を保存", class = "btn-primary")
    if (disabled) save_button$attribs$disabled <- "disabled"
    shiny::div(class = "pk-actions", save_button,
      if (disabled) shiny::tags$button(type = "button", class = "btn btn-default", disabled = "disabled", "CSVをダウンロード") else shiny::downloadButton(ns("csv"), "CSVをダウンロード"))
  })
  output$save_status <- shiny::renderUI(shiny::p(class = "pk-caption", if (dirty()) "未適用の変更を確定してください。" else if (is_saved()) paste0("保存済み：", receipt()$name) else "現在の結果は未保存です。CSVは全行・全列を出力します。"))
  invisible(list(base = base, steps = steps, prepared = prepared, result = result, dirty = dirty,
    diagnostics = diagnostics, receipt = receipt, mode = active_mode, notice = notice, editing = editing))
})
