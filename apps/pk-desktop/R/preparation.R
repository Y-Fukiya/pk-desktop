# Declarative transformations: no user supplied expression is evaluated.
pkapp_table_check <- function(data) {
  pk_assert(is.data.frame(data) && ncol(data) > 0L && !anyNA(names(data)) &&
    all(nzchar(trimws(names(data)))) && identical(names(data), trimws(names(data))) &&
    !anyDuplicated(names(data)), "列名が空欄・重複・前後に空白を含む表は使えません。")
  pk_assert(all(vapply(data, function(x) is.atomic(x) && is.null(dim(x)) &&
    (is.character(x) || is.numeric(x) || is.logical(x) || is.factor(x) || inherits(x, c("POSIXct", "Date"))), logical(1))), "リスト列・複合列は使えません。")
  invisible(data)
}
pkapp_read_table <- function(path, format = tolower(tools::file_ext(path))) {
  pk_assert(length(format) == 1 && format %in% c("csv", "xpt", "sas7bdat", "rds", "parquet"), "CSV/XPT/SAS7BDAT/RDS/Parquetを指定してください。")
  need <- switch(format, xpt = "haven", sas7bdat = "haven", parquet = if (requireNamespace("arrow", quietly = TRUE)) "arrow" else "nanoparquet", NULL)
  if (!is.null(need)) pk_assert(requireNamespace(need, quietly = TRUE), paste("必要なRパッケージ：", need))
  data <- switch(format, csv = pk_csv(path), xpt = haven::read_xpt(path, .name_repair = "minimal"), sas7bdat = haven::read_sas(path, .name_repair = "minimal"), rds = readRDS(path),
    parquet = if (need == "arrow") arrow::read_parquet(path) else
      nanoparquet::read_parquet(path, options = nanoparquet::parquet_options(read_int64_type = "integer64")))
  if (is.data.frame(data)) for (name in names(data)) {
    if (inherits(data[[name]], "integer64")) {
      pk_assert(requireNamespace("bit64", quietly = TRUE), "integer64列の正確な読込にはbit64が必要です。")
      data[[name]] <- as.character(data[[name]])
    }
  }
  pkapp_table_check(data)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  for (name in names(data)) {
    if (is.factor(data[[name]])) data[[name]] <- as.character(data[[name]])
    if (inherits(data[[name]], "haven_labelled")) data[[name]] <- haven::zap_labels(data[[name]])
  }
  data
}
pkapp_prepare <- function(data, steps = list()) {
  pkapp_table_check(data)
  pk_assert(is.list(steps), "処理条件はリストで指定してください。")
  before <- nrow(data)
  trace <- list()
  for (step in steps) {
    n <- nrow(data)
    pk_assert(is.list(step) && length(step$op) == 1 && step$op %in% c("filter", "select", "rename", "convert", "where", "sort", "distinct", "elapsed_time", "blq_rules"), "未対応の前処理です。")
    row_trace <- NULL
    step_summary <- NULL
    if (step$op %in% c("elapsed_time", "blq_rules")) {
      value <- if (step$op == "elapsed_time") desktop_prepare_elapsed(data, step) else desktop_prepare_blq(data, step)
      data <- value$data
      row_trace <- value$rows
      step_summary <- value$summary
    } else if (step$op == "where") {
      keep <- desktop_predicate(step$predicate, data)
      pk_assert(is.logical(keep) && length(keep) %in% c(1L, nrow(data)), "絞り込み式は行ごとの真偽値が必要です。")
      if (length(keep) == 1L) keep <- rep(keep, nrow(data))
      data <- data[!is.na(keep) & keep, , drop = FALSE]
    } else if (step$op %in% c("sort", "distinct")) {
      columns <- unlist(step$columns, use.names = FALSE)
      pk_assert(is.character(columns) && length(columns) > 0L && !anyDuplicated(columns) && all(columns %in% names(data)), "対象列を指定してください。")
      if (step$op == "distinct") data <- data[!duplicated(data[columns]), , drop = FALSE] else {
        pk_assert(is.null(step$decreasing) || (is.logical(step$decreasing) && length(step$decreasing) == 1L && !is.na(step$decreasing)), "降順指定は真偽値です。")
        data <- data[do.call(order, c(data[columns], list(decreasing = isTRUE(step$decreasing), na.last = TRUE))), , drop = FALSE]
      }
    } else if (step$op == "select") {
      columns <- unlist(step$columns, use.names = FALSE)
      pk_assert(length(columns) > 0 && !anyDuplicated(columns) && all(columns %in% names(data)), "残す列を選択してください。")
      data <- data[columns]
    } else {
      column <- step$column
      pk_assert(is.character(column) && length(column) == 1 && column %in% names(data), "指定した列がありません。")
      x <- data[[column]]
      if (step$op == "rename") {
        pk_assert(is.character(step$name) && length(step$name) == 1 && !is.na(step$name) &&
          nzchar(trimws(step$name)) && identical(step$name, trimws(step$name)) &&
          !(step$name %in% setdiff(names(data), column)), "新しい列名が空欄・重複・前後に空白を含んでいます。")
        names(data)[names(data) == column] <- step$name
      } else if (step$op == "convert") {
        pk_assert(length(step$type) == 1 && step$type %in% c("character", "numeric"), "型はcharacterまたはnumericです。")
        pk_assert(!(column %in% c("USUBJID", "SUBJID", "STUDYID") && step$type == "numeric"), "ID列は文字型のまま保持してください。")
        data[[column]] <- if (step$type == "numeric") pk_number(x, column, allow_na = TRUE) else as.character(x)
      } else {
        operator <- step$operator
        pk_assert(length(operator) == 1 && operator %in% c("eq", "ne", "in", "ge", "le", "gt", "lt", "missing", "present"), "未対応の比較条件です。")
        values <- unlist(step$values, use.names = FALSE)
        if (operator %in% c("ge", "le", "gt", "lt")) {
          x <- pk_number(x, column, allow_na = TRUE)
          values <- pk_number(values, "比較値")
        }
        if (!operator %in% c("missing", "present")) pk_assert(length(values) >= 1 && !anyNA(values) &&
          (operator == "in" || length(values) == 1), "比較値を指定してください。")
        keep <- switch(operator, eq = x == values, ne = x != values, `in` = x %in% values,
          ge = x >= values, le = x <= values, gt = x > values, lt = x < values,
          missing = is.na(x), present = !is.na(x))
        data <- data[!is.na(keep) & keep, , drop = FALSE]
      }
    }
    rownames(data) <- NULL
    trace[[length(trace) + 1L]] <- list(step = step, before = n, after = nrow(data))
    if (!is.null(row_trace)) trace[[length(trace)]]$rows <- row_trace
    if (!is.null(step_summary)) trace[[length(trace)]]$summary <- step_summary
  }
  list(data = data, counts = c(input = before, result = nrow(data)), trace = trace)
}
desktop_tables_init <- function(engine) {
  engine$tables_signal <- shiny::reactiveVal(0L)
  engine$tables_dir <- file.path(engine$project, "prepared")
  dir.create(engine$tables_dir, showWarnings = FALSE)
}
desktop_tables <- function(engine) {
  paths <- list.files(engine$tables_dir, pattern = "^table-", full.names = TRUE)
  lapply(paths, function(path) desktop_read(file.path(path, "manifest.json")))
}
desktop_table_path <- function(engine, id) {
  pk_assert(is.character(id) && length(id) == 1 && grepl("^table-[a-f0-9]{24}$", id), "無効な保存表IDです。")
  file.path(engine$tables_dir, id)
}
desktop_table_get <- function(engine, id) {
  path <- desktop_table_path(engine, id)
  manifest <- desktop_read(file.path(path, "manifest.json"))
  desktop_verify_files(path, manifest$files)
  readRDS(file.path(path, "result.rds"))
}
desktop_table_save <- function(engine, name, inputs, recipe, channel = "ui", actor = "local user", frozen = NULL, provenance = NULL) {
  pk_assert(is.character(name) && length(name) == 1 && nzchar(trimws(name)) && nchar(name) <= 100, "保存名は1〜100文字です。")
  pk_assert(channel %in% c("ui", "mcp", "r"), "無効な操作元です。")
  value <- if (is.null(frozen)) desktop_recipe_run(inputs, recipe) else frozen
  if (recipe$kind %in% c("nca", "nca_assembly", "nca_annotation", "nca_covariates")) recipe$specification <- value$specification
  id <- paste0("table-", substr(desktop_hash(list(Sys.time(), tempfile(), name)), 1, 24))
  stage <- tempfile(".table-", engine$tables_dir)
  dir.create(stage)
  Sys.chmod(stage, "0700")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  saveRDS(inputs, file.path(stage, "inputs.rds"), version = 3)
  saveRDS(value$data, file.path(stage, "result.rds"), version = 3)
  pk_write_csv(value$data, file.path(stage, "result.csv"))
  desktop_atomic(recipe, file.path(stage, "recipe.json"))
  if (!is.null(value$trace)) desktop_atomic(value$trace, file.path(stage, "trace.json"))
  # Keep role attribution with the immutable result. Legacy/unverifiable records stay explicitly unknown.
  if (identical(recipe$kind, "nca") && length(value$specification$group_roles %||% list())) {
    execution <- provenance$execution %||% list()
    audit <- (engine$nca %||% list())$audit %||% list()
    found <- Filter(function(event) identical(event$action, "configure") && identical(event$source, engine$nca$source) && !is.null(execution$revision) &&
      as.numeric(event$revision) <= as.numeric(execution$revision) &&
      identical(desktop_hash(event$specification), desktop_hash(value$specification)), audit)
    origin <- if (length(found)) {
      event <- tail(found, 1L)[[1]]
      list(status = "matched", actor = event$actor, channel = event$channel, at = event$at,
        identity = event$identity, revision = event$revision)
    } else list(status = "unavailable")
    desktop_atomic(list(format = "pk-nca-role-provenance-1", roles_hash = desktop_hash(value$specification$group_roles),
      roles = value$specification$group_roles, origin = origin), file.path(stage, "role_provenance.json"))
  }
  if (identical(recipe$kind, "nca_annotation") && !is.null(provenance$source_ids$result_source)) {
    origin_path <- desktop_table_path(engine, provenance$source_ids$result_source)
    origin_manifest <- desktop_read(file.path(origin_path, "manifest.json"))
    if ("role_provenance.json" %in% names(origin_manifest$files)) {
      desktop_verify_files(origin_path, origin_manifest$files)
      inherited <- desktop_read(file.path(origin_path, "role_provenance.json"))
      if (identical(inherited$roles_hash, desktop_hash(inputs$result$specification$group_roles %||% list())))
        desktop_atomic(inherited, file.path(stage, "role_provenance.json"))
    }
  }
  if (identical(recipe$kind, "nca")) {
    pk_write_csv(value$trace$rows, file.path(stage, "row_handling.csv"))
  }
  writeLines(c("# source('/path/to/source/apps/pk-desktop/pkapp.R') first",
    "args <- commandArgs(trailingOnly = TRUE)", "result <- pkapp_replay(args[[1]])", "print(result$counts)"), file.path(stage, "replay.R"))
  writeLines(capture.output(sessionInfo()), file.path(stage, "session_info.txt"))
  files <- list.files(stage)
  Sys.chmod(file.path(stage, files), "0600")
  manifest <- list(id = id, name = name, kind = recipe$kind, created_at = desktop_time(), channel = channel,
    actor = actor, counts = as.list(value$counts), code_hashes = desktop_code_hashes,
    files = stats::setNames(lapply(file.path(stage, files), pk_hash), files))
  if (!is.null(provenance)) manifest$provenance <- provenance
  desktop_atomic(manifest, file.path(stage, "manifest.json"))
  pk_assert(file.rename(stage, desktop_table_path(engine, id)), "保存表を確定できません。")
  engine$tables_signal(shiny::isolate(engine$tables_signal()) + 1L)
  manifest
}
desktop_recipe_run <- function(inputs, recipe) {
  if (identical(recipe$kind, "nca_covariates")) return(pkapp_prepare_nca_covariates(inputs$source, inputs$dm, inputs$vs, recipe$specification))
  if (identical(recipe$kind, "nca_annotation")) return(pkapp_annotate_nca(inputs$result, inputs$dm, inputs$vs, recipe$specification))
  if (identical(recipe$kind, "nca_assembly")) return(pkapp_assemble_nca(inputs$conc, inputs$pc, inputs$ex, recipe$specification))
  if (identical(recipe$kind, "nca")) return(pkapp_nca(inputs$source, recipe$specification))
  if (identical(recipe$kind, "prepare")) return(pkapp_prepare(inputs$source, recipe$steps))
  pk_assert(identical(recipe$kind, "join"), "未対応の処理記録です。")
  left <- inputs$left
  right <- inputs$right
  # New workspace recipes keep the complete preparation and parameter selection.
  # Legacy join recipes continue to replay their already prepared snapshots.
  preparation_trace <- NULL
  if (!is.null(inputs$left_source)) {
    prepared <- pkapp_prepare(inputs$left_source, recipe$left_steps %||% list())
    left <- prepared$data
    preparation_trace <- prepared$trace
  }
  if (!is.null(inputs$right_source)) {
    right <- inputs$right_source
    if (!is.null(recipe$right_param) && !identical(recipe$right_param, "__ALL__")) {
      pk_assert("PARAMCD" %in% names(right), "保存条件のPARAMCD列がありません。")
      right <- right[!is.na(right$PARAMCD) & right$PARAMCD == recipe$right_param, , drop = FALSE]
    }
  }
  by <- unlist(recipe$by)
  if (isTRUE(recipe$left_csv_keys)) left <- desktop_merge_csv_keys(left, right, by)
  if (isTRUE(recipe$right_csv_keys)) right <- desktop_merge_csv_keys(right, left, by)
  result <- pkapp_merge(left, right, by, unlist(recipe$columns), recipe$join, recipe$right_name)
  if (!is.null(preparation_trace)) result$trace <- list(left_preparation = preparation_trace)
  result
}
pkapp_replay <- function(folder) {
  manifest <- desktop_read(file.path(folder, "manifest.json"))
  desktop_verify_files(folder, manifest$files)
  pk_assert(identical(manifest$code_hashes, desktop_code_hashes), "コードの版が異なります。保存時と同じ配布版で再実行してください。")
  result <- desktop_recipe_run(readRDS(file.path(folder, "inputs.rds")), desktop_read(file.path(folder, "recipe.json")))
  original <- readRDS(file.path(folder, "result.rds"))
  pk_assert(identical(result$data, original), "再実行結果が保存結果と一致しません。")
  result
}
desktop_prepare_ui <- function(id) {
  ns <- shiny::NS(id)
  desktop_ui_page("データ前処理", "データを選び、条件を追加して結果を確認します。保存した表はデータ結合で使えます。",
    controls = shiny::tagList(
      desktop_ui_section("1", "データを選ぶ",
        shiny::selectInput(ns("source"), "入力データ", c("ADNCA", "ADPP", "ファイル" = "FILE"), width = "100%"),
        shiny::conditionalPanel("input.source === 'FILE'", ns = ns,
          shiny::fileInput(ns("file"), "読み込むファイル", accept = c(".csv", ".xpt", ".sas7bdat", ".rds", ".parquet"),
            buttonLabel = desktop_i18n_text("ファイルを選択", "Choose file"), placeholder = "選択されていません", width = "100%"),
          shiny::p(class = "pk-caption", "CSV / XPT / SAS7BDAT / RDS / Parquet に対応しています。")),
        shiny::p(class = "pk-caption", "入力データを変えると、追加済みの条件はリセットされます。")),
      desktop_ui_section("2", "加工条件を追加する",
        shiny::selectInput(ns("op"), "操作", c("行の絞り込み" = "filter", "列の選択" = "select", "列の改名" = "rename", "型の変換" = "convert", "採血・投与日時から経過時間" = "elapsed_time", "投与前後のBLQ規則" = "blq_rules"), width = "100%"),
        shiny::conditionalPanel("['filter','rename','convert'].indexOf(input.op) >= 0", ns = ns,
          shiny::selectInput(ns("column"), "対象列", character(), width = "100%")),
        shiny::conditionalPanel("input.op === 'filter'", ns = ns,
          shiny::selectInput(ns("operator"), "比較", c("等しい" = "eq", "等しくない" = "ne", "以上（数値）" = "ge", "以下（数値）" = "le", "欠測" = "missing", "非欠測" = "present"), width = "100%"),
          shiny::conditionalPanel("input.operator !== 'missing' && input.operator !== 'present'", ns = ns,
            shiny::textInput(ns("value"), "比較値", width = "100%"),
            shiny::p(class = "pk-caption", "IDなどの先頭ゼロは、そのまま保持します。"))),
        shiny::conditionalPanel("input.op === 'select'", ns = ns,
          shiny::selectizeInput(ns("columns"), "残す列", character(), multiple = TRUE, width = "100%")),
        shiny::conditionalPanel("input.op === 'rename'", ns = ns,
          shiny::textInput(ns("new_name"), "新しい列名", width = "100%")),
        shiny::conditionalPanel("input.op === 'convert'", ns = ns,
          shiny::selectInput(ns("type"), "変換先", c("文字" = "character", "数値" = "numeric"), width = "100%")),
        desktop_pk_prepare_controls(ns),
        shiny::p(class = "pk-caption", "入力した条件は「条件を追加」で反映されます。"),
        shiny::div(class = "pk-actions", shiny::uiOutput(ns("add_action")), shiny::uiOutput(ns("reset_action"))))),
    body = shiny::tagList(shiny::uiOutput(ns("notice_ui")), shiny::uiOutput(ns("result_panel")),
      desktop_ui_section("3", "前処理表を保存する",
        shiny::textInput(ns("name"), "保存する表の名前", "前処理済みデータ", width = "100%"),
        shiny::uiOutput(ns("publish_action")),
        shiny::p(class = "pk-caption", "現在の結果と処理条件を保存します。元データは変更しません。"),
        shiny::uiOutput(ns("save_status")))))
}

desktop_prepare_step_label <- function(step) {
  value <- paste(unlist(step$values, use.names = FALSE), collapse = "、")
  switch(step$op,
    elapsed_time = paste0(step$output %||% "NCA_TIME_H", " = 採血日時（", step$sample_column, "）−投与日時（", step$dose_column, "）［h］ / UTC", step$utc_offset %||% "+09:00"),
    blq_rules = {
      rules <- if (identical(step$mode, "positional")) c("pre", "first", "middle", "last", "all_blq") else c("pre", "post")
      paste0("BLQ：", paste(vapply(rules, function(position) desktop_rule_label(paste0(position, "_",
        step[[paste0(position, "_rule")]] %||% step$first_rule %||% "")), character(1)), collapse = " / "))
    },
    where = "詳細フィルタ（条件の詳細に記録）",
    sort = paste("並べ替え：", paste(unlist(step$columns), collapse = "・"), if (isTRUE(step$decreasing)) "降順" else "昇順"),
    distinct = paste("重複除去：", paste(unlist(step$columns), collapse = "・")),
    filter = switch(step$operator,
      eq = paste0(step$column, " が「", value, "」の行を残す"),
      ne = paste0(step$column, " が「", value, "」以外の行を残す"),
      `in` = paste0(step$column, " が「", value, "」のいずれかに一致する行を残す"),
      ge = paste0(step$column, " が ", value, " 以上の行を残す"),
      le = paste0(step$column, " が ", value, " 以下の行を残す"),
      gt = paste0(step$column, " が ", value, " より大きい行を残す"),
      lt = paste0(step$column, " が ", value, " より小さい行を残す"),
      missing = paste0(step$column, " が欠測の行を残す"),
      present = paste0(step$column, " が欠測でない行を残す")),
    select = paste("残す列：", paste(unlist(step$columns, use.names = FALSE), collapse = "、")),
    rename = paste0("列名を変更：", step$column, " → ", step$name),
    convert = paste0(step$column, " を", if (identical(step$type, "numeric")) "数値型" else "文字型", "に変換"))
}

desktop_prepare_server <- function(id, engine) shiny::moduleServer(id, function(input, output, session) {
  original <- shiny::reactiveVal(NULL)
  steps <- shiny::reactiveVal(list())
  notice <- shiny::reactiveVal("")
  notice_tone <- shiny::reactiveVal("info")
  phase <- shiny::reactiveVal("empty")
  valid_input <- shiny::reactiveVal(FALSE)
  saved_state <- shiny::reactiveVal(NULL)
  say <- function(message, tone = "info") { notice(message); notice_tone(tone) }
  shiny::observeEvent(list(input$source, input$file, engine$data_signal()), {
    valid_input(FALSE); original(NULL); steps(list()); saved_state(NULL)
    phase("empty"); say("")
    shiny::updateSelectInput(session, "column", choices = character())
    shiny::updateSelectizeInput(session, "columns", choices = character(), selected = character())
    if (is.null(input$source) || (identical(input$source, "FILE") && is.null(input$file))) return(invisible(NULL))
    phase("loading")
    tryCatch({
      value <- if (input$source == "FILE") {
        pkapp_read_table(input$file$datapath, tolower(tools::file_ext(input$file$name)))
      } else engine$data[[input$source]]
      pkapp_table_check(value)
      original(value); valid_input(TRUE); phase("ready")
      say("データを読み込みました。加工条件を追加するか、そのまま保存できます。")
    }, error = function(e) {
      phase("error")
      say(paste("データを読み込めませんでした。", conditionMessage(e)), "error")
    })
  }, ignoreNULL = FALSE, priority = 100)
  result <- shiny::reactive({ shiny::req(valid_input(), original()); pkapp_prepare(original(), steps()) })
  shiny::observeEvent(names(result()$data), {
    choices <- names(result()$data)
    elapsed <- Filter(function(x) identical(x$op, "elapsed_time"), steps())
    preferred <- if (length(elapsed)) tail(elapsed, 1L)[[1]]$output %||% "NCA_TIME_H" else NULL
    desktop_pk_prepare_update_columns(session, input, choices, data = result()$data, preferred_time = preferred)
    shiny::updateSelectInput(session, "column", choices = choices)
    shiny::updateSelectizeInput(session, "columns", choices = choices, selected = choices)
  })
  shiny::observeEvent(input$add, {
    if (!isTRUE(valid_input())) return(invisible(NULL))
    tryCatch({
      shiny::req(valid_input())
      step <- switch(input$op,
        filter = list(op = "filter", column = input$column, operator = input$operator, values = list(input$value)),
        select = list(op = "select", columns = as.list(input$columns)),
        rename = list(op = "rename", column = input$column, name = input$new_name),
        convert = list(op = "convert", column = input$column, type = input$type), desktop_pk_prepare_form(input))
      candidate <- c(steps(), list(step))
      pkapp_prepare(original(), candidate)
      steps(candidate); saved_state(NULL)
      say("条件を追加しました。結果を確認し、必要なら前処理表を保存してください。")
    }, error = function(e) say(paste("条件を追加できませんでした。表示中の結果は変更していません。", conditionMessage(e)), "error"))
  })
  shiny::observeEvent(input$reset, {
    if (!isTRUE(valid_input())) return(invisible(NULL))
    steps(list()); saved_state(NULL)
    say("条件をリセットしました。入力データをそのまま表示しています。")
  })
  shiny::observeEvent(input$publish, {
    if (!isTRUE(valid_input())) return(invisible(NULL))
    tryCatch({
      shiny::req(valid_input())
      saved <- desktop_table_save(engine, input$name, list(source = original()), list(kind = "prepare", steps = steps()))
      saved_state(saved)
      say(paste0("「", saved$name, "」を保存しました。上部の Module → データ結合を開き、左表・右表でこの名前を選べます。"), "success")
    }, error = function(e) say(paste("保存できませんでした。", conditionMessage(e)), "error"))
  })
  shiny::observeEvent(input$name, {
    saved <- saved_state()
    if (is.null(saved)) return(invisible(NULL))
    if (!identical(input$name, saved$name)) {
      say("保存名を変更しました。現在の名前では未保存です。")
    } else {
      say(paste0("「", saved$name, "」は保存済みです。データ結合の左表・右表で選べます。"), "success")
    }
  }, ignoreInit = TRUE)
  action_button <- function(id, label, enabled, class = NULL) {
    button <- shiny::actionButton(session$ns(id), label, class = class)
    if (!isTRUE(enabled)) button$attribs$disabled <- "disabled"
    button
  }
  output$add_action <- shiny::renderUI(action_button("add", "条件を追加", valid_input()))
  output$reset_action <- shiny::renderUI(action_button("reset", "条件をリセット", valid_input() && length(steps()) > 0L))
  output$publish_action <- shiny::renderUI(action_button("publish", "前処理表を保存", valid_input(), "btn-primary"))
  output$save_status <- shiny::renderUI({
    saved <- saved_state()
    if (is.null(saved)) return(shiny::p(class = "pk-caption", if (valid_input()) "現在の結果は未保存です。" else "データを読み込むと保存できます。"))
    current <- identical(input$name, saved$name)
    shiny::tagList(shiny::p(class = "pk-caption", if (current) paste0("保存済み：", saved$name) else
      paste0("現在の名前では未保存です。直前の保存名：", saved$name)),
      shiny::tags$details(shiny::tags$summary(if (current) "保存記録のID" else "直前の保存記録のID"), shiny::tags$code(saved$id)))
  })
  output$result_panel <- shiny::renderUI({
    if (!valid_input()) {
      if (identical(phase(), "loading")) return(desktop_ui_empty("データを読み込んでいます", "読込が終わると、件数とプレビューを表示します。"))
      if (identical(phase(), "error")) return(desktop_ui_empty("ファイルを確認してください", "上のメッセージを確認し、入力データを選び直してください。"))
      return(desktop_ui_empty("データを選択してください", "左の「データを選ぶ」で ADNCA・ADPP を選ぶか、ファイルを読み込んでください。"))
    }
    shiny::div(class = "pk-stack", shiny::uiOutput(session$ns("metrics")),
      shiny::div(class = "pk-card", shiny::h3("適用済みの条件"), shiny::uiOutput(session$ns("condition_list")),
        shiny::tags$details(shiny::tags$summary("処理条件の詳細（JSON）"), shiny::verbatimTextOutput(session$ns("steps")))),
      shiny::div(class = "pk-card", shiny::h3("前処理結果のプレビュー"),
        shiny::p(class = "pk-caption", "先頭30行を表示します。保存時は全行・全列が対象です。数値の表示は小数点以下6桁です。"),
        shiny::textOutput(session$ns("counts")),
        shiny::uiOutput(session$ns("zero_result")),
        shiny::div(class = "pk-table-scroll", tabindex = "0", role = "region", `aria-label` = "前処理結果の表。横・縦にスクロールできます。", shiny::tableOutput(session$ns("preview")))))
  })
  output$metrics <- shiny::renderUI({
    value <- result()
    desktop_ui_metrics(c("入力（行）" = nrow(original()), "前処理後（行）" = nrow(value$data), "結果の列数" = ncol(value$data)))
  })
  output$condition_list <- shiny::renderUI({
    value <- result()
    if (!length(steps())) return(shiny::p(class = "pk-caption", "条件はまだありません。入力データをそのまま表示しています。"))
    shiny::tags$ol(class = "pk-condition-list", lapply(seq_along(steps()), function(i) {
      entry <- value$trace[[i]]
      shiny::tags$li(shiny::div(desktop_prepare_step_label(steps()[[i]])),
        shiny::p(class = "pk-caption", sprintf("%d 行 → %d 行", entry$before, entry$after)))
    }))
  })
  output$counts <- shiny::renderText(sprintf("入力 %d 行 → 前処理後 %d 行 / %d 列", nrow(original()), nrow(result()$data), ncol(result()$data)))
  output$zero_result <- shiny::renderUI({
    if (nrow(result()$data) == 0L) desktop_ui_notice("条件に一致する行はありません。条件をリセットするか、入力データを選び直してください。0行の結果も保存できます。", "warning")
  })
  output$steps <- shiny::renderText(desktop_json(steps()))
  output$preview <- shiny::renderTable(head(result()$data, 30), digits = 6, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)
  output$notice <- shiny::renderText(notice())
  output$notice_ui <- shiny::renderUI({ if (nzchar(notice())) desktop_ui_notice(notice(), notice_tone()) })
})
