# A small data-only predicate language shared by datamods, MCP and replay.
# Neither generated R text nor a client expression is evaluated here.
desktop_predicate <- function(node, data, depth = 0L) {
  pk_assert(is.list(node) && depth < 30L, "絞り込み条件が不正または複雑すぎます。")
  if (identical(names(node), "column")) {
    pk_assert(is.character(node$column) && length(node$column) == 1L && node$column %in% names(data), "絞り込み対象列がありません。")
    return(data[[node$column]])
  }
  if (identical(names(node), "value")) {
    value <- unlist(node$value, use.names = FALSE)
    pk_assert(is.null(value) || (is.atomic(value) && !anyNA(value) && length(value) <= 10000L), "比較値が不正です。")
    return(value)
  }
  pk_assert(setequal(names(node), c("op", "args")) && is.character(node$op) && length(node$op) == 1L &&
    node$op %in% c("&", "|", "!", "%in%", ">=", "<=", ">", "<", "==", "!=", "is.na"), "未対応の絞り込み式です。")
  pk_assert(is.list(node$args) && length(node$args) == if (node$op %in% c("!", "is.na")) 1L else 2L, "比較条件の引数が不正です。")
  args <- lapply(node$args, desktop_predicate, data = data, depth = depth + 1L)
  a <- args[[1]]; b <- if (length(args) == 2L) args[[2]] else NULL
  if (!node$op %in% c("%in%", "!", "is.na")) pk_assert(length(a) == length(b) || length(a) == 1L || length(b) == 1L, "比較値の長さが一致しません。")
  switch(node$op, `&` = a & b, `|` = a | b, `!` = !a, `%in%` = a %in% b,
    `>=` = a >= b, `<=` = a <= b, `>` = a > b, `<` = a < b, `==` = a == b, `!=` = a != b, is.na = is.na(a))
}
desktop_predicate_from_expr <- function(expr) {
  if (is.null(expr)) return(list(value = TRUE))
  if (is.symbol(expr)) return(list(column = as.character(expr)))
  if (is.atomic(expr)) return(list(value = as.list(expr)))
  pk_assert(is.call(expr) && is.symbol(expr[[1]]), "対応できないdatamods条件です。")
  op <- as.character(expr[[1]])
  if (op == "(") return(desktop_predicate_from_expr(expr[[2]]))
  list(op = op, args = lapply(as.list(expr)[-1], desktop_predicate_from_expr))
}
desktop_handling_ui <- function(id) {
  ns <- shiny::NS(id)
  desktop_ui_page("複数条件でデータを絞り込む", "値の選択や数値範囲で絞り込み、結果を作業テーブルへ保存できます。保存後は「データ準備」の結合や解析用データの設定で選べます。",
    controls = shiny::tagList(
      shiny::selectInput(ns("source"), "入力データ", c("ADNCA", "ADPP", "ファイル" = "FILE")),
      shiny::conditionalPanel("input.source === 'FILE'", ns = ns,
        shiny::fileInput(ns("file"), "CSV / XPT / SAS7BDAT / RDS / Parquet", accept = c(".csv", ".xpt", ".sas7bdat", ".rds", ".parquet"))),
      shiny::selectizeInput(ns("vars"), "絞り込む列（複数可）", character(), multiple = TRUE),
      if (requireNamespace("datamods", quietly = TRUE)) datamods::filter_data_ui(ns("filters"), max_height = "55vh") else
        desktop_ui_notice("datamodsが未導入です。install.Rを実行して再起動してください。", "error")),
    body = shiny::tagList(
      shiny::textOutput(ns("notice")), shiny::uiOutput(ns("counts")),
      shiny::div(class = "pk-table-scroll", shiny::tableOutput(ns("preview"))),
      shiny::textInput(ns("name"), "作業テーブルの保存名", "詳細フィルタ済みデータ"),
      shiny::actionButton(ns("save"), "結果と条件を保存", class = "btn-primary"),
      shiny::p(class = "pk-caption", "プレビューは先頭30行です。保存には全行・全列を使います。元データと解析条件は変わりません。"),
      shiny::tags$details(shiny::tags$summary("再実行用のRコード（datamods）"), shiny::verbatimTextOutput(ns("code")))))
}
desktop_handling_server <- function(id, engine) shiny::moduleServer(id, function(input, output, session) {
  if (!requireNamespace("datamods", quietly = TRUE)) return(invisible(NULL))
  notice <- shiny::reactiveVal("")
  output$notice <- shiny::renderText(notice())
  shiny::observeEvent(engine$tables_signal(), {
    tables <- desktop_tables(engine)
    choices <- c("ADNCA", "ADPP", "ファイル" = "FILE", stats::setNames(vapply(tables, `[[`, "", "id"), vapply(tables, `[[`, "", "name")))
    shiny::updateSelectInput(session, "source", choices = choices, selected = shiny::isolate(input$source) %||% "ADNCA")
  }, ignoreNULL = FALSE)
  data <- shiny::reactive({
    engine$data_signal(); shiny::req(input$source)
    if (input$source == "FILE") {
      shiny::req(input$file)
      pkapp_read_table(input$file$datapath, tolower(tools::file_ext(input$file$name)))
    } else desktop_source(engine, input$source)
  })
  shiny::observeEvent(data(), {
    shiny::updateSelectizeInput(session, "vars", choices = names(data()), selected = intersect(c("SEX", "ARM", "PARAMCD"), names(data())))
    notice("入力を変更しました。絞り込み条件を確認してください。")
  })
  filters <- datamods::filter_data_server("filters", data = data, vars = shiny::reactive(input$vars %||% character()),
    name = shiny::reactive("source"), drop_ids = FALSE, widget_char = "select", widget_num = "range", label_na = "欠測を含む")
  prepared <- shiny::reactive({
    filtered <- filters$filtered()
    predicate <- desktop_predicate_from_expr(filters$expr())
    steps <- list(list(op = "where", predicate = predicate))
    result <- pkapp_prepare(data(), steps)
    # Detect package behavior changes or unsupported classes rather than saving a divergent recipe.
    rownames(filtered) <- NULL
    pk_assert(identical(result$data, filtered), "絞り込みの再現性を確認できません。条件を見直してください。")
    list(result = result, steps = steps)
  })
  output$counts <- shiny::renderUI(shiny::p(paste(nrow(data()), "行 →", nrow(prepared()$result$data), "行")))
  output$preview <- shiny::renderTable(utils::head(prepared()$result$data, 30L), striped = TRUE, digits = 6)
  output$code <- shiny::renderText({ prepared(); paste(deparse(filters$code(), width.cutoff = 100L), collapse = "\n") })
  shiny::observeEvent(input$save, tryCatch({
    value <- prepared()
    saved <- desktop_table_save(engine, input$name, list(source = data()), list(kind = "prepare", steps = value$steps), "ui", "local user")
    notice(paste("保存しました：", saved$name, "（", saved$counts$result, "行）。データ準備の一覧から選べます。"))
  }, error = function(e) notice(conditionMessage(e))))
  invisible(list(data = data, prepared = prepared, filters = filters))
})

desktop_import_table_api <- function(engine, args) {
  desktop_revision(engine, args$base_revision, args$project_id)
  path <- normalizePath(args$path, mustWork = TRUE)
  desktop_check(startsWith(path, paste0(engine$project, .Platform$file.sep)) && !dir.exists(path), "E_PATH_DENIED", "Place the input file inside the active project")
  desktop_table_save(engine, args$name, list(source = pkapp_read_table(path)), list(kind = "prepare", steps = list(),
    origin = list(format = tools::file_ext(path), sha256 = pk_hash(path))), "mcp", args$actor,
    provenance = if (identical(engine$mode, "nca")) list(identity = desktop_nca_identity_metadata(args, "mcp")) else NULL)
}
desktop_validate_tables_api <- function(engine, args) {
  desktop_revision(engine, args$base_revision, args$project_id)
  folder <- tempfile(".mapping-", engine$project); dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  for (name in c("ADNCA", "ADPP")) saveRDS(desktop_source(engine, args[[tolower(name)]]), file.path(folder, paste0(name, ".rds")))
  bundle <- pkapp_import_bundle(file.path(folder, "ADNCA.rds"), file.path(folder, "ADPP.rds"), file.path(folder, "bundle"), args$mapping, args$time_unit)
  info <- pk_inspect(bundle)
  id <- paste0("validation-", substr(desktop_hash(list(pk_load_bundle(bundle)$hashes, engine$spec$revision)), 1, 24))
  destination <- file.path(engine$project, "validated", id)
  if (!dir.exists(destination)) {
    dir.create(dirname(destination), showWarnings = FALSE)
    pk_assert(file.rename(bundle, destination), "検証済みデータを保存できません。")
  }
  receipt <- list(id = id, project_id = engine$spec$project_id, base_revision = engine$spec$revision,
    sources = list(ADNCA = args$adnca, ADPP = args$adpp), mapping = args$mapping, time_unit = args$time_unit,
    hashes = pk_load_bundle(destination)$hashes)
  desktop_atomic(receipt, file.path(destination, "validation.json"))
  list(validation_id = id, rows = stats::setNames(lapply(c("ADNCA", "ADPP"), function(n) nrow(pk_load_bundle(destination)[[n]])), c("ADNCA", "ADPP")),
    n_subjects = info$n_subjects, parameters = as.list(info$eligible_parameters), effect = "Apply replaces analysis inputs and resets view conditions; existing Runs remain.")
}
desktop_apply_tables_api <- function(engine, args) {
  desktop_revision(engine, args$base_revision, args$project_id)
  desktop_check(is.character(args$validation_id) && length(args$validation_id) == 1L && grepl("^validation-[a-f0-9]{24}$", args$validation_id), "E_VALIDATION", "Invalid validation ID")
  folder <- file.path(engine$project, "validated", args$validation_id)
  receipt <- desktop_read(file.path(folder, "validation.json"))
  desktop_revision(engine, receipt$base_revision, receipt$project_id)
  desktop_verify_files(folder, receipt$hashes)
  desktop_load_bundle(engine, folder, args$base_revision, args$project_id, "mcp", args$actor, args$reason)
}
