# Validated module definitions, never arbitrary R supplied through MCP.
desktop_module_private <- function(path, directory = FALSE) {
  info <- file.info(path)
  desktop_check(nrow(info) == 1L && !is.na(info$isdir) && identical(info$isdir, directory) &&
    identical(Sys.readlink(path), "") && !is.na(info$uname) && identical(info$uname, unname(Sys.info()[["user"]])),
    "E_VALIDATION", "Exploration storage must be an owned, non-symlink path.")
  mode <- if (directory) "0700" else "0600"
  desktop_check(isTRUE(Sys.chmod(path, mode, use_umask = FALSE)) && bitwAnd(as.integer(file.info(path)$mode), 63L) == 0L,
    "E_IO", "Cannot secure exploration storage permissions.")
  invisible(path)
}
desktop_modules_init <- function(engine) {
  engine$modules_dir <- file.path(engine$project, "modules")
  dir.create(engine$modules_dir, showWarnings = FALSE, mode = "0700")
  desktop_module_private(engine$modules_dir, directory = TRUE)
  path <- file.path(engine$modules_dir, "registry.json")
  is_link <- function(x) { target <- Sys.readlink(x); length(target) == 1L && !is.na(target) && nzchar(target) }
  if (file.exists(path) || is_link(path)) desktop_module_private(path)
  engine$modules <- if (file.exists(path)) desktop_read(path) else list(revision = 0L, entries = list())
  # Older releases may have written 0644 snapshots. Secure only owned, expected paths.
  paths <- list.files(engine$modules_dir, pattern = "^module-[a-f0-9]{24}$", full.names = TRUE)
  for (folder in paths) {
    desktop_module_private(folder, directory = TRUE)
    for (name in c("data.rds", "definition.json")) {
      file <- file.path(folder, name)
      if (file.exists(file) || is_link(file)) desktop_module_private(file)
    }
  }
  engine$modules_signal <- shiny::reactiveVal(engine$modules$revision)
}
desktop_module_catalog <- function() list(
  list(module = "pk_scatterplot", label = "散布図", package = "PK Desktop", available = TRUE,
    options = list(x = "numeric column (required)", y = "numeric column (required)", color = "column (optional)")),
  list(module = "pk_distribution", label = "分布図", package = "PK Desktop", available = TRUE,
    options = list(x = "numeric column (required)", bins = "integer 5..100 (default 30)")),
  list(module = "pk_data_table", label = "データ表", package = "PK Desktop", available = TRUE, options = list()),
  list(module = "tm_data_table", label = "teal 汎用データ表", package = "teal.modules.general",
    available = requireNamespace("teal.modules.general", quietly = TRUE), options = list()))
desktop_modules_list <- function(engine) {
  catalog <- desktop_module_catalog()
  if (identical(engine$mode, "nca") && !(requireNamespace("teal", quietly = TRUE) && requireNamespace("teal.data", quietly = TRUE)))
    catalog <- lapply(catalog, function(x) { x$available <- FALSE; x })
  list(revision = engine$modules$revision, catalog = catalog, entries = engine$modules$entries)
}
desktop_module_check <- function(module, data, options) {
  pkapp_table_check(data)
  catalog <- desktop_module_catalog()
  index <- match(module, vapply(catalog, `[[`, "", "module"))
  pk_assert(length(index) == 1L && !is.na(index), "未対応のモジュールです。対応一覧から選んでください。")
  pk_assert(isTRUE(catalog[[index]]$available), "このモジュールのパッケージをinstall.R --with-modulesで導入して再起動してください。")
  pk_assert(is.list(options) && (length(options) == 0L || (!is.null(names(options)) && !anyDuplicated(names(options)))), "モジュール設定が不正です。")
  allowed <- switch(module, pk_scatterplot = c("x", "y", "color"), pk_distribution = c("x", "bins"), character())
  pk_assert(all(names(options) %in% allowed), "未対応の設定項目です。Rコードは指定できません。")
  for (name in intersect(c("x", "y", "color"), allowed)) {
    value <- options[[name]]
    if (name == "color" && is.null(value)) next
    pk_assert(is.character(value) && length(value) == 1L && value %in% names(data), paste("列を選んでください：", name))
    if (name != "color") pk_assert(is.numeric(data[[value]]) && any(is.finite(data[[value]])), "軸には有限の数値を含む列を指定してください。")
  }
  # AVAL is a long-form value shared by PK parameters and units. Never plot it
  # across different parameters or units in the generic chart modules.
  axes <- unlist(options[intersect(names(options), c("x", "y"))], use.names = FALSE)
  if (module %in% c("pk_scatterplot", "pk_distribution") && "AVAL" %in% axes &&
      any(c("PARAMCD", "AVALU") %in% names(data))) {
    pk_assert(all(c("PARAMCD", "AVALU") %in% names(data)), "AVALの図にはパラメータと単位の両列が必要です。")
    keep <- is.finite(data$AVAL)
    p <- as.character(data$PARAMCD[keep]); u <- as.character(data$AVALU[keep])
    pk_assert(length(p) > 0L && !anyNA(p) && !anyNA(u) && all(nzchar(trimws(p))) && all(nzchar(trimws(u))) &&
      length(unique(p)) == 1L && length(unique(u)) == 1L,
      "AVALの図は、1パラメータ・1単位に絞り込んだ表が必要です。")
    if ("PARAM_STATUS" %in% names(data))
      pk_assert(all(!is.na(data$PARAM_STATUS[keep]) & data$PARAM_STATUS[keep] == "calculated"),
        "方針割当値などは通常の計算値と分けて確認してください。")
    if ("NCA_WARNING" %in% names(data))
      pk_assert(all(is.na(data$NCA_WARNING[keep]) | !nzchar(data$NCA_WARNING[keep])),
        "警告付きの値は「結果の探索」で状態を確認してください。")
  }
  if (module == "pk_distribution") pk_assert(is.null(options$bins) || (is.numeric(options$bins) && length(options$bins) == 1 &&
    is.finite(options$bins) && options$bins == floor(options$bins) && options$bins >= 5 && options$bins <= 100), "区間数は5〜100の整数です。")
  invisible(TRUE)
}
desktop_module_build <- function(entry, data) {
  desktop_module_check(entry$module, data, entry$options)
  if (entry$module == "tm_data_table") return(teal.modules.general::tm_data_table(label = entry$label, datanames = "DATA"))
  teal::module(label = entry$label, datanames = NULL,
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(shiny::p(class = "pk-caption", "追加時の入力スナップショットによる探索表示です。PK解析のRun・Word帳票には含まれません。"),
        if (entry$module == "pk_data_table") desktop_grid_ui(ns("grid")) else shiny::plotOutput(ns("plot"), height = "440px"))
    }, server = function(id) shiny::moduleServer(id, function(input, output, session) {
      if (entry$module == "pk_data_table") desktop_grid_server("grid", shiny::reactive(data)) else {
        output$plot <- shiny::renderPlot({
          config <- entry$options
          if (entry$module == "pk_distribution") ggplot2::ggplot(data, ggplot2::aes(x = .data[[config$x]])) +
            ggplot2::geom_histogram(bins = config$bins %||% 30L, fill = "#176b79", color = "white") + ggplot2::theme_minimal() else {
            plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[config$x]], y = .data[[config$y]]))
            if (!is.null(config$color)) plot <- plot + ggplot2::aes(color = .data[[config$color]])
            plot + ggplot2::geom_point(alpha = .65) + ggplot2::theme_minimal()
          }
        }, res = 110)
      }
    }))
}
desktop_module_mutate <- function(engine, args, remove = FALSE, channel = "mcp") {
  desktop_revision(engine, args$base_revision, args$project_id)
  desktop_check(identical(as.numeric(args$modules_revision), as.numeric(engine$modules$revision)), "E_REVISION_CONFLICT", "Module list changed; read pk_list_modules again")
  for (value in list(args$actor, args$reason)) pk_assert(is.character(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(value)) && nchar(value) <= 500L, "操作元と変更理由が必要です。")
  state <- engine$modules
  if (remove) {
    index <- match(args$id, vapply(state$entries, `[[`, "", "id"))
    pk_assert(length(index) == 1L && !is.na(index) && isTRUE(state$entries[[index]]$enabled), "有効なモジュールを選んでください。")
    state$entries[[index]]$enabled <- FALSE
    state$entries[[index]]$removed <- list(at = desktop_time(), channel = channel, actor = args$actor, reason = args$reason)
    entry <- state$entries[[index]]
  } else {
    if (identical(engine$mode, "nca")) {
      desktop_nca_idle(engine)
      desktop_check(requireNamespace("teal", quietly = TRUE) && requireNamespace("teal.data", quietly = TRUE),
        "E_VALIDATION", "The optional teal and teal.data packages are required for exploratory modules.")
      desktop_check(is.character(args$source) && length(args$source) == 1L && grepl("^table-[a-f0-9]{24}$", args$source),
        "E_VALIDATION", "Select a saved work table.")
    }
    pk_assert(length(state$entries) < 100L && sum(vapply(state$entries, function(e) isTRUE(e$enabled), logical(1))) < 12L, "有効なモジュールは12個までです。不要なものを解除してください。")
    pk_assert(is.character(args$label) && length(args$label) == 1L && !is.na(args$label) && nzchar(trimws(args$label)) && nchar(args$label) <= 80L, "名前は1〜80文字で指定してください。")
    data <- desktop_source(engine, args$source)
    if (identical(engine$mode, "nca") && args$module %in% c("pk_scatterplot", "pk_distribution")) {
      axes <- unlist(args$options[intersect(names(args$options), c("x", "y"))], use.names = FALSE)
      # Generic prepared tables do not retain authenticated NCA identity metadata.
      # Use the dedicated result view, which preserves period/analyte strata.
      pk_markers <- intersect(c("PARAMCD", "PARAM", "AVALU", "PARAM_STATUS", "NCA_EXCLUDE"), names(data))
      desktop_check(length(pk_markers) < 2L && !("AVAL" %in% axes && length(pk_markers) > 0L),
        "E_VALIDATION", "For NCA values use Result Exploration, which keeps analysis strata separate.")
      if (any(c("CONC", "NCA_CONC") %in% axes) && "UNIT" %in% names(data)) {
        units <- trimws(as.character(data$UNIT))
        desktop_check(!anyNA(units) && all(nzchar(units)) && length(unique(units)) == 1L,
          "E_VALIDATION", "Concentration plots require a single recorded unit.")
      }
    }
    desktop_module_check(args$module, data, args$options)
    id <- paste0("module-", substr(desktop_hash(list(Sys.time(), tempfile())), 1, 24))
    entry <- list(id = id, module = args$module, source = args$source, label = args$label, options = args$options,
      enabled = TRUE, created_at = desktop_time(), channel = channel, actor = args$actor, reason = args$reason,
      input_rows = nrow(data), input_columns = ncol(data), app_version = desktop_version,
      packages = list(teal = as.character(utils::packageVersion("teal"))))
    desktop_module_build(entry, data) # Constructor validation before registration.
    folder <- file.path(engine$modules_dir, id)
    desktop_check(dir.create(folder, mode = "0700"), "E_IO", "Cannot create private module directory.")
    desktop_module_private(folder, directory = TRUE)
    published <- FALSE
    on.exit(if (!published) unlink(folder, recursive = TRUE), add = TRUE)
    saveRDS(data, file.path(folder, "data.rds"), version = 3)
    desktop_module_private(file.path(folder, "data.rds"))
    entry$input_sha256 <- pk_hash(file.path(folder, "data.rds"))
    desktop_atomic(entry, file.path(folder, "definition.json"))
    desktop_module_private(file.path(folder, "definition.json"))
    state$entries <- c(state$entries, list(entry))
  }
  state$revision <- state$revision + 1L
  desktop_atomic(state, file.path(engine$modules_dir, "registry.json"))
  if (!remove) published <- TRUE
  engine$modules <- state
  engine$modules_signal(state$revision)
  list(revision = state$revision, entry = entry)
}
desktop_extensions_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "pk-data-page pk-workspace pk-extensions",
    shiny::div(class = "pk-page-heading", shiny::div(shiny::h1("探索モジュール"),
      shiny::p("画面またはAIから追加した表示を確認できます。追加時のデータと設定を保存し、再起動後も一覧に残します。"))),
    shiny::div(class = "pk-card",
      shiny::selectInput(ns("selected"), desktop_i18n_text("開くモジュール", "Module to open"), character()),
      shiny::textOutput(ns("notice")),
      shiny::actionButton(ns("remove"), desktop_i18n_text("選択したモジュールを解除", "Remove selected module")),
      shiny::tags$details(shiny::tags$summary(desktop_i18n_text("モジュールを追加", "Add module")),
        shiny::selectInput(ns("kind"), desktop_i18n_text("種類", "Type"), c("散布図" = "pk_scatterplot", "分布図" = "pk_distribution", "データ表" = "pk_data_table", "teal 汎用データ表（追加パッケージ）" = "tm_data_table")),
        shiny::selectInput(ns("source"), desktop_i18n_text("入力データ", "Input data"), c("ADNCA", "ADPP")),
        shiny::uiOutput(ns("options")), shiny::textInput(ns("label"), desktop_i18n_text("表示名", "Display name"), "追加の解析"),
        shiny::actionButton(ns("add"), desktop_i18n_text("追加して開く", "Add and open"), class = "btn-primary"))),
    shiny::div(id = ns("hosts"), class = "pk-teal-host"))
}
desktop_extensions_server <- function(id, engine) shiny::moduleServer(id, function(input, output, session) {
  notice <- shiny::reactiveVal(if (identical(engine$mode, "nca") && !(requireNamespace("teal", quietly = TRUE) && requireNamespace("teal.data", quietly = TRUE)))
    "追加モジュールには teal と teal.data の導入が必要です。「結果の探索」は追加パッケージなしで使えます。" else
    "追加したモジュールがここに表示されます。AIも同じ一覧を使います。")
  output$notice <- shiny::renderText(notice())
  mounted <- character()
  shiny::observeEvent(engine$tables_signal(), {
    tables <- desktop_tables(engine)
    sources <- if (identical(engine$mode, "nca")) character() else c("ADNCA", "ADPP")
    sources <- c(sources, stats::setNames(vapply(tables, `[[`, "", "id"), vapply(tables, `[[`, "", "name")))
    selected <- shiny::isolate(input$source)
    if (is.null(selected) || !selected %in% sources) selected <- if (length(sources)) unname(sources[1]) else character()
    shiny::updateSelectInput(session, "source", choices = sources, selected = selected)
  }, ignoreNULL = FALSE)
  output$options <- shiny::renderUI({
    if (!identical(engine$mode, "nca")) engine$data_signal()
    shiny::req(input$source, input$kind)
    data <- desktop_source(engine, input$source)
    numeric <- names(data)[vapply(data, is.numeric, logical(1))]
    ns <- session$ns
    shiny::tagList(
      if (input$kind %in% c("pk_scatterplot", "pk_distribution")) shiny::selectInput(ns("x"), "X軸", numeric, selected = if ("TIME_H" %in% numeric) "TIME_H" else numeric[1]),
      if (input$kind == "pk_scatterplot") shiny::tagList(shiny::selectInput(ns("y"), "Y軸", numeric, selected = if ("AVAL" %in% numeric) "AVAL" else numeric[1]),
        shiny::selectInput(ns("color"), "色分け", c("なし" = "", names(data)))),
      if (input$kind == "pk_distribution") shiny::numericInput(ns("bins"), "区間数", 30, min = 5, max = 100))
  })
  scope <- function() list(base_revision = if (identical(engine$mode, "nca")) engine$nca$revision else engine$spec$revision,
    project_id = if (identical(engine$mode, "nca")) engine$nca$project_id else engine$spec$project_id,
    modules_revision = engine$modules$revision, actor = "local user", reason = "画面で探索モジュールを変更")
  shiny::observeEvent(input$add, tryCatch({
    options <- switch(input$kind, pk_scatterplot = c(list(x = input$x, y = input$y), if (nzchar(input$color %||% "")) list(color = input$color)),
      pk_distribution = list(x = input$x, bins = input$bins), list())
    desktop_module_mutate(engine, c(scope(), list(module = input$kind, source = input$source, label = input$label, options = options)), channel = "ui")
    notice("追加しました。追加時のデータを使用しています。")
  }, error = function(e) notice(conditionMessage(e))))
  shiny::observeEvent(input$remove, tryCatch({
    desktop_module_mutate(engine, c(scope(), list(id = input$selected)), remove = TRUE, channel = "ui")
    notice("表示を解除しました。追加・解除の記録とデータは保持しています。")
  }, error = function(e) notice(conditionMessage(e))))
  shiny::observeEvent(engine$modules_signal(), {
    active <- Filter(function(entry) isTRUE(entry$enabled), engine$modules$entries)
    ids <- vapply(active, `[[`, "", "id")
    added <- setdiff(ids, mounted)
    selected <- if (length(added)) tail(added, 1) else shiny::isolate(input$selected)
    if (!length(selected) || !selected %in% ids) selected <- if (length(ids)) ids[[1]] else character()
    shiny::updateSelectInput(session, "selected", choices = stats::setNames(ids, vapply(active, `[[`, "", "label")), selected = selected)
    for (entry in active) if (!entry$id %in% mounted) local({
      current <- entry
      tryCatch({
        file <- file.path(engine$modules_dir, current$id, "data.rds")
        desktop_check(identical(pk_hash(file), current$input_sha256), "E_INPUT_CHANGED", "追加モジュールのデータが変更されています。")
        data <- readRDS(file)
        module <- teal::modules(desktop_module_build(current, data))
        shiny::insertUI(paste0("#", session$ns("hosts")), where = "beforeEnd", ui = shiny::conditionalPanel(
          sprintf("input.selected === '%s'", current$id), ns = session$ns,
          teal::ui_teal(session$ns(current$id), module)))
        teal::srv_teal(current$id, data = teal.data::teal_data(DATA = data), modules = module,
          filter = teal::teal_slices(include_varnames = list(DATA = character()), allow_add = FALSE), reporter = NULL)
        mounted <<- c(mounted, current$id)
      }, error = function(e) notice(paste(current$label, conditionMessage(e))))
    })
  }, ignoreNULL = FALSE)
})
pkapp_modules <- function(project) desktop_modules_list(desktop_r_engine(project))
pkapp_module_app <- function(project, id) {
  engine <- desktop_r_engine(project)
  index <- match(id, vapply(engine$modules$entries, `[[`, "", "id"))
  pk_assert(length(index) == 1L && !is.na(index), "保存したモジュールIDを指定してください。")
  entry <- engine$modules$entries[[index]]
  file <- file.path(engine$modules_dir, id, "data.rds")
  desktop_check(identical(pk_hash(file), entry$input_sha256), "E_INPUT_CHANGED", "追加時のデータと一致しません。")
  data <- readRDS(file)
  teal::init(data = teal.data::teal_data(DATA = data), modules = teal::modules(desktop_module_build(entry, data)),
    filter = teal::teal_slices(include_varnames = list(DATA = character()), allow_add = FALSE))
}
pkapp_add_module <- function(project, module, source, label, options = list(), reason, actor = "R user") {
  engine <- desktop_r_engine(project)
  desktop_module_mutate(engine, list(module = module, source = source, label = label, options = options, reason = reason, actor = actor,
    base_revision = engine$spec$revision, project_id = engine$spec$project_id, modules_revision = engine$modules$revision), channel = "r")
}
