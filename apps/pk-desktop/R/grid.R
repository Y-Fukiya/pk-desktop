# Display-only pagination and diagnostics. Never alter or round the source table.
desktop_grid_columns <- function(data) {
  if (is.null(data)) return(data.frame())
  data.frame(
    "列名" = names(data),
    "型" = vapply(data, function(x) paste(class(x), collapse = "/"), character(1)),
    "欠測行数" = vapply(data, function(x) sum(is.na(x)), integer(1)),
    check.names = FALSE, stringsAsFactors = FALSE)
}

desktop_grid_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "pk-grid",
    shiny::uiOutput(ns("column_styles")),
    shiny::div(class = "pk-grid-toolbar pk-actions",
      shiny::selectInput(ns("search_column"), "表示検索の対象列", choices = character(), width = "190px"),
      shiny::textInput(ns("search"), "表示内を検索（部分一致）", placeholder = "被験者IDなどを入力", width = "240px"),
      shiny::tags$details(class = "pk-grid-column-picker",
        shiny::tags$summary(shiny::textOutput(ns("column_summary"), inline = TRUE)),
        shiny::div(class = "pk-grid-columns", style = "max-height:240px;overflow:auto;min-width:220px",
          shiny::checkboxGroupInput(ns("visible_columns"), "表示する列を1つ以上選択", choices = character())),
        shiny::p(class = "pk-caption", "この選択は画面表示だけに反映されます。"))),
    shiny::textOutput(ns("summary")),
    shiny::div(class = "pk-table-scroll", tabindex = "0", role = "region",
      `aria-label` = "データ表。横と縦にスクロールできます。", shiny::tableOutput(ns("table"))),
    shiny::div(class = "pk-grid-pagination pk-actions",
      shiny::selectInput(ns("page_size"), "1ページの行数", choices = c("30", "50", "100"), selected = "30", width = "120px"),
      shiny::uiOutput(ns("previous_action")), shiny::textOutput(ns("page_label"), inline = TRUE), shiny::uiOutput(ns("next_action"))),
    shiny::p(class = "pk-caption", "検索・列選択・ページ移動は表示のみです。数値は有効数字4桁で表示し、保存やCSV出力には元の全行・全列・精度を使います。"))
}

desktop_grid_server <- function(id, data, preferred = shiny::reactive(character()),
                                highlight = shiny::reactive(character())) {
  shiny::moduleServer(id, function(input, output, session) {
    source <- shiny::reactive({
      value <- data()
      if (is.null(value)) return(NULL)
      shiny::validate(shiny::need(is.data.frame(value), "表示する表を選択してください。"))
      value
    })
    page <- shiny::reactiveVal(1L)
    default_columns <- shiny::reactive({
      value <- source()
      if (is.null(value)) return(character())
      requested <- intersect(as.character(preferred()), names(value))
      if (length(requested)) requested else head(names(value), 8L)
    })
    previous_fields <- shiny::reactiveVal(character())
    shiny::observeEvent(list(names(source()), preferred()), {
      value <- source()
      fields <- if (is.null(value)) character() else names(value)
      selected <- intersect(shiny::isolate(input$visible_columns), fields)
      if (!length(selected)) selected <- default_columns()
      # Newly joined columns should be immediately discoverable. Retain the
      # existing display choices, adding only new columns explicitly preferred
      # by the caller; a row filter does not restore columns the user hid.
      newly_preferred <- intersect(as.character(preferred()), setdiff(fields, previous_fields()))
      selected <- unique(c(selected, newly_preferred))
      previous_fields(fields)
      candidates <- intersect(c("USUBJID", "SUBJID", "STUDYID"), fields)
      selected_search <- shiny::isolate(input$search_column)
      if (length(selected_search) != 1L || !selected_search %in% fields)
        selected_search <- if (length(candidates)) candidates[1] else head(fields, 1L)
      shiny::updateCheckboxGroupInput(session, "visible_columns", choices = fields, selected = selected)
      shiny::updateSelectInput(session, "search_column", choices = fields, selected = selected_search)
    }, ignoreNULL = FALSE)
    columns <- shiny::reactive({
      fields <- names(source())
      selected <- input$visible_columns
      if (is.null(selected)) return(default_columns())
      intersect(selected, fields)
    })
    search_column <- shiny::reactive({
      fields <- names(source())
      selected <- input$search_column
      if (length(selected) == 1L && selected %in% fields) return(selected)
      candidates <- intersect(c("USUBJID", "SUBJID", "STUDYID"), fields)
      if (length(candidates)) candidates[1] else head(fields, 1L)
    })
    query <- shiny::reactive({
      value <- input$search
      if (!is.character(value) || length(value) != 1L || is.na(value)) "" else value
    })
    filtered <- shiny::reactive({
      value <- source()
      if (is.null(value) || !nzchar(query())) return(value)
      field <- search_column()
      if (!length(field)) return(value[0, , drop = FALSE])
      text <- as.character(value[[field]])
      keep <- !is.na(text) & grepl(query(), text, fixed = TRUE)
      value[keep, , drop = FALSE]
    })
    page_size <- shiny::reactive({
      value <- suppressWarnings(as.integer(input$page_size))
      if (length(value) != 1L || is.na(value) || !value %in% c(30L, 50L, 100L)) 30L else value
    })
    pages <- shiny::reactive({
      count <- if (is.null(filtered())) 0L else nrow(filtered())
      max(1L, as.integer(ceiling(count / page_size())))
    })
    shiny::observeEvent(list(source(), query(), search_column(), page_size()), page(1L), ignoreNULL = FALSE, priority = 20)
    current_page <- shiny::reactive(min(max(1L, page()), pages()))
    shiny::observeEvent(input$previous, page(max(1L, current_page() - 1L)))
    shiny::observeEvent(input$next_page, page(min(pages(), current_page() + 1L)))
    row_indices <- shiny::reactive({
      value <- filtered()
      if (is.null(value) || nrow(value) == 0L) return(integer())
      first <- (current_page() - 1L) * page_size() + 1L
      seq.int(first, min(nrow(value), first + page_size() - 1L))
    })
    view <- shiny::reactive({
      value <- filtered()
      if (is.null(value)) return(NULL)
      value[row_indices(), columns(), drop = FALSE]
    })
    status <- shiny::reactive({
      original <- source()
      if (is.null(original)) return(list(available = FALSE, total_rows = NA_integer_, filtered_rows = NA_integer_,
        visible_rows = 0L, total_columns = NA_integer_, visible_columns = 0L, page = 1L, pages = 1L))
      list(available = TRUE, total_rows = nrow(original), filtered_rows = nrow(filtered()),
        visible_rows = length(row_indices()), total_columns = ncol(original), visible_columns = length(columns()),
        page = current_page(), pages = pages())
    })
    output$column_summary <- shiny::renderText({
      value <- status()
      if (!value$available) "表示する列" else sprintf("表示する列 %d / %d", value$visible_columns, value$total_columns)
    })
    output$summary <- shiny::renderText({
      value <- status()
      if (!value$available) return("表示する結果はまだありません。")
      indices <- row_indices()
      shown <- if (length(indices)) sprintf("%d–%d", min(indices), max(indices)) else "0"
      if (nzchar(query())) sprintf("表示 %s / 検索一致 %d 行（全 %d 行）・表示 %d / 全 %d 列。検索は表示のみに反映されています。",
        shown, value$filtered_rows, value$total_rows, value$visible_columns, value$total_columns)
      else sprintf("表示 %s / 全 %d 行・表示 %d / 全 %d 列", shown, value$total_rows, value$visible_columns, value$total_columns)
    })
    output$table <- shiny::renderUI({
      if (is.null(source())) return(NULL)
      shiny::validate(shiny::need(length(columns()) > 0L, "表示する列を1つ以上選択してください。"))
      desktop_display_table(view())
    })
    output$column_styles <- shiny::renderUI({
      positions <- which(columns() %in% highlight())
      if (!length(positions)) return(NULL)
      # Escape every namespace character as a CSS code point. Column labels
      # never enter CSS: only their integer positions are used as selectors.
      escaped_id <- paste0("\\", as.character(as.hexmode(utf8ToInt(session$ns("table")))), " ", collapse = "")
      target <- paste0("#", escaped_id, " table")
      headers <- paste0(target, " thead th:nth-child(", positions, ")", collapse = ",")
      boundary <- paste0(target, c(" thead th:nth-child(", " tbody td:nth-child("), min(positions), ")", collapse = ",")
      shiny::tags$style(shiny::HTML(paste0(headers, "{background:#eaf2fb;color:#2a5d8f;}",
        boundary, "{border-left:2px solid #a9c5e6;}")))
    })
    button <- function(id, label, enabled) {
      value <- shiny::actionButton(session$ns(id), label)
      if (!isTRUE(enabled)) value$attribs$disabled <- "disabled"
      value
    }
    output$previous_action <- shiny::renderUI(button("previous", "前へ", !is.null(source()) && current_page() > 1L))
    output$next_action <- shiny::renderUI(button("next_page", "次へ", !is.null(source()) && current_page() < pages()))
    output$page_label <- shiny::renderText({
      if (is.null(source())) return("—")
      sprintf("%d / %d ページ", current_page(), pages())
    })
    invisible(list(data = source, filtered = filtered, view = view, page = current_page,
      columns = columns, status = status, metadata = shiny::reactive(desktop_grid_columns(source()))))
  })
}

# Diagnostic tables preserve original row order, IDs, types and values.
# Missing keys are reported separately, and are excluded from duplicate/unmatched checks.
desktop_join_diagnostics <- function(left, right, by) {
  empty <- function(x) if (is.data.frame(x)) x[0, , drop = FALSE] else data.frame()
  result <- list(error = NULL, missing_left = empty(left), missing_right = empty(right),
    duplicate_right = empty(right), unmatched_left = empty(left),
    row_indices = list(missing_left = integer(), missing_right = integer(), duplicate_right = integer(), unmatched_left = integer()),
    counts = list(left = if (is.data.frame(left)) nrow(left) else 0L,
      right = if (is.data.frame(right)) nrow(right) else 0L,
      missing_left = 0L, missing_right = 0L, duplicate_right_rows = 0L, duplicate_right_keys = 0L,
      duplicate_right_excess = 0L, unmatched_left = NA_integer_, matched_left = NA_integer_))
  fail <- function(message) { result$error <- message; result }
  if (!is.data.frame(left) || !is.data.frame(right)) return(fail("左表と右表を選択してください。"))
  for (value in list(left, right)) {
    if (anyNA(names(value)) || anyDuplicated(names(value)) || any(!nzchar(trimws(names(value)))))
      return(fail("表の列名に空欄または重複があります。"))
  }
  if (!is.character(by) || !length(by) || anyNA(by) || anyDuplicated(by) ||
      !all(by %in% intersect(names(left), names(right)))) return(fail("両方の表にある結合キーを1つ以上選択してください。"))
  missing <- function(value) {
    Reduce(`|`, lapply(value[by], function(x) is.na(x) | trimws(as.character(x)) == ""), init = rep(FALSE, nrow(value)))
  }
  missing_l <- missing(left); missing_r <- missing(right)
  result$row_indices$missing_left <- which(missing_l)
  result$row_indices$missing_right <- which(missing_r)
  result$missing_left <- left[missing_l, , drop = FALSE]
  result$missing_right <- right[missing_r, , drop = FALSE]
  result$counts$missing_left <- sum(missing_l); result$counts$missing_right <- sum(missing_r)
  right_valid <- right[!missing_r, , drop = FALSE]
  keys <- right_valid[by]
  duplicated_group <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
  duplicate_indices <- which(!missing_r)[duplicated_group]
  result$row_indices$duplicate_right <- duplicate_indices
  result$duplicate_right <- right[duplicate_indices, , drop = FALSE]
  result$counts$duplicate_right_rows <- length(duplicate_indices)
  result$counts$duplicate_right_keys <- sum(!duplicated(keys[duplicated_group, , drop = FALSE]))
  result$counts$duplicate_right_excess <- sum(duplicated(keys))
  if (!requireNamespace("dplyr", quietly = TRUE)) return(fail("結合キーの照合にはdplyrが必要です。"))
  left_valid <- left[!missing_l, , drop = FALSE]
  index_name <- ".pk_diagnostic_row"
  while (index_name %in% c(names(left), names(right))) index_name <- paste0(index_name, "_")
  indexed <- left_valid
  indexed[[index_name]] <- which(!missing_l)
  unmatched <- tryCatch(dplyr::anti_join(indexed, right_valid[by], by = by, na_matches = "never"), error = identity)
  if (inherits(unmatched, "error")) return(fail(paste("結合キーを照合できません。列の型を確認してください。", conditionMessage(unmatched))))
  unmatched_indices <- unmatched[[index_name]]
  result$row_indices$unmatched_left <- unmatched_indices
  result$unmatched_left <- left[unmatched_indices, , drop = FALSE]
  result$counts$unmatched_left <- length(unmatched_indices)
  result$counts$matched_left <- nrow(left_valid) - length(unmatched_indices)
  result
}
