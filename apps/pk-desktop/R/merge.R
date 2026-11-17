# Join an observational table to a unique lookup without changing its row grain.
pkapp_merge <- function(left, right, by, columns = setdiff(names(right), by),
                        join = c("left", "inner"), right_name = "RIGHT") {
  join <- match.arg(join)
  pk_assert(requireNamespace("teal.transform", quietly = TRUE),
            "teal.transform が必要です。apps/pk-desktop/install.R を実行してください。")
  for (table in list(left, right)) {
    pk_assert(is.data.frame(table) && !anyNA(names(table)) && all(nzchar(trimws(names(table)))) &&
                !anyDuplicated(names(table)), "表の列名は空欄や重複のない名前にしてください。")
  }
  pk_assert(is.character(by) && length(by) > 0L && !anyNA(by) && !anyDuplicated(by) &&
              all(by %in% intersect(names(left), names(right))), "両方の表にある結合キーを選んでください。")
  pk_assert(is.character(columns) && length(columns) > 0L && !anyNA(columns) && !anyDuplicated(columns) &&
              all(columns %in% setdiff(names(right), by)), "右表から追加する列を1つ以上選んでください。")
  pk_assert(is.character(right_name) && length(right_name) == 1L && !is.na(right_name) &&
              nzchar(right_name), "右表の名前を指定してください。")
  for (table in list(left, right)) {
    invalid <- vapply(table[by], function(x) anyNA(x) || any(trimws(as.character(x)) == ""), logical(1))
    pk_assert(!any(invalid), paste("結合キーに欠測または空欄があります：", paste(by[invalid], collapse = ", ")))
  }
  duplicates <- sum(duplicated(right[by]))
  pk_assert(duplicates == 0L, sprintf("右表のキーが重複しています（2行目以降 %d 行）。PARAMCDや投与期で絞るか、結合キーを追加してください。", duplicates))

  lookup <- right[c(by, columns)]
  extra <- ifelse(columns %in% names(left), paste0(right_name, "_", columns), columns)
  pk_assert(!anyDuplicated(c(names(left), extra)), "追加する列名が衝突しています。右表の列名か選択を変更してください。")
  names(lookup) <- c(by, extra)

  selector <- function(name, fields) {
    list(dataname = name, internal_id = name, keys = by, select = fields, filters = NULL, reshape = FALSE)
  }
  method <- switch(join, left = "dplyr::left_join", inner = "dplyr::inner_join")
  calls <- teal.transform::get_merge_call(
    selector_list = list(selector("LEFT", setdiff(names(left), by)), selector("RIGHT", extra)),
    join_keys = teal.data::join_keys(teal.data::join_key("LEFT", "RIGHT", by)),
    merge_function = method, anl_name = "MERGED"
  )
  scope <- list2env(list(MERGED_1 = left, MERGED_2 = lookup), parent = baseenv())
  merged <- eval(as.expression(calls), envir = scope)
  unmatched <- nrow(dplyr::anti_join(left[by], lookup[by], by = by))
  expected <- if (join == "left") nrow(left) else nrow(left) - unmatched
  pk_assert(nrow(merged) == expected, "結合後の行数が期待値と一致しません。")
  list(data = merged, added_columns = stats::setNames(extra, columns),
       counts = c(left = nrow(left), right = nrow(right), matched = nrow(left) - unmatched,
                                  unmatched = unmatched, result = nrow(merged)))
}

desktop_merge_csv_keys <- function(csv, other, keys) {
  # Uploaded CSV columns are text. Only parse keys explicitly numeric in the built-in table.
  for (key in intersect(keys, intersect(names(csv), names(other)))) {
    if (is.numeric(other[[key]])) csv[[key]] <- pk_number(csv[[key]], key, allow_na = TRUE)
  }
  csv
}

desktop_merge_ui <- function(id) {
  ns <- shiny::NS(id)
  sources <- list("元データ" = c("ADNCA", "ADPP"), "ファイル" = c("外部CSV" = "CSV"))
  desktop_ui_page("データ結合", "左表の行を基準に、右表の列を追加します。件数とプレビューを確認して保存してください。",
    controls = shiny::tagList(
      desktop_ui_section(1, "結合する表を選ぶ",
        shiny::selectInput(ns("left"), "左表（基準）", sources, selected = "ADNCA"),
        shiny::conditionalPanel("input.left === 'CSV'", ns = ns,
          shiny::fileInput(ns("left_csv"), "左表のCSV（UTF-8）", accept = ".csv", buttonLabel = "ファイルを選ぶ", placeholder = "未選択")),
        shiny::selectInput(ns("right"), "右表（追加元）", sources, selected = "ADPP"),
        shiny::conditionalPanel("input.right === 'CSV'", ns = ns,
          shiny::fileInput(ns("right_csv"), "右表のCSV（UTF-8）", accept = ".csv", buttonLabel = "ファイルを選ぶ", placeholder = "未選択")),
        shiny::p(class = "pk-caption", "前処理で保存した表は、選択肢の「保存済みの表」に表示されます。")),
      desktop_ui_section(2, "結合条件を設定する",
        shiny::uiOutput(ns("param_input")), shiny::uiOutput(ns("key_input")), shiny::uiOutput(ns("column_input")),
        shiny::selectInput(ns("method"), "結合方法", c("左表の全行を残す" = "left", "一致した行だけ残す" = "inner")),
        shiny::p(class = "pk-caption", "条件を変更すると、右側の件数とプレビューが更新されます。"))),
    body = shiny::tagList(
      desktop_ui_section(3, "結合結果を確認する",
        shiny::uiOutput(ns("qc_summary")),
        shiny::uiOutput(ns("preview_caption")),
        shiny::div(class = "pk-table-scroll", tabindex = "0", role = "region", `aria-label` = "結合結果のプレビュー。横と縦にスクロールできます。",
          shiny::tableOutput(ns("preview")))),
      desktop_ui_section(4, "結果を保存する",
        shiny::textInput(ns("save_name"), "結合結果の保存名", "結合済みデータ"),
        shiny::div(class = "pk-actions", shiny::uiOutput(ns("save_action")), shiny::uiOutput(ns("download_ui"))),
        shiny::p(class = "pk-caption", "「結合結果と履歴を保存」はアプリ内に結果と再実行条件を保存します。CSVは全行・全列をPCへダウンロードします。"),
        shiny::uiOutput(ns("saved_ui")))))
}

desktop_merge_server <- function(id, engine) shiny::moduleServer(id, function(input, output, session) {
  if (!is.null(engine$tables_signal)) shiny::observe({
    engine$tables_signal()
    tables <- desktop_tables(engine)
    saved <- stats::setNames(vapply(tables, function(x) x$id, character(1)),
      vapply(tables, function(x) paste(x$name, substr(x$id, 7, 12)), character(1)))
    choices <- list("元データ" = c("ADNCA", "ADPP"))
    if (length(saved)) choices[["保存済みの表"]] <- saved
    choices[["ファイル"]] <- c("外部CSV" = "CSV")
    shiny::updateSelectInput(session, "left", choices = choices, selected = shiny::isolate(input$left) %||% "ADNCA")
    shiny::updateSelectInput(session, "right", choices = choices, selected = shiny::isolate(input$right) %||% "ADPP")
  })
  read_source <- function(source, file) {
    shiny::req(source)
    if (identical(source, "CSV")) {
      shiny::validate(shiny::need(!is.null(file), "結合するCSVを選択してください。"))
      return(pk_csv(file$datapath))
    }
    if (startsWith(source, "table-")) return(desktop_table_get(engine, source))
    shiny::req(source %in% c("ADNCA", "ADPP"))
    if (!is.null(engine$data_signal)) engine$data_signal()
    engine$data[[source]]
  }
  left <- shiny::reactive(read_source(input$left, input$left_csv))
  right <- shiny::reactive(read_source(input$right, input$right_csv))
  output$param_input <- shiny::renderUI({
    if (!identical(input$right, "ADPP")) return(NULL)
    codes <- unique(right()$PARAMCD)
    preferred <- intersect(c("CMAXSS", "CMAX"), codes)
    shiny::selectInput(session$ns("param"), "右表のPARAMCD", codes, selected = if (length(preferred)) preferred[1] else codes[1])
  })
  output$key_input <- shiny::renderUI({
    tryCatch({
      common <- intersect(names(left()), names(right()))
      shiny::tagList(shiny::selectizeInput(session$ns("keys"), "結合キー", common, multiple = TRUE,
                            selected = intersect(c("STUDYID", "USUBJID"), common)),
        shiny::p(class = "pk-caption", "右表は結合キーごとに1行が必要です。複数のPKパラメータがある場合は、先にPARAMCDを絞ります。"))
    }, error = function(e) NULL)
  })
  output$column_input <- shiny::renderUI({
    tryCatch({
      choices <- setdiff(names(right()), input$keys)
      selected <- if (identical(input$right, "ADPP")) intersect(c("PARAMCD", "AVAL", "AVALU"), choices) else choices
      shiny::selectizeInput(session$ns("columns"), "右表から追加する列", choices, multiple = TRUE, selected = selected)
    }, error = function(e) NULL)
  })
  result <- shiny::reactive({
    waiting <- function(message) list(error = message, state = "empty")
    if (is.null(input$left) || is.null(input$right)) return(waiting("左表と右表を選択してください。"))
    if (identical(input$left, "CSV") && is.null(input$left_csv)) return(waiting("左表のCSVを選択してください。"))
    if (identical(input$right, "CSV") && is.null(input$right_csv)) return(waiting("右表のCSVを選択してください。"))
    if (!length(input$keys)) return(waiting("両方の表にある結合キーを1つ以上選択してください。"))
    if (!length(input$columns)) return(waiting("右表から追加する列を1つ以上選択してください。"))
    tryCatch({
      lhs <- left()
      rhs <- right()
      if (identical(input$right, "ADPP")) {
        if (!length(input$param) || !input$param %in% rhs$PARAMCD) return(waiting("右表のPARAMCDを選択してください。"))
        rhs <- rhs[rhs$PARAMCD == input$param, , drop = FALSE]
      }
      if (!length(input$method)) return(waiting("結合方法を選択してください。"))
      if (identical(input$left, "CSV") && !identical(input$right, "CSV"))
        lhs <- desktop_merge_csv_keys(lhs, rhs, input$keys)
      if (identical(input$right, "CSV") && !identical(input$left, "CSV"))
        rhs <- desktop_merge_csv_keys(rhs, lhs, input$keys)
      value <- pkapp_merge(lhs, rhs, input$keys, input$columns, input$method, input$right)
      value$inputs <- list(left = lhs, right = rhs)
      value$recipe <- list(kind = "join", by = as.list(input$keys), columns = as.list(input$columns), join = input$method, right_name = input$right)
      value
    }, error = function(e) list(error = if (nzchar(conditionMessage(e))) conditionMessage(e) else "結合条件を確認してください。", state = "error"))
  })
  # Bind the save receipt to the exact displayed data, conditions and name.
  # Rendering a previous success after editing conditions would imply the new result was saved.
  result_signature <- shiny::reactive({
    value <- result()
    if (is.null(value$data)) return(NULL)
    desktop_hash(list(inputs = value$inputs, recipe = value$recipe,
      sources = c(input$left, input$right), save_name = input$save_name))
  })
  save_receipt <- shiny::reactiveVal(NULL)
  saved_notice <- shiny::reactive({
    receipt <- save_receipt()
    if (is.null(result()$data)) return("")
    if (is.null(receipt)) return("現在の結合結果は未保存です。プレビューを確認して保存してください。")
    if (!identical(receipt$signature, result_signature())) return("条件または保存名が変わりました。現在の結果は未保存です。")
    receipt$message
  })
  shiny::observeEvent(input$save_join, {
    tryCatch({
      value <- result()
      pk_assert(!is.null(value$data), "結合結果がありません。表と結合条件を確認してください。")
      saved <- desktop_table_save(engine, input$save_name, value$inputs, value$recipe)
      save_receipt(list(signature = result_signature(), message = paste("保存しました：", saved$name), id = saved$id, tone = "success"))
    }, error = function(e) save_receipt(list(signature = result_signature(), message = conditionMessage(e), tone = "error")))
  })
  output$saved <- shiny::renderText(saved_notice())
  output$saved_ui <- shiny::renderUI({
    message <- saved_notice()
    if (!nzchar(message)) return(NULL)
    receipt <- save_receipt()
    current <- identical(receipt$signature, result_signature())
    shiny::tagList(desktop_ui_notice(message, if (current) receipt$tone else "info"),
      if (current && !is.null(receipt$id)) shiny::tags$details(shiny::tags$summary("保存記録のID"), shiny::tags$code(receipt$id)))
  })
  output$save_action <- shiny::renderUI({
    button <- shiny::actionButton(session$ns("save_join"), "結合結果と履歴を保存", class = "btn-primary")
    if (is.null(result()$data)) button$attribs$disabled <- "disabled"
    button
  })
  output$qc <- shiny::renderText({
    value <- result()
    if (is.null(value$data)) return(value$error)
    n <- value$counts
    sprintf("左表 %d 行・右表 %d 行・左表の一致 %d 行／不一致 %d 行・結合後 %d 行。プレビューはキー・主なPK列・追加列の先頭30行、CSVは全行・全列です。",
            n["left"], n["right"], n["matched"], n["unmatched"], n["result"])
  })
  output$qc_summary <- shiny::renderUI({
    value <- result()
    if (is.null(value$data)) {
      if (identical(value$state, "empty")) return(desktop_ui_empty("結合の準備", value$error))
      return(desktop_ui_notice(paste("結合できません。", value$error), "error"))
    }
    n <- value$counts
    labels <- c("左表", if (identical(input$right, "ADPP")) "右表（PARAMCD絞込後）" else "右表", "左表の一致", "左表の不一致", "結合後")
    message <- if (n[["left"]] == 0) "左表は0行です。結合結果も0行です。左表の選択や前処理条件を確認してください。0行の結果も保存できます。" else
      if (n[["unmatched"]] == 0) "左表の全行に対応する右表のデータが見つかりました。右表のキー重複はありません。" else
      if (identical(input$method, "left")) "一致しない左表の行も残します。その行の追加列は欠測になります。右表のキー重複はありません。" else
        "一致しない左表の行は結合結果から除外されます。右表のキー重複はありません。"
    shiny::tagList(desktop_ui_metrics(stats::setNames(paste(format(n, big.mark = ",", trim = TRUE), "行"), labels)),
      desktop_ui_notice(message, if (n[["unmatched"]] > 0 || n[["left"]] == 0) "warning" else "info"))
  })
  output$preview_caption <- shiny::renderUI({
    value <- result()
    if (is.null(value$data)) return(NULL)
    shiny::p(class = "pk-caption", sprintf("プレビュー：先頭 %d 行。結合キー・主なPK列・追加列を表示しています。横にスクロールできます。CSVには全 %d 行・%d 列を出力します。",
      min(30L, nrow(value$data)), nrow(value$data), ncol(value$data)))
  })
  output$preview <- shiny::renderTable({
    value <- result()
    if (is.null(value$data)) return(NULL)
    fields <- unique(c(input$keys, intersect(c("TIME_H", "PARAMCD", "AVAL", "AVALU"), names(left())), unname(value$added_columns)))
    head(value$data[fields], 30L)
  }, digits = 6, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)
  output$download_ui <- shiny::renderUI({
    if (!is.null(result()$data)) shiny::downloadButton(session$ns("csv"), "結合結果をCSVで保存")
  })
  output$csv <- shiny::downloadHandler(
    filename = function() paste("PK-merge", input$left, paste0(input$right, ".csv"), sep = "-"),
    contentType = "text/csv; charset=UTF-8",
    content = function(path) {
      joined <- result()$data
      shiny::req(!is.null(joined))
      pk_write_csv(joined, path)
    })
})
