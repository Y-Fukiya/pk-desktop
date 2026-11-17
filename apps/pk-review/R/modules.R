pk_live_config <- function(path, session) {
  shiny::reactivePoll(1000, session, checkFunc = function() pk_hash(path), valueFunc = function() pk_read_config(path))
}

pk_module_result <- function(data, cfg, bundle) {
  shiny::reactive({
    current <- list(ADNCA = data()[["ADNCA"]], ADPP = data()[["ADPP"]], hashes = bundle$hashes, manifest = bundle$manifest)
    pk_analyse(current, cfg())
  })
}

pk_settings_module <- function(path, bundle) {
  info <- pk_inspect(dirname(file.path(pk_read_config(path)$bundle, "ADNCA.csv")))
  teal::module(
    label = "解析・表示設定",
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::h3("表示と帳票の共通設定"),
        shiny::p("設定を保存すると、すべての図表と帳票に反映されます。MCPで変更した設定も自動で読み込みます。"),
        shiny::textOutput(ns("revision")),
        shiny::textOutput(ns("sync")),
        shiny::fluidRow(
          shiny::column(4,
            shiny::textInput(ns("title"), "帳票タイトル"),
            shiny::selectInput(ns("profile"), "濃度プロファイル", choices = c("全期間" = "all", "最終投与間隔" = "steady_state", "トラフ" = "trough")[c("all", "steady_state", "trough") %in% info$profiles]),
            shiny::selectInput(ns("x"), "時間軸", c("初回投与後時間" = "TIME_H", "解析投与後時間" = "TAD_H")),
            shiny::selectInput(ns("scale"), "濃度軸", c("線形" = "linear", "対数" = "log10")),
            shiny::selectInput(ns("group_by"), "層別", c("全体" = "NONE", stats::setNames(info$group_columns, info$group_columns)))
          ),
          shiny::column(4,
            shiny::selectInput(ns("statistic"), "代表値", c("算術平均" = "mean", "中央値" = "median", "幾何平均" = "geometric_mean")),
            shiny::selectInput(ns("interval"), "帯", c("SD" = "sd", "5–95パーセンタイル" = "p05_p95", "なし" = "none")),
            shiny::checkboxInput(ns("show_individual"), "個体別濃度推移を重ねる", TRUE),
            shiny::checkboxInput(ns("include_mdv"), "MDV対象レコードを濃度表示に含める", FALSE),
            shiny::selectInput(ns("blq"), "BLQの扱い", c("除外" = "exclude", "記録値のまま" = "as_recorded")),
            shiny::numericInput(ns("digits"), "帳票の表示有効桁数", 3, min = 1, max = 6)
          ),
          shiny::column(4,
            shiny::selectizeInput(ns("parameters"), "ADPPのパラメータ", choices = info$parameters, multiple = TRUE),
            shiny::selectizeInput(ns("subjects"), "対象個体（空欄は全例）", choices = info$subjects, multiple = TRUE),
            shiny::checkboxInput(ns("include_listings"), "個体別一覧をWordにも含める", FALSE),
            shiny::textInput(ns("actor"), "変更者（自己申告）", Sys.info()[["user"]]),
            shiny::textInput(ns("reason"), "変更理由（必須）"),
            shiny::actionButton(ns("apply"), "設定を保存", class = "btn-primary"),
            shiny::actionButton(ns("reload"), "保存済み設定を読み直す"),
            shiny::uiOutput(ns("message"))
          )
        ),
        shiny::hr(),
        shiny::p("ADNCAは濃度、ADPPは計算済みPKパラメータです。時間軸や濃度の除外設定を変えても、ADPPの値は再計算しません。"),
        shiny::p("個体別CSVは帳票に常に全件添付されます。SD帯は算術平均で使用できます。TAD軸は最終投与間隔で使用します。")
      )
    },
    server = function(id, data) shiny::moduleServer(id, function(input, output, session) {
      cfg <- pk_live_config(path, session)
      editing_revision <- shiny::reactiveVal(NULL)
      loading <- shiny::reactiveVal(NULL)
      ready <- shiny::reactiveVal(FALSE)
      message <- shiny::reactiveVal(NULL)
      form <- shiny::reactive(list(title = input$title, profile = input$profile, x = input$x, scale = input$scale,
                                  group_by = input$group_by, statistic = input$statistic, interval = input$interval,
                                  blq = input$blq, show_individual = input$show_individual, include_mdv = input$include_mdv,
                                  include_listings = input$include_listings, digits = as.integer(input$digits),
                                  parameters = as.list(input$parameters), subjects = as.list(input$subjects)))
      populate <- function(c) {
        ready(FALSE)
        loading(c)
        shiny::updateTextInput(session, "title", value = c$title)
        for (name in c("profile", "x", "scale", "group_by", "statistic", "interval", "blq")) shiny::updateSelectInput(session, name, selected = c[[name]])
        for (name in c("parameters", "subjects")) shiny::updateSelectizeInput(session, name, selected = unlist(c[[name]]))
        for (name in c("show_individual", "include_mdv", "include_listings")) shiny::updateCheckboxInput(session, name, value = c[[name]])
        shiny::updateNumericInput(session, "digits", value = c$digits)
      }
      shiny::observeEvent(TRUE, { populate(cfg()) }, once = TRUE)
      shiny::observe({
        c <- loading()
        if (!is.null(c) && !ready() && identical(form(), c[names(form())])) {
          editing_revision(c$revision)
          ready(TRUE)
        }
      })
      shiny::observeEvent(input$reload, { populate(cfg()); message(NULL) })
      output$revision <- shiny::renderText(paste("保存済みリビジョン", cfg()$revision))
      output$sync <- shiny::renderText({
        if (!ready()) "保存済み設定を画面に反映中です。"
        else if (cfg()$revision != editing_revision()) "外部で設定が変更されました。未保存の編集を確認し、保存済み設定を読み直してください。"
        else paste("編集中のリビジョン", editing_revision())
      })
      shiny::observeEvent(input$apply, {
        tryCatch({
          pk_assert(ready(), "設定の読込完了を待ってください。")
          saved <- pk_update_config(path, form(), editing_revision(), input$actor, input$reason)
          editing_revision(saved$revision)
          message(list(ok = TRUE, text = "保存しました。図表と帳票に同じ条件が反映されます。"))
        }, error = function(e) message(list(ok = FALSE, text = conditionMessage(e))))
      })
      output$message <- shiny::renderUI({
        m <- message()
        if (!is.null(m)) shiny::div(class = if (m$ok) "text-success" else "text-danger", m$text)
      })
    }), datanames = c("ADNCA", "ADPP")
  )
}

pk_concentration_module <- function(path, bundle) {
  teal::module(label = "濃度推移 ADNCA",
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(shiny::h3("濃度と時間"), shiny::textOutput(ns("counts")),
                     shiny::textOutput(ns("warnings")),
                     shiny::plotOutput(ns("profile"), height = "460px"),
                     shiny::h4("時点別要約"), shiny::div(style = "overflow-x:auto", shiny::tableOutput(ns("table"))))
    },
    server = function(id, data) shiny::moduleServer(id, function(input, output, session) {
      cfg <- pk_live_config(path, session)
      result <- pk_module_result(data, cfg, bundle)
      output$counts <- shiny::renderText({
        c <- result()$counts
        paste(c$subjects, "例選択 / 濃度への寄与", c$concentration_subjects, "例 /", c$concentration_rows, "濃度レコード /", c$excluded_concentration_rows, "除外 / revision", cfg()$revision)
      })
      output$warnings <- shiny::renderText(paste(unlist(result()$warnings), collapse = "\n"))
      output$profile <- shiny::renderPlot(pk_plot_concentration(result()), res = 110)
      output$table <- shiny::renderTable(pk_display_table(result()$concentration_summary, cfg()$digits), striped = TRUE, bordered = TRUE)
    }), datanames = c("ADNCA", "ADPP")
  )
}

pk_parameters_module <- function(path, bundle) {
  teal::module(label = "PKパラメータ ADPP",
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(shiny::h3("PKパラメータ"), shiny::p("計算済みADPPの値を表示します。ANL01FLがある場合はYのみを集計します。"),
                     shiny::textOutput(ns("warnings")),
                     shiny::plotOutput(ns("plot"), height = "500px"),
                     shiny::div(style = "overflow-x:auto", shiny::tableOutput(ns("table"))))
    },
    server = function(id, data) shiny::moduleServer(id, function(input, output, session) {
      cfg <- pk_live_config(path, session)
      result <- pk_module_result(data, cfg, bundle)
      output$warnings <- shiny::renderText(paste(unlist(result()$warnings), collapse = "\n"))
      output$plot <- shiny::renderPlot(pk_plot_parameters(result()), res = 110)
      output$table <- shiny::renderTable(pk_display_table(result()$parameter_summary, cfg()$digits, TRUE), striped = TRUE, bordered = TRUE)
    }), datanames = c("ADNCA", "ADPP")
  )
}

pk_report_module <- function(path, bundle) {
  teal::module(label = "帳票出力",
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(shiny::h3("現在の条件で帳票を作成"),
                     shiny::p("Word、HTML、図、集計CSV、個体別CSV、設定と入力のチェックサムをまとめて出力します。"),
                     shiny::downloadButton(ns("download"), "帳票セットを作成してダウンロード"),
                     shiny::h4("出力対象"), shiny::verbatimTextOutput(ns("counts")),
                     shiny::h4("適用条件"), shiny::verbatimTextOutput(ns("config")))
    },
    server = function(id, data) shiny::moduleServer(id, function(input, output, session) {
      cfg <- pk_live_config(path, session)
      result <- pk_module_result(data, cfg, bundle)
      output$counts <- shiny::renderPrint(result()$counts)
      output$config <- shiny::renderText(jsonlite::toJSON(cfg(), auto_unbox = TRUE, pretty = TRUE))
      output$download <- shiny::downloadHandler(
        filename = function() "pk-review.zip",
        content = function(file) {
          a <- shiny::isolate(result())
          pk_assert(pk_read_config(path)$revision == a$config$revision, "Settings changed; refresh the report preview before downloading")
          # Re-read and verify the source hashes before producing a report snapshot.
          current <- pk_load_bundle(a$config$bundle)
          pk_validate_config(a$config, current)
          directory <- tempfile("pk-report-")
          on.exit(unlink(directory, recursive = TRUE))
          pk_write_report(a, current, directory)
          old <- getwd()
          setwd(directory)
          on.exit(setwd(old), add = TRUE)
          utils::zip(file, list.files(directory), flags = "-r9X")
        }, contentType = "application/zip"
      )
    }), datanames = c("ADNCA", "ADPP")
  )
}
