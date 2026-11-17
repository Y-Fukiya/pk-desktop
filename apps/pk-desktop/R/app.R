desktop_module_server <- function(id, engine, include_reports = TRUE) shiny::moduleServer(id, function(input, output, session) {
  output$revision <- shiny::renderText({
    engine$signal()
    paste("Revision", engine$spec$revision, "・更新元", engine$spec$updated_by,
          "・", engine$info$n_subjects, "例 ・ ADNCA：濃度 / ADPP：計算済みPKパラメータ")
  })
  output$editor <- shiny::renderUI({
    engine$signal()
    p <- engine$spec$conc_plot
    ns <- session$ns
    shiny::tagList(
      shiny::div(style = "display:none", shiny::numericInput(ns("base_revision"), "revision", engine$spec$revision)),
      shiny::selectInput(ns("profile"), "濃度の対象期間", choices = engine$info$profiles, selected = p$profile),
      shiny::selectInput(ns("x"), "時間軸", c("初回投与からの実時間（h）" = "TIME_H", "解析投与からの実時間（h）" = "TAD_H"), selected = p$x),
      shiny::selectizeInput(ns("group"), "層別（図・表共通、複数可）", engine$info$group_columns, selected = unlist(p$group_by), multiple = TRUE),
      shiny::selectizeInput(ns("facet"), "濃度図の分割（最大2列）", engine$info$facet_columns, selected = unlist(p$facet_by), multiple = TRUE),
      shiny::selectInput(ns("scale"), "濃度軸", c("線形" = "linear", "対数" = "log10"), selected = p$scale),
      shiny::selectInput(ns("statistic"), "代表値", c("算術平均" = "mean", "中央値" = "median", "幾何平均" = "geometric_mean"), selected = p$statistic),
      shiny::selectInput(ns("error"), "帯", c("なし" = "none", "SD" = "sd", "SE" = "se", "5–95パーセンタイル" = "p05_p95"), selected = p$error),
      shiny::checkboxInput(ns("individual"), "個別推移を重ねる", p$show_individual),
      shiny::selectInput(ns("blq"), "BLQ", c("除外" = "exclude", "記録値を使用" = "as_recorded"), selected = p$blq),
      shiny::textInput(ns("reason"), "変更理由", "濃度表示条件の変更")
    )
  })
  output$param_editor <- shiny::renderUI({
    engine$signal()
    shiny::tagList(
      shiny::div(style = "display:none", shiny::numericInput(session$ns("param_revision"), "revision", engine$spec$revision)),
      shiny::selectizeInput(session$ns("paramcd"), "集計するADPPパラメータ", choices = engine$info$eligible_parameters,
                           selected = unlist(engine$spec$param_summary$paramcd), multiple = TRUE))
  })
  output$base_revision_input <- shiny::renderUI({
    engine$signal()
    shiny::div(style = "display:none", shiny::numericInput(session$ns("base_revision"), "revision", engine$spec$revision))
  })
  output$report_editor <- shiny::renderUI({
    engine$signal()
    shiny::tagList(
      shiny::div(style = "display:none", shiny::numericInput(session$ns("report_revision"), "revision", engine$spec$revision)),
      shiny::textInput(session$ns("report_title"), "次のRunの帳票タイトル", engine$spec$report$title),
      shiny::numericInput(session$ns("report_digits"), "表示する有効数字", engine$spec$report$digits, min = 1, max = 6))
  })
  notice <- shiny::reactiveVal("条件を保存すると解析に反映されます。帳票には保存済みの実行記録（Run）を使います。")
  action <- function(expr) tryCatch({ force(expr); notice("保存しました。画面とMCPに同じ条件を適用しています。") }, error = function(e) notice(conditionMessage(e)))
  shiny::observeEvent(input$apply, action(desktop_apply(engine, "conc_plot",
    list(profile = input$profile, x = input$x, group_by = as.list(setdiff(input$group, "NONE")), facet_by = as.list(input$facet),
         statistic = input$statistic %||% engine$spec$conc_plot$statistic,
         show_individual = input$individual %||% engine$spec$conc_plot$show_individual,
         scale = input$scale, error = input$error, blq = input$blq), input$base_revision, engine$spec$project_id, "ui", "local user", input$reason)))
  shiny::observeEvent(input$apply_params, action(desktop_apply(engine, "param_summary", list(paramcd = as.list(input$paramcd)),
    input$param_revision, engine$spec$project_id, "ui", "local user", "ADPP parameter selection")))
  shiny::observeEvent(input$apply_report, action(desktop_apply(engine, "report",
    list(title = input$report_title, digits = as.integer(input$report_digits)), input$report_revision,
    engine$spec$project_id, "ui", "local user", "Report title and display precision")))
  result <- shiny::reactive({
    engine$signal()
    pk_analyse(engine$data, desktop_config(engine$spec))
  })
  output$plot <- shiny::renderPlot(pk_plot_concentration(result()), res = 110)
  output$table <- shiny::renderTable(pk_display_table(result()$parameter_summary, engine$spec$report$digits, TRUE), striped = TRUE)
  output$warnings <- shiny::renderText(paste(unlist(result()$warnings), collapse = "\n"))
  output$notice <- shiny::renderText(notice())
  shiny::observeEvent(input$run, {
    tryCatch({
      r <- desktop_run(engine, input$base_revision, engine$spec$project_id, "ui")
      notice(paste("実行記録を保存しました：", r$run_id))
    }, error = function(e) notice(conditionMessage(e)))
  })
  if (isTRUE(include_reports)) {
  output$run_selector <- shiny::renderUI({
    engine$runs_signal()
    runs <- desktop_runs(engine)
    ids <- vapply(runs, function(r) r$run_id, character(1))
    labels <- vapply(runs, function(r) paste("rev", r$revision, "·", r$created_at, "·", substr(r$run_id, nchar(r$run_id) - 5, nchar(r$run_id))), character(1))
    selected <- shiny::isolate(input$report_run)
    if (is.null(selected) || !selected %in% ids) selected <- if (length(ids)) ids[[1]] else character()
    shiny::selectInput(session$ns("report_run"), "確認・再実行・帳票に使うRun", stats::setNames(ids, labels), selected = selected)
  })
  output$selected_run_summary <- shiny::renderUI({
    engine$runs_signal()
    if (is.null(input$report_run) || !nzchar(input$report_run)) return(
      desktop_ui_empty("保存済みRunを選択してください", "解析画面で「集計と実行記録を保存」を押すと、条件・図・集計をまとめて保存できます。"))
    tryCatch({
      folder <- desktop_run_folder(engine, input$report_run)
      manifest <- desktop_verify_run(folder)
      spec <- desktop_read(file.path(folder, "spec.json"))
      shiny::div(class = "pk-run-summary",
        shiny::span(class = "pk-status-pill", "保存済み"),
        shiny::h3(spec$report$title),
        shiny::p(paste("Revision", manifest$revision, "·", manifest$created_at)),
        shiny::p(class = "pk-run-id", manifest$run_id),
        shiny::p(class = "pk-caption", "Word帳票には、このRunに保存した図・集計・表示桁数を使います。現在の解析画面の変更は反映されません。"))
    }, error = function(e) desktop_ui_notice(conditionMessage(e), "error"))
  })
  output$run_manifest <- shiny::renderText({
    shiny::req(input$report_run)
    tryCatch(desktop_json(desktop_verify_run(desktop_run_folder(engine, input$report_run))), error = function(e) conditionMessage(e))
  })
  shiny::observeEvent(input$replay_run, {
    tryCatch({
      value <- desktop_replay_run(engine, input$report_run, input$base_revision, engine$spec$project_id, "ui")
      notice(paste("再実行の集計CSV一致を確認しました：", value$run_id, "。現在の表示条件は保持しています。"))
    }, error = function(e) notice(conditionMessage(e)))
  })
  job_label <- function(status) {
    labels <- c(queued = "順番待ち", running = "作成中", succeeded = "作成完了", failed = "作成に失敗", cancelled = "取消済み")
    if (status %in% names(labels)) unname(labels[[status]]) else status
  }
  jobs <- shiny::reactive({ shiny::invalidateLater(500, session); desktop_list_jobs(engine) })
  job_ids <- shiny::reactive(vapply(jobs(), function(j) j$job_id, character(1)))
  shiny::observeEvent(job_ids(), {
    selected <- shiny::isolate(input$report_job)
    ids <- job_ids()
    if (is.null(selected) || !selected %in% ids) selected <- if (length(ids)) ids[[1]] else character()
    labels <- vapply(jobs(), function(job) paste(job_label(job$status), "· rev", job$revision, "·", job$requested_at), character(1))
    shiny::updateSelectInput(session, "report_job", choices = stats::setNames(ids, labels), selected = selected)
  }, ignoreNULL = FALSE)
  shiny::observeEvent(input$export_report, {
    tryCatch({
      job <- desktop_export_report(engine, input$report_run, engine$spec$project_id, "ui", "local user", input$report_reason)
      shiny::updateSelectInput(session, "report_job", choices = c(job$job_id, setdiff(job_ids(), job$job_id)), selected = job$job_id)
      notice(paste("Word作成を受け付けました：", job$job_id))
    }, error = function(e) notice(conditionMessage(e)))
  })
  shiny::observeEvent(input$cancel_report, {
    tryCatch({
      job <- desktop_cancel_job(engine, input$report_job, engine$spec$project_id)
      notice(paste("帳票ジョブ：", job$status))
    }, error = function(e) notice(conditionMessage(e)))
  })
  output$job_status <- shiny::renderText({
    all <- jobs()
    if (!length(all)) return("帳票ジョブはありません。")
    paste(vapply(all, function(j) paste(j$job_id, "·", j$status, "· rev", j$revision), character(1)), collapse = "\n")
  })
  output$job_detail <- shiny::renderText({
    jobs()
    if (is.null(input$report_job) || !nzchar(input$report_job)) return("")
    tryCatch({
      job <- desktop_get_job(engine, input$report_job)
      if (job$status == "succeeded") paste("保存先：", job$artifacts$docx, "\nSHA-256：", job$artifacts$docx_sha256)
      else paste("状態：", job$status, job$error$message %||% "")
    }, error = function(e) conditionMessage(e))
  })
  output$job_summary <- shiny::renderUI({
    jobs()
    if (is.null(input$report_job) || !nzchar(input$report_job)) return(
      shiny::p(class = "pk-caption", "帳票ジョブを選択すると、作成状況を確認できます。"))
    tryCatch({
      job <- desktop_get_job(engine, input$report_job)
      shiny::tagList(
        desktop_ui_notice(paste0(job_label(job$status), if (!is.null(job$error$message)) paste0("：", job$error$message)),
          if (job$status == "failed") "error" else if (job$status == "succeeded") "success" else "info"),
        shiny::p(class = "pk-caption", paste("この帳票：Revision", job$revision, "·", job$run_id)))
    }, error = function(e) desktop_ui_notice(conditionMessage(e), "error"))
  })
  output$job_actions <- shiny::renderUI({
    jobs()
    if (is.null(input$report_job) || !nzchar(input$report_job)) return(NULL)
    tryCatch({
      job <- desktop_get_job(engine, input$report_job)
      if (identical(job$status, "succeeded")) {
        shiny::downloadButton(session$ns("download_report"), "完了したWordを保存", class = "btn-primary")
      } else if (job$status %in% c("queued", "running")) {
        shiny::actionButton(session$ns("cancel_report"), "選択ジョブを取消")
      }
    }, error = function(e) desktop_ui_notice(conditionMessage(e), "error"))
  })
  output$download_report <- shiny::downloadHandler(
    filename = function() paste0("PK-", input$report_job, ".docx"),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content = function(file) {
      job <- desktop_get_job(engine, input$report_job)
      desktop_check(identical(job$status, "succeeded"), "E_VALIDATION", "Choose a completed report job")
      desktop_check(file.copy(job$artifacts$docx, file), "E_IO", "Cannot copy report for download")
    })
  }
})
# The analysis keeps teal's supported module lifecycle. Desktop navigation,
# data preparation and immutable Run reporting sit outside its module menu.
desktop_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "pk-analysis",
    shiny::div(class = "pk-page-heading", shiny::div(
      shiny::div(class = "pk-eyebrow", "ANALYSIS"), shiny::h1("PK解析"),
      shiny::p("濃度の推移と計算済みPKパラメータを確認します。ADPPの値は再計算しません。")),
      shiny::textOutput(ns("revision"), container = shiny::div)),
    shiny::div(class = "pk-analysis-layout",
      shiny::tags$aside(class = "pk-analysis-controls pk-card", `aria-label` = "解析条件",
        shiny::h2("表示条件"), shiny::uiOutput(ns("editor")),
        shiny::actionButton(ns("apply"), "表示条件を保存", class = "btn-primary"),
        shiny::tags$details(open = NA, shiny::tags$summary("集計するパラメータ"),
          shiny::uiOutput(ns("param_editor")), shiny::actionButton(ns("apply_params"), "パラメータを保存")),
        shiny::tags$details(shiny::tags$summary("次のRunの帳票設定"),
          shiny::p(class = "pk-caption", "保存済みRunの帳票設定は変わりません。"),
          shiny::uiOutput(ns("report_editor")), shiny::actionButton(ns("apply_report"), "帳票条件を保存"))),
      shiny::tags$section(class = "pk-analysis-results",
        shiny::div(class = "pk-status-message", role = "status", `aria-live` = "polite", shiny::textOutput(ns("notice"))),
        shiny::div(class = "pk-analysis-warning", role = "status", shiny::textOutput(ns("warnings"))),
        shiny::tags$section(class = "pk-card", shiny::h2("濃度の推移"), shiny::plotOutput(ns("plot"), height = "420px")),
        shiny::tags$section(class = "pk-card", shiny::h2("ADPP要約"),
          shiny::div(class = "pk-table-scroll", tabindex = "0", `aria-label` = "ADPP要約表。横方向にスクロールできます。", shiny::tableOutput(ns("table")))),
        shiny::div(class = "pk-analysis-savebar", shiny::span("現在の保存済み条件で、図と集計を記録します。"),
          shiny::actionButton(ns("run"), "集計と実行記録を保存", class = "btn-primary")))))
}

desktop_reports_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "pk-reports",
    shiny::div(class = "pk-page-heading", shiny::div(
      shiny::div(class = "pk-eyebrow", "RESULTS & REPORTS"), shiny::h1("結果・帳票"),
      shiny::p("保存済みの実行記録（Run）を選んで確認し、Word帳票を作成します。"))),
    shiny::uiOutput(ns("base_revision_input")),
    shiny::div(class = "pk-status-message", role = "status", `aria-live` = "polite", shiny::textOutput(ns("notice"))),
    shiny::div(class = "pk-report-layout",
      shiny::tags$section(class = "pk-card", shiny::h2("1. 実行記録を選ぶ"),
        shiny::uiOutput(ns("run_selector")), shiny::uiOutput(ns("selected_run_summary")),
        shiny::conditionalPanel("input.report_run != null && input.report_run !== ''", ns = ns,
          shiny::tags$details(shiny::tags$summary("記録の詳細・再実行"),
            shiny::p(class = "pk-caption", "保存条件で再実行し、集計CSVの一致を確認します。現在の表示条件は保持します。"),
            shiny::actionButton(ns("replay_run"), "保存条件で再実行して照合"),
            shiny::verbatimTextOutput(ns("run_manifest"))))),
      shiny::tags$section(class = "pk-card", shiny::h2("2. Word帳票を作成"),
        shiny::p(class = "pk-caption", "左で選んだRunを使用します。"),
        shiny::conditionalPanel("input.report_run != null && input.report_run !== ''", ns = ns,
          shiny::textInput(ns("report_reason"), "帳票作成の理由", "PKシミュレーション結果の確認"),
          shiny::actionButton(ns("export_report"), "選択したRunからWordを作成", class = "btn-primary")),
        shiny::hr(), shiny::h3("作成状況"),
        shiny::selectInput(ns("report_job"), "確認する帳票ジョブ", character()),
        shiny::uiOutput(ns("job_summary")), shiny::uiOutput(ns("job_actions")),
        shiny::tags$details(shiny::tags$summary("選択した帳票の作成記録"), shiny::verbatimTextOutput(ns("job_detail"))),
        shiny::tags$details(shiny::tags$summary("すべての帳票ジョブ"), shiny::verbatimTextOutput(ns("job_status"))))))
}

desktop_analysis_context <- function(engine) {
  # This strip describes only the tables currently connected to the analysis.
  # Saving a workbench table intentionally does not change those inputs.
  shiny::div(class = "pk-analysis-context-content",
    shiny::span(class = "pk-context-label", "現在の解析データ"),
    shiny::span(class = "pk-data-count", "ADNCA", shiny::strong(paste(format(nrow(engine$data$ADNCA), big.mark = ","), "行"))),
    shiny::span(class = "pk-data-count", "ADPP", shiny::strong(paste(format(nrow(engine$data$ADPP), big.mark = ","), "行"))),
    shiny::span(class = "pk-context-note", "作業テーブルの保存だけでは、解析データは変わりません。"))
}

desktop_app <- function(engine, token) {
  modules <- teal::modules(teal::module(label = "PK解析", datanames = NULL,
    ui = desktop_analysis_ui, server = function(id) desktop_module_server(id, engine, include_reports = FALSE)))
  ui <- bslib::page_navbar(
    title = shiny::span(class = "pk-brand", "PK Desktop", shiny::span(class = "pk-version", desktop_version)),
    id = "workspace_navigation", selected = "data", fillable = FALSE,
    theme = bslib::bs_theme(version = 5, primary = "#176b79", base_font = "system-ui"),
    window_title = "PK Desktop", lang = "ja",
    header = shiny::tagList(desktop_workbench_styles(), desktop_i18n_controls(),
      shiny::div(class = "pk-analysis-context", shiny::uiOutput("analysis_context"),
        shiny::actionButton("configure_data", "解析データを設定", class = "btn-sm"))),
    bslib::nav_panel(desktop_i18n_text("EX・PC・濃度", "EX / PC / concentrations"), value = "assembly", desktop_assembly_ui("assembly")),
    bslib::nav_panel(desktop_i18n_text("NCA入力：DM・VS", "NCA inputs: DM / VS"), value = "covariates", desktop_annotation_ui("covariates", input_mode = TRUE)),
    bslib::nav_panel(desktop_i18n_text("データ準備", "Data preparation"), value = "data",
      shiny::div(class = "pk-data-page", desktop_workbench_ui("workbench"),
        shiny::tags$details(id = "analysis-import-settings", class = "pk-import-settings",
          shiny::tags$summary("解析用データの設定"),
          shiny::p(class = "pk-import-intro", "ADNCA・ADPPとして使う表と列の対応を確認します。検証してから解析データへ適用してください。"),
          desktop_import_ui("import")))),
    bslib::nav_panel("詳細フィルタ", value = "handling", desktop_handling_ui("handling")),
    bslib::nav_panel("NCA", value = "nca", desktop_nca_ui("nca")),
    bslib::nav_panel("結果への背景項目追加（任意）", value = "annotation", desktop_annotation_ui("annotation")),
    bslib::nav_panel(desktop_i18n_text("探索モジュール", "Exploration modules"), value = "extensions", desktop_extensions_ui("extensions")),
    bslib::nav_panel("解析", value = "analysis",
      shiny::div(class = "pk-teal-host", teal::ui_teal("pk_analysis", modules))),
    bslib::nav_panel("結果・帳票", value = "reports", desktop_reports_ui("reports")))
  shiny::shinyApp(ui, function(input, output, session) {
    # Authenticate before any module observers, data access or worker callbacks.
    query <- shiny::isolate(shiny::getQueryString(session))
    if (!identical(query[["token"]], token)) {
      session$close()
      return(invisible(NULL))
    }
    output$analysis_context <- shiny::renderUI({
      engine$data_signal()
      desktop_analysis_context(engine)
    })
    shiny::observeEvent(input$configure_data, {
      bslib::nav_select("workspace_navigation", "data", session = session)
      session$sendCustomMessage("pk-open-import", list(id = "analysis-import-settings"))
    })
    desktop_workbench_server("workbench", engine)
    desktop_assembly_server("assembly", engine)
    desktop_import_server("import", engine)
    desktop_handling_server("handling", engine)
    desktop_nca_server("nca", engine)
    desktop_annotation_server("annotation", engine)
    desktop_annotation_server("covariates", engine, input_mode = TRUE)
    desktop_extensions_server("extensions", engine)
    desktop_module_server("reports", engine)
    analysis_data <- shiny::reactive({
      engine$data_signal()
      teal.data::teal_data(ADNCA = engine$data$ADNCA, ADPP = engine$data$ADPP)
    })
    teal::srv_teal("pk_analysis", data = analysis_data, modules = modules,
      filter = teal::teal_slices(include_varnames = list(ADNCA = character(), ADPP = character()), allow_add = FALSE),
      reporter = NULL)
  })
}
