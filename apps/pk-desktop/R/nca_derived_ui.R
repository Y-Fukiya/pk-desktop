desktop_nca_derived_ui_labels <- function() {
  definitions <- desktop_i18n_choice_definitions()[["nca-derived_parameters"]]
  stats::setNames(vapply(names(definitions), function(code) {
    label <- definitions[[code]][[1L]]
    suffix <- paste0("（", code, "）")
    substr(label, 1L, nchar(label) - nchar(suffix))
  }, character(1)), names(definitions))
}

desktop_nca_derived_form <- function(spec) {
  list(derived_enabled = length(spec$derived_parameters) > 0L || !is.null(spec[["dose"]]),
    derived_parameters = unlist(spec$derived_parameters, use.names = FALSE) %||% character(),
    dose_amount = spec[["dose"]]$amount_column %||% "", dose_unit = spec[["dose"]]$unit_column %||% "", dose_route = spec[["dose"]]$route_column %||% "",
    dose_basis = spec[["dose"]]$basis %||% "", dose_reference = spec[["dose"]]$reference %||% "",
    dose_meaning_confirmed = identical(spec[["dose"]]$meaning, "actual_administered"),
    dose_ev_values = unlist(spec[["dose"]]$extravascular_values, use.names = FALSE) %||% character(),
    body_weight = !is.null(spec[["body_size"]]$weight), body_bsa_mode = spec[["body_size"]]$bsa$mode %||% "")
}

desktop_nca_derived_form_spec <- function(controls, data) {
  if (!isTRUE(controls$derived_enabled)) return(list())
  pk_assert(isTRUE(controls$dose_meaning_confirmed), "投与量の列が計画量でなく実投与量であることを確認してください。")
  spec <- list(derived_parameters = as.list(controls$derived_parameters %||% character()),
    dose = list(amount_column = controls$dose_amount %||% "", unit_column = controls$dose_unit %||% "",
      basis = controls$dose_basis %||% "", meaning = "actual_administered", reference = controls$dose_reference %||% "",
      route_column = controls$dose_route %||% "", extravascular_values = as.list(controls$dose_ev_values %||% character())))
  body <- list(); context <- desktop_covariate_context(data)
  binding <- function(role) {
    selected <- context$roles[[role]]
    pk_assert(!is.null(selected), paste("NCA入力：DM・VSで採用・保存してください：", role))
    selected[c("value_column", "unit_column")]
  }
  if (isTRUE(controls$body_weight)) body$weight <- binding("weight")
  mode <- controls$body_bsa_mode %||% ""
  if (mode == "recorded") body$bsa <- c(list(mode = mode), binding("bsa"))
  else if (mode == "mosteller") body$bsa <- list(mode = mode, height = binding("height"))
  if (length(body)) spec[["body_size"]] <- body
  spec
}

desktop_nca_derived_controls <- function(data, input, ns, form = NULL) {
  selected <- function(name, fallback = "") {
    if (!is.null(form)) return(form$controls[[name]] %||% fallback)
    shiny::isolate(input[[name]]) %||% fallback
  }
  dose_columns <- if ("NCA_EX_ROW" %in% names(data)) tryCatch(desktop_dose_input_columns(data), error = function(e) character()) else names(data)
  choices <- c("列を選択" = "", stats::setNames(dose_columns, dose_columns))
  shiny::tagList(
    shiny::checkboxInput(ns("derived_enabled"), desktop_i18n_text("実投与量・体格を使う追加PKを計算する", "Calculate additional PK parameters using actual dose and body size"), selected("derived_enabled", FALSE)),
    shiny::conditionalPanel("input.derived_enabled === true", ns = ns,
      desktop_workbench_select(ns("derived_parameters"), desktop_i18n_text("追加パラメータ（アプリ独自コード）", "Additional parameters (application-specific codes)"), desktop_i18n_choices("nca-derived_parameters"), selected("derived_parameters", character()), multiple = TRUE),
      shiny::div(class = "pk-form-grid",
        desktop_workbench_select(ns("dose_amount"), if ("NCA_EX_ROW" %in% names(data)) desktop_i18n_text("実投与量の値列（基準EXの保存列）", "Actual dose value column (saved reference EX column)") else desktop_i18n_text("実投与量の値列（解析単位ごとの申告値）", "Actual dose value column (declared per analysis unit)"), choices, selected("dose_amount")),
        desktop_workbench_select(ns("dose_unit"), desktop_i18n_text("投与量の単位列", "Dose unit column"), choices, selected("dose_unit")),
        desktop_workbench_select(ns("dose_basis"), desktop_i18n_text("記録された投与量の基準", "Recorded dose basis"), desktop_i18n_choices("nca-dose_basis"), selected("dose_basis")),
        desktop_workbench_select(ns("dose_reference"), desktop_i18n_text("実投与量の対応", "Actual dose reference"), desktop_i18n_choices("nca-dose_reference"), selected("dose_reference")),
        desktop_workbench_select(ns("dose_route"), desktop_i18n_text("投与経路の列", "Dose route column"), choices, selected("dose_route")),
        shiny::uiOutput(ns("dose_ev_controls"))),
      shiny::checkboxInput(ns("dose_meaning_confirmed"), desktop_i18n_text("列の値は実際に投与された量であり、計画量・処方量ではないことを確認した", "I confirm that the column contains actual administered dose, not planned or prescribed dose"), selected("dose_meaning_confirmed", FALSE)),
      shiny::p(class = "pk-caption", desktop_i18n_text("追加PKは単回投与または独立した投与期が対象です。上の投与状況・完全な投与履歴を確認してください。CL/F・Vz/Fは血管外投与の見かけ値です。AUCinfとλzの採用条件を継承します。", "Additional PK parameters apply to single dosing or isolated dosing periods. Confirm the dosing context and complete dosing history above. CL/F and Vz/F are apparent extravascular parameters and inherit the acceptance conditions for AUCinf and lambda-z.")),
      shiny::p(class = "pk-caption", desktop_i18n_text("代謝物では、投与した化合物の用量を用いた見かけ値となります。CL/F・Vz/Fの意味と適否は試験仕様に従って確認してください。", "For metabolites, apparent values use the dose of the administered compound. Assess the meaning and suitability of CL/F and Vz/F according to the study specification.")),
      shiny::p(class = "pk-caption", desktop_i18n_text("単位はCmax/Dose=1/L、AUC/Dose=h/L、CL/F=L/h、Vz/F=Lです。体格正規化には/kgまたは/m2を使います。実投与量mg/kg・mg/m2は採用した体格から絶対量mgへ換算します。", "Units are Cmax/Dose in 1/L, AUC/Dose in h/L, CL/F in L/h, and Vz/F in L. Body-size normalization uses /kg or /m². Doses recorded in mg/kg or mg/m² are converted to mg using the selected measurements.")),
      shiny::actionButton(ns("derived_check"), desktop_i18n_text("追加PKの計算可否を確認", "Check whether additional PK can be calculated")), shiny::uiOutput(ns("derived_readiness"))))
}

desktop_nca_derived_body_controls <- function(data, context, input, ns, form = NULL) {
  selected <- function(name, fallback) {
    if (!is.null(form)) return(form$controls[[name]] %||% fallback)
    shiny::isolate(input[[name]]) %||% fallback
  }
  measurement <- function(role, ja, en) {
    binding <- context$roles[[role]]
    shiny::tags$li(desktop_i18n_text(ja, en), " · ", if (is.null(binding)) desktop_i18n_text("未採用", "Not selected") else {
      values <- unique(paste(data[[binding$value_column]], data[[binding$unit_column]]))
      shiny::tagList(binding$value_column, " / ", binding$unit_column,
        " · ", paste(head(values, 3L), collapse = ", "), if (length(values) > 3L) " …")
    })
  }
  shiny::tagList(
    shiny::conditionalPanel("input.derived_enabled !== true", ns = ns,
      shiny::p(class = "pk-caption", desktop_i18n_text("体格を使うには追加PKを有効にします。", "Enable additional PK to use body-size measurements."))),
    shiny::conditionalPanel("input.derived_enabled === true", ns = ns,
      shiny::checkboxInput(ns("body_weight"), desktop_i18n_text("採用体重を使う", "Use selected weight"), selected("body_weight", FALSE)),
      desktop_workbench_select(ns("body_bsa_mode"), desktop_i18n_text("体表面積", "Body surface area"),
        desktop_i18n_choices("nca-body_bsa_mode"), selected("body_bsa_mode", "")),
      shiny::tags$ul(class = "pk-caption", measurement("weight", "採用体重", "Selected weight"),
        measurement("height", "採用身長", "Selected height"), measurement("bsa", "記録BSA", "Recorded BSA")),
      shiny::p(class = "pk-caption", desktop_i18n_text("Mosteller式は √(身長cm × 体重kg / 3600) です。測定値はNCA入力のDM・VSで採用します。", "The Mosteller equation is √(height in cm × weight in kg / 3600). Select measurements in NCA Input / DM and VS."))))
}

desktop_nca_derived_details <- function(group) {
  trace <- group$derived
  if (is.null(trace)) return(NULL)
  quantity <- function(name, x) data.frame(入力 = name, 元の値 = x$original_value %||% NA_real_,
    元単位 = x$conversion$original_unit %||% "", 正規化単位 = x$conversion$normalized_unit %||% "", 換算後の値 = x$converted_value %||% NA_real_, 確認範囲 = if (is.null(x$range_qc)) "" else paste(x$range_qc$lower, "–", x$range_qc$upper), 換算係数 = x$conversion$factor %||% NA_real_,
    計算用の値 = x$value %||% NA_real_, 計算用単位 = if (name == "実投与量") "mg" else x$conversion$canonical_unit %||% if (name == "体表面積") "m2" else "",
    状態 = if (nzchar(x$reason %||% "")) desktop_rule_label(x$reason) else paste(vapply(x$baseline_status %||% "採用", desktop_rule_label, character(1)), collapse = ", "), check.names = FALSE)
  shiny::tags$details(shiny::tags$summary("投与量・体格の計算根拠"),
    desktop_display_table(rbind(quantity("実投与量", trace$dose), quantity("体重", trace$weight), if (!is.null(trace$bsa$height)) quantity("身長", trace$bsa$height), quantity("体表面積", trace$bsa))),
    shiny::p(class = "pk-caption", paste0("投与量の基準：", trace$dose$basis %||% "未確認", "。経路：", paste(trace$dose$route, collapse = ", "),
      "。EX元行：", paste(trace$dose$ex_row %||% "申告値", collapse = ", "), "。CL/F = 絶対投与量mg / AUCinf_obs(mg*h/L)、Vz/F = CL/F / λz。")),
    if (!is.null(trace$bsa$formula)) shiny::p(class = "pk-caption", trace$bsa$formula),
    shiny::p(class = "pk-caption", "採用測定の元行・日時・来院・基準時点の確認は保存したNCA入力に保持しています。範囲は運用上の確認目安であり、範囲外では依存するパラメータだけを保留します。"))
}
