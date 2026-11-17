desktop_nca_ui_hours <- function(value, unit) {
  if (is.null(value)) return(NULL)
  factor <- switch(unit, h = 1, min = 1/60, day = 24, NA_real_)
  value * factor
}

desktop_nca_ui_from_hours <- function(value, unit) {
  if (is.null(value)) return(NULL)
  factor <- switch(unit, h = 1, min = 1/60, day = 24, NA_real_)
  value / factor
}

desktop_nca_terminal_profile <- function(value, profile) {
  profiles <- value$trace$terminal$profiles %||% list()
  found <- Filter(function(x) identical(as.character(x$profile), as.character(profile)), profiles)
  if (length(found)) found[[1L]] else NULL
}

desktop_nca_terminal_details <- function(value, profile) {
  terminal <- desktop_nca_terminal_profile(value, profile)
  if (is.null(terminal)) return(NULL)
  candidates <- terminal$candidates %||% list()
  rows <- lapply(candidates, function(candidate) {
    fit <- candidate$diagnostics %||% list()
    data.frame(選択 = isTRUE(candidate$selected), 元行 = paste(unlist(candidate$source_rows), collapse = ", "),
      開始h = candidate$start_h, 終了h = candidate$end_h, 点数 = candidate$n_points,
      Lambda_z = fit$lambda.z %||% NA_real_, 半減期h = fit$half.life %||% NA_real_, R2 = fit$r.squared %||% NA_real_,
      調整R2 = fit$adj.r.squared %||% NA_real_, 半減期幅 = fit$span.ratio %||% NA_real_, REASON = candidate$reason %||% "", check.names = FALSE)
  })
  reasons <- unique(c(unlist(terminal$reason), unlist(terminal$quality_reasons)))
  provenance <- terminal$eligibility$evidence$provenance
  shiny::div(class = "pk-profile-qc", shiny::h4("終末相の選択と診断"),
    shiny::p(paste("方法", if (terminal$method == "manual") "時間区間指定" else "自動・末尾の連続点", "・状態", desktop_rule_label(terminal$status))),
    if (identical(provenance, "declared_raw")) shiny::p(class = "pk-caption", desktop_i18n_text("投与履歴の根拠：申告のみ。EX・PC・濃度の照合による確認記録はありません。", "Dosing history evidence is based on attestation only; no reconciliation record for EX, PC and concentration data is available.")),
    if (identical(provenance, "assembly")) shiny::p(class = "pk-caption", desktop_i18n_text("投与履歴の根拠：EX・PC・濃度の照合記録。", "Dosing history evidence: EX, PC and concentration reconciliation record.")),
    if (length(reasons)) desktop_ui_notice(desktop_nca_diagnostics(reasons), "warning"),
    shiny::p(class = "pk-caption", paste0("実測Clastに基づくAUCINF候補：", desktop_display_value(terminal$observed_aucinf %||% NA_real_),
      "、外挿率：", desktop_display_value(terminal$extrapolated_pct %||% NA_real_), "%。QCによる保留値も診断に残します。赤い丸と破線は選択点・適合で、受理状態は結果表で確認します。")),
    if (length(rows)) shiny::tags$details(open = NA, shiny::tags$summary("候補区間・元行・適合診断"),
      shiny::div(class = "pk-table-scroll", desktop_display_table(do.call(rbind, rows)))))
}

desktop_nca_dose_details <- function(group) {
  review <- group$dose_review
  if (is.null(review) || !identical(review$reason, "intervening_dose")) return(NULL)
  affected <- review$affected_rows
  if (is.data.frame(affected)) {
    columns <- c(source_row = "解析入力の元行", conc_row = "濃度表の元行", pc_row = "PCの元行", time_h = "投与基準からh")
    affected <- affected[intersect(names(columns), names(affected))]
    names(affected) <- unname(columns[names(affected)])
  }
  shiny::div(class = "pk-profile-qc", shiny::h4("解析区間内の追加投与"),
    desktop_ui_notice(desktop_nca_diagnostics(review$reason), "warning"),
    shiny::p(class = "pk-caption", desktop_i18n_text("次回投与以降の採血が解析区間に含まれるため、この解析単位のPK値を保留します。元行と基準投与を確認してください。", "PK values for this analysis unit are withheld because the analysis interval contains samples at or after the next dose. Check the source rows and reference dose.")),
    shiny::tags$dl(class = "pk-profile-qc-metadata",
      shiny::div(shiny::tags$dt("基準投与"), shiny::tags$dd(desktop_display_value(review$reference))),
      shiny::div(shiny::tags$dt("次回投与のEX元行"), shiny::tags$dd(desktop_display_value(review$next_dose$ex_row))),
      shiny::div(shiny::tags$dt("次回投与の記録キー"), shiny::tags$dd(desktop_display_value(review$next_dose$key))),
      shiny::div(shiny::tags$dt("次回投与までの時間（h）"), shiny::tags$dd(desktop_display_value(review$next_h)))),
    if (is.data.frame(affected) && nrow(affected)) shiny::div(class = "pk-table-scroll", desktop_display_table(affected)))
}

desktop_nca_display_tlast <- function(group) {
  if (identical(group$status, "excluded")) NA_real_ else group$tlast_quant
}

desktop_nca_display_data <- function(value) {
  data <- value$data
  data$TLAST_QUANT_H[data$PROFILE_STATUS %in% "excluded"] <- NA_real_
  first <- c(unlist(value$trace$output_key_mapping, use.names = FALSE), "PROFILE_STATUS", "NCA_WARNING")
  data[c(intersect(first, names(data)), setdiff(names(data), first))]
}

desktop_nca_warning_context <- function(value) {
  remaining <- as.character(value$warnings)
  raw <- value$trace$warnings
  if (!is.null(raw)) {
    raw <- as.character(raw)
    raw <- raw[!is.na(raw) & nzchar(trimws(raw))]
    projection_input <- value
    projection_input$trace$warnings <- raw
    translated <- vapply(raw, desktop_nca_warning_text, character(1), USE.NAMES = FALSE)
    projected <- desktop_nca_public_warnings(projection_input)
    remaining <- c(as.character(raw), setdiff(remaining, c(raw, translated, projected)))
  }
  remaining <- remaining[!is.na(remaining) & nzchar(trimws(remaining))]
  profiles <- lapply(value$trace$groups, function(group) {
    prefix <- paste0("Profile ", group$profile, ": ")
    terminal_prefix <- paste0("Profile ", group$profile, " terminal: ")
    regular <- startsWith(remaining, prefix)
    terminal <- startsWith(remaining, terminal_prefix)
    messages <- as.character(group$warning %||% character())
    extra <- vapply(substring(remaining[regular], nchar(prefix) + 1L), function(payload) {
      known <- messages[order(nchar(messages), decreasing = TRUE)]
      while (nzchar(payload)) {
        matched <- known[payload == known | startsWith(payload, paste0(known, "; "))]
        if (!length(matched)) break
        payload <- substring(payload, nchar(matched[[1]]) + 1L)
        if (startsWith(payload, "; ")) payload <- substring(payload, 3L)
      }
      payload
    }, character(1))
    terminal_messages <- unlist(lapply(substring(remaining[terminal], nchar(terminal_prefix) + 1L), function(payload) {
      parts <- strsplit(payload, ";[ ]*", perl = TRUE)[[1]]
      if (any(!is.na(vapply(parts, desktop_nca_diagnostic_label, character(1))))) parts else payload
    }), use.names = FALSE)
    terminal_messages <- desktop_nca_expand_messages(terminal_messages)
    messages <- desktop_nca_expand_messages(c(messages, extra[nzchar(extra)], terminal_messages))
    remaining <<- remaining[!regular & !terminal]
    if (!length(messages)) return(NULL)
    parameter <- grepl(paste0("^(CMAX|TMAX|AUCLST|AUCPART|AUCPROC|LAMZ|LAMZHL|AUCIFO|AUCPEO|", paste(names(desktop_nca_derived_parameters()), collapse="|"), "): "), messages)
    causes <- messages
    causes[parameter] <- sub("^[A-Z]+: ", "", messages[parameter])
    keys <- unlist(value$trace$output_key_mapping, use.names = FALSE)
    selected <- rep(TRUE, nrow(value$data))
    if (length(keys) == length(group$identity) && all(keys %in% names(value$data))) {
      for (i in seq_along(keys)) {
        column <- as.character(value$data[[keys[i]]]); target <- as.character(group$identity[[i]])
        selected <- selected & if (is.na(target)) is.na(column) else !is.na(column) & column == target
      }
    } else selected[] <- FALSE
    details <- lapply(unique(causes), function(cause) {
      parameters <- sub(": .*", "", messages[parameter & causes == cause])
      if (all(c("NCA_EXCLUDE", "PARAMCD") %in% names(value$data))) {
        matching <- vapply(value$data$NCA_EXCLUDE, function(reason) !is.na(reason) && cause %in% desktop_nca_reason_parts(reason), logical(1))
        parameters <- unique(c(parameters, as.character(value$data$PARAMCD[selected & matching])))
      }
      list(cause = cause, parameters = parameters, terminal = cause %in% terminal_messages)
    })
    list(profile = group$profile, identity = group$identity, review = group$dose_review, details = details)
  })
  profiles <- Filter(Negate(is.null), profiles)
  list(profiles = profiles, remaining = unique(remaining))
}

desktop_nca_warnings <- function(value, context = NULL) {
  context <- context %||% desktop_nca_warning_context(value)
  profiles <- context$profiles
  remaining <- context$remaining
  causes <- unique(unlist(lapply(profiles, function(p) vapply(p$details, `[[`, character(1), "cause")), use.names = FALSE))
  counts <- stats::setNames(vapply(causes, function(cause) sum(vapply(profiles, function(p)
    any(vapply(p$details, function(d) identical(d$cause, cause), logical(1))), logical(1))), integer(1)), causes)
  identity_text <- function(p) {
    identity <- paste(paste(names(p$identity), vapply(p$identity, function(x) if (!length(x) || is.na(x[[1]])) "<欠測>" else as.character(x[[1]]), character(1)), sep = "="), collapse = " / ")
    if (nzchar(identity)) identity else paste0("解析単位 ", p$profile)
  }
  dose_evidence <- function(p) {
    review <- p$review
    if (!identical(review$reason, "intervening_dose")) return(NULL)
    shiny::div(paste0("追加投与：基準投与から ", if (is.finite(review$next_h %||% NA_real_)) paste0(desktop_display_value(review$next_h), " h") else "時間の記録なし（旧形式など）",
      "、EX元行：", if (length(review$next_dose$ex_row) && !is.na(review$next_dose$ex_row)) desktop_display_value(review$next_dose$ex_row) else "記録なし（旧形式など）", "。"))
  }
  render_detail <- function(d, terminal_label = FALSE) shiny::div(
    if (length(d$parameters)) desktop_i18n_text(paste(d$parameters, collapse = "・"), paste(d$parameters, collapse = ", ")),
    if (length(d$parameters) && terminal_label && d$terminal) desktop_i18n_text("・", " · "),
    if (terminal_label && d$terminal) desktop_i18n_text("終末相", "Terminal phase"),
    if (length(d$parameters) || (terminal_label && d$terminal)) desktop_i18n_text("：", ": "),
    desktop_nca_diagnostics(d$cause))
  repeated <- causes[counts >= 2L]
  grouped <- lapply(repeated, function(cause) {
    relevant <- Filter(function(p) any(vapply(p$details, function(d) identical(d$cause, cause), logical(1))), profiles)
    ja <- desktop_nca_diagnostic_label(cause)
    label <- if (!is.na(ja)) desktop_i18n_diagnostic(cause, ja) else desktop_i18n_text("未翻訳の診断（詳細を確認）", "Untranslated diagnostic (see details)")
    terminal_flags <- vapply(relevant, function(p) any(vapply(Filter(function(d) identical(d$cause, cause), p$details), function(d) isTRUE(d$terminal), logical(1))), logical(1))
    mixed_terminal <- length(unique(terminal_flags)) > 1L
    shiny::tags$details(class = "pk-warning-group", shiny::tags$summary(label,
      if (all(terminal_flags)) shiny::span(class = "pk-warning-category", desktop_i18n_text("終末相を含む", "Includes terminal phase")),
      shiny::span(class = "pk-warning-count", length(relevant), " ", desktop_i18n_text("解析単位", "analysis units"))),
      if (is.na(ja)) desktop_nca_diagnostics(cause),
      lapply(relevant, function(p) shiny::div(class = "pk-profile-warning", shiny::strong(identity_text(p)),
        lapply(Filter(function(d) identical(d$cause, cause), p$details), function(d) shiny::div(class = "pk-warning-unit-context",
          if (length(d$parameters)) shiny::tagList(desktop_i18n_text("対象パラメータ：", "Affected parameters: "), desktop_i18n_text(paste(d$parameters, collapse = "・"), paste(d$parameters, collapse = ", "))),
          if (mixed_terminal && d$terminal) desktop_i18n_text("（終末相）", " (terminal phase)"))), dose_evidence(p))))
  })
  single <- lapply(profiles, function(p) {
    details <- Filter(function(d) !d$cause %in% repeated, p$details)
    if (!length(details)) return(NULL)
    terminal <- Filter(function(d) isTRUE(d$terminal), details)
    regular <- Filter(function(d) !isTRUE(d$terminal), details)
    shiny::div(class = "pk-profile-warning", shiny::strong(identity_text(p)), lapply(regular, render_detail),
      if (length(terminal)) shiny::div(shiny::span(desktop_i18n_text("終末相", "Terminal phase"), desktop_i18n_text("：", ": ")), lapply(terminal, render_detail)),
      dose_evidence(p))
  })
  shiny::tagList(grouped, single, desktop_nca_diagnostics(unique(remaining)))
}

desktop_nca_selected_warnings <- function(value, profile, context = NULL) {
  context <- context %||% desktop_nca_warning_context(value)
  chosen <- Filter(function(p) identical(as.character(p$profile), as.character(profile)), context$profiles)
  group <- Filter(function(g) identical(as.character(g$profile), as.character(profile)), value$trace$groups)
  if (!length(group)) return(NULL)
  details <- if (length(chosen)) chosen[[1]]$details else list()
  known_causes <- vapply(details, `[[`, character(1), "cause")
  other <- desktop_nca_expand_messages(group[[1]]$reason %||% character())
  other <- other[!is.na(other) & nzchar(other) & !other %in% known_causes]
  if (!length(details) && !length(other)) return(NULL)
  identity <- paste(paste(names(group[[1]]$identity), unlist(group[[1]]$identity), sep = "="), collapse = " / ")
  desktop_ui_notice(shiny::tagList(
    shiny::strong(desktop_i18n_text("選択中の解析単位の警告", "Warnings for the selected analysis unit")),
    if (nzchar(identity)) shiny::div(class = "pk-selected-warning-identity", identity),
    lapply(details, function(d) shiny::div(class = "pk-selected-warning-item",
      if (length(d$parameters)) desktop_i18n_text(
        paste0(paste(d$parameters, collapse = "・"), "："),
        paste0(paste(d$parameters, collapse = ", "), ": ")),
      desktop_nca_diagnostics(d$cause))),
    desktop_nca_diagnostics(other)), "warning")
}

desktop_nca_summary <- function(value, with_diagnostics = FALSE) {
  keys <- unlist(value$trace$output_key_mapping, use.names = FALSE)
  parameters <- unique(value$data$PARAMCD)
  diagnostics <- list()
  rows <- lapply(value$trace$groups, function(group) {
    identity <- as.data.frame(group$identity, stringsAsFactors = FALSE)
    names(identity) <- keys
    selected <- rep(TRUE, nrow(value$data))
    for (key in keys) selected <- selected & as.character(value$data[[key]]) == as.character(identity[[key]])
    data <- value$data[selected, , drop = FALSE]
    pk_assert(!anyDuplicated(data$PARAMCD), "結果に同じ解析単位・PARAMCDの重複があります。")
    for (parameter in parameters) identity[[parameter]] <- data$AVAL[match(parameter, data$PARAMCD)]
    identity$TLAST_QUANT_H <- desktop_nca_display_tlast(group)
    identity$PROFILE_STATUS <- group$status
    assigned <- data$PARAMCD[data$PARAM_STATUS == "policy_assigned"]
    unavailable <- data$PARAMCD[data$PARAM_STATUS == "not_calculable"]
    qc <- group$predose_qc
    exclusions <- unique(data$NCA_EXCLUDE[!is.na(data$NCA_EXCLUDE) & nzchar(data$NCA_EXCLUDE)])
    exclusions <- unique(unlist(lapply(exclusions, desktop_nca_reason_parts), use.names = FALSE))
    qc_warning <- if (!is.null(qc) && qc$status == "exceeds") {
      if (is.finite(qc$pct %||% NA_real_) && is.finite(qc$threshold_pct %||% NA_real_))
        paste0("投与前QC：閾値超過（実測 ", sprintf("%.1f", qc$pct), "%、閾値 ", sprintf("%.1f", qc$threshold_pct), "%）")
      else "投与前QC：閾値超過"
    } else if (!is.null(qc) && qc$status == "not_evaluable") "投与前QC：評価不可"
    else if (!is.null(qc) && qc$status == "disabled" && any(grepl("predose_qc_required_for_prior_dose", exclusions, fixed = TRUE))) "投与前QC：未設定"
    if (nzchar(group$dose_review$reason %||% "")) qc_warning <- NULL
    if (!is.null(qc_warning)) exclusions <- gsub("\\bpredose_qc_exceeded\\b", qc_warning, exclusions, perl = TRUE)
    warnings <- unique(c(exclusions, if (!is.null(qc_warning) && !any(grepl(qc_warning, exclusions, fixed = TRUE))) qc_warning,
      if (group$n_missing_time > 0) paste0("時間欠測 ", group$n_missing_time, " 行"),
      if (group$n_missing_concentration > 0) paste0("濃度欠測 ", group$n_missing_concentration, " 行"),
      if (length(assigned)) paste0("方針による値：", paste(assigned, collapse = ", ")),
      if (length(unavailable)) paste0("算出不可：", paste(unavailable, collapse = ", "))))
    # Keep an explicit empty entry: dropping NULL would shift all later
    # diagnostics to the wrong analysis unit in a mixed healthy/warning table.
    diagnostics[length(diagnostics) + 1L] <<- list(warnings)
    identity$NCA_WARNING <- paste(warnings, collapse = "; ")
    identity[c(keys, "PROFILE_STATUS", "NCA_WARNING", parameters, "TLAST_QUANT_H")]
  })
  result <- do.call(rbind, rows)
  if (with_diagnostics) attr(result, "nca_warning_parts") <- diagnostics
  result
}

desktop_nca_plot_data <- function(value, profile, scale = c("linear", "log")) {
  scale <- match.arg(scale)
  rows <- value$trace$rows[value$trace$rows$PROFILE == profile, , drop = FALSE]
  original <- suppressWarnings(as.numeric(rows$CONC_ORIGINAL))
  rows$PLOT_ORIGINAL <- original
  rows$PLOT_KIND <- ifelse(rows$IMPUTED, "c0", ifelse(rows$BLQ & is.finite(rows$CONC_USED) & !rows$MDV,
    "blq", ifelse(rows$USE_OBS, "observed", "excluded")))
  rows$PLOT_CONC <- ifelse(rows$PLOT_KIND %in% c("c0", "blq"), rows$CONC_USED, original)
  finite <- is.finite(rows$TIME_H) & is.finite(rows$PLOT_CONC)
  rows$PLOT_TERMINAL <- if ("USE_TERMINAL" %in% names(rows)) !is.na(rows$USE_TERMINAL) & rows$USE_TERMINAL else rep(FALSE, nrow(rows))
  rows$PLOT_NONPOSITIVE <- finite & rows$PLOT_CONC <= 0
  rows$PLOT_VALID <- finite & if (scale == "log") rows$PLOT_CONC > 0 else rows$PLOT_CONC >= 0
  rows
}

desktop_nca_plot <- function(value, profile, scale = c("linear", "log"), language = "ja") {
  scale <- match.arg(scale)
  rows <- desktop_nca_plot_data(value, profile, scale)
  group <- Filter(function(x) x$profile == profile, value$trace$groups)[[1]]
  valid <- rows$PLOT_VALID
  family <- if (identical(Sys.info()[["sysname"]], "Darwin")) "Hiragino Sans" else "sans"
  if (identical(names(grDevices::dev.cur()), "pdf")) family <- "Japan1GothicBBB"
  old <- graphics::par(mar = c(4.5, 5.1, 4.3, 1.2), mgp = c(3, .8, 0),
    family = family, fg = "#23343c", col.axis = "#64747b", col.lab = "#23343c", cex.axis = .9, bty = "l")
  on.exit(graphics::par(old), add = TRUE)
  if (!any(valid)) {
    graphics::plot.new()
    graphics::text(.5, .5, if (identical(language, "en")) {
      if (scale == "log") "No positive concentrations and times to plot" else "No numeric concentrations and times to plot"
    } else if (scale == "log") "対数表示できる正の濃度・時間がありません" else "描画できる数値の濃度・時間がありません")
    return(invisible(rows))
  }
  terminal <- desktop_nca_terminal_profile(value, profile)
  tlast <- desktop_nca_display_tlast(group)
  has_tlast <- is.finite(tlast)
  has_terminal <- !is.null(terminal$selected)
  legend_labels <- if (identical(language, "en")) {
    c("Observed, not BLQ", "BLQ-processed", "Imputed C0", "Excluded record",
      if (has_tlast) "Last quantifiable time", if (has_terminal) "Selected terminal points / fit")
  } else {
    c("非BLQ実測値", "BLQ処理値", "C0補完", "対象外の記録値",
      if (has_tlast) "最終定量時点", if (has_terminal) "終末相の選択点・適合")
  }
  legend_cex <- .8
  figure_padding <- .1
  figure_width <- graphics::par("fin")[1]
  char_size <- graphics::par("cin") * graphics::par("cex") * legend_cex
  column_width <- max(graphics::strwidth(legend_labels, units = "inches", cex = legend_cex)) + 4 * char_size[1]
  legend_width <- figure_width - 2 * figure_padding
  legend_cols <- max(1L, min(3L, floor(legend_width / column_width)))
  legend_rows <- ceiling(length(legend_labels) / legend_cols)
  row_height <- max(char_size[2], graphics::strheight(legend_labels, units = "inches", cex = legend_cex))
  margins <- graphics::par("mai")
  margins[3] <- max(margins[3], legend_rows * row_height + char_size[2] + 2 * figure_padding)
  graphics::par(mai = margins)

  xlab <- if (identical(language, "en")) "Time from reference dose (h)" else "投与基準時刻からの時間（h）"
  label_cex <- graphics::par("cex.lab")
  label_width <- graphics::strwidth(xlab, units = "inches", cex = label_cex, font = graphics::par("font.lab"))
  centered_width <- figure_width - abs(margins[2] - margins[4]) - 2 * figure_padding
  if (label_width > centered_width) xlab <- list(xlab, cex = label_cex * centered_width / label_width)

  limits <- if (scale == "log") range(rows$PLOT_CONC[valid]) * c(.8, 1.25) else c(0, max(rows$PLOT_CONC[valid], 1e-12) * 1.12)
  graphics::plot(rows$TIME_H[valid], rows$PLOT_CONC[valid], type = "n", xlab = xlab,
    ylab = if (identical(language, "en")) paste0("Concentration (", value$trace$concentration_unit, ")", if (scale == "log") " - log scale" else "") else paste0("濃度（", value$trace$concentration_unit, "）", if (scale == "log") "・対数目盛" else ""), las = 1,
    log = if (scale == "log") "y" else "", ylim = limits)
  graphics::grid(col = "#e4ebeb")
  path <- rows[rows$USE_AUC & is.finite(rows$TIME_H) & is.finite(rows$CONC_USED), ]
  path <- path[order(path$TIME_H), ]
  if (scale == "log") path$CONC_USED[path$CONC_USED <= 0] <- NA_real_
  if (nrow(path) > 1L) graphics::lines(path$TIME_H, path$CONC_USED, col = "#176b79", lwd = 2)
  colors <- c(observed = "#176b79", blq = "#b07816", c0 = "#7a4c9a", excluded = "#88979b")
  shapes <- c(observed = 16, blq = 17, c0 = 18, excluded = 4)
  for (kind in names(colors)) {
    keep <- valid & rows$PLOT_KIND == kind
    graphics::points(rows$TIME_H[keep], rows$PLOT_CONC[keep], col = colors[kind], pch = shapes[kind], cex = 1.3)
  }
  selected <- rows$PLOT_TERMINAL & valid
  if (any(selected)) graphics::points(rows$TIME_H[selected], rows$PLOT_CONC[selected], col = "#a23e58", pch = 1, cex = 1.8, lwd = 2)
  if (has_terminal) {
    fit <- terminal$selected
    diagnostics <- fit$diagnostics
    if (is.finite(diagnostics$intercept %||% NA_real_) && is.finite(diagnostics$lambda.z %||% NA_real_)) {
      times <- seq(fit$start_h, fit$end_h, length.out = 100L)
      concentrations <- exp(diagnostics$intercept - diagnostics$lambda.z * times)
      keep <- is.finite(concentrations) & concentrations > 0
      graphics::lines(times[keep], concentrations[keep], col = "#a23e58", lty = 3, lwd = 2)
    }
  }
  if (has_tlast) graphics::abline(v = tlast, col = "#176b79", lty = 2)
  legend_x <- graphics::grconvertX(.5, "nfc", "user")
  legend_y <- graphics::grconvertY(graphics::par("fin")[2] - figure_padding, "inches", "user")
  graphics::legend(legend_x, legend_y, xjust = .5, xpd = NA, ncol = legend_cols,
    legend = legend_labels, col = c(colors, if (has_tlast) "#176b79", if (has_terminal) "#a23e58"),
    pch = c(shapes, if (has_tlast) NA, if (has_terminal) 1),
    lty = c(rep(0, 4), if (has_tlast) 2, if (has_terminal) 3), bty = "n", cex = legend_cex)
  invisible(rows)
}

desktop_nca_condition_table <- function(specification, source = NULL) {
  labels <- c(id = "被験者ID列", time = "時間列", concentration = "濃度列", groups = "追加グループ",
    time_unit = "入力の時間単位", concentration_unit = "濃度単位", unit_column = "濃度単位列",
    blq_column = "BLQフラグ列", blq = "BLQ行の扱い", mdv_column = "MDVフラグ列", flag_convention = "フラグの規約",
    scope = "対象範囲", time_origin = "投与基準時刻（h）", start = "解析開始（h）", end = "解析終了（h、未指定は最終観測）",
    c0_method = "C0の方針", predose_window_h = "C0投与前値の最大遡及時間（h）", include_aucproc = "AUCPROCの出力",
    all_blq_policy = "全点BLQの方針", predose_qc_threshold_pct = "投与前QCの閾値（%）", predose_qc_window_h = "投与前QCの遡及時間（h）",
    predose_qc_lloq_source = "投与前QCのLLOQ参照元", predose_qc_lloq_column = "投与前QCのLLOQ列",
    predose_qc_lloq_value = "投与前QCのLLOQ定数", terminal_method = "終末相の選択方法", dosing_context = "投与状況", washout_attestation = "休薬期間の確認", dose_history_confirmed = "投与履歴の完全性確認",
    infusion_end_h = "点滴終了（投与基準からh）", terminal_start_h = "終末相の開始（h）", terminal_end_h = "終末相の終了（h）",
    terminal_min_r_squared = "終末相の最小R²", terminal_min_span_ratio = "終末相の最小半減期幅", aucinf_max_extrap_pct = "外挿率の上限（%）",
    terminal_qc_action = "終末相QC不合格時", auc_method = "AUC計算法", pknca_version = "PKNCAの版", dose = "実投与量の入力・基準・経路", body_size = "採用体格・算出方法", derived_parameters = "投与量・体格を使う追加PK")
  values <- vapply(specification, function(x) {
    if (is.null(x)) return("未指定")
    if (!length(x)) return("なし")
    x <- unlist(x, use.names = FALSE)
    if (is.numeric(x)) return(paste(format(x, digits = 17, trim = TRUE), collapse = ", "))
    if (is.logical(x)) return(if (isTRUE(x)) "有効" else "無効")
    if (identical(x, "")) return("使わない")
    paste(x, collapse = ", ")
  }, character(1))
  fields <- names(specification)
  names <- unname(labels[fields]); names[is.na(names)] <- fields[is.na(names)]
  if (!is.null(source)) { names <- c("入力元", names); values <- c(source, values) }
  desktop_display_table(data.frame(条件 = names, 設定 = unname(values), check.names = FALSE))
}

desktop_nca_audit_view <- function(events) {
  events <- rev(tail(events, 10L))
  if (!length(events)) return(shiny::p(class = "pk-caption", desktop_i18n_text("変更履歴はまだありません。", "There is no change history yet.")))
  labels <- c(create = "プロジェクト作成", configure = "条件変更", run = "計算完了", save = "保存",
    run_start = "計算開始", run_failed = "計算失敗", run_cancelled = "計算取消",
    disclosure = "AIへの数値返却設定", disclosure_grant = "AIへの数値返却を許可", disclosure_revoke = "AIへの数値返却を解除")
  value <- function(x) if (is.null(x)) desktop_i18n_text("未設定", "Not set") else desktop_json(x)
  missing_value <- function(x) if (is.null(x)) desktop_i18n_text("未記録", "Not recorded") else x
  audit_table <- function(headers, rows) shiny::tags$table(class = "table table-striped table-sm",
    shiny::tags$thead(shiny::tags$tr(lapply(headers, function(x) shiny::tags$th(desktop_i18n_text(x[[1]], x[[2]]))))),
    shiny::tags$tbody(lapply(rows, function(row) shiny::tags$tr(lapply(row, shiny::tags$td)))))
  labels_en <- c(create = "Project created", configure = "Settings changed", run = "Calculation completed", save = "Saved",
    run_start = "Calculation started", run_failed = "Calculation failed", run_cancelled = "Calculation cancelled",
    disclosure = "AI numeric disclosure setting", disclosure_grant = "AI numeric disclosure enabled", disclosure_revoke = "AI numeric disclosure revoked")
  shiny::tags$details(shiny::tags$summary(desktop_i18n_text("操作と変更の履歴（最新10件）", "Activity and change history (last 10)")),
    lapply(events, function(event) {
      identity <- event$identity %||% list()
      action <- unname(labels[event$action]); if (is.na(action)) action <- event$action
      fields <- list(
        list(desktop_i18n_text("操作経路（サーバー記録）", "Channel (server record)"), missing_value(event$channel)),
        list(desktop_i18n_text("OSユーザー（サーバー取得）", "OS user (server record)"), missing_value(identity$os_user)),
        list(desktop_i18n_text("申告者", "Declared actor"), missing_value(identity$declared_actor %||% event$actor)),
        list(desktop_i18n_text("クライアント名・版（初期化時の自己申告）", "Client name and version (self-declared)"), if (is.null(identity$client_name)) missing_value(NULL) else paste(identity$client_name, identity$client_version %||% "")),
        list(desktop_i18n_text("理由", "Reason"), missing_value(event$reason)))
      changes <- event$changes %||% list()
      action_label <- if (event$action %in% names(labels_en)) desktop_i18n_text(action, unname(labels_en[[event$action]])) else action
      shiny::tags$details(shiny::tags$summary(event$at, " ", action_label, " [", event$channel, "]"),
        audit_table(list(c("項目", "Item"), c("記録", "Record")), fields),
        if (length(changes)) audit_table(list(c("変更項目", "Field"), c("変更前", "Before"), c("変更後", "After")),
          lapply(changes, function(x) list(x$field, value(x$before), value(x$after)))))
    }), shiny::p(class = "pk-caption", desktop_i18n_text("申告者とクライアント名は本人確認を示しません。OSユーザーはアプリを実行しているユーザーです。", "The declared actor and client name do not establish identity. The OS user is the user running the application.")))
}

desktop_nca_review_fields <- c(dosing = "dosing_context", blq = "blq", c0 = "c0_method", units = "time_unit")

desktop_nca_advanced_labels <- function() list(
  additional = c("追加パラメータ", "Additional parameters"),
  body = c("体格補正", "Body-size adjustment"),
  qc = c("QC基準の数値", "Numeric QC criteria"),
  ai = c("AI共有", "AI sharing"),
  adpp = c("ADPP候補", "ADPP candidate"))

desktop_nca_focus_link <- function(ns, id, label = desktop_i18n_text("入力欄へ", "Go to input")) {
  shiny::tags$a(href = paste0("#", ns(id)), `data-nca-focus` = ns(id), label)
}

desktop_nca_ui <- function(id) {
  ns <- shiny::NS(id)
  review_card <- function(key, title, ...) {
    shiny::tags$section(class = "pk-card pk-nca-judgment", id = ns(paste0("judgment-", key)),
      shiny::h3(class = "pk-section-title", title), ...,
      shiny::uiOutput(ns(paste0("review_", key)), class = "pk-nca-review", `aria-live` = "polite"))
  }
  policy_select <- function(field, label) {
    shiny::selectInput(ns(field), label, desktop_i18n_choices(paste0("nca-", field)), selected = "", selectize = FALSE)
  }
  advanced_section <- function(key, ...) {
    label <- desktop_nca_advanced_labels()[[key]]
    shiny::tags$section(id = ns(paste0("advanced-", key)), class = "pk-nca-advanced-section",
      shiny::h4(desktop_i18n_text(label[[1]], label[[2]])), ...)
  }

  desktop_ui_page("NCA", desktop_i18n_text("実測値・補完点・最終定量時点を確認して、Cmax・Tmax・AUClastを計算します。", "Review observed values, imputed points, and the last quantifiable time point, and calculate Cmax, Tmax, and AUClast."),
    eyebrow = "PK DESKTOP / NCA", class = "pk-nca",
    controls = shiny::tagList(
      htmltools::tagAppendAttributes(desktop_ui_section(NULL, desktop_i18n_text("濃度データ", "Concentration data"),
        desktop_workbench_select(ns("source"), desktop_i18n_text("入力元", "Source"), c("ファイル" = "FILE")),
        shiny::conditionalPanel("input.source === 'FILE'", ns = ns,
          shiny::fileInput(ns("file"), "CSV / XPT / SAS7BDAT / RDS / Parquet", accept = c(".csv", ".xpt", ".sas7bdat", ".rds", ".parquet"),
            buttonLabel = desktop_i18n_text("ファイルを選択", "Choose file"), placeholder = "未選択")),
        shiny::uiOutput(ns("mapping")),
        shiny::tags$details(shiny::tags$summary(desktop_i18n_text("入力データの確認（先頭10行）", "Review input data (first 10 rows)")),
          shiny::div(class = "pk-table-scroll", shiny::uiOutput(ns("preview"))))),
        class = "pk-nca-full", id = ns("concentration_data")),
      shiny::h3(class = "pk-nca-full pk-nca-judgment-heading", desktop_i18n_text("最初に確認する4項目", "Four checks before running NCA")),
      review_card("dosing", desktop_i18n_text("投与状況", "Dosing context"),
        shiny::div(class = "pk-form-grid",
          policy_select("dosing_context", desktop_i18n_text("解析区間の投与状況", "Dosing context for the analysis interval")),
          shiny::numericInput(ns("infusion_end_h"), desktop_i18n_text("点滴終了（投与基準からh、非点滴は0）", "End of infusion (h after reference dose, 0 for non-infusion)"), 0, min = 0)),
        shiny::checkboxInput(ns("dose_history_confirmed"), desktop_i18n_text("関連する過去の投与から観測終了までの投与履歴が完全であることを確認した", "I confirm that the dosing history is complete from relevant prior doses through the end of observation"), FALSE),
        shiny::p(class = "pk-caption", desktop_i18n_text("投与履歴は終末相・追加PKの適格性を確認するために使います。", "Dosing history is used to assess eligibility for terminal and additional PK parameters.")),
        shiny::conditionalPanel("input.dosing_context === 'isolated_single_period'", ns = ns,
          desktop_workbench_select(ns("washout_attestation"), desktop_i18n_text("前回投与からの休薬期間", "Washout since the previous dose"), desktop_i18n_choices("nca-washout_attestation")),
          shiny::p(class = "pk-caption", desktop_i18n_text("前回投与がある独立した単回投与期では、休薬期間の確認と投与前QCの合格が必要です。", "An isolated single-dose period with a prior dose requires a confirmed washout and passing predose QC.")),
          shiny::p(class = "pk-caption", desktop_i18n_text("外因性の単回投与期が対象です。推定半減期から休薬の十分性は判定しません。BEの適否判定や被験者の自動除外は行いません。", "Applies to isolated periods of exogenous single dosing. It does not infer adequate washout from the estimated half-life, assess bioequivalence eligibility, or automatically exclude subjects.")))),
      review_card("blq", desktop_i18n_text("BLQの扱い", "BLQ handling"),
        shiny::uiOutput(ns("blq_controls")), shiny::uiOutput(ns("prepared_hint")),
        desktop_workbench_select(ns("all_blq_policy"), desktop_i18n_text("全点BLQの解析単位の扱い", "Handling of analysis units with all observations BLQ"), desktop_i18n_choices("nca-all_blq_policy")),
        shiny::p(class = "pk-caption", desktop_i18n_text("全点BLQの0は方針に基づく値として記録します。Tmaxと最終定量時点は欠測のままです。", "For an all-BLQ profile, zero is recorded as a policy-assigned value. Tmax and the last quantifiable time point remain missing."))),
      review_card("c0", desktop_i18n_text("C0の方針", "C0 policy"),
        policy_select("c0_method", desktop_i18n_text("開始点がない場合のC0（選択必須）", "C0 when the starting value is missing (selection required)")),
        shiny::conditionalPanel("input.c0_method === 'predose'", ns = ns,
          shiny::numericInput(ns("predose_window_h"), desktop_i18n_text("投与前値の最大遡及時間（h）", "Maximum look-back period for predose value (h)"), NA, min = 0)),
        shiny::p(class = "pk-caption", desktop_i18n_text("C0補完は投与基準から開始する場合だけに適用し、実測の開始値は上書きしません。", "C0 imputation applies only when analysis starts at the reference dose and never overwrites an observed starting value."))),
      review_card("units", desktop_i18n_text("単位", "Units"),
        shiny::div(class = "pk-form-grid",
          desktop_workbench_select(ns("time_unit"), desktop_i18n_text("入力の時間単位", "Input time unit"), desktop_i18n_choices("nca-time_unit")),
          shiny::textInput(ns("concentration_unit"), desktop_i18n_text("濃度単位（単位列がない場合）", "Concentration unit (if no unit column is available)"), "")),
        shiny::uiOutput(ns("unit_summary"))),
      htmltools::tagAppendAttributes(desktop_ui_section(NULL, desktop_i18n_text("計算範囲とQC", "Analysis range and QC"),
        shiny::div(class = "pk-nca-qc-grid",
          shiny::div(
            shiny::div(class = "pk-form-grid",
              shiny::div(shiny::numericInput(ns("time_origin"), desktop_i18n_text("投与基準時刻（h）", "Reference dosing time (h)"), 0), shiny::uiOutput(ns("time_equiv_origin"))),
              shiny::div(shiny::numericInput(ns("start"), desktop_i18n_text("解析開始（h）", "Analysis start (h)"), 0), shiny::uiOutput(ns("time_equiv_start"))),
              desktop_workbench_select(ns("auc_method"), desktop_i18n_text("AUC計算法", "AUC calculation method"), desktop_i18n_choices("nca-auc_method"))),
            shiny::checkboxInput(ns("last_observation"), desktop_i18n_text("各解析単位の最終観測まで", "Through the last observation in each analysis unit"), TRUE),
            shiny::conditionalPanel("!input.last_observation", ns = ns,
              shiny::numericInput(ns("end"), desktop_i18n_text("解析終了（h）", "Analysis end (h)"), 24), shiny::uiOutput(ns("time_equiv_end"))),
            shiny::p(class = "pk-caption", desktop_i18n_text("基準時刻・解析開始・解析終了は選択した入力の時間単位で入力します。計算条件には時間（h）に換算して保存します。", "Enter the reference time, analysis start and analysis end in the selected input time unit. Calculation settings are stored in hours (h).")),
            shiny::p(class = "pk-caption", desktop_i18n_text("Tmaxと結果の時刻は投与基準からの時間です。最大濃度が複数の時刻で同じ場合、Tmaxは最初の時刻です。開始を後にずらした面積はAUCPARTとして区別します。", "Tmax and reported times are relative to the reference dose. When the maximum concentration occurs more than once, Tmax is the first such time. Area for a later start is distinguished as AUCPART.")),
            desktop_workbench_select(ns("terminal_method"), desktop_i18n_text("終末相の選択", "Terminal-phase selection"), desktop_i18n_choices("nca-terminal_method")),
            shiny::conditionalPanel("input.terminal_method === 'manual'", ns = ns,
              shiny::div(class = "pk-form-grid",
                shiny::numericInput(ns("terminal_start_h"), desktop_i18n_text("終末相の開始（h、全解析単位に共通）", "Terminal-phase start (h, common to all analysis units)"), NA, min = 0),
                shiny::numericInput(ns("terminal_end_h"), desktop_i18n_text("終末相の終了（h、全解析単位に共通）", "Terminal-phase end (h, common to all analysis units)"), NA, min = 0))),
            shiny::conditionalPanel("input.terminal_method !== 'none' || input.derived_enabled === true", ns = ns,
              desktop_workbench_select(ns("terminal_qc_action"), desktop_i18n_text("QC不合格時", "When QC fails"), desktop_i18n_choices("nca-terminal_qc_action")),
              shiny::p(class = "pk-caption", desktop_i18n_text("終末相には定量可能な実測値を3点以上使います。AUCINFは実測Clastに基づき、確認済み単回投与または条件を満たす独立した投与期で計算します。", "Terminal analysis uses at least three observed quantifiable concentrations. AUCINF uses observed Clast and is calculated for confirmed single dosing or an eligible isolated dosing period.")))),
          shiny::div(
            shiny::checkboxInput(ns("predose_qc"), desktop_i18n_text("投与前濃度とCmaxの比を確認する（自動除外なし）", "Check predose concentration as a percentage of Cmax (no automatic exclusion)"), FALSE),
            shiny::p(class = "pk-caption", desktop_i18n_text("QCの結果は被験者や試験期のBE解析への採否を判定しません。対象ガイドラインと解析計画に従って確認してください。", "QC results do not determine whether a subject or study period is eligible for a bioequivalence analysis. Review them against the applicable guidance and analysis plan.")),
            shiny::conditionalPanel("input.predose_qc", ns = ns, shiny::uiOutput(ns("predose_qc_lloq_controls"))),
            shiny::uiOutput(ns("qc_summary")),
            shiny::uiOutput(ns("optional_summary"))))),
        class = "pk-nca-qc-card", id = ns("range_qc")),
      shiny::tags$details(id = ns("advanced"), class = "pk-card pk-nca-full pk-nca-advanced",
        shiny::tags$summary(id = ns("advanced-summary"), desktop_i18n_text("詳細設定", "Advanced settings"),
          shiny::uiOutput(ns("advanced_changes"), inline = TRUE, class = "pk-nca-advanced-changes")),
        shiny::div(class = "pk-nca-advanced-content",
          advanced_section("additional",
            shiny::checkboxInput(ns("include_aucproc"), desktop_i18n_text("処理後の終点までの面積も出力（独自指標AUCPROC）", "Also output area to the processed endpoint (custom parameter AUCPROC)"), FALSE),
            shiny::uiOutput(ns("derived_controls"))),
          advanced_section("body", shiny::uiOutput(ns("body_controls"))),
          advanced_section("qc",
            shiny::p(class = "pk-caption", desktop_i18n_text("解析計画に合わせて基準を設定してください。終末相の自動選択は補正済みR²を使い、QC判定は通常のR²を使います。", "Set criteria according to the analysis plan. Automatic terminal-phase selection uses adjusted R². QC uses unadjusted R².")),
            shiny::div(class = "pk-form-grid",
              shiny::numericInput(ns("terminal_min_r_squared"), desktop_i18n_text("最小R²", "Minimum unadjusted R² for terminal-phase QC"), .9, min = 0, max = 1, step = .01),
              shiny::numericInput(ns("terminal_min_span_ratio"), desktop_i18n_text("最小区間幅（半減期数）", "Minimum span (number of half-lives)"), 2, min = 0),
              shiny::numericInput(ns("aucinf_max_extrap_pct"), desktop_i18n_text("AUC外挿率の上限（%）", "Maximum extrapolated AUC (%)"), 20, min = 0, max = 100),
              shiny::numericInput(ns("predose_qc_threshold_pct"), desktop_i18n_text("確認する閾値（Cmaxの%）", "Review threshold (% of Cmax)"), 5, min = 0),
              shiny::numericInput(ns("predose_qc_window_h"), desktop_i18n_text("確認対象の投与前採血の遡及時間（h）", "Predose sampling look-back period for QC (h)"), NA, min = 0),
              shiny::uiOutput(ns("predose_qc_lloq_value_control")))),
          advanced_section("ai", shiny::uiOutput(ns("disclosure"))),
          advanced_section("adpp", shiny::uiOutput(ns("adpp_order_controls")), shiny::uiOutput(ns("adpp_mapping_controls")), shiny::uiOutput(ns("adpp_draft_exports")))))),
    body = shiny::tagList(shiny::uiOutput(ns("shared_nca")), shiny::uiOutput(ns("notice")),
      shiny::div(id = ns("results_anchor"), shiny::uiOutput(ns("results"))),
      desktop_ui_section(NULL, desktop_i18n_text("保存と出力", "Save and export"), shiny::textInput(ns("name"), desktop_i18n_text("保存名", "Save name"), "NCA結果"), shiny::uiOutput(ns("exports")),
        shiny::p(class = "pk-caption", desktop_i18n_text("作業表には入力・条件・結果・行別処理記録を保存します。CSVは全行・全精度です。既存ADPPは変更しません。", "The working table retains inputs, settings, results and row-level processing records. CSV exports contain all rows at full precision. Existing ADPP data are not modified."))),
      shiny::div(class = "pk-nca-runbar", role = "region", `aria-label` = "NCAの実行と保存 / Run and save NCA",
        shiny::uiOutput(ns("period_order_run_hint"), class = "pk-period-run-hint"),
        shiny::uiOutput(ns("first_use_issues"), class = "pk-nca-issues", `aria-live` = "polite"),
        shiny::uiOutput(ns("run_action"), inline = TRUE), shiny::uiOutput(ns("actions")),
        shiny::textOutput(ns("run_state"), inline = TRUE))))
}

desktop_nca_review_state <- function() {
  keys <- names(desktop_nca_review_fields)
  empty <- stats::setNames(vector("list", length(keys)), keys)
  list(loading = FALSE, contexts = empty, receipts = empty,
    generation = stats::setNames(integer(length(keys)), keys))
}

desktop_nca_review_transition <- function(state, context, event = "sync", choice = NULL) {
  keys <- names(desktop_nca_review_fields)
  changed <- keys[!vapply(keys, function(key) identical(state$contexts[[key]], context$keys[[key]]), logical(1))]
  reset <- if (event == "load_begin") keys else changed
  state$generation[reset] <- state$generation[reset] + 1L
  state$receipts[reset] <- rep(list(NULL), length(reset))
  state$contexts <- context$keys
  if (event == "load_begin") state$loading <- TRUE
  if (event %in% c("load_cancel", "load_complete")) state$loading <- FALSE
  if (event == "load_complete") {
    for (key in keys) if (!is.null(context$values[[key]]))
      state$receipts[[key]] <- list(status = "loaded", value = context$values[[key]])
  }
  for (key in keys) if (!identical(state$receipts[[key]]$value, context$values[[key]]))
    state$receipts[key] <- list(NULL)
  if (state$loading || !context$mapped || !is.list(choice)) return(state)
  key <- choice$key
  if (length(key) != 1L || !is.character(key) || !key %in% keys ||
      (key == "blq" && context$prepared)) return(state)
  generation <- choice$generation
  value <- context$values[[key]]
  if (is.numeric(generation) && length(generation) == 1L && is.finite(generation) &&
      state$generation[[key]] == generation && !is.null(value) && identical(choice$value, value))
    state$receipts[[key]] <- list(status = "confirmed", value = value)
  state
}

desktop_nca_review_status <- function(state, context, key) {
  if (state$loading) return("loading")
  if (key == "blq" && context$prepared) return("applied")
  if (is.null(context$values[[key]])) return("unselected")
  receipt <- state$receipts[[key]]
  if (identical(state$contexts[[key]], context$keys[[key]]) &&
      identical(receipt$value, context$values[[key]])) return(receipt$status)
  "unconfirmed"
}

desktop_nca_server <- function(id, engine) shiny::moduleServer(id, function(input, output, session) {
  current <- shiny::reactiveVal(NULL)
  ui_language <- shiny::reactive({
    lang <- session$rootScope()$input$pk_ui_language
    if (identical(lang, "en")) "en" else "ja"
  })
  scroll_to_results <- function() {
    session$onFlushed(function() session$sendCustomMessage("pk-nca-results-scroll", list(id = session$ns("results_anchor"))), once = TRUE)
  }
  notice <- shiny::reactiveVal("入力データと計算条件を選び、「NCAを実行」を押してください。")
  saved <- shiny::reactiveVal(NULL)
  shared_revision <- shiny::reactiveVal(if (identical(engine$mode, "nca")) engine$nca$revision else NULL)
  loaded_form <- shiny::reactiveVal(NULL)
  loading_shared <- shiny::reactiveVal(FALSE)
  previous_time_unit <- shiny::reactiveVal("h")
  pending_time_conversion <- shiny::reactiveVal(NULL)
  # Preserve the exact hour value behind each displayed input. A newly edited
  # display value replaces it; a value sent by our own unit conversion does not.
  time_boundary_cache <- shiny::reactiveVal(list())
  pending_gui_run <- shiny::reactiveVal(NULL)
  acknowledged_result_id <- shiny::reactiveVal(NULL)
  shared_state <- shiny::reactive({
    if (!identical(engine$mode, "nca")) return(NULL)
    engine$nca_signal()
    desktop_nca_get(engine)
  })
  shared_busy <- shiny::reactive({
    state <- shared_state()
    !is.null(state$job) && identical(state$job$status, "running")
  })
  shiny::observeEvent(shared_state()$result_id, {
    acknowledged_result_id(NULL)
    shiny::updateCheckboxInput(session, "disclosure_ack", value = FALSE)
  }, ignoreNULL = FALSE)
  shiny::observeEvent(input$disclosure_ack, {
    acknowledged_result_id(if (isTRUE(input$disclosure_ack)) input$disclosure_result_id else NULL)
  }, ignoreNULL = FALSE)
  shared_form <- shiny::reactive({
    form <- loaded_form()
    if (!is.null(form) && identical(input$source, form$source)) form else NULL
  })
  selected_form <- function(name, fallback) {
    form <- shared_form()
    if (!is.null(form)) form$controls[[name]] else fallback
  }
  finite_time <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  boundary_hours <- function(name, value, unit, cache = shiny::isolate(time_boundary_cache())) {
    if (!finite_time(value)) return(NULL)
    saved <- cache[[name]]
    if (!is.null(saved) && identical(as.numeric(value), as.numeric(saved$display))) return(saved$hours)
    desktop_nca_ui_hours(value, unit)
  }
  shiny::observe({
    if (!is.null(pending_time_conversion()) || isTRUE(loading_shared())) return()
    unit <- input$time_unit %||% "h"
    cache <- shiny::isolate(time_boundary_cache())
    for (name in c("time_origin", "start", "end")) {
      value <- input[[name]]
      if (finite_time(value)) {
        old <- cache[[name]]
        if (is.null(old) || !identical(as.numeric(value), as.numeric(old$display)))
          cache[[name]] <- list(hours = desktop_nca_ui_hours(value, unit), display = as.numeric(value), entered_unit = unit, entered_value = as.numeric(value))
      } else cache[name] <- list(NULL)
    }
    time_boundary_cache(cache)
  }, priority = 100)
  shiny::observeEvent(input$time_unit, {
    unit <- input$time_unit %||% "h"
    old <- shiny::isolate(previous_time_unit())
    previous_time_unit(unit)
    if (identical(unit, old) || isTRUE(shiny::isolate(loading_shared()))) return()
    fields <- c("time_origin", "start", "end")
    prior_target <- shiny::isolate(pending_time_conversion())
    cache <- shiny::isolate(time_boundary_cache())
    if (is.null(prior_target)) for (name in fields) {
      v <- shiny::isolate(input[[name]])
      if (finite_time(v) && (is.null(cache[[name]]) || !identical(as.numeric(v), as.numeric(cache[[name]]$display))))
        cache[[name]] <- list(hours = desktop_nca_ui_hours(v, old), display = as.numeric(v), entered_unit = old, entered_value = as.numeric(v))
    }
    hours <- stats::setNames(lapply(fields, function(name) {
      if (!is.null(prior_target) && !is.null(cache[[name]])) return(cache[[name]]$hours)
      boundary_hours(name, shiny::isolate(input[[name]]), old, cache)
    }), fields)
    target <- stats::setNames(lapply(fields, function(name) {
      v <- hours[[name]]; saved <- cache[[name]]
      if (!finite_time(v)) return(NULL)
      if (!is.null(saved) && identical(saved$entered_unit, unit) && finite_time(saved$entered_value))
        return(signif(saved$entered_value, 15))
      signif(desktop_nca_ui_from_hours(v, unit), 6)
    }), fields)
    target <- Filter(Negate(is.null), target)
    if (any(!vapply(target, is.finite, logical(1)))) {
      previous_time_unit(old)
      shiny::updateSelectInput(session, "time_unit", selected = old)
      notice("時間単位を換算できる数値範囲を超えたため、元の単位に戻しました。入力値を確認してください。")
      return()
    }
    if (!length(target)) return()
    for (name in names(target)) {
      saved <- cache[[name]] %||% list()
      saved$hours <- hours[[name]]; saved$display <- as.numeric(target[[name]])
      cache[[name]] <- saved
    }
    time_boundary_cache(cache)
    attr(target, "unit") <- unit
    pending_time_conversion(target)
    for (name in names(target)) shiny::updateNumericInput(session, name, value = target[[name]])
  }, ignoreInit = TRUE, priority = 200)
  shiny::observe({
    target <- pending_time_conversion()
    if (is.null(target)) return()
    if (!is.null(attr(target, "unit")) && !identical(input$time_unit, attr(target, "unit"))) return()
    received <- vapply(names(target), function(name) {
      value <- input[[name]]
      finite_time(value) && identical(as.numeric(value), as.numeric(target[[name]]))
    }, logical(1))
    if (all(received)) pending_time_conversion(NULL)
  })
  shiny::observe({
    unit <- input$time_unit %||% "h"
    if (!unit %in% c("h", "min", "day")) return()
    en <- identical(ui_language(), "en")
    labels <- if (en) c(time_origin = "Reference dosing time", start = "Analysis start", end = "Analysis end") else c(time_origin = "投与基準時刻", start = "解析開始", end = "解析終了")
    for (name in names(labels)) shiny::updateNumericInput(session, name, label = paste0(labels[[name]], " (", unit, ")"))
  })
  for (name in c("time_origin", "start", "end")) local({
    field <- name
    output[[switch(field, time_origin = "time_equiv_origin", start = "time_equiv_start", end = "time_equiv_end")]] <- shiny::renderUI({
      unit <- input$time_unit %||% "h"
      if (!is.null(pending_time_conversion())) return(NULL)
      value <- boundary_hours(field, input[[field]], unit)
      if (is.null(value) || !is.finite(value)) return(NULL)
      if (identical(unit, "h") && identical(as.numeric(input[[field]]), as.numeric(value))) return(NULL)
      shiny::p(class = "pk-caption", if (identical(unit, "h")) desktop_i18n_text("計算に使う値：", "Value used for calculation: ") else "= ",
        paste0(format(value, digits = 15, trim = TRUE), " h"))
    })
  })
  time_boundary_error <- shiny::reactive({
    if (!identical(input$last_observation, FALSE)) return(NULL)
    if (!finite_time(input$end)) return(c("解析終了を入力してください。", "Enter an analysis end time."))
    unit <- input$time_unit %||% "h"
    start_h <- boundary_hours("start", input$start, unit)
    end_h <- boundary_hours("end", input$end, unit)
    if (!finite_time(start_h) || !finite_time(end_h) || end_h <= start_h) return(c("解析終了は解析開始より後にしてください。", "The analysis end must be later than the analysis start."))
    NULL
  })
  output$shared_nca <- shiny::renderUI({
    if (!identical(engine$mode, "nca")) return(NULL)
    engine$nca_signal()
    state <- shared_state()
    labels <- c(none = "未計算", current = "計算済み", stale = "条件変更・再実行が必要", failed = "計算失敗", running = "計算中")
    spec <- state$specification
    shiny::div(class = "pk-card", shiny::h3(desktop_i18n_text("共有NCA条件と結果", "Shared NCA settings and results")),
      shiny::p(desktop_i18n_text("共有リビジョン", "Shared revision"), " ", state$revision, " / ",
        desktop_i18n_text("画面の基準", "Screen base"), " ", shared_revision(), " / ",
        desktop_i18n_text(labels[[state$result_status]], c(none = "Not calculated", current = "Calculated",
          stale = "Settings changed", failed = "Calculation failed", running = "Calculating")[[state$result_status]])),
      if (shared_busy()) shiny::div(class = "pk-notice pk-notice-info", role = "status", `aria-live` = "polite",
        if (identical(state$job$channel, "mcp")) "AIがNCAを計算中です。完了すると結果を更新します。" else "NCAを計算中です。完了すると結果を更新します。"),
      if (!is.null(state$job) && identical(state$job$status, "failed"))
        desktop_ui_notice("共有NCAの計算に失敗しました。入力と条件を確認して再実行してください。", "error"),
      if (!identical(as.numeric(shared_revision()), as.numeric(state$revision)))
        shiny::p(class = "pk-warning", desktop_i18n_text("共有条件が更新されました。読み込むと画面の下書きを置き換え、共有結果を表示します。", "Shared settings have changed. Loading them replaces the current screen draft and displays the shared results.")),
      shiny::actionButton(session$ns("shared_refresh"), desktop_i18n_text("共有条件・結果を画面に読み込む", "Load shared settings and results")),
      shiny::p(class = "pk-caption", desktop_i18n_text("読み込みは明示的に行います。画面で変更した条件は「NCAを実行」で共有します。", "Loading is explicit. Screen changes are shared when you select Run NCA.")),
      desktop_nca_audit_view(engine$nca$audit),
      if (!is.null(spec)) shiny::tags$details(shiny::tags$summary(desktop_i18n_text("共有条件の確認", "Review shared settings")),
        desktop_nca_condition_table(spec, state$source)))
  })
  output$disclosure <- shiny::renderUI({
    state <- shared_state()
    if (is.null(state) || is.null(state$result_id) || !identical(state$result_status, "current"))
      return(shiny::p(class = "pk-caption", desktop_i18n_text("AIへのPK数値の返却は未許可です。計算結果を確認してから、この結果に限って許可できます。", "Returning numeric PK results to AI is not authorized. After reviewing the results, you may authorize it for this result only.")))
    access <- desktop_nca_disclosure(engine, state$result_id)
    shiny::div(class = "pk-nca-disclosure",
      shiny::h4(desktop_i18n_text("AIへ返す結果", "Results returned to AI")),
      shiny::p(if (isTRUE(access$allowed)) desktop_i18n_text("この結果のPK数値の返却を許可しています。", "Returning numeric PK values is authorized for this result.") else desktop_i18n_text("現在は件数・状態のみを返します。PK値の集計は直接返しません。", "Only counts and statuses are currently returned. Numeric PK summaries are not directly returned.")),
      shiny::p(class = "pk-caption", desktop_i18n_text("対象の結果ID", "Result ID"), " ", state$result_id),
      shiny::p(class = "pk-warning", desktop_i18n_text("許可すると平均・最小・最大のPK値をAIへ返します。1例や絞り込み済みの結果では個別値になります。クラウドAIでは、そのAIサービスへ情報が渡ります。", "If authorized, mean, minimum and maximum PK values are returned to the AI. For one subject or a filtered result, these can be individual values. If a cloud AI service is used, this information is sent to that service.")),
      shiny::p(class = "pk-caption", desktop_i18n_text("数値集計の直接返却を制御します。絞り込み後の件数やQC判定から値を推測できる場合があり、個人値の秘匿・匿名化は保証しません。", "This controls direct return of numeric summaries. Counts after filtering and QC outcomes may allow values to be inferred; confidentiality or anonymization of individual values is not guaranteed.")),
      shiny::div(style = "display:none", shiny::textInput(session$ns("disclosure_result_id"), NULL, state$result_id)),
      if (!isTRUE(access$allowed)) shiny::checkboxInput(session$ns("disclosure_ack"), desktop_i18n_text("表示された結果IDの数値をAIへ返すことを確認しました", "I confirm that numeric values for the displayed result ID will be returned to AI"), FALSE),
      shiny::uiOutput(session$ns("disclosure_actions")),
      shiny::p(class = "pk-caption", desktop_i18n_text("許可はこの結果だけに適用します。条件の再設定・再計算・再起動で失効します。返却済みの情報をAIから削除する機能ではありません。", "Permission applies only to this result and expires when settings are changed, calculations are rerun, or the application restarts. It does not delete information already returned to the AI.")))
  })
  output$disclosure_actions <- shiny::renderUI({
    state <- shared_state()
    if (is.null(state$result_id)) return(NULL)
    access <- desktop_nca_disclosure(engine, state$result_id)
    if (isTRUE(access$allowed)) return(shiny::actionButton(session$ns("disclosure_revoke"), desktop_i18n_text("数値の返却許可を解除", "Revoke numeric disclosure")))
    button <- shiny::actionButton(session$ns("disclosure_allow"), desktop_i18n_text("この結果の数値をAIへ返すことを許可", "Allow numeric disclosure for this result"))
    if (!isTRUE(input$disclosure_ack) || !identical(acknowledged_result_id(), state$result_id) || shared_busy()) button$attribs$disabled <- "disabled"
    button
  })
  shiny::observeEvent(input$disclosure_allow, {
    tryCatch({
      pk_assert(isTRUE(input$disclosure_ack), "数値返却の確認欄を選択してください。")
      pk_assert(!is.null(acknowledged_result_id()) && identical(acknowledged_result_id(), input$disclosure_result_id), "確認した結果が変わりました。現在の結果を確認し直してください。")
      desktop_nca_set_disclosure(engine, input$disclosure_result_id, TRUE, "ui")
      shiny::updateCheckboxInput(session, "disclosure_ack", value = FALSE)
      notice("表示された結果に限り、AIへのPK数値の返却を許可しました。")
    }, error = function(e) notice(paste("許可できませんでした：", conditionMessage(e))))
  })
  shiny::observeEvent(input$disclosure_revoke, {
    tryCatch({
      desktop_nca_set_disclosure(engine, input$disclosure_result_id, FALSE, "ui")
      notice("AIへのPK数値の返却許可を解除しました。")
    }, error = function(e) notice(paste("解除できませんでした：", conditionMessage(e))))
  })
  output$run_action <- shiny::renderUI({
    button <- shiny::actionButton(session$ns("run"), desktop_i18n_text("NCAを実行", "Run NCA"), class = "btn-primary")
    blockers <- run_blockers()
    if (shared_busy() || length(blockers)) button$attribs$disabled <- "disabled"
    if (length(blockers)) {
      issue <- blockers[[1]]
      button$attribs$title <- localized_issue(issue)
      button$attribs$`aria-describedby` <- session$ns(if (issue$period_hint) "period_order_run_hint" else "first_use_issues")
    }
    button
  })
  shiny::observe({
    engine$tables_signal()
    tables <- desktop_tables(engine)
    choices <- c("ファイル" = "FILE")
    if (!is.null(engine$data)) choices <- c(choices, "現在の濃度データ（ADNCA）" = "ADNCA")
    if (length(tables)) choices <- c(choices, stats::setNames(vapply(tables, `[[`, "", "id"), vapply(tables, `[[`, "", "name")))
    selection <- shiny::isolate(if (loading_shared()) loaded_form()$source else input$source) %||% "FILE"
    shiny::updateSelectInput(session, "source", choices = choices, selected = if (selection %in% choices) selection else "FILE")
  })
  source_data <- shiny::reactive({
    if (!is.null(engine$data_signal)) engine$data_signal()
    source <- input$source %||% "FILE"
    tryCatch({
      if (source == "FILE") {
        if (is.null(input$file)) return(NULL)
        pkapp_read_table(input$file$datapath, tolower(tools::file_ext(input$file$name)))
      } else if (source == "ADNCA") engine$data$ADNCA else desktop_table_get(engine, source)
    }, error = function(e) structure(conditionMessage(e), class = "nca_input_error"))
  })
  output$derived_controls <- shiny::renderUI({
    data <- source_data()
    if (!is.data.frame(data)) return(NULL)
    desktop_nca_derived_controls(data, input, session$ns, if (loading_shared()) shared_form() else NULL)
  })
  body_context <- shiny::reactive({
    data <- source_data()
    if (is.data.frame(data)) tryCatch(desktop_covariate_context(data), error = function(e) NULL)
  })
  output$body_controls <- shiny::renderUI({
    data <- source_data()
    if (!is.data.frame(data)) return(NULL)
    desktop_nca_derived_body_controls(data, body_context(), input, session$ns, if (loading_shared()) shared_form() else NULL)
  })
  output$dose_ev_controls <- shiny::renderUI({
    data <- source_data()
    if (!is.data.frame(data)) return(NULL)
    form <- if (loading_shared()) shared_form() else NULL
    route <- if (!is.null(form)) form$controls$dose_route else input$dose_route
    values <- if (mapped_column(route, data)) unique(as.character(data[[route]])) else character()
    values <- values[!is.na(values) & nzchar(trimws(values))]
    selected <- if (!is.null(form)) form$controls$dose_ev_values else shiny::isolate(input$dose_ev_values)
    desktop_workbench_select(session$ns("dose_ev_values"),
      desktop_i18n_text("血管外投与と確認した経路の値（CL/F・Vz/F用）", "Route values confirmed as extravascular (for CL/F and Vz/F)"),
      stats::setNames(values, values), intersect(selected, values), multiple = TRUE)
  })
  readiness <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$derived_check, {
    tryCatch({
      assert_nca_ready()
      data <- source_data(); spec <- specification()
      value <- pkapp_nca(data, spec)
      selected <- value$data[value$data$PARAMCD %in% unlist(spec$derived_parameters), , drop = FALSE]
      counts <- stats::aggregate(rep(1L, nrow(selected)), selected[c("PARAMCD", "AVALU", "PARAM_STATUS")], sum)
      names(counts)[4] <- "PROFILES"
      readiness(list(hash = desktop_hash(list(data, spec)), counts = counts,
        issues = unique(selected[!is.na(selected$NCA_EXCLUDE), c("PARAMCD", "NCA_EXCLUDE"), drop = FALSE])))
    }, error = function(e) { readiness(NULL); notice(conditionMessage(e)) })
  })
  output$derived_readiness <- shiny::renderUI({
    r <- readiness(); if (is.null(r)) return(NULL)
    current_hash <- tryCatch(desktop_hash(list(source_data(), specification())), error = function(e) NULL)
    if (!identical(current_hash, r$hash)) return(shiny::p(class = "pk-caption", desktop_i18n_text("条件変更後に計算可否を再確認してください。", "After changing settings, recheck whether calculation is available.")))
    shiny::tagList(desktop_display_table(r$counts), if (nrow(r$issues)) desktop_display_table(r$issues))
  })
  output$mapping <- shiny::renderUI({
    data <- source_data()
    if (!is.data.frame(data)) return(NULL)
    columns <- names(data)
    choose <- function(candidates) { hit <- intersect(candidates, columns); if (length(hit)) hit[1] else columns[1] }
    optional <- c("使わない" = "", stats::setNames(columns, columns))
    ns <- session$ns
    prepared <- all(c("NCA_CONC", "NCA_BLQ", "NCA_MDV", "NCA_BLQ_RULE") %in% columns)
    context <- if (prepared) tryCatch(desktop_pk_blq_context(data), error = function(e) e) else NULL
    context_keys <- if (is.list(context) && !inherits(context, "error")) unlist(context$profile_columns, use.names = FALSE) else character()
    if (!length(context_keys) && "NCA_ASSEMBLY_PROFILE_COLUMNS" %in% columns) {
      context_keys <- tryCatch(unlist(jsonlite::fromJSON(data$NCA_ASSEMBLY_PROFILE_COLUMNS[1L]), use.names = FALSE), error = function(e) character())
    }
    identity <- head(intersect(c("USUBJID", "ID", "SUBJID", "Subject"), if (length(context_keys)) context_keys else columns), 1L)
    if (!length(identity)) identity <- if (length(context_keys)) context_keys[1] else columns[1]
    groups <- if (length(context_keys)) setdiff(context_keys, identity) else desktop_pk_default_groups(columns, identity)
    concentration <- choose(c("NCA_CONC", "AVAL", "CONC", "conc", "Concentration"))
    time_choices <- desktop_pk_time_choices(data, c(identity, groups, concentration,
      intersect(c("NCA_BLQ", "NCA_MDV", "BLQ", "MDV"), columns)))
    preferred_time <- intersect(c(if (is.list(context) && !inherits(context, "error")) context$time_column,
      "NCA_TIME_H", "TAD_H", "TIME_H", "TIME", "time", "Time"), unname(time_choices))
    selected_time <- if (length(preferred_time)) preferred_time[1] else ""
    form <- shared_form()
    if (!is.null(form) && !form$controls$time %in% unname(time_choices))
      time_choices <- c(time_choices, stats::setNames(form$controls$time, form$controls$time))
    flags <- shiny::div(class = "pk-form-grid",
      desktop_workbench_select(ns("blq_column"), desktop_i18n_text("BLQフラグ列", "BLQ flag column"), optional, selected = selected_form("blq_column", if (prepared) "NCA_BLQ" else if ("BLQ" %in% columns) "BLQ" else "")),
      desktop_workbench_select(ns("mdv_column"), desktop_i18n_text("MDVフラグ列", "MDV flag column"), optional, selected = selected_form("mdv_column", if (prepared) "NCA_MDV" else if ("MDV" %in% columns) "MDV" else "")))
    shiny::tagList(shiny::div(class = "pk-form-grid",
      desktop_workbench_select(ns("id"), desktop_i18n_text("被験者ID列", "Subject ID column"), columns, selected_form("id", identity)),
      shiny::div(desktop_workbench_select(ns("time"), desktop_i18n_text("時間列", "Time column"), c(desktop_i18n_choices("nca-time"), time_choices), selected_form("time", selected_time)),
        shiny::uiOutput(ns("time_hint"))),
      desktop_workbench_select(ns("concentration"), desktop_i18n_text("濃度列", "Concentration column"), columns, selected_form("concentration", concentration)),
      desktop_workbench_select(ns("unit_column"), desktop_i18n_text("濃度単位列", "Concentration unit column"), optional, selected = selected_form("unit_column", if (length(intersect(c("AVALU", "UNIT"), columns))) intersect(c("AVALU", "UNIT"), columns)[1] else ""))),
      desktop_workbench_select(ns("groups"), desktop_i18n_text("追加グループ（試験・分析対象物・投与期など）", "Additional grouping columns (study, analyte, period, etc.)"), columns,
        selected = selected_form("groups", groups), multiple = TRUE),
      shiny::uiOutput(ns("group_role_controls")), shiny::uiOutput(ns("period_order_controls")),
      if (inherits(context, "error")) desktop_ui_notice(conditionMessage(context), "warning"),
      if (prepared) shiny::conditionalPanel("input.concentration !== 'NCA_CONC'", ns = ns, flags) else flags,
      desktop_workbench_select(ns("scope"), desktop_i18n_text("対象範囲", "Rows to include"), if ("SSNCAFL" %in% columns) desktop_i18n_choices("nca-scope") else desktop_i18n_choices("nca-scope")["全行"], selected = selected_form("scope", if ("SSNCAFL" %in% columns) "ss" else "all")),
      desktop_workbench_select(ns("flag_convention"), desktop_i18n_text("フラグの規約", "Flag convention"), desktop_i18n_choices("nca-flag_convention"),
        selected = selected_form("flag_convention", shiny::isolate(input$flag_convention) %||% "strict")))
  })
  group_role_field <- function(column) paste0("group_role_column_", digest::digest(column, algo = "sha256", serialize = FALSE))
  output$group_role_controls <- shiny::renderUI({
    groups <- input$groups %||% character()
    if (!length(groups)) return(NULL)
    roles <- desktop_i18n_choices("nca-group-role")
    form <- shared_form()
    saved <- if (!is.null(form)) form$specification$group_roles %||% list() else list()
    controls <- lapply(groups, function(column) {
      field <- group_role_field(column)
      entry <- Filter(function(x) identical(x$column, column), saved)
      remembered <- if (length(entry)) entry[[1]]$role else ""
      selected <- if (loading_shared() || is.null(input[[field]])) remembered else input[[field]]
      htmltools::tagAppendAttributes(shiny::selectInput(session$ns(field), shiny::span(column, desktop_i18n_text(" の役割", " role")), choices = roles, selected = selected, selectize = FALSE), class = "pk-i18n-role-select")
    })
    shiny::tagList(shiny::p(class = "pk-caption", desktop_i18n_text("ADPP候補を出力する場合は、試験を含むすべてのグループ列に役割を指定してからNCAを実行してください。役割は推測しません。", "To export an ADPP candidate, assign roles to every grouping column, including study, before running NCA. Roles are not inferred.")),
      shiny::div(class = "pk-form-grid", controls))
  })
  # Keep the dropdown and missing-value notice in the same stable order.
  period_display_sort <- function(values) {
    if (!length(values)) return(values)
    ordering <- desktop_explore_period_order(values)
    if (isTRUE(ordering$ok)) return(values[order(ordering$number[match(values, ordering$labels)])])
    normalized <- enc2utf8(values); Encoding(normalized) <- "UTF-8"
    values[order(normalized, method = "radix")]
  }
  period_order_state <- shiny::reactive({
    groups <- input$groups %||% character()
    periods <- groups[vapply(groups, function(column) identical(input[[group_role_field(column)]], "period"), logical(1))]
    data <- source_data()
    if (!isTRUE(input$manual_period_order) || length(periods) != 1L || !is.data.frame(data) || !periods[[1]] %in% names(data)) return(NULL)
    all_values <- unique(as.character(data[[periods[[1]]]]))
    values <- all_values[!is.na(all_values) & nzchar(trimws(all_values))]
    chosen <- input$period_order_values %||% character()
    if (!is.character(chosen) || anyNA(chosen)) chosen <- character()
    sorted_values <- period_display_sort(values)
    missing <- sorted_values[!sorted_values %in% chosen]
    valid <- length(values) > 0L && length(values) <= 100L && length(values) == length(all_values) && !anyDuplicated(chosen) && length(chosen) == length(values) && setequal(chosen, values)
    list(values = values, chosen = chosen, missing = missing, valid = valid, invalid_data = length(values) != length(all_values))
  })
  period_label <- function(values) gsub("\t", "⇥", gsub(" ", "␠", values, fixed = TRUE), fixed = TRUE)
  output$period_order_progress <- shiny::renderUI({
    state <- period_order_state(); if (is.null(state)) return(NULL)
    count <- length(intersect(state$chosen, state$values)); total <- length(state$values)
    shiny::p(class = if (state$valid) "pk-caption" else "pk-caption pk-period-warning", paste0(count, " / ", total),
      desktop_i18n_text(" 選択済み。", " selected."),
      if (length(state$missing)) shiny::tagList(desktop_i18n_text(" 未選択：", " Not selected: "), paste(period_label(state$missing), collapse = "、")),
      if (state$invalid_data) desktop_i18n_text(" 期の欠測・空白を元データで確認してください。", " Check missing or blank period values in the source data."),
      if (total > 100L) desktop_i18n_text(" 期の値は100種類まで指定できます。", " At most 100 period values are supported."))
  })
  output$period_order_run_hint <- shiny::renderUI({
    issues <- Filter(function(issue) issue$period_hint, run_issues())
    if (!length(issues)) return(NULL)
    shiny::tagList(lapply(issues, function(issue) shiny::span(class = "pk-period-warning",
      desktop_i18n_text(issue$message[[1]], issue$message[[2]]))))
  })
  shiny::outputOptions(output, "period_order_run_hint", suspendWhenHidden = FALSE)
  period_order_scope <- shiny::reactiveVal(NULL)
  output$period_order_controls <- shiny::renderUI({
    groups <- input$groups %||% character()
    periods <- groups[vapply(groups, function(column) identical(input[[group_role_field(column)]], "period"), logical(1))]
    if (length(periods) != 1L) { period_order_scope(NULL); return(NULL) }
    scope <- paste(input$source %||% "", periods[[1]], sep = "\n")
    old_scope <- shiny::isolate(period_order_scope())
    changed <- !identical(old_scope, scope)
    period_order_scope(scope)
    form <- shared_form(); previous <- if (!is.null(form)) form$specification$period_order else NULL
    restored <- !is.null(previous) && identical(previous$column, periods[[1]])
    active <- if (changed || is.null(shiny::isolate(input$manual_period_order)) || loading_shared()) restored else isTRUE(shiny::isolate(input$manual_period_order))
    data <- source_data()
    values <- if (is.data.frame(data) && periods[[1]] %in% names(data)) unique(as.character(data[[periods[[1]]]])) else character()
    values <- values[!is.na(values) & nzchar(values)]
    value <- if (changed || loading_shared() || is.null(shiny::isolate(input$period_order_values))) {
      if (restored) unlist(previous$values, use.names = FALSE) else character()
    } else shiny::isolate(input$period_order_values)
    # HTML selectize initializes selected values in option order, so restore
    # selected options first in their saved order rather than source row order.
    selected_values <- value[value %in% values]
    # Sort the complete set before removing selected values, so both the
    # dropdown and the missing-value notice retain the same ordering rule.
    sorted_values <- period_display_sort(values)
    unselected <- sorted_values[!sorted_values %in% selected_values]
    ordered_values <- c(selected_values, unselected)
    labels <- period_label(ordered_values)
    choices <- stats::setNames(ordered_values, labels)
    shiny::tagList(shiny::checkboxInput(session$ns("manual_period_order"), desktop_i18n_text("期の表示順を自分で指定する", "Specify period display order"), value = active),
      shiny::conditionalPanel("input.manual_period_order", ns = session$ns,
        shiny::selectizeInput(session$ns("period_order_values"), shiny::span(periods[[1]], desktop_i18n_text(" の値を表示順に選択", " values in display order")),
          choices = choices, selected = value, multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "期を順番にすべて選択")),
        shiny::uiOutput(session$ns("period_order_progress")),
        shiny::p(class = "pk-caption", desktop_i18n_text("一覧から期を表示したい順にすべて選びます。␠は半角スペースです。選択した期の値は計算条件としてMCPにも共有されます。同じデータの全試験に共通の順序です。投与情報と照合してください。", "Select all period values in display order. ␠ represents a space. Selected values are shared with MCP as calculation settings. The order is common to all studies in the same dataset; check it against dosing information."))))
  })
  prepared_mapping <- shiny::reactive({
    data <- source_data()
    is.data.frame(data) && identical(input$concentration, "NCA_CONC") &&
      all(c("NCA_BLQ", "NCA_MDV", "NCA_BLQ_RULE") %in% names(data))
  })
  review_state <- shiny::reactiveVal(desktop_nca_review_state())
  review_keys <- names(desktop_nca_review_fields)
  policy_values <- lapply(desktop_nca_review_fields[review_keys != "units"], function(field) {
    setdiff(unname(desktop_i18n_choices(paste0("nca-", field))), "")
  })
  source_identity <- shiny::reactive({
    data <- source_data()
    list(source = input$source %||% "FILE",
      upload = if (identical(input$source, "FILE")) input$file$datapath,
      content = if (is.data.frame(data)) desktop_hash(data))
  })
  mapped_column <- function(column, data) {
    is.character(column) && length(column) == 1L && column %in% names(data)
  }
  unit_context <- shiny::reactive(list(time = input$time_unit %||% "h",
    column = input$unit_column %||% "", manual = input$concentration_unit %||% ""))
  review_context <- shiny::reactive({
    data <- source_data()
    mapped <- is.data.frame(data) && all(vapply(list(input$id, input$time, input$concentration),
      mapped_column, logical(1), data = data))
    common <- list(source_identity(), input$id, input$time, input$concentration, input$groups %||% character(), input$scope %||% "all")
    prepared <- prepared_mapping()
    flags <- list(if (prepared) "NCA_BLQ" else input$blq_column %||% "", if (prepared) "NCA_MDV" else input$mdv_column %||% "",
      input$flag_convention %||% "strict")
    units <- unit_context()
    values <- lapply(names(policy_values), function(key) {
      value <- input[[desktop_nca_review_fields[[key]]]]
      if (length(value) == 1L && value %in% policy_values[[key]]) value
    })
    names(values) <- names(policy_values)
    values$units <- TRUE
    list(keys = list(dosing = common, blq = c(common, flags, list(prepared)),
      c0 = c(common, flags, list(units$time, input$predose_window_h %||% NA_real_)),
      units = c(common, list(units, flags[[3]]))),
      values = values, mapped = mapped, prepared = prepared)
  })
  update_review <- function(event = "sync", choice = NULL) {
    previous <- shiny::isolate(review_state())
    context <- review_context()
    next_state <- desktop_nca_review_transition(previous, context, event, choice)
    if (!identical(previous, next_state)) review_state(next_state)
  }
  shiny::observe(update_review(), priority = 150)
  shiny::observeEvent(input$review_choice, {
    if (!loading_shared() && is.null(pending_time_conversion())) update_review(choice = input$review_choice)
  }, priority = 100)
  review_status <- shiny::reactive({
    state <- review_state()
    context <- review_context()
    stats::setNames(vapply(review_keys, function(key) desktop_nca_review_status(state, context, key), character(1)), review_keys)
  })
  review_labels <- list(unselected = c("未選択", "Unselected"), unconfirmed = c("未確認", "Unconfirmed"),
    loading = c("読み込み中", "Loading"), loaded = c("読み込み済み・要確認", "Loaded, review required"),
    confirmed = c("確認済み", "Confirmed"), applied = c("適用済み", "Applied"))
  for (key in review_keys) local({
    card <- key
    output[[paste0("review_", card)]] <- shiny::renderUI({
      state <- review_state()
      status <- review_status()[[card]]
      words <- review_labels[[status]]
      label <- if (card == "units") c("表示された単位を確認", "Confirm the displayed units") else c("表示された選択を確認", "Confirm this choice")
      button_id <- paste0("confirm_", card)
      shiny::div(`data-review-state` = status, `data-generation` = state$generation[[card]],
        `data-review-value` = review_context()$values[[card]] %||% "",
        `data-review-key` = card, `data-review-input` = session$ns(desktop_nca_review_fields[[card]]),
        `data-review-event` = session$ns("review_choice"),
        `data-review-manual-unit` = if (card == "units") unit_context()$manual,
        `data-review-manual-input` = if (card == "units") session$ns("concentration_unit"),
        shiny::span(class = "pk-nca-review-status", desktop_i18n_text(words[[1]], words[[2]])),
        if (status %in% c("unconfirmed", "loaded") && review_context()$mapped && is.null(pending_time_conversion()))
          shiny::tags$button(type = "button", class = "btn btn-default", id = session$ns(button_id), `data-nca-confirm` = "",
            desktop_i18n_text(label[[1]], label[[2]])))
    })
  })
  output$unit_summary <- shiny::renderUI({
    data <- source_data()
    if (!is.data.frame(data)) return(NULL)
    units <- unit_context()
    if (!mapped_column(units$column, data)) return(shiny::p(class = "pk-caption",
      if (nzchar(trimws(units$manual))) desktop_i18n_text("手入力の濃度単位を使います。", "Uses the manually entered concentration unit.")
      else desktop_i18n_text("単位列を選ぶか、濃度単位を入力してください。", "Choose a unit column or enter the concentration unit."),
      " ", desktop_nca_focus_link(session$ns, "unit_column", desktop_i18n_text("単位列へ", "Go to unit column"))))
    selected_rows <- rep(TRUE, nrow(data))
    if (identical(input$scope, "ss") && "SSNCAFL" %in% names(data))
      selected_rows <- tryCatch(desktop_pk_flag(data$SSNCAFL, "SSNCAFL", input$flag_convention %||% "strict"), error = function(e) selected_rows)
    values <- unique(trimws(as.character(data[[units$column]][selected_rows])))
    values <- values[!is.na(values) & nzchar(values)]
    shiny::p(class = "pk-caption", desktop_i18n_text("対応する濃度単位列", "Mapped concentration unit column"), " ", shiny::strong(units$column),
      if (length(values)) paste0(" · ", paste(head(values, 5L), collapse = " / "), if (length(values) > 5L) " …"),
      if (!length(values)) desktop_i18n_text("（範囲内に単位の記録なし）", " (no recorded units in scope)"), " ",
      desktop_nca_focus_link(session$ns, "unit_column", desktop_i18n_text("単位列へ", "Go to unit column")))
  })
  supplement_enabled <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(list(source_data(), input$concentration), {
    if (loading_shared()) return()
    supplement_enabled(FALSE)
    shiny::updateCheckboxInput(session, "predose_qc_supplement", value = FALSE)
  }, priority = 100)
  shiny::observeEvent(input$predose_qc_supplement, {
    supplement_enabled(prepared_mapping() && isTRUE(input$predose_qc_supplement))
  })
  qc_lloq_active <- shiny::reactive(isTRUE(input$predose_qc) && (!prepared_mapping() || supplement_enabled()))
  shiny::observeEvent(list(input$id, input$groups, input$concentration, input$blq_column, input$mdv_column,
    input$unit_column, input$predose_qc, supplement_enabled(), input$predose_qc_lloq_source, input$predose_qc_lloq_column), {
    data <- source_data()
    if (!is.data.frame(data) || loading_shared()) return()
    choices <- desktop_pk_time_choices(data, c(input$id, input$groups, input$concentration, input$blq_column, input$mdv_column,
      input$unit_column, if (qc_lloq_active() && identical(input$predose_qc_lloq_source, "column")) input$predose_qc_lloq_column))
    selected <- shiny::isolate(input$time)
    if (is.null(selected)) return()
    shiny::updateSelectInput(session, "time", choices = c(desktop_i18n_choices("nca-time"), choices),
      selected = if (length(selected) == 1L && selected %in% unname(choices)) selected else "")
  })
  output$time_hint <- shiny::renderUI({
    data <- source_data()
    if (is.data.frame(data) && length(input$time) == 1L && input$time %in% names(data))
      shiny::p(class = "pk-caption", desktop_i18n_text(desktop_pk_time_hint(data, input$time), desktop_pk_time_hint(data, input$time, "en")))
  })
  shiny::observeEvent(source_data(), {
    data <- source_data()
    if (!is.data.frame(data) || !is.null(shared_form()) || !"NCA_BLQ_CONTEXT" %in% names(data)) return()
    context <- tryCatch(desktop_pk_blq_context(data), error = function(e) NULL)
    if (is.null(context)) return()
    old_unit <- shiny::isolate(input$time_unit %||% "h")
    if (!identical(old_unit, "h")) {
      # The prepared context is already in hours. Switch all boundaries as one
      # update; otherwise its new origin can be misread as minutes or days.
      cache <- shiny::isolate(time_boundary_cache())
      pending <- shiny::isolate(pending_time_conversion())
      end_h <- if (!is.null(pending) && !is.null(cache$end)) cache$end$hours else
        boundary_hours("end", shiny::isolate(input$end), old_unit, cache)
      target <- list(time_origin = as.numeric(context$time_origin_h), start = as.numeric(context$time_origin_h))
      if (finite_time(end_h)) target$end <- if (!is.null(cache$end) && identical(cache$end$entered_unit, "h")) signif(cache$end$entered_value, 15) else signif(end_h, 6)
      else if (!is.null(end_h)) {
        shiny::updateNumericInput(session, "end", value = NA_real_)
        cache$end <- NULL
        notice("解析終了を時間に換算できません。値を入力し直してください。")
      }
      for (name in names(target)) {
        if (name == "end") { cache$end$hours <- end_h; cache$end$display <- as.numeric(target$end) }
        else cache[[name]] <- list(hours = as.numeric(context$time_origin_h), display = as.numeric(target[[name]]), entered_unit = "h", entered_value = as.numeric(target[[name]]))
      }
      time_boundary_cache(cache)
      previous_time_unit("h")
      attr(target, "unit") <- "h"
      pending_time_conversion(target)
      shiny::updateSelectInput(session, "time_unit", selected = "h")
      for (name in names(target)) shiny::updateNumericInput(session, name, value = target[[name]])
      return()
    }
    shiny::updateNumericInput(session, "time_origin", value = context$time_origin_h)
    shiny::updateNumericInput(session, "start", value = context$time_origin_h)
  })
  shiny::observeEvent(list(source_data(), input$time), {
    if (!is.null(shared_form())) return()
    data <- source_data()
    context <- if (is.data.frame(data) && "NCA_BLQ_CONTEXT" %in% names(data)) tryCatch(desktop_pk_blq_context(data), error = function(e) NULL) else NULL
    if ((identical(input$time, "NCA_TIME_H") && "NCA_TIME_H" %in% names(data)) ||
      (!is.null(context) && identical(input$time, context$time_column)))
      shiny::updateSelectInput(session, "time_unit", selected = "h")
  })
  previous_prepared <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(list(source_data(), input$concentration), {
    columns <- names(source_data())
    if (loading_shared()) { previous_prepared(prepared_mapping()); return() }
    if (prepared_mapping()) {
      shiny::updateSelectInput(session, "blq_column", selected = "NCA_BLQ")
      shiny::updateSelectInput(session, "mdv_column", selected = "NCA_MDV")
    } else if (previous_prepared()) {
      shiny::updateSelectInput(session, "blq_column", selected = if ("BLQ" %in% columns) "BLQ" else "")
      shiny::updateSelectInput(session, "mdv_column", selected = if ("MDV" %in% columns) "MDV" else "")
    }
    previous_prepared(prepared_mapping())
  })
  output$blq_controls <- shiny::renderUI({
    if (previous_prepared()) return(NULL)
    shiny::selectInput(session$ns("blq"), desktop_i18n_text("BLQフラグ付き行", "Rows flagged as BLQ"), desktop_i18n_choices("nca-blq"),
      selected = if (loading_shared()) selected_form("blq", "") else shiny::isolate(input$blq) %||% "", selectize = FALSE)
  })
  output$prepared_hint <- shiny::renderUI({
    if (!prepared_mapping()) return(NULL)
    context <- tryCatch(desktop_pk_blq_context(source_data()), error = function(e) NULL)
    step <- context$step
    source <- step$lloq_source %||% "constant"
    bound <- if (source == "column") shiny::tagList(desktop_i18n_text("列", "Column"), " ", step$lloq_column)
      else if (is.null(step$lloq_value) || all(is.na(step$lloq_value))) desktop_i18n_text("未指定", "Unspecified")
      else shiny::tagList(desktop_i18n_text("定数", "Constant"), " ", desktop_display_value(step$lloq_value))
    shiny::p(class = "pk-caption pk-protected", desktop_i18n_text(
      "前処理済みの NCA_CONC / NCA_BLQ / NCA_MDV の記録値を使います。BLQ規則の変更はデータ準備で再適用してください。",
      "Uses recorded values from the preprocessed NCA_CONC / NCA_BLQ / NCA_MDV columns. Reapply BLQ rules in Data Preparation to make changes."), " ",
      desktop_i18n_text("投与前QCは保存済みの前処理の条件を参照します。", "Predose QC uses the saved preprocessing settings."), " ", bound, " ",
      desktop_i18n_text("（濃度と同じ単位）", "(same unit as concentration)"),
      if (!is.null(context) && identical(as.integer(context$version), 1L) && source == "constant")
        shiny::tagList(" ", desktop_i18n_text("旧形式の定数LLOQは検証できないためQCには使用しません。", "QC cannot use a legacy constant LLOQ because it cannot be verified.")))
  })
  qc_selected <- function(name, fallback) {
    if (loading_shared()) return(selected_form(name, fallback))
    shiny::isolate(input[[name]]) %||% fallback
  }
  output$predose_qc_lloq_controls <- shiny::renderUI({
    columns <- names(source_data())
    ns <- session$ns
    controls <- shiny::tagList(desktop_workbench_select(ns("predose_qc_lloq_source"), desktop_i18n_text("投与前QCで使用するLLOQ（濃度と同じ単位）", "LLOQ for predose QC (same unit as concentration)"),
      desktop_i18n_choices("nca-predose_qc_lloq_source"), selected = qc_selected("predose_qc_lloq_source", "")),
      shiny::conditionalPanel("input.predose_qc_lloq_source === 'column'", ns = ns,
        desktop_workbench_select(ns("predose_qc_lloq_column"), desktop_i18n_text("投与前QCのLLOQ列", "LLOQ column for predose QC"),
          c("列を選択" = "", stats::setNames(columns, columns)),
          selected = qc_selected("predose_qc_lloq_column", ""))),
      shiny::p(class = "pk-caption", desktop_i18n_text("BLQの投与前値はLLOQによる上限で確認します。LLOQ未指定や不正値の場合はQCを評価できません。NCAの計算値は変わりません。", "A BLQ predose value is assessed using the LLOQ as an upper bound. QC cannot be evaluated when LLOQ is unspecified or invalid. NCA calculated values do not change.")))
    if (!prepared_mapping()) return(controls)
    shiny::tagList(shiny::checkboxInput(ns("predose_qc_supplement"), desktop_i18n_text("保存済みの前処理の条件でLLOQが未指定の行を投与前QC用に補う", "Fill missing LLOQ values for predose QC from saved preprocessing settings"), value = shiny::isolate(supplement_enabled())),
      shiny::p(class = "pk-caption", desktop_i18n_text("検証済みの前処理の条件にあるLLOQを優先します。不正値・検証不能のLLOQは補いません。BLQ処理・PK計算・前処理記録は変更しません。", "The LLOQ in validated preprocessing settings takes precedence. Invalid or unverifiable LLOQ values are not filled in. BLQ processing, PK calculation and preprocessing records are not changed.")),
      shiny::conditionalPanel("input.predose_qc_supplement === true", ns = ns, controls))
  })
  output$predose_qc_lloq_value_control <- shiny::renderUI({
    ns <- session$ns
    condition <- "input.predose_qc === true && input.predose_qc_lloq_source === 'constant'"
    if (prepared_mapping()) condition <- paste(condition, "&& input.predose_qc_supplement === true")
    shiny::conditionalPanel(condition, ns = ns,
      shiny::numericInput(ns("predose_qc_lloq_value"), desktop_i18n_text("投与前QCのLLOQ定数（濃度と同じ単位）", "Constant LLOQ for predose QC (same unit as concentration)"),
        qc_selected("predose_qc_lloq_value", NA_real_), min = 0))
  })
  nca_issue <- function(field, ja, en, blocking = TRUE, period_hint = FALSE) {
    list(field = field, message = c(ja, en), blocking = blocking, period_hint = period_hint)
  }
  positive <- function(x) finite_time(x) && x > 0
  terminal_settings_active <- shiny::reactive(!identical(input$terminal_method %||% "none", "none") || isTRUE(input$derived_enabled))
  terminal_qc_fields <- c("terminal_min_r_squared", "terminal_min_span_ratio", "aucinf_max_extrap_pct")
  run_issues <- shiny::reactive({
    issues <- list()
    add <- function(...) issues[[length(issues) + 1L]] <<- nca_issue(...)
    time_error <- time_boundary_error()
    if (!is.null(time_error)) add("end", time_error[[1]], time_error[[2]], period_hint = TRUE)
    period <- period_order_state()
    if (!is.null(period) && !period$valid)
      add("period_order_values", "期の値をすべて選択してから実行してください。", "Select every period value before running NCA.", period_hint = TRUE)
    data <- source_data()
    if (!is.data.frame(data)) {
      add("source", "濃度データを読み込んでください。", "Load concentration data.")
      return(issues)
    }
    if (loading_shared()) {
      add(NULL, "共有条件を読み込み中です。入力元と計算条件の反映を待ってください。", "Wait for the shared source and settings to finish loading.")
      return(issues)
    }
    if (!is.null(pending_time_conversion())) {
      add(NULL, "時間単位を換算中です。", "Converting time units.")
      return(issues)
    }
    if (!review_context()$mapped) {
      if (!mapped_column(input$id, data)) add("id", "被験者ID列を選択してください。", "Select a subject ID column.")
      if (!mapped_column(input$time, data)) add("time", "時間列を選択してください。", "Select a time column.")
      if (!mapped_column(input$concentration, data)) add("concentration", "濃度列を選択してください。", "Select a concentration column.")
      return(issues)
    }
    state <- review_status()
    labels <- list(dosing = c("表示された投与状況を選択・確認してください。", "Select and confirm the dosing context."),
      blq = c("行ごとのBLQ方針を選択・確認してください。", "Select and confirm the row-level BLQ policy."),
      c0 = if (state[["c0"]] == "unselected")
        c("開始点がない場合のC0の方針を選択してください。補完しない場合も明示して選択してください。", "Select a C0 policy for a missing starting value, including when you want no imputation.")
        else c("表示されたC0の方針を確認してください。", "Confirm the displayed C0 policy."),
      units = c("表示された単位を確認してください。", "Confirm the displayed units."))
    for (key in names(state)[!state %in% c("confirmed", "applied")]) {
      field <- if (state[[key]] %in% c("unconfirmed", "loaded")) paste0("confirm_", key) else desktop_nca_review_fields[[key]]
      add(field, labels[[key]][[1]], labels[[key]][[2]])
    }
    if (identical(input$c0_method, "predose") && !positive(input$predose_window_h))
      add("predose_window_h", "投与前値の最大遡及時間（h）は正の数値を指定してください。", "Enter a positive look-back time (h) for the C0 predose value.")
    if (isTRUE(input$predose_qc)) {
      if (!positive(input$predose_qc_threshold_pct))
        add("predose_qc_threshold_pct", "投与前濃度の確認閾値は正の数値を指定してください。", "Enter a positive predose QC threshold.")
      if (!positive(input$predose_qc_window_h))
        add("predose_qc_window_h", "投与前濃度を確認する最大遡及時間を指定してください。", "Enter a positive look-back time for predose QC.")
      if (qc_lloq_active()) {
        source <- input$predose_qc_lloq_source %||% ""
        if (source == "column" && !mapped_column(input$predose_qc_lloq_column, data))
          add("predose_qc_lloq_column", "投与前QCのLLOQ列を選択してください。", "Select the LLOQ column for predose QC.")
        if (source == "" || (source == "constant" && !positive(input$predose_qc_lloq_value)))
          add(if (source == "constant") "predose_qc_lloq_value" else "predose_qc_lloq_source",
            "有効なLLOQがないBLQ投与前値は、QCを評価できません。", "Predose QC cannot evaluate BLQ values without a valid LLOQ.", blocking = FALSE)
      }
    }
    if (terminal_settings_active()) {
      limits <- c(1, Inf, 100)
      labels <- list(c("最小R²を0〜1で指定してください。", "Enter a minimum R² between 0 and 1."),
        c("最小区間幅は0以上の数値を指定してください。", "Enter a nonnegative minimum span."),
        c("AUC外挿率の上限を0〜100で指定してください。", "Enter a maximum extrapolated AUC between 0 and 100 percent."))
      for (i in seq_along(terminal_qc_fields)) {
        field <- terminal_qc_fields[[i]]
        value <- input[[field]]
        if (!finite_time(value) || value < 0 || value > limits[[i]]) add(field, labels[[i]][[1]], labels[[i]][[2]])
      }
    }
    if (isTRUE(input$derived_enabled)) {
      if (!isTRUE(input$dose_meaning_confirmed))
        add("dose_meaning_confirmed", "投与量の列が計画量でなく実投与量であることを確認してください。", "Confirm that the dose column contains actual administered doses.")
      if (!mapped_column(input$dose_amount, data)) add("dose_amount", "実投与量の値列を選択してください。", "Select the actual dose value column.")
      if (!mapped_column(input$dose_unit, data)) add("dose_unit", "投与量の単位列を選択してください。", "Select the dose unit column.")
      if (!mapped_column(input$dose_route, data)) add("dose_route", "投与経路の列を選択してください。", "Select the dose route column.")
      if (!nzchar(input$dose_basis %||% "")) add("dose_basis", "投与量の基準を選択してください。", "Select the dose basis.")
      if (!nzchar(input$dose_reference %||% "")) add("dose_reference", "実投与量の対応を選択してください。", "Select the actual dose reference.")
      roles <- body_context()$roles
      bsa <- input$body_bsa_mode %||% ""
      if (isTRUE(input$body_weight) && is.null(roles$weight))
        add("body_weight", "体重を使うにはNCA入力のDM・VSで体重を採用して保存してください。", "Select and save weight in NCA Input / DM and VS before using it.")
      if (bsa == "recorded" && is.null(roles$bsa))
        add("body_bsa_mode", "記録BSAを使うにはNCA入力のDM・VSでBSAを採用して保存してください。", "Select and save BSA in NCA Input / DM and VS before using the recorded BSA.")
      if (bsa == "mosteller" && (!isTRUE(input$body_weight) || is.null(roles$height)))
        add("body_bsa_mode", "Mosteller式には採用した体重と身長が必要です。", "The Mosteller equation requires selected weight and height.")
    }
    issues
  })
  run_blockers <- shiny::reactive(Filter(function(issue) issue$blocking, run_issues()))
  localized_issue <- function(issue) issue$message[[if (identical(ui_language(), "en")) 2L else 1L]]
  assert_nca_ready <- function() {
    blockers <- run_blockers()
    if (length(blockers)) pk_assert(FALSE, localized_issue(blockers[[1]]))
  }
  output$first_use_issues <- shiny::renderUI({
    issues <- Filter(function(x) !x$period_hint, run_issues())
    if (!length(issues)) return(NULL)
    shiny::tags$ul(lapply(issues, function(x) shiny::tags$li(class = if (x$blocking) "pk-period-warning",
      desktop_i18n_text(x$message[[1]], x$message[[2]]), if (!is.null(x$field)) shiny::tagList(" ", desktop_nca_focus_link(session$ns, x$field)))))
  })
  shiny::outputOptions(output, "first_use_issues", suspendWhenHidden = FALSE)
  output$qc_summary <- shiny::renderUI({
    number <- function(x) if (finite_time(x)) desktop_display_value(x) else "—"
    shiny::tagList(
      if (terminal_settings_active()) shiny::p(class = "pk-caption",
        desktop_i18n_text("終末相のQC", "Terminal QC"), " · R² ≥ ", number(input$terminal_min_r_squared),
        " · ", desktop_i18n_text("区間幅", "Span"), " ≥ ", number(input$terminal_min_span_ratio),
        " · ", desktop_i18n_text("AUC外挿率", "Extrapolated AUC"), " ≤ ", number(input$aucinf_max_extrap_pct), "%"),
      if (isTRUE(input$predose_qc)) shiny::p(class = "pk-caption", desktop_i18n_text("投与前QC", "Predose QC"), " · ",
        number(input$predose_qc_threshold_pct), "% Cmax / ", number(input$predose_qc_window_h), " h",
        if (qc_lloq_active() && identical(input$predose_qc_lloq_source, "constant"))
          paste0(" · LLOQ ", number(input$predose_qc_lloq_value))))
  })
  output$optional_summary <- shiny::renderUI({
    parameters <- c(if (isTRUE(input$include_aucproc)) "AUCPROC", if (isTRUE(input$derived_enabled)) input$derived_parameters)
    body <- if (isTRUE(input$derived_enabled)) Filter(Negate(is.null), list(
      if (isTRUE(input$body_weight)) desktop_i18n_text("採用体重", "Selected weight"),
      if (identical(input$body_bsa_mode, "recorded")) desktop_i18n_text("記録BSA", "Recorded BSA"),
      if (identical(input$body_bsa_mode, "mosteller")) desktop_i18n_text("Mosteller式", "Mosteller equation")))
    shiny::tagList(
      if (length(parameters)) shiny::p(class = "pk-caption", desktop_i18n_text("追加出力", "Additional outputs"), " · ", paste(parameters, collapse = ", ")),
      if (length(body)) shiny::p(class = "pk-caption", desktop_i18n_text("体格補正", "Body-size adjustment"), " · ", htmltools::tagList(body)))
  })
  output$advanced_changes <- shiny::renderUI({
    defaults <- c(desktop_nca_terminal_defaults()[terminal_qc_fields], list(predose_qc_threshold_pct = 5))
    numeric_changed <- any(vapply(names(defaults), function(field) !is.null(input[[field]]) && !isTRUE(input[[field]] == defaults[[field]]), logical(1)))
    changed <- c(additional = isTRUE(input$include_aucproc) || isTRUE(input$derived_enabled),
      body = isTRUE(input$body_weight) || nzchar(input$body_bsa_mode %||% ""),
      qc = numeric_changed || finite_time(input$predose_qc_window_h) || finite_time(input$predose_qc_lloq_value),
      ai = isTRUE(shared_state()$disclosure$allowed) || isTRUE(input$disclosure_ack),
      adpp = isTRUE(input$adpp_manual_order) || isTRUE(input$adpp_use_mapping))
    if (!any(changed)) return(NULL)
    labels <- desktop_nca_advanced_labels()[names(changed)[changed]]
    shiny::tagList(desktop_i18n_text("変更あり", "Changed"), " · ",
      desktop_i18n_text(paste(vapply(labels, `[[`, "", 1L), collapse = "・"), paste(vapply(labels, `[[`, "", 2L), collapse = ", ")))
  })
  specification <- shiny::reactive({
    shiny::req(is.null(pending_time_conversion()))
    input_time_unit <- input$time_unit %||% "h"
    blq <- input$blq %||% ""
    spec <- list(id = input$id, time = input$time, concentration = input$concentration,
    groups = as.list(input$groups), time_unit = input$time_unit %||% "h", concentration_unit = input$concentration_unit %||% "",
    unit_column = input$unit_column %||% "", blq_column = if (prepared_mapping()) "NCA_BLQ" else input$blq_column %||% "",
    mdv_column = if (prepared_mapping()) "NCA_MDV" else input$mdv_column %||% "",
    scope = input$scope %||% "all", time_origin = boundary_hours("time_origin", input$time_origin %||% 0, input_time_unit), start = boundary_hours("start", input$start %||% 0, input_time_unit),
    end = if (isTRUE(input$last_observation)) NULL else boundary_hours("end", input$end, input_time_unit),
    c0_method = input$c0_method %||% "", predose_window_h = if (identical(input$c0_method, "predose")) input$predose_window_h else NULL,
    all_blq_policy = input$all_blq_policy %||% "missing",
    predose_qc_threshold_pct = if (isTRUE(input$predose_qc)) input$predose_qc_threshold_pct else NULL,
    predose_qc_window_h = if (isTRUE(input$predose_qc)) input$predose_qc_window_h else NULL,
    predose_qc_lloq_source = if (qc_lloq_active()) input$predose_qc_lloq_source %||% "" else "",
    predose_qc_lloq_column = if (qc_lloq_active() && identical(input$predose_qc_lloq_source, "column")) input$predose_qc_lloq_column %||% "" else "",
    predose_qc_lloq_value = if (qc_lloq_active() && identical(input$predose_qc_lloq_source, "constant")) input$predose_qc_lloq_value else NULL,
    flag_convention = input$flag_convention %||% "strict", include_aucproc = isTRUE(input$include_aucproc),
      auc_method = input$auc_method %||% "lin up/log down", blq = if (prepared_mapping()) "as_recorded" else if (identical(blq, "")) "exclude" else blq)
    role_entries <- lapply(input$groups %||% character(), function(column) {
      role <- input[[group_role_field(column)]] %||% ""
      if (!nzchar(role)) return(NULL)
      list(column = column, role = role)
    })
    role_entries <- Filter(Negate(is.null), role_entries)
    if (length(role_entries)) spec$group_roles <- role_entries
    if (isTRUE(input$manual_period_order)) {
      period_columns <- vapply(Filter(function(x) identical(x$role, "period"), role_entries), `[[`, "", "column")
      values <- input$period_order_values %||% character()
      if (!is.character(values) || anyNA(values)) values <- character()
      spec$period_order <- list(column = if (length(period_columns) == 1L) period_columns[[1]] else "", values = as.list(values))
    }
    terminal_method <- input$terminal_method %||% "none"
    form <- shared_form()
    if (!identical(terminal_method, "none") || isTRUE(input$derived_enabled)) {
      terminal <- desktop_nca_terminal_defaults()
      for (name in names(terminal)) if (!is.null(input[[name]])) terminal[name] <- list(input[[name]])
      if (identical(terminal$dosing_context, "")) terminal$dosing_context <- "ongoing"
      if (!identical(terminal_method, "manual")) terminal[c("terminal_start_h", "terminal_end_h")] <- list(NULL, NULL)
      spec <- c(spec, terminal)
    } else if (!is.null(form) && identical(form$specification$terminal_method, "none")) {
      fields <- intersect(names(desktop_nca_terminal_defaults()), names(form$specification))
      spec <- c(spec, form$specification[fields])
    }
    if (!is.null(form)) {
      accepted <- form$specification
      if (!identical(input$c0_method, "predose") && identical(input$c0_method, accepted$c0_method))
        spec["predose_window_h"] <- accepted["predose_window_h"]
      if (!isTRUE(input$predose_qc) && is.null(accepted$predose_qc_threshold_pct)) {
        for (name in c("predose_qc_window_h", "predose_qc_lloq_source", "predose_qc_lloq_column", "predose_qc_lloq_value"))
          spec[name] <- accepted[name]
      }
    }
    controls <- stats::setNames(lapply(names(desktop_nca_derived_form(list())), function(name) input[[name]]), names(desktop_nca_derived_form(list())))
    c(spec, desktop_nca_derived_form_spec(controls, source_data()))
  })
  apply_shared_controls <- function(form) {
    selectors <- c("id", "time", "concentration", "groups", "unit_column", "blq_column", "mdv_column", "scope",
      "time_unit", "c0_method", "auc_method", "flag_convention", "all_blq_policy", "blq",
      "predose_qc_lloq_source", "predose_qc_lloq_column", "terminal_method", "dosing_context", "washout_attestation", "terminal_qc_action")
    numbers <- c("time_origin", "start", "end", "predose_window_h", "predose_qc_threshold_pct", "predose_qc_window_h", "predose_qc_lloq_value", "infusion_end_h", "terminal_start_h", "terminal_end_h",
      "terminal_min_r_squared", "terminal_min_span_ratio", "aucinf_max_extrap_pct")
    previous_time_unit(form$controls$time_unit %||% "h")
    pending_time_conversion(NULL)
    form_unit <- form$controls$time_unit %||% "h"
    form_cache <- list()
    for (name in c("time_origin", "start", "end")) if (finite_time(form$controls[[name]]))
      form_cache[[name]] <- list(hours = form$controls[[name]], display = if (identical(form_unit, "h")) signif(form$controls[[name]], 15) else signif(desktop_nca_ui_from_hours(form$controls[[name]], form_unit), 6), entered_unit = "h", entered_value = form$controls[[name]])
    time_boundary_cache(form_cache)
    for (name in selectors) shiny::updateSelectInput(session, name, selected = if (name == "groups") form$controls[[name]] %||% character() else form$controls[[name]])
    for (name in numbers) shiny::updateNumericInput(session, name, value = if (name %in% names(form_cache)) form_cache[[name]]$display else form$controls[[name]])
    for (name in c("last_observation", "include_aucproc", "predose_qc", "predose_qc_supplement", "dose_history_confirmed"))
      shiny::updateCheckboxInput(session, name, value = form$controls[[name]])
    shiny::updateTextInput(session, "concentration_unit", value = form$controls$concentration_unit)
    entries <- form$specification$group_roles %||% list()
    for (column in form$controls$groups %||% character()) {
      found <- Filter(function(x) identical(x$column, column), entries)
      shiny::updateSelectInput(session, group_role_field(column), selected = if (length(found)) found[[1]]$role else "")
    }
    manual <- form$specification$period_order %||% NULL
    shiny::updateCheckboxInput(session, "manual_period_order", value = !is.null(manual))
    shiny::updateSelectizeInput(session, "period_order_values", selected = if (is.null(manual)) character() else unlist(manual$values, use.names = FALSE))
    for (name in c("derived_parameters", "dose_amount", "dose_unit", "dose_route", "dose_basis", "dose_reference", "dose_ev_values", "body_bsa_mode"))
      shiny::updateSelectInput(session, name, selected = form$controls[[name]])
    for (name in c("derived_enabled", "dose_meaning_confirmed", "body_weight")) shiny::updateCheckboxInput(session, name, value = form$controls[[name]])
    supplement_enabled(form$controls$predose_qc_supplement)
  }
  shiny::observeEvent(input$shared_refresh, {
    if (!identical(engine$mode, "nca")) return()
    tryCatch({
      state <- desktop_nca_get(engine)
      pk_assert(!is.null(state$source), "共有条件はまだありません。画面で条件を選び、NCAを実行してください。")
      spec <- state$specification
      controls <- c(utils::modifyList(desktop_nca_terminal_defaults(), spec, keep.null = TRUE), desktop_nca_derived_form(spec))
      controls$terminal_start_h <- controls$terminal_start_h %||% NA_real_
      controls$terminal_end_h <- controls$terminal_end_h %||% NA_real_
      controls$groups <- unlist(spec$groups, use.names = FALSE) %||% character()
      controls$last_observation <- is.null(spec$end)
      controls$predose_qc <- !is.null(spec$predose_qc_threshold_pct)
      controls$predose_qc_supplement <- nzchar(spec$predose_qc_lloq_source)
      for (name in c("end", "predose_window_h", "predose_qc_threshold_pct", "predose_qc_window_h", "predose_qc_lloq_value"))
        if (is.null(controls[[name]])) controls[[name]] <- NA_real_
      update_review("load_begin")
      current(NULL); saved(NULL); loading_shared(TRUE)
      form <- list(source = state$source, specification = spec, controls = controls)
      loaded_form(form); shared_revision(state$revision)
      shiny::updateSelectInput(session, "source", selected = state$source)
      apply_shared_controls(form)
      notice("共有条件を読み込み中です。画面の入力元・列・計算条件を確認してください。")
    }, error = function(e) notice(paste("読み込みできませんでした：", conditionMessage(e))))
  }, priority = 300)
  shiny::observeEvent(list(input$source, if (identical(input$source, "FILE")) input$file$datapath else NULL), {
    form <- loaded_form()
    if (is.null(form) || !identical(input$source, form$source)) {
      for (name in c("derived_enabled", "dose_meaning_confirmed", "body_weight")) shiny::updateCheckboxInput(session, name, value = FALSE)
      shiny::updateCheckboxInput(session, "manual_period_order", value = FALSE)
      shiny::updateSelectizeInput(session, "period_order_values", selected = character())
      for (name in c("derived_parameters", "dose_amount", "dose_unit", "dose_route", "dose_basis", "dose_reference", "dose_ev_values", "body_bsa_mode")) shiny::updateSelectInput(session, name, selected = character())
    }
    if (is.null(form)) return()
    if (loading_shared() && identical(input$source, form$source)) apply_shared_controls(form)
    else if (!identical(input$source, form$source)) {
      loaded_form(NULL)
      if (loading_shared()) { loading_shared(FALSE); update_review("load_cancel") }
    }
  }, ignoreInit = TRUE)
  fingerprint <- shiny::reactive({
    spec <- tryCatch(specification(), error = function(e) NULL)
    if (is.null(spec)) return(NULL)
    desktop_hash(list(source = input$source, file = if (identical(input$source, "FILE")) input$file$name else NULL,
      data = source_data(), specification = spec))
  })
  result <- shiny::reactive({
    value <- current()
    if (is.null(value) || loading_shared() || !identical(value$fingerprint, fingerprint())) return(NULL)
    if (identical(engine$mode, "nca")) {
      engine$nca_signal()
      if (!identical(as.numeric(value$revision), as.numeric(engine$nca$revision)) ||
          !desktop_nca_current(engine, list(hash = value$shared_hash))) return(NULL)
    }
    value
  })
  warning_context <- shiny::reactive({
    value <- result()
    if (is.null(value)) return(NULL)
    desktop_nca_warning_context(value$value)
  })
  draft_matches_shared <- shiny::reactive({
    if (!identical(engine$mode, "nca")) return(FALSE)
    engine$nca_signal()
    if (!identical(input$source, engine$nca$source)) return(FALSE)
    tryCatch(identical(desktop_hash(desktop_nca_specification(source_data(), specification())),
      desktop_hash(engine$nca$specification)), error = function(e) FALSE)
  })
  shiny::observe({
    if (!identical(engine$mode, "nca")) return()
    engine$nca_signal()
    pending <- shiny::isolate(pending_gui_run())
    state <- desktop_nca_get(engine)
    if (!is.null(pending) && identical(state$job$job_id, pending$job_id) && state$job$status %in% c("succeeded", "failed", "cancelled")) {
      pending_gui_run(NULL)
      if (identical(state$job$status, "succeeded") && identical(pending$fingerprint, fingerprint())) {
        r <- engine$nca_results[[state$job$result_id]]
        if (!is.null(r) && desktop_nca_current(engine, r)) {
          current(list(value = r$value, input = r$input, fingerprint = pending$fingerprint, revision = pending$revision,
            shared_hash = r$hash, shared_result_id = r$id))
          scroll_to_results()
          notice("計算しました。濃度推移・結果・行別処理記録を確認して保存してください。")
        }
      } else if (identical(state$job$status, "failed")) {
        notice("計算できませんでした。入力と条件を確認して再実行してください。")
      }
    }
    matches <- draft_matches_shared()
    if (loading_shared() && matches) {
      loading_shared(FALSE)
      update_review("load_complete")
      notice("共有条件を読み込みました。濃度推移・結果・計算条件を確認できます。")
    }
    if (!matches || !identical(as.numeric(shared_revision()), as.numeric(engine$nca$revision))) return()
    if (is.null(state$result_id)) return()
    r <- engine$nca_results[[state$result_id]]
    existing <- shiny::isolate(current())
    if (!is.null(existing) && identical(existing$shared_result_id, r$id) && identical(existing$fingerprint, fingerprint())) return()
    current(list(value = r$value, input = r$input, fingerprint = fingerprint(), revision = engine$nca$revision,
      shared_hash = r$hash, shared_result_id = r$id))
    saved(NULL)
  })
  output$preview <- shiny::renderUI({
    data <- source_data()
    if (is.data.frame(data)) {
      first <- intersect(c(input$id, input$time, input$concentration, input$blq_column, input$mdv_column, "NCA_BLQ_RULE"), names(data))
      desktop_display_table(head(data[c(first, setdiff(names(data), first))], 10))
    }
  })
  output$notice <- shiny::renderUI({
    data <- source_data()
    if (inherits(data, "nca_input_error")) return(desktop_ui_notice(paste("読み込めませんでした：", data), "error"))
    if (!is.null(current()) && is.null(result())) return(desktop_ui_notice("入力または条件が変わりました。NCAを再実行してください。", "warning"))
    message <- notice()
    if (identical(message, "入力データと計算条件を選び、「NCAを実行」を押してください。"))
      message <- desktop_i18n_text(message, "Select the input data and calculation settings, then run NCA.")
    if (identical(message, "計算しました。濃度推移・結果・行別処理記録を確認して保存してください。"))
      message <- desktop_i18n_text(message, "NCA completed. Review the concentration profiles, results, and row-level processing records, then save.")
    desktop_ui_notice(message, if (is.character(message) && grepl("できませんでした", message)) "error" else "info")
  })
  # Re-rendering a disabled button can reset its client counter to zero.
  # Only increasing counters represent a user action, not input binding updates.
  last_run_count <- 0
  last_publish_count <- 0
  shiny::observeEvent(list(input$run, input$shared_publish), {
    run_count <- input$run %||% 0
    publish_count <- input$shared_publish %||% 0
    if (!is.numeric(run_count) || length(run_count) != 1L || !is.finite(run_count)) run_count <- 0
    if (!is.numeric(publish_count) || length(publish_count) != 1L || !is.finite(publish_count)) publish_count <- 0
    triggered <- run_count > last_run_count || publish_count > last_publish_count
    last_run_count <<- run_count
    last_publish_count <<- publish_count
    if (!triggered) return()
    tryCatch({
      pk_assert(!shared_busy(), "NCAを計算中です。完了後に実行してください。")
      if (identical(engine$mode, "nca")) desktop_nca_revision(engine, shared_revision(), engine$nca$project_id)
      assert_nca_ready()
      current(NULL); saved(NULL)
      data <- source_data()
      spec <- specification()
      run_fingerprint <- fingerprint()
      if (identical(engine$mode, "nca")) {
        spec <- desktop_nca_specification(data, spec)
        source <- input$source
        if (identical(source, "FILE")) {
          accepted_source <- engine$nca$source
          same_input <- !is.null(accepted_source) && identical(desktop_hash(desktop_table_get(engine, accepted_source)), desktop_hash(data))
          source <- if (same_input) accepted_source else desktop_table_save(engine, "NCA input", list(source = data), list(kind = "prepare", steps = list()))$id
        }
        args <- list(project_id = engine$nca$project_id, base_revision = shared_revision(), source = source,
          specification = spec, actor = "local user", reason = "Run GUI conditions")
        state <- desktop_nca_configure(engine, args, "ui"); shared_revision(state$revision)
        args$base_revision <- state$revision
        job <- desktop_nca_start_job(engine, args, "ui")
        pending_gui_run(list(job_id = job$job_id, fingerprint = run_fingerprint, revision = state$revision))
        notice("NCAを計算中です。完了すると結果を更新します。")
      } else {
        value <- shiny::withProgress(message = "NCAを計算しています", value = .5, pkapp_nca(data, spec))
        current(list(value = value, input = data, fingerprint = run_fingerprint))
        scroll_to_results()
      }
      if (!identical(engine$mode, "nca")) notice("計算しました。濃度推移・結果・行別処理記録を確認して保存してください。")
    }, error = function(e) notice(paste(if (identical(ui_language(), "en")) "Calculation failed." else "計算できませんでした：", conditionMessage(e))))
  })
  output$results <- shiny::renderUI({
    value <- result()
    if (is.null(value)) return(desktop_ui_empty(desktop_i18n_text("計算結果はまだありません", "No calculation results yet"), desktop_i18n_text("解析単位と開始点の扱いを確認して実行してください。", "Check the analysis unit and starting-point handling, then run NCA.")))
    ns <- session$ns
    warning_overview <- desktop_ui_notice(desktop_nca_warnings(value$value, warning_context()), "warning")
    if (!is.null(warning_overview)) warning_overview <- htmltools::tagAppendAttributes(warning_overview, class = "pk-warning-overview")
    shiny::tagList(desktop_ui_metrics(c("入力行" = value$value$counts[["input"]], "使用行" = value$value$counts[["used"]],
      "除外行" = value$value$counts[["excluded"]], "解析単位" = value$value$counts[["curves"]], "C0補完" = value$value$counts[["imputed"]]),
      translated_labels = c("入力行" = "Input rows", "使用行" = "Rows used", "除外行" = "Rows excluded", "解析単位" = "Analysis units", "C0補完" = "Imputed C0")),
      warning_overview,
      desktop_ui_section(NULL, desktop_i18n_text("濃度推移と最終定量時点", "Concentration profile and last quantifiable time"), shiny::uiOutput(ns("profile_selector")),
        shiny::uiOutput(ns("selected_warning"), class = "pk-selected-warning-output"),
        shiny::radioButtons(ns("plot_scale"), desktop_i18n_text("濃度軸の表示", "Concentration axis scale"), choiceNames = list(desktop_i18n_text("線形", "Linear"), desktop_i18n_text("片対数", "Semi-log")), choiceValues = list("linear", "log"), selected = "linear", inline = TRUE),
        shiny::plotOutput(ns("plot"), height = "350px"), shiny::uiOutput(ns("profile_quality")), shiny::uiOutput(ns("plot_note"))),
      desktop_ui_section(NULL, desktop_i18n_text("NCA結果", "NCA results"),
        shiny::p(class = "pk-caption", desktop_i18n_text("時間は投与基準からのh、濃度単位は", "Times are hours from the reference dose; concentration unit: "), value$value$trace$concentration_unit,
          desktop_i18n_text("、AUCは h*濃度単位です。終末相を有効にした場合、LAMZは1/h、LAMZHLはh、AUCPEOは%、AUCIFOは実測Clastに基づきます。AUCLSTの終点はTLAST_QUANT_Hです。全点BLQで方針により0を割り当てた値には定量終点がありません。表示は有効数字4桁、CSVは全精度です。", ". AUC uses h × concentration unit. If terminal analysis is enabled, LAMZ uses 1/h, LAMZHL uses h, AUCPEO uses %, and AUCIFO uses observed Clast. AUCLST ends at TLAST_QUANT_H. Policy-assigned zeros for all-BLQ profiles have no quantifiable endpoint. The display uses four significant digits; CSV retains full precision.")),
        shiny::tabsetPanel(id = ns("result_view"),
          shiny::tabPanel(desktop_i18n_text("解析単位ごと", "By analysis unit"), value = "by_unit", shiny::div(class = "pk-table-scroll", shiny::uiOutput(ns("wide_table")))),
          shiny::tabPanel(desktop_i18n_text("解析単位の詳細", "Analysis unit details"), value = "unit_details", shiny::uiOutput(ns("profile_details"))),
          shiny::tabPanel(desktop_i18n_text("縦持ち結果", "Long-form results"), value = "long_results", shiny::div(class = "pk-table-scroll", shiny::uiOutput(ns("table")))),
          shiny::tabPanel(desktop_i18n_text("行別の処理", "Row-level processing"), value = "row_processing", shiny::div(class = "pk-table-scroll", shiny::uiOutput(ns("audit_table")))),
          shiny::tabPanel(desktop_i18n_text("計算条件", "Calculation settings"), value = "settings", shiny::uiOutput(ns("conditions"))))))
  })
  output$profile_selector <- shiny::renderUI({
    shiny::req(result())
    groups <- result()$value$trace$groups
    labels <- vapply(groups, function(x) paste(paste(names(x$identity), unlist(x$identity), sep = "="), collapse = " / "), character(1))
    choices <- stats::setNames(vapply(groups, function(x) as.character(x$profile), character(1)), labels)
    selected <- shiny::isolate(input$profile)
    desktop_workbench_select(session$ns("profile"), desktop_i18n_text("表示する解析単位", "Analysis unit to display"), choices,
      selected = if (length(selected) == 1L && selected %in% choices) selected else choices[[1]])
  })
  selected_profile <- shiny::reactive({
    shiny::req(result())
    profiles <- vapply(result()$value$trace$groups, function(x) x$profile, integer(1))
    selected <- suppressWarnings(as.integer(input$profile))
    if (length(selected) == 1L && !is.na(selected) && selected %in% profiles) selected else profiles[1]
  })
  plot_scale <- shiny::reactive(if (identical(input$plot_scale, "log")) "log" else "linear")
  output$plot <- shiny::renderPlot({
    shiny::req(result())
    desktop_nca_plot(result()$value, selected_profile(), plot_scale(), ui_language())
  }, res = 96, execOnResize = TRUE)
  output$profile_quality <- shiny::renderUI({
    shiny::req(result())
    group <- Filter(function(x) x$profile == selected_profile(), result()$value$trace$groups)[[1]]
    shiny::p(class = "pk-caption pk-profile-counts", desktop_i18n_text(
      sprintf("この解析単位：入力 %s 行 / 時間欠測 %s 行 / 濃度欠測 %s 行 / 除外 %s 行。両方欠測の行は各欠測件数に含みます。",
        group$n_input, group$n_missing_time, group$n_missing_concentration, group$n_excluded),
      sprintf("This analysis unit: input %s rows / missing time %s rows / missing concentration %s rows / excluded %s rows. Rows missing both are counted in both missing-value counts.",
        group$n_input, group$n_missing_time, group$n_missing_concentration, group$n_excluded)))
  })
  output$profile_details <- shiny::renderUI({
    shiny::req(result())
    value <- result()$value
    group <- Filter(function(x) x$profile == selected_profile(), value$trace$groups)[[1]]
    keys <- unlist(value$trace$output_key_mapping, use.names = FALSE)
    selected <- rep(TRUE, nrow(value$data))
    for (i in seq_along(keys)) selected <- selected & as.character(value$data[[keys[i]]]) == as.character(group$identity[[i]])
    columns <- c("PARAMCD", "PARAM", "AVAL", "AVALU", "PARAM_STATUS", "NCA_EXCLUDE")
    assigned <- value$data$PARAMCD[selected & value$data$PARAM_STATUS == "policy_assigned"]
    qc <- group$predose_qc
    shiny::tagList(shiny::p(class = "pk-caption", paste(paste(names(group$identity), unlist(group$identity), sep = "="), collapse = " / ")),
      desktop_nca_dose_details(group),
      desktop_nca_derived_details(group),
      desktop_nca_terminal_details(value, selected_profile()),
      if (!is.null(qc) && qc$status != "disabled") {
        bound <- if (is.na(qc$concentration) && is.finite(qc$lloq %||% NA_real_) && !qc$reason %in% c("unit_mismatch", "missing_unit", "unverified_lloq")) paste0("BLQの上限：濃度 < ", desktop_display_value(qc$lloq), " ", value$trace$concentration_unit,
          if (is.finite(qc$pct_upper_bound)) paste0("、投与前値 / Cmax × 100 < ", desktop_display_value(qc$pct_upper_bound), "%。") else "。比の上限は算出できません。") else ""
        metadata <- list("状態" = desktop_status_badge(qc$status), "確認理由" = desktop_nca_diagnostics(qc$reason),
          "閾値（%）" = desktop_display_value(qc$threshold_pct),
          "最大遡及時間（h）" = desktop_display_value(value$specification$predose_qc_window_h),
          "確認対象の元行" = desktop_display_value(qc$source_row), "投与基準からの時間（h）" = desktop_display_value(qc$time_h),
          "実測濃度" = desktop_display_value(qc$concentration), "Cmax" = desktop_display_value(qc$cmax),
          "実測の投与前値 / Cmax（%）" = desktop_display_value(qc$pct), "LLOQ" = desktop_display_value(qc$lloq),
          "LLOQの参照元" = desktop_nca_lloq_origin(qc$lloq_source, identical(value$specification$concentration, "NCA_CONC")),
          "投与前値/Cmaxの上限（%）" = if (is.finite(qc$pct_upper_bound)) paste("<", desktop_display_value(qc$pct_upper_bound)) else desktop_display_value(qc$pct_upper_bound))
        shiny::div(class = "pk-profile-qc",
          shiny::h4("投与前濃度の確認"),
          if (nzchar(bound)) desktop_ui_notice(bound, if (qc$status %in% c("pass", "pass_blq")) "info" else "warning"),
          shiny::tags$details(class = "pk-profile-qc-method",
            shiny::tags$summary("確認対象の選び方"),
            shiny::p(desktop_i18n_text("解析対象内で元のMDVによる除外がなく、時刻が有効な投与前行のうち、遡及時間内で最新の元行を確認します。", "Among predose source rows in scope that are not excluded by the original MDV flag and have valid times, the latest row within the look-back window is assessed."),
              "それより前に定量可能な値があっても、最新行を確認します。この確認では自動除外しません。")),
          shiny::tags$dl(class = "pk-profile-qc-metadata", lapply(names(metadata), function(label)
            shiny::div(shiny::tags$dt(label), shiny::tags$dd(metadata[[label]])))))
      },
      shiny::p(class = "pk-caption", paste0("全点BLQの方針：", if (value$specification$all_blq_policy == "zero") "Cmax・AUCを0にする" else "欠測にする",
        if (length(assigned)) paste0("。方針による値：", paste(assigned, collapse = ", ")) else "。方針による値はありません",
        "。BLQ上限によるQCでは実測濃度・実測比はNAのまま記録します。")),
      shiny::div(class = "pk-table-scroll", desktop_display_table(value$data[selected, intersect(columns, names(value$data)), drop = FALSE], bilingual_system_columns = c("PARAM_STATUS", "NCA_EXCLUDE"))))
  })
  output$plot_note <- shiny::renderUI({
    shiny::req(result())
    rows <- desktop_nca_plot_data(result()$value, selected_profile(), plot_scale())
    group <- Filter(function(x) x$profile == selected_profile(), result()$value$trace$groups)[[1]]
    shiny::tagList(shiny::p(class = "pk-caption", desktop_i18n_text("状態：", "Status: "), desktop_i18n_diagnostic(group$status, desktop_rule_label(group$status)),
      desktop_i18n_text(" / 最終定量時点：", " / Last quantifiable time: "), desktop_display_value(desktop_nca_display_tlast(group)),
      desktop_i18n_text(" h。数値として描画できない行：", " h. Rows not plotted: "), sum(!rows$PLOT_VALID),
      desktop_i18n_text(" 行。<LLOQ等を実測値0として描きません。", " rows. Values such as <LLOQ are not plotted as observed zero.")),
      if (plot_scale() == "log") shiny::p(class = "pk-caption", desktop_i18n_text("片対数表示では0以下の濃度 ", "In the semi-log plot, "), sum(rows$PLOT_NONPOSITIVE), desktop_i18n_text(" 点を描画しません。計算値・行別記録は変更していません。", " non-positive concentrations are not plotted. Calculated values and row-level records are unchanged.")),
      shiny::p(class = "pk-caption", desktop_i18n_text("線はAUCに使用する観測点を結びます。開始点不足などでAUCが欠測の場合、線だけで積分成立を示すものではありません。", "The line connects observations used for AUC. If AUC is missing, for example because the starting point is unavailable, the line alone does not establish that integration was valid.")))
  })
  output$selected_warning <- shiny::renderUI({
    shiny::req(result())
    desktop_nca_selected_warnings(result()$value, selected_profile(), warning_context())
  })
  output$wide_table <- shiny::renderUI({
    shiny::req(result()); value <- result()$value; summary <- desktop_nca_summary(value, with_diagnostics = TRUE)
    warning_parts <- head(attr(summary, "nca_warning_parts"), 100L); display <- head(summary, 100)
    for (code in intersect(names(display), unique(value$data$PARAMCD))) {
      units <- unique(value$data$AVALU[value$data$PARAMCD == code]); units <- units[!is.na(units) & nzchar(units)]
      if (length(units)) names(display)[names(display) == code] <- paste0(code, " (", paste(units, collapse=" / "), ")")
    }
    names(display)[names(display) == "TLAST_QUANT_H"] <- "TLAST_QUANT_H (h)"
    desktop_display_table(display, bilingual_system_columns = c("PROFILE_STATUS", "NCA_WARNING"), diagnostic_parts = list(NCA_WARNING = warning_parts))
  })
  output$table <- shiny::renderUI({ shiny::req(result()); desktop_display_table(head(desktop_nca_display_data(result()$value), 100), bilingual_system_columns = c("PROFILE_STATUS", "PARAM_STATUS", "NCA_WARNING", "NCA_EXCLUDE")) })
  output$audit_table <- shiny::renderUI({
    shiny::req(result())
    rows <- result()$value$trace$rows
    columns <- unique(c(unlist(result()$value$trace$output_key_mapping), "ROW", "TIME_H", "CONC_ORIGINAL", "CONC_USED",
      "BLQ_RULE", "BLQ_POSITION", "USE_AUC", "IMPUTED", "TERMINAL_ELIGIBLE", "USE_TERMINAL", "TERMINAL_REASON", "STATUS", "REASON"))
    desktop_display_table(head(rows[rows$PROFILE == selected_profile(), intersect(columns, names(rows)), drop = FALSE], 100), bilingual_system_columns = c("BLQ_RULE", "BLQ_POSITION", "STATUS", "REASON"))
  })
  output$conditions <- shiny::renderUI({
    shiny::req(result())
    shiny::tagList(desktop_nca_condition_table(result()$value$specification),
      shiny::p(class = "pk-caption", desktop_i18n_text("全条件・処理記録・PKNCAオプションは計算条件JSONから出力できます。", "All settings, processing records and PKNCA options can be exported in the calculation-settings JSON.")))
  })
  output$actions <- shiny::renderUI({
    button <- shiny::actionButton(session$ns("save"), desktop_i18n_text("作業表を保存", "Save working table"), class = "btn-default")
    if (is.null(result()) || shared_busy()) button$attribs$disabled <- "disabled"
    button
  })
  output$run_state <- shiny::renderText({
    en <- identical(ui_language(), "en")
    if (!is.null(pending_time_conversion())) return(if (en) "Converting time units" else "時間単位を換算中")
    if (shared_busy()) return(if (identical(shared_state()$job$channel, "mcp")) if (en) "AI is running NCA" else "AIがNCAを計算中" else if (en) "NCA in progress" else "NCAを計算中")
    if (is.null(result())) if (is.null(current())) if (en) "Not calculated" else "未計算" else if (en) "Settings changed; rerun required" else "条件変更・再実行が必要"
    else {
      review <- any(vapply(result()$value$trace$groups, function(x) x$status != "ok", logical(1)))
      if (en) paste0(if (is.null(saved())) "Calculated" else "Saved", if (review) " / Review required" else "", if (is.null(saved())) " / Unsaved" else "")
      else paste0(if (is.null(saved())) "計算済み" else "保存済み", if (review) "・要確認" else "", if (is.null(saved())) "・未保存" else "")
    }
  })
  output$exports <- shiny::renderUI({
    if (is.null(result())) return(shiny::p(class = "pk-caption", desktop_i18n_text("計算後にCSV・JSONを出力できます。", "CSV and JSON exports are available after calculation.")))
    shiny::div(class = "pk-actions", shiny::downloadButton(session$ns("download"), desktop_i18n_text("結果CSV", "Results CSV")),
      shiny::downloadButton(session$ns("audit_download"), desktop_i18n_text("行別処理CSV", "Row-level processing CSV")), shiny::downloadButton(session$ns("settings_download"), desktop_i18n_text("計算条件JSON", "Calculation settings JSON")))
  })
  adpp_analyte_columns <- shiny::reactive({
    if (is.null(result())) return(character())
    roles <- result()$value$specification$group_roles %||% list()
    vapply(Filter(function(x) identical(x$role, 'analyte'),roles), `[[`, '', 'column')
  })
  output$adpp_order_controls <- shiny::renderUI({
    if (isTRUE(input$adpp_use_mapping)) return(NULL)
    columns <- adpp_analyte_columns()
    if (!length(columns)) return(NULL)
    selected <- shiny::isolate(input$adpp_order_column) %||% columns[[1]]
    if (!selected %in% columns) selected <- columns[[1]]
    shiny::tagList(
      shiny::checkboxInput(session$ns('adpp_manual_order'), '分析対象の仮番号の順序を指定する', FALSE),
      shiny::conditionalPanel('input.adpp_manual_order',ns=session$ns,
        shiny::selectInput(session$ns('adpp_order_column'),'順序を指定する分析対象列',choices=columns,selected=selected),
        shiny::uiOutput(session$ns('adpp_order_values_controls')),
        shiny::uiOutput(session$ns('adpp_order_progress')),
        shiny::p(class='pk-caption','すべての値を希望する番号順に選択します。番号はこの出力の仮番号です。検体など他の列の組合せによって連番が増えるため、対応表を確認してください。')))
  })
  output$adpp_order_values_controls <- shiny::renderUI({
    shiny::req(result(), input$adpp_order_column)
    column <- input$adpp_order_column
    if (!column %in% adpp_analyte_columns()) return(NULL)
    key <- unname(unlist(result()$value$trace$output_key_mapping[column],use.names=FALSE))
    data <- result()$value$data
    if (length(key)!=1L || !key %in% names(data)) return(NULL)
    values <- unique(as.character(data[[key]]))
    values <- values[!is.na(values) & nzchar(trimws(values))]
    values <- values[order(enc2utf8(values),method='radix')]
    chosen <- shiny::isolate(input$adpp_order_values) %||% character()
    chosen <- chosen[chosen %in% values]
    options <- c(chosen,values[!values %in% chosen])
    shiny::selectizeInput(session$ns('adpp_order_values'),paste0(column,' の値を仮番号順に選択'),
      choices=stats::setNames(options,period_label(options)),selected=chosen,multiple=TRUE,
      options=list(plugins=list('remove_button'),placeholder='すべての値を希望する順に選択'))
  })
  output$adpp_order_progress <- shiny::renderUI({
    if (!isTRUE(input$adpp_manual_order) || is.null(result())) return(NULL)
    column <- input$adpp_order_column %||% ''
    if (!column %in% adpp_analyte_columns()) return(NULL)
    key <- unname(unlist(result()$value$trace$output_key_mapping[column],use.names=FALSE))
    if (length(key)!=1L || !key %in% names(result()$value$data)) return(NULL)
    values <- unique(as.character(result()$value$data[[key]])); chosen <- input$adpp_order_values %||% character()
    missing <- values[!values %in% chosen]
    ok <- !anyNA(values) && all(nzchar(trimws(values))) && !anyDuplicated(chosen) && length(chosen)==length(values) && setequal(chosen,values)
    shiny::p(class=if (ok) 'pk-caption' else 'pk-caption pk-period-warning',
      paste0(length(intersect(chosen,values)), ' / ',length(values),' 選択済み。',
        if (length(missing)) paste0(' 未選択：',paste(period_label(missing),collapse='、'),'。すべて選択してください。') else ''))
  })
  # A fresh input ID is created for every mode/result change. Shiny can retain
  # old server values after the corresponding file input leaves the DOM.
  adpp_mapping_epoch <- shiny::reactiveVal(0L)
  adpp_mapping_mode <- shiny::reactive(if (identical(input$adpp_mapping_mode,'suffix')) 'suffix' else 'full')
  shiny::observeEvent(adpp_mapping_mode(), { adpp_mapping_epoch(shiny::isolate(adpp_mapping_epoch()) + 1L) }, ignoreInit=TRUE, priority=1000)
  shiny::observeEvent(result(), { adpp_mapping_epoch(shiny::isolate(adpp_mapping_epoch()) + 1L) }, ignoreInit=TRUE, priority=1000)
  adpp_mapping_input_id <- shiny::reactive(paste0('adpp_',adpp_mapping_mode(),'_mapping_file_',adpp_mapping_epoch()))
  adpp_mapping_file <- shiny::reactive(input[[adpp_mapping_input_id()]])
  # The override is attached to the specific uploaded suffix file; re-uploading
  # the suffix file also requires a fresh override selection.
  adpp_override_epoch <- shiny::reactiveVal(0L)
  shiny::observeEvent(adpp_mapping_file(), { adpp_override_epoch(shiny::isolate(adpp_override_epoch()) + 1L) },
    ignoreInit=TRUE, ignoreNULL=FALSE, priority=900)
  adpp_override_input_id <- shiny::reactive(paste0('adpp_override_file_',adpp_mapping_epoch(),'_',adpp_override_epoch()))
  adpp_override_file <- shiny::reactive(input[[adpp_override_input_id()]])
  output$adpp_mapping_controls <- shiny::renderUI({
    if (is.null(result())) return(NULL)
    shiny::div(class='pk-adpp-mapping',shiny::p(class='pk-caption','対応表CSVはUTF-8で保存してください。Excelでは「CSV UTF-8」を選択します。'),
      shiny::checkboxInput(session$ns('adpp_use_mapping'),'試験固有の対応表を使用する',FALSE),
      shiny::conditionalPanel('input.adpp_use_mapping',ns=session$ns,
        shiny::radioButtons(session$ns('adpp_mapping_mode'),'指定方法',choices=c('個別対応表'='full','接尾辞（分析対象・検体ごと）'='suffix'),selected='full'),
        shiny::p(class='pk-caption','指定方法を切り替えると、選択済みのファイルは解除されます。接尾辞CSVを選び直した場合も、個別指定CSVの再選択が必要です。'),
        shiny::conditionalPanel("input.adpp_mapping_mode != 'suffix'",ns=session$ns,
          shiny::downloadButton(session$ns('adpp_mapping_template'),'個別対応表ひな形CSV')),
        shiny::conditionalPanel("input.adpp_mapping_mode == 'suffix'",ns=session$ns,
          shiny::p(class='pk-caption','接尾辞なしは1組だけ指定できます。8文字を超える行は個別に指定してください。'),
          shiny::div(class='pk-adpp-step',shiny::downloadButton(session$ns('adpp_suffix_template'),'① 接尾辞ひな形CSVを取得'))),
        shiny::uiOutput(session$ns('adpp_mapping_upload')),
        shiny::conditionalPanel("input.adpp_mapping_mode == 'suffix'",ns=session$ns,
          shiny::div(class='pk-adpp-step',shiny::uiOutput(session$ns('adpp_override_action'))),
          shiny::uiOutput(session$ns('adpp_override_upload')))))
  })
  output$adpp_mapping_upload <- shiny::renderUI({
    label <- if (identical(input$adpp_mapping_mode,'suffix')) '② 記入した接尾辞CSVを選択' else '記入済み個別対応表（CSV）'
    shiny::div(class='pk-adpp-step',shiny::fileInput(session$ns(adpp_mapping_input_id()),label,accept='.csv',buttonLabel='ファイルを選択',placeholder='未選択'))
  })
  output$adpp_override_upload <- shiny::renderUI({
    shiny::fileInput(session$ns(adpp_override_input_id()),'④ 個別指定CSVを選択（必要な場合）',accept='.csv',buttonLabel='ファイルを選択',placeholder='未選択')
  })
  output$adpp_override_action <- shiny::renderUI({
    checked <- tryCatch({
      pk_assert(!is.null(adpp_mapping_file()) && is.character(adpp_mapping_file()$datapath), '先に接尾辞CSVを選択してください。')
      pkapp_adpp_suffix_override_template(result()$value,desktop_adpp_read_mapping_csv(adpp_mapping_file()$datapath))
    },error=function(e) NULL)
    if (is.null(checked)) return(shiny::tagList(shiny::tags$button(type='button',class='btn btn-default',disabled='disabled',`aria-disabled`='true','③ 8文字超の個別指定ひな形CSVを取得'),
      shiny::p(class='pk-caption','②の接尾辞CSVを選択・確認すると取得できます。')))
    if (!nrow(checked)) return(shiny::p(class='pk-caption','③ 8文字を超える行はありません。個別指定は不要です。'))
    shiny::tagList(shiny::downloadButton(session$ns('adpp_override_template'),'③ 8文字超の個別指定ひな形CSVを取得'),
      shiny::p(class='pk-caption',paste0('8文字を超える行：',nrow(checked),'件。')))
  })
  output$adpp_mapping_template <- shiny::downloadHandler(filename=function() 'PARAMETER_MAPPING_TEMPLATE.csv', content=function(file) {
    shiny::req(result()); pk_write_csv(pkapp_adpp_mapping_template(result()$value),file)
  })
  output$adpp_suffix_template <- shiny::downloadHandler(filename=function() 'PARAMETER_SUFFIX_TEMPLATE.csv', content=function(file) {
    shiny::req(result()); pk_write_csv(pkapp_adpp_suffix_template(result()$value),file)
  })
  output$adpp_override_template <- shiny::downloadHandler(filename=function() 'PARAMETER_OVERRIDE_TEMPLATE.csv', content=function(file) {
    shiny::req(result())
    pk_assert(!is.null(adpp_mapping_file()) && is.character(adpp_mapping_file()$datapath), '先に接尾辞の対応表を選択してください。')
    pk_write_csv(pkapp_adpp_suffix_override_template(result()$value,desktop_adpp_read_mapping_csv(adpp_mapping_file()$datapath)),file)
  })
  adpp_mapping_context <- shiny::reactive({
    if (!isTRUE(input$adpp_use_mapping)) return(list(mapping=NULL,inputs=NULL))
    pk_assert(!is.null(adpp_mapping_file()) && is.character(adpp_mapping_file()$datapath), '記入済み対応表のCSVを選択してください。')
    mapping <- desktop_adpp_read_mapping_csv(adpp_mapping_file()$datapath)
    if (identical(input$adpp_mapping_mode,'suffix')) {
      override <- if (!is.null(adpp_override_file()) && is.character(adpp_override_file()$datapath))
        desktop_adpp_read_mapping_csv(adpp_override_file()$datapath) else NULL
      return(list(mapping=pkapp_adpp_expand_suffix_mapping(result()$value,mapping,override),
        inputs=list(method='suffix',suffix_mapping=mapping,overrides=override)))
    }
    list(mapping=mapping,inputs=list(method='individual',mapping=mapping))
  })
  adpp_mapping <- shiny::reactive(adpp_mapping_context()$mapping)
  adpp_user_order <- shiny::reactive({
    if (isTRUE(input$adpp_use_mapping) || !isTRUE(input$adpp_manual_order)) return(NULL)
    list(column=input$adpp_order_column %||% '',values=as.list(input$adpp_order_values %||% character()))
  })
  shiny::observeEvent(result(), {
    shiny::updateCheckboxInput(session,'adpp_manual_order',value=FALSE)
    shiny::updateSelectizeInput(session,'adpp_order_values',selected=character())
  },ignoreInit=TRUE)
  adpp_draft <- shiny::reactive({
    shiny::req(result())
    tryCatch({
      item <- result(); rec <- saved()
      provenance <- list(channel='ui', save_status=if (is.null(rec)) 'unsaved' else 'saved_ui_record',
        saved_result_id=if (is.null(rec)) NULL else rec$id,
        project_id=engine$nca$project_id %||% NULL, revision=item$revision %||% NULL,
        nca_result_id=item$shared_result_id %||% NULL, verification='UI session context; independently confirm saved artifacts')
      pkapp_adpp_draft(item$value,source_input=item$input,provenance=provenance,analyte_order=adpp_user_order(),parameter_mapping=adpp_mapping(),mapping_inputs=adpp_mapping_context()$inputs)
    }, error = function(e) e)
  })
  output$adpp_draft_exports <- shiny::renderUI({
    if (is.null(result())) return(NULL)
    candidate <- adpp_draft()
    failed <- inherits(candidate, 'error')
    download <- function(id,ja,en) {
      label <- desktop_i18n_text(ja,en)
      if (!failed) return(shiny::downloadButton(session$ns(id),label))
      shiny::tags$button(type='button',class='btn btn-default',disabled='disabled',`aria-disabled`='true',label)
    }
    shiny::tagList(shiny::p(class="pk-caption", if (is.null(saved())) desktop_i18n_text("未保存のNCA結果から作成します。", "Created from unsaved NCA results.") else desktop_i18n_text("画面で保存したNCA結果から作成します。", "Created from NCA results saved in this session.")),
      shiny::p(class="pk-caption", desktop_i18n_text("提出用ではありません。解析対象フラグ、処置・期、ADSLとの対応、用語・単位を試験仕様と照合してください。検体に応じたパラメータの適否は未確認です。", "Not for submission. Check analysis flags, treatment and period, alignment with ADSL, terminology and units against the study specification. Parameter appropriateness for each specimen has not been verified.")),
      if (failed) desktop_ui_notice(paste('ADPP候補：',conditionMessage(candidate)), 'warning'),
      if (failed && isTRUE(input$adpp_use_mapping) && !identical(input$adpp_mapping_mode,'suffix') && !is.null(adpp_mapping_file())) {
        diag <- tryCatch(desktop_adpp_mapping_diagnostics(result()$value,desktop_adpp_read_mapping_csv(adpp_mapping_file()$datapath)),error=function(e) NULL)
        if (!is.null(diag)) shiny::tagList(lapply(names(diag),function(kind) {
          if (!length(diag[[kind]])) return(NULL)
          label <- c(missing='不足',extra='余分',duplicate='重複')[[kind]]
          shiny::p(class='pk-caption',paste0(label,'（最大3件）：',paste(diag[[kind]],collapse='／')))
        }))
      },
      if (!failed) shiny::tagList(shiny::p(class='pk-caption',desktop_i18n_text('PARAMCD対応のプレビュー（先頭10行）。','PARAMCD mapping preview (first 10 rows).')),
        desktop_display_table(head(candidate$record$parameter_mapping,10))),
      shiny::div(class="pk-actions", if (requireNamespace("zip",quietly=TRUE)) download("adpp_bundle","レビュー一式ZIP","Review bundle (ZIP)"), download("adpp_csv","ADPP候補CSV","ADPP candidate (CSV)"),
        download("adpp_metadata","変数説明CSV","Variable metadata (CSV)"),download("adpp_review","確認用NCA結果CSV","NCA review results (CSV)"), download("adpp_record","計算記録JSON","Calculation record (JSON)")))
  })
  output$adpp_bundle <- shiny::downloadHandler(filename=function() "ADPP_CANDIDATE_REVIEW.zip", content=function(file) {
    shiny::req(result()); item <- result(); rec <- saved()
    provenance <- list(channel='ui',save_status=if (is.null(rec)) 'unsaved' else 'saved_ui_record',
      saved_result_id=if (is.null(rec)) NULL else rec$id,project_id=engine$nca$project_id %||% NULL,
      revision=item$revision %||% NULL,nca_result_id=item$shared_result_id %||% NULL,
      verification='UI session context; independently confirm saved artifacts')
    pkapp_write_adpp_draft(item$value,file,source_input=item$input,provenance=provenance,analyte_order=adpp_user_order(),parameter_mapping=adpp_mapping(),mapping_inputs=adpp_mapping_context()$inputs)
  }, contentType="application/zip")
  output$adpp_csv <- shiny::downloadHandler(filename=function() "ADPP_CANDIDATE.csv", content=function(file) {
    candidate <- adpp_draft(); if (inherits(candidate,"error")) stop(candidate); pk_write_csv(candidate$data,file)
  })
  output$adpp_metadata <- shiny::downloadHandler(filename=function() "ADPP_CANDIDATE_METADATA.csv", content=function(file) {
    candidate <- adpp_draft(); if (inherits(candidate,"error")) stop(candidate); pk_write_csv(candidate$metadata,file)
  })
  output$adpp_review <- shiny::downloadHandler(filename=function() "ADPP_CANDIDATE_REVIEW.csv", content=function(file) {
    candidate <- adpp_draft(); if (inherits(candidate,"error")) stop(candidate); pk_write_csv(candidate$review,file)
  })
  output$adpp_record <- shiny::downloadHandler(filename=function() "ADPP_CANDIDATE_RECORD.json", content=function(file) {
    candidate <- adpp_draft(); if (inherits(candidate,"error")) stop(candidate); desktop_atomic(candidate$record,file)
  })
  for (name in c("derived_controls", "dose_ev_controls", "body_controls", "derived_readiness",
      "predose_qc_lloq_controls", "predose_qc_lloq_value_control", "disclosure", "disclosure_actions",
      "adpp_order_controls", "adpp_order_values_controls", "adpp_order_progress", "adpp_mapping_controls",
      "adpp_mapping_upload", "adpp_override_upload", "adpp_override_action", "adpp_draft_exports"))
    shiny::outputOptions(output, name, suspendWhenHidden = FALSE)
  shiny::observeEvent(input$save, {
    tryCatch({
      pk_assert(!shared_busy(), "NCAを計算中です。完了後に保存してください。")
      value <- result()
      pk_assert(!is.null(value), "入力・条件が変わりました。NCAを再実行してください。")
      provenance <- if (identical(engine$mode, "nca")) list(execution = engine$nca_results[[value$shared_result_id]]$execution,
        saved_by = "local user", save_reason = "Save GUI result") else NULL
      receipt <- desktop_table_save(engine, input$name, list(source = value$input), list(kind = "nca", specification = value$value$specification),
        frozen = if (identical(engine$mode, "nca")) value$value else NULL, provenance = provenance)
      saved(receipt); notice(paste("保存しました：", receipt$name, "。データ準備・入力元から選択できます。"))
    }, error = function(e) notice(paste("保存できませんでした：", conditionMessage(e))))
  })
  output$download <- shiny::downloadHandler(filename = function() "NCA-results.csv", content = function(file) {
    shiny::req(result()); pk_write_csv(result()$value$data, file)
  })
  output$audit_download <- shiny::downloadHandler(filename = function() "NCA-row-handling.csv", content = function(file) {
    shiny::req(result()); pk_write_csv(result()$value$trace$rows, file)
  })
  output$settings_download <- shiny::downloadHandler(filename = function() "NCA-settings.json", content = function(file) {
    shiny::req(result()); desktop_atomic(result()$value$trace, file)
  })
})

desktop_nca_engine <- function(project) {
  engine <- new.env(parent = emptyenv())
  engine$project <- normalizePath(project, mustWork = TRUE)
  desktop_tables_init(engine)
  desktop_nca_init(engine)
  desktop_explore_init(engine)
  desktop_modules_init(engine)
  engine
}

desktop_nca_app <- function(engine, token) {
  ui <- bslib::page_navbar(title = paste("PK Desktop", desktop_version), window_title = "PK Desktop NCA", lang = "ja",
    id = "workspace_navigation", selected = "assembly", fillable = FALSE,
    theme = bslib::bs_theme(version = 5, primary = "#176b79", base_font = "system-ui"),
    header = shiny::tagList(desktop_workbench_styles(), desktop_i18n_controls()),
    bslib::nav_panel(desktop_i18n_text("EX・PC・濃度", "EX / PC / concentration"), value = "assembly", desktop_assembly_ui("assembly")),
    bslib::nav_panel(desktop_i18n_text("NCA入力：DM・VS", "DM/VS before NCA"), value = "covariates", desktop_annotation_ui("covariates", input_mode = TRUE)),
    bslib::nav_panel(desktop_i18n_text("データ準備", "Data preparation"), value = "data", shiny::div(class = "pk-data-page", desktop_workbench_ui("workbench"))),
    bslib::nav_panel("NCA", value = "nca", desktop_nca_ui("nca")),
    bslib::nav_panel(desktop_i18n_text("結果の探索", "Explore results"), value = "explore", desktop_explore_ui("explore")),
    bslib::nav_panel(if (requireNamespace("teal", quietly = TRUE) && requireNamespace("teal.data", quietly = TRUE)) desktop_i18n_text("追加の探索モジュール", "Additional exploration") else desktop_i18n_text("追加の探索モジュール（要 teal）", "Additional exploration (requires teal)"), value = "extensions", desktop_extensions_ui("extensions")),
    bslib::nav_panel(desktop_i18n_text("結果表示用DM・VS追加（再計算なし）", "DM/VS after NCA"), value = "annotation", desktop_annotation_ui("annotation")))
  shiny::shinyApp(ui, function(input, output, session) {
    query <- shiny::isolate(shiny::getQueryString(session))
    if (!identical(query[["token"]], token)) { session$close(); return(invisible(NULL)) }
    desktop_workbench_server("workbench", engine)
    desktop_assembly_server("assembly", engine)
    desktop_nca_server("nca", engine)
    desktop_explore_server("explore", engine)
    desktop_extensions_server("extensions", engine)
    desktop_annotation_server("annotation", engine)
    desktop_annotation_server("covariates", engine, input_mode = TRUE)
  })
}

desktop_nca_launch <- function(project, port, api_port, launch_browser,
    start_api = httpuv::startServer, stop_api = httpuv::stopServer, run_app = shiny::runApp) {
  valid <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x == floor(x) && x >= 1024 && x <= 65535
  desktop_check(valid(port) && valid(api_port) && port != api_port, "E_VALIDATION", "Use distinct UI/API ports between 1024 and 65535")
  desktop_check(is.logical(launch_browser) && length(launch_browser) == 1 && !is.na(launch_browser), "E_VALIDATION", "launch_browser must be TRUE or FALSE")
  owner <- desktop_acquire_project(project)
  on.exit(desktop_release_project(owner), add = TRUE)
  engine <- desktop_nca_engine(owner$project)
  on.exit(desktop_nca_jobs_close(engine), add = TRUE, after = FALSE)
  runtime <- desktop_r_runtime(engine, api_port, port, "nca")
  listener <- NULL; written <- FALSE
  on.exit({ if (!is.null(listener)) try(stop_api(listener), silent = TRUE); if (written) desktop_remove_r_runtime(engine, runtime) }, add = TRUE, after = FALSE)
  listener <- start_api("127.0.0.1", api_port, desktop_http_app(engine, runtime))
  desktop_write_r_runtime(engine, runtime); written <- TRUE
  url <- paste0("http://127.0.0.1:", port, "/?token=", runtime$token)
  run_app(desktop_nca_app(engine, runtime$token), host = "127.0.0.1", port = port,
    launch.browser = function(base_url) { if (launch_browser) utils::browseURL(url) else message("Open this local session URL: ", url) }, display.mode = "normal")
  invisible(NULL)
}
pkapp_launch_nca <- function(project = file.path(pk_root, "outputs/pk-desktop/nca"), port = 3879L, api_port = 3880L, launch_browser = TRUE) {
  desktop_nca_launch(project, port, api_port, launch_browser)
}
