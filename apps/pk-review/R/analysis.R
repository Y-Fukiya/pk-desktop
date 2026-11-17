pk_sample_sd <- function(x, center) {
  deviations <- x - center
  if (all(is.finite(deviations))) {
    scale <- max(abs(deviations))
    if (scale == 0) return(0)
    # Center before scaling to retain differences between nearby large values.
    return(stats::sd(deviations / scale) * scale)
  }
  scale <- max(abs(x))
  stats::sd(x / scale) * scale
}

pk_summary_values <- function(x) {
  observed <- x[!is.na(x)]
  positive <- observed[observed > 0]
  avg <- if (length(observed)) mean(observed) else NA_real_
  sd <- if (length(observed) > 1) pk_sample_sd(observed, avg) else NA_real_
  geometric_cv <- if (length(positive) > 1) {
    variance <- stats::var(log(positive))
    # sqrt(exp(v)-1) = exp(v/2) * sqrt(1-exp(-v)); retain small-v accuracy.
    100 * exp(variance / 2) * sqrt(-expm1(-variance))
  } else NA_real_
  result <- data.frame(n = length(observed), missing = sum(is.na(x)), n_positive = length(positive),
             mean = avg, sd = sd, cv_percent = if (!is.na(avg) && avg != 0) (sd / avg) * 100 else NA_real_,
             geometric_mean = if (length(positive)) exp(mean(log(positive))) else NA_real_,
             geometric_cv_percent = geometric_cv,
             median = if (length(observed)) stats::median(observed) else NA_real_,
             min = if (length(observed)) min(observed) else NA_real_,
             max = if (length(observed)) max(observed) else NA_real_,
             p05 = if (length(observed)) unname(stats::quantile(observed, .05, type = 2)) else NA_real_,
             p95 = if (length(observed)) unname(stats::quantile(observed, .95, type = 2)) else NA_real_)
  invalid <- names(result)[vapply(result, function(v) any(is.infinite(v) | is.nan(v)), logical(1))]
  pk_assert(!length(invalid), paste("Summary outside the supported numeric range:", paste(invalid, collapse = ", ")))
  result
}

pk_summarise <- function(d, keys) {
  pk_assert(nrow(d) > 0, "No records remain under the selected conditions")
  # Integer codes keep distinct tuples distinct even when labels contain dots.
  groups <- interaction(lapply(d[keys], function(x) match(x, unique(x))), sep = ":", drop = TRUE, lex.order = TRUE)
  pieces <- split(d, groups)
  out <- do.call(rbind, lapply(pieces, function(p) cbind(p[1, keys, drop = FALSE], pk_summary_values(p$AVAL))))
  rownames(out) <- NULL
  out[do.call(order, out[keys]), , drop = FALSE]
}

pk_group <- function(d, variable) {
  if (length(variable) > 1L) {
    # JSON tuples distinguish missing values and embedded separators exactly.
    columns <- lapply(variable, function(v) if (v == "DOSE_MG") pk_dose_key(d[[v]]) else as.character(d[[v]]))
    names(columns) <- variable
    d$.GROUP <- vapply(seq_len(nrow(d)), function(i) as.character(jsonlite::toJSON(
      lapply(columns, function(x) x[i]), auto_unbox = TRUE, na = "null")), character(1))
    return(d)
  }
  d$.GROUP <- if (variable == "NONE") rep("All subjects", nrow(d)) else as.character(d[[variable]])
  if (variable == "DOSE_MG") d$.GROUP <- pk_dose_key(d[[variable]])
  d$.GROUP[!is.na(d$.GROUP) & d$.GROUP == ""] <- NA_character_
  d
}

pk_analyse <- function(data, cfg) {
  pk_validate_config(cfg, data)
  conc <- data$ADNCA
  params <- data$ADPP
  if (length(cfg$subjects)) {
    conc <- conc[conc$USUBJID %in% unlist(cfg$subjects), , drop = FALSE]
    params <- params[params$USUBJID %in% unlist(cfg$subjects), , drop = FALSE]
  }
  cohort <- sort(unique(conc$USUBJID))
  if (cfg$profile != "all") {
    flag <- if (cfg$profile == "steady_state") "SSNCAFL" else "TROUGHFL"
    conc <- conc[pk_flag(conc[[flag]]), , drop = FALSE]
  }
  before <- nrow(conc)
  mdv <- if (!cfg$include_mdv && "MDV" %in% names(conc)) pk_flag(conc$MDV) else rep(FALSE, nrow(conc))
  blq <- if (cfg$blq == "exclude" && "BLQ" %in% names(conc)) pk_flag(conc$BLQ) else rep(FALSE, nrow(conc))
  conc <- conc[!(mdv | blq), , drop = FALSE]
  conc <- pk_group(conc, cfg$group_by)
  if (length(cfg$facet_by)) {
    conc$.FACET <- pk_group(conc, cfg$facet_by)$.GROUP
    missing_facet <- "(missing)"
    while (missing_facet %in% conc$.FACET) missing_facet <- paste(missing_facet, "[NA]")
    conc$.FACET[is.na(conc$.FACET)] <- missing_facet
  }
  conc$.TIME <- conc[[cfg$x]]
  pk_assert(!anyNA(conc$.TIME), "Selected concentration records have missing times")
  params <- params[params$PARAMCD %in% unlist(cfg$parameters), , drop = FALSE]
  excluded_adpp <- if ("ANL01FL" %in% names(params)) !pk_flag(params$ANL01FL, "ANL01FL") else rep(FALSE, nrow(params))
  params <- pk_group(params[!excluded_adpp, , drop = FALSE], cfg$group_by)
  cs <- pk_summarise(conc, c(".GROUP", if (length(cfg$facet_by)) ".FACET", "AVALU", ".TIME"))
  ps <- pk_summarise(params, c(".GROUP", "PARAMCD", "AVALU"))
  # Group missing values before assigning a display label that cannot collide.
  missing_label <- "(missing)"
  observed_groups <- c(conc$.GROUP, params$.GROUP)
  while (missing_label %in% observed_groups) missing_label <- paste(missing_label, "[NA]")
  label <- function(x) replace(x, is.na(x), missing_label)
  conc$.GROUP <- label(conc$.GROUP)
  params$.GROUP <- label(params$.GROUP)
  cs$.GROUP <- label(cs$.GROUP)
  ps$.GROUP <- label(ps$.GROUP)
  cs$center <- cs[[cfg$statistic]]
  spread <- if (cfg$interval == "se") cs$sd / sqrt(cs$n) else cs$sd
  cs$lower <- if (cfg$interval %in% c("sd", "se")) cs$mean - spread else if (cfg$interval == "p05_p95") cs$p05 else NA_real_
  cs$upper <- if (cfg$interval %in% c("sd", "se")) cs$mean + spread else if (cfg$interval == "p05_p95") cs$p95 else NA_real_
  pk_assert(!any(vapply(cs[c("center", "lower", "upper")], function(x) any(is.infinite(x) | is.nan(x)), logical(1))),
            "Selected interval outside the supported numeric range")
  if (cfg$scale == "log10" && cfg$interval != "none") {
    pk_assert(!any(cs$center > 0 & cs$lower <= 0, na.rm = TRUE),
              "Selected interval has a nonpositive lower bound on the log axis; choose linear or interval=none")
  }
  qc <- data$manifest$quality_checks
  if (length(qc)) {
    qc <- do.call(rbind, lapply(qc, as.data.frame, stringsAsFactors = FALSE))
    qc <- qc[qc$USUBJID %in% cohort, , drop = FALSE]
  } else qc <- data.frame(USUBJID = character(), check = character(), status = character(), relative_change = character(), rule = character())
  warnings <- unlist(data$manifest$warnings, use.names = FALSE)
  if (any(qc$status == "FAIL")) warnings <- c(warnings, paste("UPSTREAM QC FAIL:", paste(qc$USUBJID[qc$status == "FAIL"], collapse = ", ")))
  if (cfg$profile == "steady_state" && !nrow(qc)) warnings <- c(warnings, "Upstream steady-state QC metadata not provided; this view selects the labelled final dose interval only.")
  if (cfg$profile == "steady_state" && nrow(qc) && length(setdiff(cohort, qc$USUBJID))) warnings <- c(warnings, paste("Upstream QC unavailable for:", paste(setdiff(cohort, qc$USUBJID), collapse = ", ")))
  list(config = cfg, concentrations = conc, parameters = params,
       quality_checks = qc, warnings = as.list(warnings),
       concentration_summary = cs, parameter_summary = ps,
       counts = list(subjects = length(cohort), concentration_subjects = length(unique(conc$USUBJID[!is.na(conc$AVAL)])),
                     parameter_subjects = length(unique(params$USUBJID[!is.na(params$AVAL)])),
                     concentration_rows = nrow(conc), parameter_rows = nrow(params),
                     excluded_concentration_rows = before - nrow(conc), mdv_rows = sum(mdv), blq_rows = sum(blq),
                     excluded_adpp_rows = sum(excluded_adpp),
                     missing_concentrations = sum(is.na(conc$AVAL)), missing_parameters = sum(is.na(params$AVAL)),
                     nonpositive_concentrations = sum(conc$AVAL <= 0, na.rm = TRUE)),
       notes = c("ADNCA contains concentrations; ADPP contains precomputed PK parameters.",
                 "ADPP is filtered by subjects and ANL01FL, not recalculated when concentration display settings change.",
                 "SD is a sample standard deviation. The 5th to 95th percentile interval uses quantile type 2 and is not a confidence interval.",
                 if (cfg$interval == "se") "SE = sample SD / sqrt(n), with n the nonmissing concentration count in each time/group/facet/unit cell; the SE band is not a confidence interval.",
                 "Geometric summaries use positive values only; n_positive is exported separately.",
                 "Log plots hide nonpositive values only after aggregation; arithmetic summaries remain unchanged."))
}

pk_group_labels <- function(values, variables) {
  if (length(variables) <= 1L) return(values)
  vapply(values, function(value) {
    parts <- jsonlite::fromJSON(value, simplifyVector = FALSE)
    paste(vapply(names(parts), function(name) paste0(name, ": ", if (is.null(parts[[name]])) "<NA>" else encodeString(parts[[name]], quote = '"')), character(1)), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}
pk_plot_concentration <- function(a) {
  cfg <- a$config
  d <- a$concentrations
  s <- a$concentration_summary
  if (cfg$scale == "log10") {
    d$AVAL[d$AVAL <= 0] <- NA_real_
    for (name in c("center", "lower", "upper")) s[[name]][s[[name]] <= 0] <- NA_real_
  }
  p <- ggplot2::ggplot()
  if (cfg$show_individual) {
    p <- p + ggplot2::geom_line(data = d, ggplot2::aes(x = .TIME, y = AVAL, group = USUBJID), color = "#87989f", linewidth = .35, alpha = .32, na.rm = TRUE)
  }
  if (cfg$interval != "none") {
    # All-missing intervals (e.g. SD with one subject) cannot form a polygon.
    # Retain missing points within valid groups so gaps are not bridged here.
    ribbon <- dplyr::ungroup(dplyr::filter(dplyr::group_by_at(s, c(".GROUP", "AVALU", if (length(cfg$facet_by)) ".FACET")),
      any(is.finite(lower) & is.finite(upper))))
    if (nrow(ribbon)) p <- p + ggplot2::geom_ribbon(data = ribbon, ggplot2::aes(x = .TIME, ymin = lower, ymax = upper, fill = .GROUP, group = .GROUP), alpha = .16, na.rm = TRUE)
  }
  p <- p + ggplot2::geom_line(data = s, ggplot2::aes(x = .TIME, y = center, color = .GROUP, group = .GROUP), linewidth = .9, na.rm = TRUE) +
    ggplot2::geom_point(data = s, ggplot2::aes(x = .TIME, y = center, color = .GROUP), size = 1.5, na.rm = TRUE) +
    ggplot2::facet_wrap(if (length(cfg$facet_by)) ggplot2::vars(.FACET, AVALU) else ggplot2::vars(AVALU), scales = "free_y") +
    ggplot2::labs(x = if (cfg$x == "TAD_H") "Time after analysis dose (h)" else "Time since first dose (h)", y = "Concentration", color = if (identical(cfg$group_by, "NONE")) NULL else paste(cfg$group_by, collapse = " × "), fill = if (identical(cfg$group_by, "NONE")) NULL else paste(cfg$group_by, collapse = " × "),
                  subtitle = paste(cfg$statistic, if (cfg$interval == "sd") "with SD" else if (cfg$interval == "se") "with SE" else if (cfg$interval == "p05_p95") "with 5th to 95th percentiles" else "", "|", cfg$profile,
                    if (length(cfg$facet_by)) paste("| Panels:", paste(cfg$facet_by, collapse = " × ")))) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
  if (cfg$scale == "log10") p <- p + ggplot2::scale_y_log10()
  p <- p + ggplot2::scale_color_discrete(labels = function(x) pk_group_labels(x, cfg$group_by)) +
    ggplot2::scale_fill_discrete(labels = function(x) pk_group_labels(x, cfg$group_by)) +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = if (length(unique(s$.GROUP)) > 2) 2 else 1, byrow = TRUE),
      fill = ggplot2::guide_legend(ncol = if (length(unique(s$.GROUP)) > 2) 2 else 1, byrow = TRUE)) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 9), legend.text = ggplot2::element_text(size = 8))
  p
}

pk_plot_parameters <- function(a) {
  d <- a$parameters
  ggplot2::ggplot(d, ggplot2::aes(x = .GROUP, y = AVAL, fill = .GROUP)) +
    ggplot2::geom_boxplot(width = .5, outlier.shape = NA, alpha = .25, na.rm = TRUE) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = .12, height = 0, seed = 17), size = .9, alpha = .5, na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(PARAMCD, AVALU), scales = "free_y", ncol = 3) +
    ggplot2::labs(x = if (identical(a$config$group_by, "NONE")) NULL else paste(a$config$group_by, collapse = " × "), y = "Parameter value", fill = NULL) +
    ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(legend.position = "none", panel.grid.minor = ggplot2::element_blank())
}

pk_display_table <- function(s, digits = 3, parameters = FALSE) {
  f <- function(x) vapply(x, function(value) {
    if (is.na(value)) "NE" else format(signif(value, digits), scientific = FALSE, trim = TRUE)
  }, character(1))
  data.frame(Group = s$.GROUP,
             Measure = if (parameters) paste(s$PARAMCD, s$AVALU) else paste(f(s$.TIME), "h", s$AVALU),
             N = s$n,
             `Mean (SD)` = paste0(f(s$mean), " (", f(s$sd), ")"),
             `Median [min, max]` = paste0(f(s$median), " [", f(s$min), ", ", f(s$max), "]"),
             `GeoMean (N positive)` = paste0(f(s$geometric_mean), " (", s$n_positive, ")"), check.names = FALSE)
}
