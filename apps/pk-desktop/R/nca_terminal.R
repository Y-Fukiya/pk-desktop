desktop_nca_terminal_defaults <- function() list(terminal_method = "none", dosing_context = "ongoing",
  dose_history_confirmed = FALSE, washout_attestation = "unconfirmed", infusion_end_h = 0, terminal_start_h = NULL, terminal_end_h = NULL,
  terminal_min_r_squared = .9, terminal_min_span_ratio = 2, aucinf_max_extrap_pct = 20,
  terminal_qc_action = "withhold")

desktop_nca_terminal_specification <- function(specification) {
  defaults <- desktop_nca_terminal_defaults()
  if (!any(names(specification) %in% names(defaults))) return(list())
  for (name in intersect(names(specification), names(defaults))) defaults[name] <- specification[name]
  s <- defaults
  choices <- list(terminal_method = c("none", "auto", "manual"), dosing_context = c("single", "isolated_single_period", "repeated_final", "ongoing"),
    washout_attestation = c("unconfirmed", "protocol_confirmed"),
    terminal_qc_action = c("flag", "withhold"))
  for (name in names(choices)) pk_assert(is.character(s[[name]]) && length(s[[name]]) == 1L &&
    !is.na(s[[name]]) && s[[name]] %in% choices[[name]], paste("終末相設定を確認してください：", name))
  pk_assert(is.logical(s$dose_history_confirmed) && length(s$dose_history_confirmed) == 1L && !is.na(s$dose_history_confirmed),
    "投与履歴の確認はTRUE/FALSEで指定してください。")
  number <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  for (name in c("infusion_end_h", "terminal_min_r_squared", "terminal_min_span_ratio", "aucinf_max_extrap_pct"))
    pk_assert(number(s[[name]]) && s[[name]] >= 0, paste("終末相の閾値は0以上の有限数です：", name))
  pk_assert(s$terminal_min_r_squared <= 1 && s$aucinf_max_extrap_pct <= 100, "R二乗は0〜1、外挿率は0〜100で指定してください。")
  for (name in c("terminal_start_h", "terminal_end_h"))
    pk_assert(is.null(s[[name]]) || (number(s[[name]]) && s[[name]] >= 0), "終末相区間は0以上の有限な時間（h）です。")
  pk_assert(s$terminal_method != "manual" || (!is.null(s$terminal_start_h) && !is.null(s$terminal_end_h) && s$terminal_end_h > s$terminal_start_h),
    "手動終末相には開始・終了時間（終了＞開始）が必要です。")
  for (name in names(s)[vapply(s, is.numeric, logical(1))]) s[[name]] <- as.numeric(s[[name]])
  s
}

desktop_nca_terminal_candidate <- function(points, tlast, options) {
  ret <- list(source_rows = as.list(points$SOURCE_ROW), start_h = min(points$TIME_H), end_h = max(points$TIME_H),
    n_points = nrow(points), selected = FALSE, reason = "", diagnostics = NULL)
  if (nrow(points) < 3L) { ret$reason <- "fewer_than_3_positive_post_cmax"; return(ret) }
  if (anyDuplicated(points$TIME_H)) { ret$reason <- "duplicate_terminal_time"; return(ret) }
  if (length(unique(log(points$ORIGINAL_NUMERIC))) < 2L) { ret$reason <- "no_log_concentration_variation"; return(ret) }
  fit <- tryCatch(suppressWarnings(PKNCA::pk.calc.half.life(points$ORIGINAL_NUMERIC, points$TIME_H,
    tmax = min(points$TIME_H) - 1, tlast = tlast, manually.selected.points = TRUE,
    options = c(options, list(min.hl.points = 3, allow.tmax.in.half.life = FALSE, adj.r.squared.factor = .0001)))), error = function(e) NULL)
  if (is.null(fit) || nrow(fit) != 1L) { ret$reason <- "terminal_fit_failed"; return(ret) }
  ret$diagnostics <- as.list(fit[1, , drop = FALSE])
  ret$diagnostics$slope <- -fit$lambda.z
  ret$diagnostics$intercept <- mean(log(points$ORIGINAL_NUMERIC)) + fit$lambda.z * mean(points$TIME_H)
  required <- c("lambda.z", "half.life", "r.squared", "adj.r.squared", "span.ratio", "clast.pred")
  if (!all(vapply(fit[required], function(x) length(x) == 1L && is.finite(x), logical(1)))) ret$reason <- "nonfinite_fit"
  else if (fit$lambda.z <= 0 || fit$half.life <= 0) ret$reason <- "nonnegative_slope"
  ret
}

desktop_nca_terminal_dose_reason <- function(data, rows, s, endpoint) {
  if (s$dosing_context == "ongoing") return("ongoing_repeated_dose")
  if (!s$dose_history_confirmed) return("dose_history_unconfirmed")
  if ("NCA_DOSE_HISTORY_CONFIRMED" %in% names(data)) {
    confirmed <- tryCatch(desktop_pk_flag(data$NCA_DOSE_HISTORY_CONFIRMED[rows], "NCA_DOSE_HISTORY_CONFIRMED"), error = function(e) NULL)
    if (is.null(confirmed) || !length(confirmed) || !all(confirmed)) return("source_dose_history_unconfirmed")
  }
  if ("NCA_DOSE_COUNT" %in% names(data)) {
    count <- desktop_pk_numeric(data$NCA_DOSE_COUNT[rows])
    if (any(count$error != "") || any(count$value < 1 | count$value != floor(count$value)) || length(unique(count$value)) != 1L)
      return("invalid_source_dose_count")
    if (s$dosing_context == "single" && any(count$value > 1)) return("single_dose_context_conflict")
  }
  if ("NCA_NEXT_DOSE_H" %in% names(data)) {
    next_dose <- desktop_pk_numeric(data$NCA_NEXT_DOSE_H[rows])
    if (any(!next_dose$error %in% c("", "missing")) || length(unique(next_dose$value[!is.na(next_dose$value)])) > 1L ||
        (anyNA(next_dose$value) && any(!is.na(next_dose$value)))) return("invalid_next_dose_evidence")
    if (any(next_dose$value <= endpoint, na.rm = TRUE)) return("intervening_dose")
    if (s$dosing_context == "single" && any(!is.na(next_dose$value))) return("single_dose_context_conflict")
    if (s$dosing_context == "repeated_final" && any(!is.na(next_dose$value))) return("final_dose_context_conflict")
  }
  markers <- intersect(c(desktop_nca_dose_evidence_columns(), "NCA_ASSEMBLY_STATUS", "NCA_ASSEMBLY_REASON",
    "NCA_ASSEMBLY_PROFILE", "NCA_ASSEMBLY_PROFILE_COLUMNS", "NCA_CONC_ROW", "NCA_PC_ROW", "NCA_EX_ROW",
    "NCA_DOSE_REFERENCE_COLUMNS"), names(data))
  if (length(markers)) {
    required <- c("NCA_DOSE_COUNT", "NCA_NEXT_DOSE_H", "NCA_DOSE_HISTORY_CONFIRMED", "NCA_DOSE_REF_ID")
    if (!all(required %in% names(data))) return("dose_evidence_requires_reassembly")
    references <- unique(as.character(data$NCA_DOSE_REF_ID[rows]))
    if (length(references) != 1L || is.na(references) || !nzchar(trimws(references))) return("invalid_dose_reference_evidence")
    if (any(!is.na(next_dose$value) & next_dose$value <= 0)) return("invalid_next_dose_evidence")
    if (any(c("NCA_DOSE_EVIDENCE_VERSION", "NCA_PREV_DOSE_H", "NCA_PRIOR_DOSE_COUNT", "NCA_LATER_DOSE_COUNT") %in% names(data))) {
      evidence <- desktop_nca_dose_evidence(data, rows, s)
      if (nzchar(evidence$reason)) return(evidence$reason)
    }
  }
  ""
}

desktop_nca_terminal_apply <- function(value, source, s) {
  if (is.null(s$terminal_method) || s$terminal_method == "none") return(value)
  parameters <- c(LAMZ = "Lambda z", LAMZHL = "Half-Life Lambda z",
    AUCIFO = "AUC Infinity Obs", AUCPEO = "AUC %Extrapolation Obs")
  units <- c(LAMZ = "1/h", LAMZHL = "h", AUCIFO = if (nzchar(value$trace$concentration_unit)) paste0("h*", value$trace$concentration_unit) else NA_character_, AUCPEO = "%")
  audit <- value$trace$rows
  audit$TERMINAL_ELIGIBLE <- FALSE; audit$USE_TERMINAL <- FALSE; audit$TERMINAL_REASON <- "outside_analysis_interval"
  traces <- list(); additions <- list(); warnings <- character()
  original_conc <- desktop_pk_numeric(audit$CONC_ORIGINAL)
  parameters_per_profile <- nrow(value$data) / length(value$trace$groups)
  for (g in seq_along(value$trace$groups)) {
    group <- value$trace$groups[[g]]
    profile <- which(audit$PROFILE == group$profile)
    original <- profile[!is.na(audit$ROW[profile]) & !audit$IMPUTED[profile]]
    base_rows <- seq.int((g - 1L) * parameters_per_profile + 1L, g * parameters_per_profile)
    base <- value$data[base_rows[1L], , drop = FALSE]
    existing <- value$data[base_rows, , drop = FALSE]
    parameter <- function(name) existing$AVAL[match(name, existing$PARAMCD)]
    reason <- rep("", length(profile))
    reason[audit$IMPUTED[profile]] <- "synthetic_point"
    reason[!audit$IMPUTED[profile] & !audit$USE_OBS[profile]] <- "outside_analysis_interval"
    reason[audit$MDV[profile]] <- "MDV"
    reason[audit$BLQ[profile]] <- "BLQ"
    reason[reason == "" & (original_conc$error[profile] != "" | is.na(original_conc$value[profile]) | original_conc$value[profile] <= 0)] <- "nonpositive_or_invalid_original_concentration"
    tmax <- parameter("TMAX")
    reason[reason == "" & (!is.finite(tmax) | !is.finite(audit$TIME_H[profile]) | audit$TIME_H[profile] <= tmax)] <- "not_after_cmax"
    reason[reason == "" & audit$TIME_H[profile] <= s$infusion_end_h] <- "not_after_infusion_end"
    if (s$terminal_method == "manual")
      reason[reason == "" & (audit$TIME_H[profile] < s$terminal_start_h | audit$TIME_H[profile] > s$terminal_end_h)] <- "outside_manual_interval"
    audit$TERMINAL_REASON[profile] <- reason
    eligible <- profile[reason == ""]
    audit$TERMINAL_ELIGIBLE[eligible] <- TRUE
    points <- audit[eligible, , drop = FALSE]
    points$ORIGINAL_NUMERIC <- original_conc$value[eligible]
    points <- points[order(points$TIME_H), , drop = FALSE]
    tlast <- base$TLAST_QUANT_H
    candidates <- list(); selected <- NULL; rejection <- ""
    if (identical(group$status, "excluded")) rejection <- "invalid_analysis_profile"
    else if (nrow(points) < 3L) rejection <- "fewer_than_3_positive_post_cmax"
    else if (anyDuplicated(points$TIME_H)) rejection <- "duplicate_terminal_time"
    eligibility <- desktop_nca_terminal_eligibility(source, audit$ROW[original], s, group, tlast)
    if (nzchar(eligibility$reason)) rejection <- eligibility$reason
    if (!nzchar(rejection)) {
      starts <- if (s$terminal_method == "manual") 1L else seq.int(nrow(points) - 2L, 1L)
      candidates <- lapply(starts, function(first) desktop_nca_terminal_candidate(points[seq.int(first, nrow(points)), , drop = FALSE], tlast, value$trace$pknca_options))
      valid <- which(vapply(candidates, function(x) !nzchar(x$reason), logical(1)))
      if (!length(valid)) rejection <- paste(unique(vapply(candidates, function(x) x$reason, character(1))), collapse = ";")
      else {
        adjusted <- vapply(candidates[valid], function(x) x$diagnostics$adj.r.squared, numeric(1))
        best <- valid[adjusted > max(adjusted) - .0001]
        chosen <- best[which.max(vapply(candidates[best], function(x) x$n_points, integer(1)))]
        candidates[[chosen]]$selected <- TRUE; selected <- candidates[[chosen]]
      }
    }
    values <- stats::setNames(rep(NA_real_, length(parameters)), names(parameters))
    reasons <- stats::setNames(rep(rejection, length(parameters)), names(parameters))
    qc <- character(); extrapolation <- NA_real_; auc <- NA_real_
    if (!is.null(selected) && !nzchar(rejection)) {
      fit <- selected$diagnostics
      if (fit$r.squared < s$terminal_min_r_squared) qc <- c(qc, "r_squared_below_threshold")
      if (fit$span.ratio < s$terminal_min_span_ratio) qc <- c(qc, "span_below_threshold")
      values[c("LAMZ", "LAMZHL")] <- c(fit$lambda.z, fit$half.life)
      used <- original[audit$SOURCE_ROW[original] %in% unlist(selected$source_rows)]
      audit$USE_TERMINAL[used] <- TRUE
      audit$TERMINAL_REASON[eligible] <- "not_selected"
      audit$TERMINAL_REASON[used] <- "selected"
      auc_reason <- ""
      if (!eligibility$aucinf_context) auc_reason <- "aucinf_requires_single_dose"
      else if (base$NCA_START_H != 0) auc_reason <- "partial_auc_start"
      else if (!is.finite(parameter("AUCLST"))) auc_reason <- "missing_auclast"
      else if (existing$PARAM_STATUS[match("AUCLST", existing$PARAMCD)] != "calculated") auc_reason <- "policy_assigned_auclast"
      else if (!is.finite(tlast) || selected$end_h != tlast) auc_reason <- "terminal_excludes_tlast"
      if (!nzchar(auc_reason)) {
        last <- original[audit$TIME_H[original] == tlast & audit$QUANTIFIABLE[original] & !audit$BLQ[original]]
        if (length(last) != 1L || !is.finite(original_conc$value[last]) || original_conc$value[last] <= 0) auc_reason <- "missing_original_clast"
        else {
          tail <- original_conc$value[last] / fit$lambda.z
          auc <- parameter("AUCLST") + tail
          extrapolation <- 100 * (tail / auc)
          if (!all(is.finite(c(tail, auc, extrapolation))) || tail <= 0 || auc <= 0 || extrapolation <= 0) auc_reason <- "aucinf_numeric_range"
          else values[c("AUCIFO", "AUCPEO")] <- c(auc, extrapolation)
        }
      }
      reasons[c("AUCIFO", "AUCPEO")] <- auc_reason
      if (length(qc) && s$terminal_qc_action == "withhold") {
        values[] <- NA_real_
        reasons[reasons == ""] <- paste(qc, collapse = ";")
      }
      if (is.finite(extrapolation) && extrapolation > s$aucinf_max_extrap_pct) {
        qc <- c(qc, "extrapolation_above_threshold")
        if (s$terminal_qc_action == "withhold") {
          values[c("AUCIFO", "AUCPEO")] <- NA_real_
          reasons[c("AUCIFO", "AUCPEO")] <- vapply(reasons[c("AUCIFO", "AUCPEO")], function(x) paste(c(x[nzchar(x)], "extrapolation_above_threshold"), collapse = ";"), character(1))
        }
      }
    }
    if (nzchar(rejection)) audit$TERMINAL_REASON[eligible] <- rejection
    if (nzchar(group$dose_review$reason %||% "")) {
      audit$TERMINAL_ELIGIBLE[profile] <- FALSE
      audit$TERMINAL_REASON[profile] <- group$dose_review$reason
    }
    warning <- unique(c(reasons[nzchar(reasons)], qc))
    added <- base[rep(1L, length(parameters)), , drop = FALSE]
    added$PARAMCD <- names(parameters); added$PARAM <- unname(parameters); added$AVAL <- unname(values); added$AVALU <- unname(units)
    added$NCA_EXCLUDE <- ifelse(nzchar(reasons), reasons, NA_character_)
    added$PARAM_STATUS <- ifelse(is.finite(values), "calculated", "not_calculable")
    if ("POLICY_ASSIGNED" %in% names(added)) added$POLICY_ASSIGNED <- FALSE
    added$NCA_WARNING <- if (length(warning)) paste(warning, collapse = ";") else NA_character_
    additions[[g]] <- added
    traces[[g]] <- list(profile = group$profile, identity = group$identity, method = s$terminal_method,
      status = if (all(is.na(values))) "not_calculable" else if (length(warning)) "review" else "calculated",
      reason = as.list(unique(reasons[nzchar(reasons)])), quality_reasons = as.list(qc), selected = selected,
      eligibility = eligibility, candidates = candidates, observed_aucinf = if (is.finite(auc)) auc else NULL,
      extrapolated_pct = if (is.finite(extrapolation)) extrapolation else NULL)
    value$trace$groups[[g]]$terminal <- traces[[g]]
    value$trace$groups[[g]]$status <- if (group$status == "excluded") "excluded" else if (group$status == "partial" || anyNA(values)) "partial" else if (group$status == "review" || length(warning)) "review" else "ok"
    value$data$PROFILE_STATUS[base_rows] <- value$trace$groups[[g]]$status
    additions[[g]]$PROFILE_STATUS <- value$trace$groups[[g]]$status
    value$trace$groups[[g]]$warning <- unique(c(group$warning, warning))
    if (length(warning)) warnings <- c(warnings, paste0("Profile ", group$profile, " terminal: ", paste(warning, collapse = ";")))
  }
  value$data <- rbind(value$data, do.call(rbind, additions)); rownames(value$data) <- NULL
  value$counts["result"] <- nrow(value$data)
  value$trace$rows <- audit
  value$trace$terminal <- list(algorithm = "pkdesktop.trailing.v1", adjusted_r_squared_tolerance = .0001,
    selection = "Trailing original positive non-BLQ points strictly after Tmax and infusion end; more points within adjusted R-squared tolerance",
    aucinf_definition = "Existing AUCLST plus original quantifiable Clast/lambda.z; eligible single-dose context only",
    profiles = traces)
  value$warnings <- c(value$warnings, warnings); value$trace$warnings <- value$warnings
  value
}

desktop_nca_terminal_eligibility <- function(data, rows, s, group, tlast) {
  decision <- list(policy_version = if (s$dosing_context == "isolated_single_period") "isolated-single-period-v1" else "legacy-dose-context-v1", context = s$dosing_context,
    washout_attestation = s$washout_attestation, history_confirmed = s$dose_history_confirmed,
    observation_end_h = group$observation_end_h, observation_time_complete = group$observation_time_complete,
    tlast_h = if (is.finite(tlast)) tlast else NULL, predose_qc = group$predose_qc,
    evidence = list(provenance = if ("NCA_ASSEMBLY_PROFILE" %in% names(data)) "assembly" else "declared_raw"),
    eligible = FALSE, aucinf_context = FALSE, reason = "")
  if (nzchar(group$dose_review$reason %||% "")) {
    decision$reason <- group$dose_review$reason
    return(decision)
  }
  if (s$dosing_context != "isolated_single_period") {
    bounds <- c(group$observation_end_h, tlast)
    endpoint <- if (any(is.finite(bounds))) max(bounds[is.finite(bounds)]) else Inf
    decision$reason <- desktop_nca_terminal_dose_reason(data, rows, s, endpoint)
  } else {
    evidence <- desktop_nca_dose_evidence(data, rows, s)
    decision$evidence <- evidence
    if (nzchar(evidence$reason)) decision$reason <- evidence$reason
    else if (!s$dose_history_confirmed) decision$reason <- "dose_history_unconfirmed"
    else if (!isTRUE(group$observation_time_complete)) decision$reason <- "incomplete_sampling_time_evidence"
    else if (!is.null(evidence$next_h) && any(c(group$observation_end_h, tlast)[is.finite(c(group$observation_end_h, tlast))] >= evidence$next_h))
      decision$reason <- "intervening_dose"
    else if (evidence$prior_count > 0 && s$washout_attestation != "protocol_confirmed") decision$reason <- "washout_unconfirmed"
    else if (evidence$prior_count > 0 && group$predose_qc$status == "exceeds") decision$reason <- "predose_qc_exceeded"
    else if (evidence$prior_count > 0 && !group$predose_qc$status %in% c("pass", "pass_blq")) decision$reason <- "predose_qc_required_for_prior_dose"
  }
  decision$eligible <- !nzchar(decision$reason)
  decision$aucinf_context <- decision$eligible && s$dosing_context %in% c("single", "isolated_single_period")
  decision
}
