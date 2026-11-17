desktop_nca_derived_parameters <- function() c(
  CMAXD = "Dose-normalized Cmax (absolute mg dose)", AUCLSTD = "Dose-normalized AUC Last (absolute mg dose)",
  AUCIFOD = "Dose-normalized AUC Infinity Obs (absolute mg dose)", CLFO = "Apparent Clearance Obs (CL/F)",
  VZFO = "Apparent Volume Lambda z Obs (Vz/F)", CLFOWT = "Apparent Clearance Obs (CL/F) per kg",
  VZFOWT = "Apparent Volume Lambda z Obs (Vz/F) per kg", CLFOBSA = "Apparent Clearance Obs (CL/F) per m2",
  VZFOBSA = "Apparent Volume Lambda z Obs (Vz/F) per m2")

desktop_dose_input_signature <- function(data, context) {
  profile <- desktop_assembly_profile_columns(data) %||% character()
  columns <- unique(c(unlist(context$columns, use.names = FALSE), profile,
    intersect(c("NCA_ASSEMBLY_PROFILE", "NCA_ASSEMBLY_PROFILE_COLUMNS"), names(data))))
  character_columns <- c(profile, unlist(context$character_columns, use.names = FALSE))
  canonical <- desktop_pk_blq_canonical(data, columns, character_columns)
  vapply(paste(desktop_pk_blq_json(context), desktop_pk_blq_rows(canonical, named = TRUE), sep = "\n"),
    digest::digest, character(1), algo = "sha256", serialize = FALSE, USE.NAMES = FALSE)
}

desktop_dose_input_columns <- function(data) {
  fail <- "基準EXの投与量・単位・経路の記録が不正です。EX・PC・濃度から再作成してください。"
  markers <- c("NCA_DOSE_INPUT_COLUMNS", "NCA_DOSE_INPUT_SIGNATURE")
  pk_assert(all(markers %in% names(data)), fail)
  text <- unique(as.character(data$NCA_DOSE_INPUT_COLUMNS))
  pk_assert(length(text) == 1L && !is.na(text), fail)
  context <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  columns <- unlist(context$columns, use.names = FALSE)
  pk_assert(is.list(context) && identical(context$version, 1L) && is.character(columns) && !anyDuplicated(columns) &&
    all(columns %in% names(data)) && all(unlist(context$character_columns) %in% columns) &&
    all(c("NCA_EX_ROW", "NCA_DOSE_REF_ID", "NCA_DOSE_EPOCH_S") %in% columns), fail)
  pk_assert(identical(as.character(data$NCA_DOSE_INPUT_SIGNATURE), desktop_dose_input_signature(data, context)), fail)
  setdiff(columns, c("NCA_EX_ROW", "NCA_DOSE_REF_ID", "NCA_DOSE_EPOCH_S"))
}

desktop_nca_derived_specification <- function(data, specification) {
  fields <- intersect(c("dose", "body_size", "derived_parameters"), names(specification))
  if (!length(fields)) return(list())
  s <- desktop_sort(jsonlite::fromJSON(desktop_json(specification[fields]), simplifyVector = FALSE))
  for (field in fields) pk_assert(desktop_schema_valid(s[[field]], desktop_nca_contract$nca_specification$properties[[field]], list()),
    paste("投与量・体格の設定を確認してください：", field))
  parameters <- unlist(s$derived_parameters, use.names = FALSE)
  if (length(parameters)) pk_assert(!is.null(s$dose), "追加パラメータには実投与量の設定が必要です。")
  binding <- function(x) pk_assert(all(unlist(x[c("value_column", "unit_column")]) %in% names(data)), "体格の値・単位列を確認してください。")
  if (!is.null(s$dose)) {
    columns <- unlist(s$dose[c("amount_column", "unit_column", "route_column")], use.names = FALSE)
    pk_assert(length(unique(columns)) == 3L && all(columns %in% names(data)), "実投与量・単位・経路は存在する別の列を選択してください。")
    assembled <- any(c("NCA_EX_ROW", "NCA_DOSE_REF_ID", "NCA_ASSEMBLY_PROFILE") %in% names(data))
    if (s$dose$reference == "selected_dose") pk_assert(all(columns %in% desktop_dose_input_columns(data)), "実投与量・単位・経路には選択した基準EXの保存列を選択してください。")
    else pk_assert(!assembled, "EX結合済み入力には選択した基準投与を使用してください。")
    invalid_ev <- toupper(trimws(unlist(s$dose$extravascular_values))) %in% c("IV", "IV BOLUS", "IV INFUSION", "INTRAVENOUS", "INTRAVENOUS BOLUS", "INTRAVENOUS INFUSION", "静脈内")
    pk_assert(!any(invalid_ev), "静脈内投与を血管外投与に指定できません。CL/F・Vz/Fは血管外投与のみです。")
  }
  if (!is.null(s$body_size)) {
    context <- desktop_covariate_context(data, c(specification$id, unlist(specification$groups, use.names = FALSE)))
    pk_assert(!is.null(context), "体格にはNCA入力のDM・VS画面で採用・保存した測定値を使ってください。")
    role <- function(x, name) {
      binding(x); selected <- context$roles[[name]]
      pk_assert(!is.null(selected) && identical(x$value_column, selected$value_column) && identical(x$unit_column, selected$unit_column), "体格の列が保存済みの測定役割と一致しません。")
    }
    if (!is.null(s$body_size$weight)) role(s$body_size$weight, "weight")
    if (!is.null(s$body_size$bsa)) {
      if (s$body_size$bsa$mode == "recorded") role(s$body_size$bsa, "bsa")
      else { pk_assert(!is.null(s$body_size$weight), "Mosteller体表面積には採用した体重が必要です。"); role(s$body_size$bsa$height, "height") }
    }
  }
  s
}

desktop_nca_unit <- function(unit, dimension, basis = "absolute") {
  if (length(unit) != 1L || is.na(unit)) return(NULL)
  raw <- unit
  unit <- gsub("μ", "u", gsub("µ", "u", trimws(unit), fixed = TRUE), fixed = TRUE)
  unit <- gsub("²", "2", gsub("^2", "2", unit, fixed = TRUE), fixed = TRUE)
  aliases <- list(mass = c(g="g", G="g", mg="mg", MG="mg", ug="ug", UG="ug", mcg="ug", MCG="ug", ng="ng", NG="ng"),
    volume = c(L="L", l="L", mL="mL", ml="mL", ML="mL"),
    weight = c(kg="kg", KG="kg", g="g", G="g", lb="lb", LB="lb"),
    height = c(cm="cm", CM="cm", m="m", M="m", `in`="in", IN="in"), bsa = c(m2="m2", M2="m2"))
  tokens <- trimws(strsplit(unit, "/", fixed = TRUE)[[1]])
  # strsplit drops trailing empty fields; require exactly the expected slash count.
  slashes <- nchar(unit) - nchar(gsub("/", "", unit, fixed = TRUE))
  token <- function(x, kind) { y <- unname(aliases[[kind]][x]); if (length(y) != 1L || is.na(y)) NULL else y }
  factor_mass <- c(g=1000, mg=1, ug=.001, ng=.000001)
  canonical <- switch(dimension, weight="kg", height="cm", bsa="m2", mass="mg", concentration="mg/L")
  if (dimension == "concentration") {
    if (slashes != 1L || length(tokens) != 2L) return(NULL)
    a <- token(tokens[1], "mass"); b <- token(tokens[2], "volume")
    if (is.null(a) || is.null(b)) return(NULL)
    normalized <- paste(a, b, sep="/"); factor <- unname(factor_mass[a] / c(L=1, mL=.001)[b])
  } else if (dimension == "mass") {
    expected <- if (basis == "absolute") 0L else 1L
    if (slashes != expected || length(tokens) != expected + 1L) return(NULL)
    a <- token(tokens[1], "mass"); if (is.null(a)) return(NULL)
    suffix <- ""
    if (expected) {
      b <- token(tokens[2], if (basis == "per_kg") "weight" else "bsa")
      required <- if (basis == "per_kg") "kg" else "m2"
      if (is.null(b) || b != required) return(NULL)
      suffix <- paste0("/", b)
    }
    normalized <- paste0(a, suffix); canonical <- paste0(canonical, suffix); factor <- unname(factor_mass[a])
  } else {
    if (slashes != 0L || length(tokens) != 1L) return(NULL)
    normalized <- token(tokens[1], dimension); if (is.null(normalized)) return(NULL)
    factors <- switch(dimension, weight=c(kg=1, g=.001, lb=.45359237), height=c(cm=1, m=100, `in`=2.54), bsa=c(m2=1))
    factor <- unname(factors[normalized])
  }
  list(original_unit=raw, normalized_unit=normalized, canonical_unit=canonical, factor=factor)
}

desktop_nca_unit_key <- function(unit, dimension = "concentration", basis = "absolute") {
  vapply(unit, function(x) { c <- desktop_nca_unit(x, dimension, basis); if (is.null(c)) trimws(x) else c$normalized_unit }, character(1))
}

desktop_nca_ranges <- function() list(weight=c(.5, 300), height=c(30, 250), bsa=c(.1, 3.5))

desktop_nca_range_qc <- function(number, dimension) {
  bounds <- desktop_nca_ranges()[[dimension]]
  if (is.null(bounds) || !is.finite(number)) return(NULL)
  list(converted_value=number, lower=bounds[1], upper=bounds[2], inclusive=TRUE, status=if (number < bounds[1] || number > bounds[2]) "review" else "pass")
}

desktop_nca_quantity <- function(data, rows, binding, dimension, context = NULL, role = dimension, basis = "absolute") {
  q <- list(value=NA_real_, converted_value=NA_real_, reason="", original_value=NA_real_, conversion=NULL, source=binding, baseline_status=NULL, observations=NULL, range_qc=NULL)
  fail <- function(reason) { q$reason <- paste0(reason, "_", role); q }
  if (is.null(binding)) return(fail("missing"))
  values <- desktop_pk_numeric(data[[binding$value_column]][rows]); units <- as.character(data[[binding$unit_column]][rows])
  q$observations <- data.frame(value=values$value, unit=units, stringsAsFactors=FALSE)
  if (any(values$error != "") || any(!is.finite(values$value)) || any(values$value <= 0)) return(fail("invalid"))
  q$original_value <- values$value[1]
  conversions <- lapply(units, desktop_nca_unit, dimension=dimension, basis=basis)
  q$conversion <- conversions[[1]]
  if (any(vapply(conversions, is.null, logical(1)))) return(fail("unsupported_unit"))
  if (length(unique(values$value)) != 1L || length(unique(vapply(conversions, `[[`, character(1), "normalized_unit"))) != 1L) return(fail("ambiguous"))
  q$converted_value <- values$value[1] * q$conversion$factor
  if (!is.null(context)) {
    status <- data[[context$roles[[role]]$status_column]][rows]; q$baseline_status <- unique(status)
    if (any(is.na(status) | status == "postdose")) return(fail("nonbaseline"))
  }
  if (!is.finite(q$converted_value) || q$converted_value <= 0) return(fail("numeric_range"))
  q$range_qc <- desktop_nca_range_qc(q$converted_value, dimension)
  if (identical(q$range_qc$status, "review")) return(fail("outside_range"))
  q$value <- q$converted_value; q
}

desktop_nca_derived_apply <- function(value, source, s) {
  requested <- unlist(s$derived_parameters, use.names = FALSE)
  if (!length(requested)) return(value)
  s <- utils::modifyList(desktop_nca_terminal_defaults(), s, keep.null = TRUE)
  parameters <- desktop_nca_derived_parameters()[requested]
  units <- c(CMAXD = "1/L", AUCLSTD = "h/L", AUCIFOD = "h/L", CLFO = "L/h", VZFO = "L", CLFOWT = "L/h/kg", VZFOWT = "L/kg", CLFOBSA = "L/h/m2", VZFOBSA = "L/m2")
  context <- desktop_covariate_context(source)
  source_groups <- desktop_pk_group(source, c(s$id, unlist(s$groups, use.names = FALSE)))$group
  mapping <- unlist(value$trace$output_key_mapping)
  concentration <- desktop_nca_unit(value$trace$concentration_unit, "concentration")
  additions <- list(); traces <- list()
  for (g in seq_along(value$trace$groups)) {
    group <- value$trace$groups[[g]]; rows <- which(source_groups == g)
    result_rows <- rep(TRUE, nrow(value$data))
    for (key in names(group$identity)) result_rows <- result_rows & as.character(value$data[[key]]) == as.character(group$identity[[key]])
    base <- value$data[result_rows, , drop = FALSE]
    weight <- desktop_nca_quantity(source, rows, s$body_size$weight, "weight", context)
    if (identical(s$body_size$bsa$mode, "mosteller")) {
      height <- desktop_nca_quantity(source, rows, s$body_size$bsa$height, "height", context)
      bsa <- list(value = NA_real_, reason = if (nzchar(weight$reason)) weight$reason else height$reason,
        formula = "Mosteller: sqrt(height_cm * weight_kg / 3600)", weight = weight, height = height)
      if (!nzchar(bsa$reason)) {
        bsa$value <- sqrt(height$value * weight$value / 3600)
        bsa$converted_value <- bsa$value
        bsa$range_qc <- desktop_nca_range_qc(bsa$value, "bsa")
        if (!is.finite(bsa$value) || bsa$value <= 0) { bsa$value <- NA_real_; bsa$reason <- "numeric_range_bsa" }
        else if (identical(bsa$range_qc$status, "review")) { bsa$value <- NA_real_; bsa$reason <- "outside_range_bsa" }
      }
    } else bsa <- desktop_nca_quantity(source, rows, s$body_size$bsa, "bsa", context)
    d <- s$dose
    dose <- desktop_nca_quantity(source, rows, list(value_column=d$amount_column, unit_column=d$unit_column), "mass", role="dose", basis=d$basis)
    dose$basis <- d$basis; dose$reference <- d$reference
    dose$source <- d[c("amount_column", "unit_column", "route_column")]
    dose$ex_row <- if ("NCA_EX_ROW" %in% names(source)) unique(source$NCA_EX_ROW[rows]) else NULL
    dose$reference_id <- if ("NCA_DOSE_REF_ID" %in% names(source)) unique(source$NCA_DOSE_REF_ID[rows]) else NULL
    if (dose$reason == "unsupported_unit_dose") dose$reason <- "unsupported_dose_unit_or_basis"
    size <- switch(d$basis, per_kg=weight, per_m2=bsa, list(value=1, reason=""))
    if (!nzchar(dose$reason) && nzchar(size$reason)) { dose$value <- NA_real_; dose$reason <- size$reason }
    if (!nzchar(dose$reason)) {
      dose$value <- dose$converted_value * size$value
      if (!is.finite(dose$value) || dose$value <= 0) { dose$value <- NA_real_; dose$reason <- "numeric_range_dose" }
    }
    if (d$basis != "absolute") dose$body_size <- size
    routes <- unique(as.character(source[[d$route_column]][rows]))
    ev <- length(routes) == 1L && !is.na(routes) && nzchar(trimws(routes)) && routes %in% unlist(d$extravascular_values)
    dose$route <- routes
    if (length(routes) != 1L) dose$reason <- "ambiguous_route"
    else if (is.na(routes) || !nzchar(trimws(routes))) dose$reason <- "missing_route"
    eligibility <- desktop_nca_terminal_eligibility(source, rows, s, group, group$tlast_quant %||% NA_real_)
    common <- if (group$status == "excluded") group$dose_review$reason %||% "invalid_analysis_profile" else
      if (!s$dosing_context %in% c("single", "isolated_single_period") || s$scope != "all") "unsupported_dosing_context" else eligibility$reason
    if (!nzchar(common) && nzchar(dose$reason)) common <- dose$reason
    if (!nzchar(common) && is.null(concentration)) common <- "unsupported_concentration_unit"
    dependency <- function(code) {
      row <- base[base$PARAMCD == code, , drop = FALSE]
      if (nrow(row) != 1L || row$PARAM_STATUS != "calculated" || !is.finite(row$AVAL) || row$AVAL < 0 || isTRUE(row$POLICY_ASSIGNED))
        return(list(value = NA_real_, reason = paste0("dependency_unavailable_", code)))
      list(value = row$AVAL, reason = "")
    }
    result <- list()
    compute <- function(code) {
      if (!is.null(result[[code]])) return(result[[code]])
      reason <- common; number <- NA_real_; dependencies <- list()
      if (!nzchar(reason)) {
        endpoint <- if (code == "CMAXD") "CMAX" else if (code == "AUCLSTD") "AUCLST" else "AUCIFO"
        if (code %in% c("CMAXD", "AUCLSTD", "AUCIFOD", "CLFO")) {
          dependencies$endpoint <- dependency(endpoint)
          reason <- dependencies$endpoint$reason
          if (!nzchar(reason) && code == "CLFO" && !ev) reason <- "unsupported_route"
          if (!nzchar(reason)) {
            endpoint_value <- dependencies$endpoint$value * concentration$factor
            if (code == "CLFO" && endpoint_value <= 0) reason <- "dependency_unavailable_AUCIFO"
            else {
              number <- if (code == "CLFO") dose$value / endpoint_value else endpoint_value / dose$value
              if (dependencies$endpoint$value > 0 && (endpoint_value == 0 || number == 0)) reason <- "numeric_range"
            }
          }
        } else {
          parent <- if (code == "VZFO") "CLFO" else if (code %in% c("CLFOWT", "CLFOBSA")) "CLFO" else "VZFO"
          dependencies$parent <- compute(parent)
          denominator <- if (code == "VZFO") dependency("LAMZ") else if (endsWith(code, "WT")) weight else bsa
          dependencies$denominator <- denominator
          reason <- if (nzchar(dependencies$parent$reason)) dependencies$parent$reason else denominator$reason
          if (!nzchar(reason) && denominator$value <= 0) reason <- "dependency_unavailable_LAMZ"
          if (!nzchar(reason)) number <- dependencies$parent$value / denominator$value
        }
      }
      if (!nzchar(reason) && (!is.finite(number) || number < 0 || (number == 0 && !code %in% c("CMAXD", "AUCLSTD", "AUCIFOD")))) reason <- "numeric_range"
      ret <- list(value = if (nzchar(reason)) NA_real_ else number, reason = reason, dependencies = dependencies)
      result[[code]] <<- ret
      ret
    }
    computed <- lapply(requested, compute)
    added <- base[rep(1L, length(requested)), , drop = FALSE]
    added$PARAMCD <- requested; added$PARAM <- unname(parameters); added$AVALU <- unname(units[requested])
    added$AVAL <- vapply(computed, `[[`, numeric(1), "value")
    reasons <- vapply(computed, `[[`, character(1), "reason")
    added$PARAM_STATUS <- ifelse(is.finite(added$AVAL), "calculated", "not_calculable")
    added$NCA_EXCLUDE <- ifelse(nzchar(reasons), reasons, NA_character_)
    inherited <- unique(base$NCA_WARNING[!is.na(base$NCA_WARNING) & nzchar(base$NCA_WARNING)])
    added$NCA_WARNING <- if (length(inherited)) paste(inherited, collapse = "; ") else NA_character_
    if ("POLICY_ASSIGNED" %in% names(added)) added$POLICY_ASSIGNED <- FALSE
    review_reasons <- unique(c(weight$reason, bsa$reason, bsa$height$reason %||% ""))
    review_reasons <- review_reasons[startsWith(review_reasons, "outside_range_")]
    oral <- length(routes) == 1L && !is.na(routes) && toupper(trimws(routes)) %in% c("ORAL", "PO", "P.O.", "経口")
    audit <- value$trace$rows
    oral_c0 <- oral && s$dosing_context %in% c("single", "isolated_single_period") && any(audit$PROFILE == group$profile & !audit$IMPUTED & audit$QUANTIFIABLE & audit$TIME_H == 0, na.rm=TRUE)
    if (oral_c0) review_reasons <- c(review_reasons, "oral_quantifiable_time_zero")
    if (length(review_reasons)) {
      warning_text <- paste(review_reasons, collapse="; ")
      value$data$NCA_WARNING[result_rows] <- ifelse(is.na(value$data$NCA_WARNING[result_rows]) | !nzchar(value$data$NCA_WARNING[result_rows]), warning_text, paste(value$data$NCA_WARNING[result_rows], warning_text, sep="; "))
      added$NCA_WARNING <- ifelse(is.na(added$NCA_WARNING) | !nzchar(added$NCA_WARNING), warning_text, paste(added$NCA_WARNING, warning_text, sep="; "))
    }
    status <- if (group$status == "excluded") "excluded" else if (group$status == "partial" || anyNA(added$AVAL)) "partial" else if (group$status == "review" || length(inherited) || length(review_reasons)) "review" else "ok"
    value$data$PROFILE_STATUS[result_rows] <- status; added$PROFILE_STATUS <- status
    value$trace$groups[[g]]$status <- status
    value$trace$groups[[g]]$warning <- unique(c(group$warning, reasons[nzchar(reasons)], review_reasons))
    new_warnings <- setdiff(unique(c(reasons[nzchar(reasons)], review_reasons)), group$warning)
    if (length(new_warnings)) value$warnings <- c(value$warnings, paste0("Profile ", group$profile, ": ", paste(new_warnings, collapse = "; ")))
    traces[[g]] <- list(profile = group$profile, identity = group$identity, dose = dose, weight = weight, bsa = bsa,
      concentration_conversion = concentration, eligibility = eligibility, review = review_reasons, parameters = result)
    value$trace$groups[[g]]$derived <- traces[[g]]
    additions[[g]] <- added
  }
  value$data <- rbind(value$data, do.call(rbind, additions)); rownames(value$data) <- NULL
  value$counts["result"] <- nrow(value$data)
  value$trace$warnings <- value$warnings
  value$trace$derived <- list(version = 1L, canonical_units = "mg, mg/L, h, kg, cm, m2", codes = "Application-defined PARAMCDs, not a CDISC mapping", profiles = traces,
    covariate_context = context, formulas = list(CMAXD = "Cmax_mg_per_L / dose_mg", AUCLSTD = "AUClast_mg_h_per_L / dose_mg", AUCIFOD = "AUCinf_obs_mg_h_per_L / dose_mg",
      CLFO = "dose_mg / AUCinf_obs_mg_h_per_L", VZFO = "CLFO / lambda_z_per_h", size_normalization = "absolute apparent parameter / selected weight_kg or BSA_m2"))
  value
}
