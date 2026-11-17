desktop_assembly_keys <- function(x, label) {
  x <- unlist(x, use.names = FALSE)
  pk_assert(is.character(x) && length(x) > 0L && !anyNA(x) && all(nzchar(x)) &&
    !anyDuplicated(x), paste(label, "must contain distinct canonical column names"))
  x
}

desktop_assembly_tuple <- function(data, keys, missing = FALSE) {
  pk_assert(all(keys %in% names(data)), paste("Missing mapped keys:", paste(setdiff(keys, names(data)), collapse = ", ")))
  n <- nrow(data)
  result <- rep("", n)
  invalid <- rep(FALSE, n)
  for (key in keys) {
    x <- data[[key]]
    if (is.factor(x)) x <- as.character(x)
    pk_assert(is.character(x) || (is.numeric(x) && !inherits(x, c("POSIXt", "Date"))),
      paste("Key must be text or numeric:", key))
    bad <- is.na(x) | if (is.character(x)) !nzchar(trimws(x)) else !is.finite(x)
    invalid <- invalid | bad
    token <- if (is.numeric(x)) sprintf("%.17g", ifelse(x == 0, 0, x)) else enc2utf8(x)
    tag <- if (is.numeric(x)) "n" else "c"
    result <- paste0(result, tag, nchar(token, type = "bytes"), ":", token)
  }
  pk_assert(missing || !any(invalid), paste("Missing or blank keys:", paste(keys, collapse = ", ")))
  result[invalid] <- NA_character_
  result
}

desktop_assembly_map <- function(data, mapping, source) {
  pkapp_table_check(data)
  pk_assert(is.list(mapping) && length(mapping) > 0L && !is.null(names(mapping)) &&
    !anyDuplicated(names(mapping)) && all(grepl("^[A-Z][A-Z0-9_]*$", names(mapping))) &&
    !any(grepl("^NCA_", names(mapping))) && all(lengths(mapping) == 1L),
    paste(source, "mapping must be canonical role = source column"))
  columns <- unlist(mapping, use.names = FALSE)
  pk_assert(is.character(columns) && !anyNA(columns) && !anyDuplicated(columns) &&
    all(columns %in% names(data)), paste(source, "mapping has missing or repeated source columns"))
  out <- data[columns]
  names(out) <- names(mapping)
  for (name in names(out)) if (is.factor(out[[name]])) out[[name]] <- as.character(out[[name]])
  required <- switch(source, CONC = c("ID", "CONC"), PC = c("ID", "SAMPLE_DTM"), EX = c("ID", "DOSE_ID", "DOSE_DTM"))
  pk_assert(all(required %in% names(out)), paste(source, "requires mapped roles", paste(required, collapse = ", ")))
  for (name in intersect(c("ID", "STUDYID"), names(out)))
    pk_assert(is.character(out[[name]]), paste(source, name, "must be character to preserve identifiers"))
  out
}

pkapp_assemble_nca <- function(conc, pc, ex, specification) {
  fields <- c("version", "mapping", "sample_keys", "profile_keys", "dose_group_keys", "dose_record_keys",
    "observation_keys", "dose_reference", "datetime", "unmatched", "missing_datetime", "dose_history_confirmed")
  pk_assert(is.list(specification) && setequal(names(specification), fields) && !anyDuplicated(names(specification)) &&
    is.numeric(specification$version) && length(specification$version) == 1L &&
    !is.na(specification$version) && specification$version == 1, "Assembly specification must contain the exact version-1 fields")
  s <- specification
  s$version <- 1L
  pk_assert(is.list(s$mapping) && setequal(names(s$mapping), c("CONC", "PC", "EX")) && !anyDuplicated(names(s$mapping)),
    "mapping must contain CONC, PC, and EX")
  cdata <- desktop_assembly_map(conc, s$mapping$CONC, "CONC")
  pdata <- desktop_assembly_map(pc, s$mapping$PC, "PC")
  edata <- desktop_assembly_map(ex, s$mapping$EX, "EX")
  pk_assert(nrow(conc) > 0L, "CONC must contain observations")
  for (name in c("sample_keys", "profile_keys", "dose_group_keys", "dose_record_keys", "observation_keys")) {
    s[[name]] <- desktop_assembly_keys(s[[name]], name)
    pk_assert("ID" %in% s[[name]], paste(name, "must include ID"))
  }
  pk_assert("DOSE_ID" %in% s$dose_record_keys, "dose_record_keys must include DOSE_ID")
  for (name in c("sample_keys", "profile_keys", "dose_group_keys", "dose_record_keys", "observation_keys")) {
    has_study <- switch(name,
      sample_keys = "STUDYID" %in% intersect(names(cdata), names(pdata)),
      profile_keys = "STUDYID" %in% union(names(cdata), names(pdata)),
      dose_group_keys = "STUDYID" %in% intersect(union(names(cdata), names(pdata)), names(edata)),
      dose_record_keys = "STUDYID" %in% names(edata),
      observation_keys = "STUDYID" %in% names(cdata))
    pk_assert(!has_study || "STUDYID" %in% s[[name]], paste(name, "must include mapped STUDYID to prevent cross-study matches"))
  }
  pk_assert(all(s$sample_keys %in% intersect(names(cdata), names(pdata))), "sample_keys must be mapped in CONC and PC")
  for (key in s$sample_keys) pk_assert(is.numeric(cdata[[key]]) == is.numeric(pdata[[key]]),
    paste("Sample key types differ:", key))
  ckey <- desktop_assembly_tuple(cdata, s$sample_keys)
  pkey <- desktop_assembly_tuple(pdata, s$sample_keys)
  observation <- desktop_assembly_tuple(cdata, s$observation_keys)
  ekey <- desktop_assembly_tuple(edata, s$dose_record_keys)
  pk_assert(!anyDuplicated(pkey), "Duplicate PC sample keys; reconcile records or choose more keys")
  pk_assert(!anyDuplicated(observation), "Duplicate CONC observation keys; include analyte/replicate or reconcile records")
  pk_assert(!anyDuplicated(ekey), "Duplicate EX dose record keys")
  for (policy in c("unmatched", "missing_datetime")) pk_assert(is.character(s[[policy]]) && length(s[[policy]]) == 1L &&
    s[[policy]] %in% c("missing", "error"), paste(policy, "must be missing or error"))
  pk_assert(is.logical(s$dose_history_confirmed) && length(s$dose_history_confirmed) == 1L &&
    !is.na(s$dose_history_confirmed), "dose_history_confirmed must be explicitly true or false")
  clock <- s$datetime
  pk_assert(is.list(clock) && setequal(names(clock), c("sample_type", "dose_type", "utc_offset")) &&
    !anyDuplicated(names(clock)), "datetime requires sample_type, dose_type, and utc_offset")
  pk_assert(is.character(clock$utc_offset) && length(clock$utc_offset) == 1L, "utc_offset must be explicit")
  desktop_fixed_offset(clock$utc_offset)
  sample_time <- desktop_timestamp_parse(pdata$SAMPLE_DTM, clock$utc_offset, clock$sample_type)
  dose_time <- desktop_timestamp_parse(edata$DOSE_DTM, clock$utc_offset, clock$dose_type)
  ref <- s$dose_reference
  pk_assert(is.list(ref) && is.character(ref$mode) && length(ref$mode) == 1L &&
    ref$mode %in% c("first", "last", "selected", "per_profile"), "Choose an explicit dose reference mode")
  expected <- c("mode", switch(ref$mode, selected = "predicate", per_profile = "assignments", NULL))
  pk_assert(setequal(names(ref), expected) && !anyDuplicated(names(ref)), "Invalid dose_reference fields")
  selected <- rep(TRUE, nrow(ex))
  if (ref$mode == "selected") {
    selected <- desktop_predicate(ref$predicate, edata)
    pk_assert(is.logical(selected) && length(selected) %in% c(1L, nrow(ex)) && !anyNA(selected),
      "Dose predicate must return nonmissing logical values")
    if (length(selected) == 1L) selected <- rep(selected, nrow(ex))
  }
  pindex <- match(ckey, pkey)
  combined <- cdata
  for (name in names(pdata)) {
    attached <- pdata[[name]][pindex]
    if (name %in% names(combined)) {
      left <- combined[[name]]
      both <- !is.na(left) & !is.na(attached)
      pk_assert(!any(both & (as.character(left) != as.character(attached))), paste("Conflicting mapped CONC/PC role:", name))
      fill <- is.na(left)
      combined[[name]][fill] <- attached[fill]
    } else combined[[name]] <- attached
  }
  pk_assert(all(s$profile_keys %in% names(combined)), "profile_keys must be mapped in CONC or PC")
  pk_assert(all(s$dose_group_keys %in% intersect(names(combined), names(edata))), "dose_group_keys must be mapped in observations and EX")
  for (key in s$dose_group_keys) pk_assert(is.numeric(combined[[key]]) == is.numeric(edata[[key]]),
    paste("Dose group key types differ:", key))
  profile <- desktop_assembly_tuple(combined, s$profile_keys, missing = TRUE)
  group <- desktop_assembly_tuple(combined, s$dose_group_keys, missing = TRUE)
  egroup <- desktop_assembly_tuple(edata, s$dose_group_keys)
  pk_assert(("STUDYID" %in% names(combined)) == ("STUDYID" %in% names(edata)),
    "STUDYID must be mapped in both observations and EX to establish complete subject-study dose history")
  history_keys <- c("ID", if ("STUDYID" %in% intersect(names(combined), names(edata))) "STUDYID")
  subject <- desktop_assembly_tuple(combined, history_keys, missing = TRUE)
  ex_subject <- desktop_assembly_tuple(edata, history_keys)
  issues <- data.frame(stage = character(), code = character(), source = character(), source_row = integer(), message = character())
  reasons <- rep("", nrow(conc))
  issue <- function(rows, stage, code, message, source = "CONC", source_rows = rows) {
    if (!length(rows)) return(invisible(NULL))
    issues <<- rbind(issues, data.frame(stage = stage, code = code, source = source,
      source_row = as.integer(source_rows), message = message, stringsAsFactors = FALSE))
    reasons[rows] <<- ifelse(nzchar(reasons[rows]), paste(reasons[rows], code, sep = ";"), code)
  }
  issue(which(is.na(pindex)), "sample_match", "unmatched_pc", "No unique PC sample matches the concentration")
  issue(which(is.na(profile)), "profile", "missing_profile_key", "The analysis profile cannot be identified")
  issue(which(is.na(group)), "dose_reference", "missing_dose_group", "The dose group cannot be identified")
  assignment_profile <- assignment_dose <- character()
  if (ref$mode == "per_profile") {
    pk_assert(is.list(ref$assignments) && length(ref$assignments) > 0L && is.null(names(ref$assignments)), "assignments must be a nonempty array")
    for (assignment in ref$assignments) {
      pk_assert(is.list(assignment) && setequal(names(assignment), c("profile", "dose")) &&
        is.list(assignment$profile) && is.list(assignment$dose) &&
        setequal(names(assignment$profile), s$profile_keys) && setequal(names(assignment$dose), s$dose_record_keys) &&
        all(lengths(assignment$profile) == 1L) && all(lengths(assignment$dose) == 1L), "Assignments require complete scalar profile and dose key objects")
      assignment_profile <- c(assignment_profile, desktop_assembly_tuple(as.data.frame(assignment$profile, stringsAsFactors = FALSE), s$profile_keys))
      assignment_dose <- c(assignment_dose, desktop_assembly_tuple(as.data.frame(assignment$dose, stringsAsFactors = FALSE), s$dose_record_keys))
    }
    pk_assert(!anyDuplicated(assignment_profile), "Duplicate per-profile dose assignments")
  }
  eindex <- rep(NA_integer_, nrow(conc))
  dose_count <- rep(NA_integer_, nrow(conc))
  next_dose <- previous_dose <- rep(NA_real_, nrow(conc))
  next_index <- rep(NA_integer_, nrow(conc))
  prior_count <- later_count <- rep(NA_integer_, nrow(conc))
  history <- rep(FALSE, nrow(conc))
  references <- list()
  for (pk in unique(profile[!is.na(profile)])) {
    rows <- which(!is.na(profile) & profile == pk)
    groups <- unique(group[rows])
    pk_assert(length(groups) == 1L, "One analysis profile spans several dose groups; add the occasion to profile_keys")
    candidates <- which(!is.na(groups) & egroup == groups)
    history_rows <- which(!is.na(subject[rows[1L]]) & ex_subject == subject[rows[1L]])
    dose_count[rows] <- length(history_rows)
    known <- length(candidates) > 0L && all(dose_time$reason[candidates] == "")
    history_known <- length(history_rows) > 0L && all(dose_time$reason[history_rows] == "")
    tied_history <- anyDuplicated(dose_time$value[history_rows][!is.na(dose_time$value[history_rows])]) > 0L
    history[rows] <- isTRUE(s$dose_history_confirmed) && history_known && !tied_history
    if (length(history_rows) && !history_known)
      issue(rows, "dose_history", "unknown_history_dose_datetime", "An EX datetime in the subject's dose history is missing or invalid")
    if (tied_history)
      issue(rows, "dose_history", "tied_dose_history", "Distinct EX records have the same actual datetime; reconcile split or simultaneous doses explicitly")
    chosen <- integer()
    problem <- ""
    if (!length(candidates)) problem <- "no_reference_dose" else if (ref$mode %in% c("first", "last")) {
      if (!known) problem <- "unknown_candidate_dose_datetime" else {
        times <- dose_time$value[candidates]
        extreme <- if (ref$mode == "first") min(times) else max(times)
        chosen <- candidates[times == extreme]
      }
    } else if (ref$mode == "selected") chosen <- candidates[selected[candidates]] else {
      ai <- match(pk, assignment_profile)
      if (!is.na(ai)) chosen <- candidates[ekey[candidates] == assignment_dose[ai]]
    }
    if (!nzchar(problem) && length(chosen) != 1L) problem <- if (!length(chosen)) "no_reference_dose" else "ambiguous_reference_dose"
    if (!nzchar(problem)) {
      eindex[rows] <- chosen
      if (dose_time$reason[chosen] != "") issue(rows, "datetime", "missing_dose_datetime", "The selected EX actual datetime is missing or invalid")
      if (!is.na(dose_time$value[chosen])) {
        earlier <- dose_time$value[history_rows][!is.na(dose_time$value[history_rows]) & dose_time$value[history_rows] < dose_time$value[chosen]]
        later <- dose_time$value[history_rows][!is.na(dose_time$value[history_rows]) & dose_time$value[history_rows] > dose_time$value[chosen]]
        if (history_known && !tied_history) {
          prior_count[rows] <- length(earlier)
          later_count[rows] <- length(later)
        }
        if (length(earlier)) {
          hours <- (max(earlier) - dose_time$value[chosen]) / 3600
          if (is.finite(hours)) previous_dose[rows] <- hours else {
            history[rows] <- FALSE
            issue(rows, "dose_history", "invalid_dose_interval", "The interval between EX doses exceeds the numeric range")
          }
        }
        if (length(later)) {
          hours <- (min(later) - dose_time$value[chosen]) / 3600
          if (is.finite(hours)) {
            next_dose[rows] <- hours
            following <- history_rows[!is.na(dose_time$value[history_rows]) & dose_time$value[history_rows] == min(later)]
            if (length(following) == 1L) next_index[rows] <- following
          } else {
            history[rows] <- FALSE
            issue(rows, "dose_history", "invalid_dose_interval", "The interval between EX doses exceeds the numeric range")
          }
        }
      }
    } else issue(rows, "dose_reference", problem, "A unique explicit reference dose cannot be assigned")
    references[[length(references) + 1L]] <- list(profile = pk, mode = ref$mode,
      candidates = as.list(candidates), history_rows = as.list(history_rows), history_keys = as.list(history_keys),
      selected = if (length(chosen) == 1L && !nzchar(problem)) chosen else NULL, reason = problem,
      evidence_version = 2L, previous_h = previous_dose[rows[1]], next_h = next_dose[rows[1]],
      prior_count = prior_count[rows[1]], later_count = later_count[rows[1]],
      dose_count = dose_count[rows[1]], source_complete = history[rows[1]])
  }
  invalid_sample <- !is.na(pindex) & sample_time$reason[pindex] != ""
  invalid_sample[is.na(invalid_sample)] <- FALSE
  issue(which(invalid_sample), "datetime", "missing_sample_datetime", "PC actual datetime is missing, partial, or invalid")
  invalid_ex <- which(dose_time$reason != "")
  if (length(invalid_ex)) issues <- rbind(issues, data.frame(stage = "dose_history", code = "invalid_ex_datetime", source = "EX",
    source_row = invalid_ex, message = dose_time$reason[invalid_ex], stringsAsFactors = FALSE))
  time <- (sample_time$value[pindex] - dose_time$value[eindex]) / 3600
  invalid_elapsed <- which(!is.na(time) & !is.finite(time))
  issue(invalid_elapsed, "datetime", "invalid_elapsed_time", "Actual elapsed time exceeds the numeric range")
  time[invalid_elapsed] <- NA_real_
  pk_assert(s$unmatched != "error" || (!anyNA(pindex) && !anyNA(eindex) && !anyNA(profile)), "Unmatched samples or unresolved dose references; inspect assembly mappings and selections")
  pk_assert(s$missing_datetime != "error" || (!anyNA(time) && !length(invalid_ex)), "Missing or invalid actual datetimes; nominal times cannot replace actual clocks")
  reserved <- c("NCA_CONC_ROW", "NCA_PC_ROW", "NCA_EX_ROW", "NCA_DOSE_REF_ID", "NCA_TIME_H", "NCA_NEXT_DOSE_H",
    "NCA_NEXT_EX_ROW", "NCA_NEXT_DOSE_REF_ID", "NCA_DOSE_INPUT_COLUMNS", "NCA_DOSE_INPUT_SIGNATURE", "NCA_DOSE_EPOCH_S",
    "NCA_DOSE_COUNT", "NCA_DOSE_EVIDENCE_VERSION", "NCA_PREV_DOSE_H", "NCA_PRIOR_DOSE_COUNT", "NCA_LATER_DOSE_COUNT",
    "NCA_DOSE_HISTORY_CONFIRMED", "NCA_DOSE_REFERENCE_COLUMNS", "NCA_ASSEMBLY_STATUS", "NCA_ASSEMBLY_REASON",
    "NCA_ASSEMBLY_PROFILE_COLUMNS", "NCA_ASSEMBLY_PROFILE")
  output <- conc
  fresh <- function(name) {
    while (name %in% names(output)) name <- paste0("SOURCE_", name)
    name
  }
  for (name in intersect(names(output), reserved)) names(output)[names(output) == name] <- fresh(paste0("CONC_", name))
  for (name in names(combined)) {
    if (name %in% names(output) && (!identical(s$mapping$CONC[[name]], name) || !identical(output[[name]], combined[[name]])))
      names(output)[names(output) == name] <- fresh(paste0("CONC_", name))
    output[[name]] <- combined[[name]]
  }
  for (name in names(pc)) output[[fresh(paste0("PC_", name))]] <- pc[[name]][pindex]
  ex_columns <- stats::setNames(character(ncol(ex)), names(ex))
  for (name in names(ex)) {
    column <- fresh(paste0("EX_", name))
    output[[column]] <- ex[[name]][eindex]
    ex_columns[[name]] <- column
  }
  for (name in setdiff(names(edata), names(combined))) {
    if (name %in% names(output)) {
      renamed <- fresh(paste0("CONC_", name))
      names(output)[names(output) == name] <- renamed
      ex_columns[ex_columns == name] <- renamed
    }
    output[[name]] <- edata[[name]][eindex]
  }
  output$NCA_CONC_ROW <- seq_len(nrow(conc)); output$NCA_PC_ROW <- pindex; output$NCA_EX_ROW <- eindex
  reference_columns <- stats::setNames(as.list(ex_columns[unlist(s$mapping$EX[s$dose_record_keys], use.names = FALSE)]), s$dose_record_keys)
  output$NCA_DOSE_REFERENCE_COLUMNS <- as.character(jsonlite::toJSON(reference_columns, auto_unbox = TRUE))
  output$NCA_DOSE_REF_ID <- ekey[eindex]; output$NCA_TIME_H <- time
  output$NCA_DOSE_EPOCH_S <- dose_time$value[eindex]
  dose_inputs <- unique(c(unname(ex_columns), "NCA_EX_ROW", "NCA_DOSE_REF_ID", "NCA_DOSE_EPOCH_S"))
  dose_input_context <- list(version = 1L, columns = as.list(dose_inputs),
    character_columns = as.list(dose_inputs[vapply(output[dose_inputs], is.character, logical(1))]))
  output$NCA_DOSE_INPUT_COLUMNS <- desktop_pk_blq_json(dose_input_context)
  output$NCA_NEXT_DOSE_H <- next_dose; output$NCA_DOSE_COUNT <- dose_count
  output$NCA_NEXT_EX_ROW <- next_index; output$NCA_NEXT_DOSE_REF_ID <- ekey[next_index]
  output$NCA_DOSE_EVIDENCE_VERSION <- 2L
  output$NCA_PREV_DOSE_H <- previous_dose
  output$NCA_PRIOR_DOSE_COUNT <- prior_count; output$NCA_LATER_DOSE_COUNT <- later_count
  output$NCA_DOSE_HISTORY_CONFIRMED <- history
  output$NCA_ASSEMBLY_STATUS <- ifelse(nzchar(reasons), "unresolved", "ok")
  output$NCA_ASSEMBLY_REASON <- reasons
  output$NCA_ASSEMBLY_PROFILE <- profile
  output$NCA_ASSEMBLY_PROFILE_COLUMNS <- as.character(jsonlite::toJSON(s$profile_keys, auto_unbox = FALSE))
  output$NCA_DOSE_INPUT_SIGNATURE <- desktop_dose_input_signature(output, dose_input_context)
  rownames(output) <- NULL
  unused <- setdiff(seq_len(nrow(pc)), pindex[!is.na(pindex)])
  counts <- c(conc = nrow(conc), pc = nrow(pc), ex = nrow(ex), matched_pc = sum(!is.na(pindex)), unmatched_conc = sum(is.na(pindex)),
    unused_pc = length(unused), unresolved_dose = sum(is.na(eindex)), missing_time = sum(is.na(time)), result = nrow(output), issues = nrow(issues))
  for (name in c("sample_keys", "profile_keys", "dose_group_keys", "dose_record_keys", "observation_keys")) s[[name]] <- as.list(s[[name]])
  list(data = output, counts = counts, specification = s, issues = issues,
    trace = list(rows = data.frame(CONC_ROW = seq_len(nrow(conc)), PC_ROW = pindex, EX_ROW = eindex,
      PROFILE = profile, SAMPLE_UTC_SECONDS = sample_time$value[pindex], DOSE_UTC_SECONDS = dose_time$value[eindex],
      TIME_H = time, STATUS = output$NCA_ASSEMBLY_STATUS, REASON = reasons, stringsAsFactors = FALSE),
      dose_references = references, dose_reference_columns = reference_columns, unused_pc_rows = unused, datetime = list(sample = sample_time, dose = dose_time), issues = issues))
}
