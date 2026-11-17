pk_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

pk_hash <- function(path) digest::digest(file = path, algo = "sha256")

pk_csv_records <- function(path) {
  contents <- readBin(path, "raw", n = file.info(path)$size)
  pk_assert(!any(contents == as.raw(0)), paste("Invalid CSV: embedded NUL byte in", basename(path)))
  if (!length(contents)) return(list())
  text <- rawToChar(contents)
  Encoding(text) <- "UTF-8"
  text <- sub("^\ufeff", "", text)
  if (!nzchar(text)) return(list())

  # Match complete fields contiguously; neither bare quotes nor suffixes after
  # a closing quote are valid. Possessive repeats keep long fields linear.
  pattern <- '\\G(?:"(?:[^"]++|"")*+"|[^",\r\n]*+)(?:,|\r\n|\r|\n|\\z)'
  # Byte offsets avoid rescanning the UTF-8 prefix for each extracted field.
  matches <- gregexpr(pattern, text, perl = TRUE, useBytes = TRUE)[[1]]
  widths <- attr(matches, "match.length")
  pk_assert(matches[1] == 1L && all(matches == c(1, head(cumsum(widths), -1L) + 1)) &&
    sum(widths) == nchar(text, type = "bytes"), "Invalid CSV quoting: malformed or unterminated field")
  tokens <- regmatches(text, list(matches))[[1]]
  tokens <- tokens[widths > 0L]
  Encoding(tokens) <- "UTF-8"
  # A trailing comma adds an empty field; a final line ending adds no record.
  if (endsWith(text, ",")) tokens <- c(tokens, "")
  ends_record <- grepl("[\r\n]\\z", tokens, perl = TRUE)
  values <- sub("(?:,|\r\n|\r|\n)\\z", "", tokens, perl = TRUE)
  blank <- values == "" & ends_record & c(TRUE, head(ends_record, -1L))
  quoted <- startsWith(values, '"')
  values[quoted] <- gsub('""', '"', substr(values[quoted], 2L, nchar(values[quoted]) - 1L), fixed = TRUE)
  record <- cumsum(c(1L, head(ends_record, -1L)))
  records <- unname(split(values, record))
  if (any(blank)) records[record[blank]] <- rep(list(character()), sum(blank))
  records
}

pk_csv <- function(path) {
  pk_assert(file.exists(path), paste("Missing file:", path))
  records <- pk_csv_records(path)
  columns <- if (length(records)) records[[1]] else character()
  pk_assert(length(columns) > 0 && all(nzchar(trimws(columns))) && identical(columns, trimws(columns)) && !anyDuplicated(columns),
            "Invalid or duplicate CSV header")
  pk_assert(all(lengths(records) == length(columns)), "CSV field count differs between records")
  values <- matrix(as.character(unlist(records[-1], use.names = FALSE)), ncol = length(columns), byrow = TRUE)
  result <- as.data.frame(values, stringsAsFactors = FALSE)
  names(result) <- columns
  result[result == "" | result == "NA"] <- NA_character_
  for (name in intersect(c("STUDYID", "ARM", "SEX", "DOSE_MG", "ROUTE", "AGE", "WT"), columns)) {
    missing <- is.na(result[[name]]) | trimws(result[[name]]) %in% c("", "NA")
    result[[name]][missing] <- NA_character_
  }
  result
}

pk_flag <- function(x, name = "analysis") {
  token <- toupper(trimws(as.character(x)))
  value <- vapply(token, function(t) {
    if (is.na(t)) return(NA)
    if (t %in% c("Y", "TRUE")) return(TRUE)
    if (t %in% c("N", "FALSE")) return(FALSE)
    if (!grepl("^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:E[+-]?[0-9]+)?$", t, perl = TRUE)) return(NA)
    parts <- strsplit(t, "E", fixed = TRUE)[[1]]
    mantissa <- sub("^[+-]", "", parts[1])
    digits <- gsub(".", "", mantissa, fixed = TRUE)
    if (!grepl("[1-9]", digits)) return(FALSE)
    if (startsWith(t, "-") || !grepl("^0*10*$", digits)) return(NA)
    fraction <- if (grepl(".", mantissa, fixed = TRUE)) nchar(sub("^[^.]*\\.", "", mantissa)) else 0L
    trailing <- nchar(digits) - nchar(sub("0+$", "", digits))
    exponent <- if (length(parts) == 2L) suppressWarnings(as.numeric(parts[2])) else 0
    # Only the integer exponent is numeric; the coefficient is checked exactly.
    if (is.finite(exponent) && exponent == fraction - trailing) TRUE else NA
  }, logical(1), USE.NAMES = FALSE)
  pk_assert(!anyNA(value), paste("Missing or invalid", name, "flag"))
  value
}

pk_identity_label <- function(data, column, subjects) {
  values <- lapply(subjects, function(id) {
    x <- unique(unlist(lapply(data[c("ADNCA", "ADPP")], function(d) {
      if (column %in% names(d)) as.character(d[d$USUBJID == id, column]) else character()
    }), use.names = FALSE))
    x <- x[!is.na(x) & nzchar(trimws(x))]
    pk_assert(length(x) <= 1L, paste("Conflicting", column, "for", id))
    x
  })
  values <- sort(unique(unlist(values, use.names = FALSE)))
  if (column == "ANALYTE") pk_assert(length(values) <= 1L, "Multiple ANALYTE values in one bundle; split analytes into separate bundles")
  if (length(values)) paste(values, collapse = ", ") else "Not provided"
}

pk_dose_key <- function(x, name = "DOSE_MG") {
  vapply(as.character(x), function(value) {
    if (is.na(value) || value %in% c("", "NA")) return(NA_character_)
    token <- trimws(value)
    pk_assert(grepl("^[+]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$", token, perl = TRUE),
              paste("Invalid nonnegative decimal", name))
    parts <- strsplit(toupper(token), "E", fixed = TRUE)[[1]]
    mantissa <- sub("^[+]", "", parts[1])
    digits <- sub("^0+", "", gsub(".", "", mantissa, fixed = TRUE))
    if (!nzchar(digits)) return("0")
    number <- suppressWarnings(as.numeric(token))
    pk_assert(is.finite(number) && number != 0, paste(name, "is outside the supported numeric range"))
    fraction <- if (grepl(".", mantissa, fixed = TRUE)) nchar(sub("^[^.]*\\.", "", mantissa)) else 0L
    significant <- sub("0+$", "", digits)
    exponent <- if (length(parts) == 2L) as.numeric(parts[2]) else 0
    power <- exponent - fraction + nchar(digits) - nchar(significant)
    position <- nchar(significant) + power
    if (position > 0 && position <= 21) {
      if (power >= 0) return(paste0(significant, strrep("0", power)))
      return(paste0(substr(significant, 1, position), ".", substring(significant, position + 1)))
    }
    if (position >= -5 && position <= 0) return(paste0("0.", strrep("0", -position), significant))
    mantissa <- paste0(substr(significant, 1, 1), if (nchar(significant) > 1) paste0(".", substring(significant, 2)) else "")
    paste0(mantissa, "e", sprintf("%.0f", position - 1))
  }, character(1), USE.NAMES = FALSE)
}

pk_number <- function(x, name, allow_na = FALSE) {
  token <- trimws(x[!is.na(x)])
  pk_assert(!any(grepl("^[+-]?(inf(inity)?|nan)$", token, ignore.case = TRUE)), paste("Non-finite", name))
  pk_assert(all(grepl("^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$", token, perl = TRUE)),
            paste("Non-numeric", name))
  # Normalize decimal syntax without rounding; jsonlite's parser round-trips doubles.
  token <- sub("^\\+", "", token)
  token <- sub("^(-?)0+([0-9])", "\\1\\2", token)
  token <- sub("^\\.", "0.", token)
  token <- sub("^-\\.", "-0.", token)
  token <- sub("\\.([eE]|$)", ".0\\1", token)
  value <- rep(NA_real_, length(x))
  if (length(token)) value[!is.na(x)] <- jsonlite::fromJSON(paste0("[", paste(token, collapse = ","), "]"))
  pk_assert(!any(is.infinite(value) | is.nan(value)), paste("Non-finite", name))
  nonzero <- grepl("[1-9]", sub("[eE].*$", "", token))
  pk_assert(!any(nonzero & value[!is.na(x)] == 0), paste(name, "is outside the supported numeric range (underflow)"))
  if (!allow_na) pk_assert(!anyNA(value), paste("Missing", name))
  value
}

pk_validate_legacy_times <- function(path) {
  raw <- pk_csv(path)
  time <- pk_number(raw$TIME_H, "ADPC TIME_H")
  conc <- pk_number(raw$AVAL, "ADPC AVAL")
  pk_assert(!any(conc < 0), "Negative concentration in ADPC")
  pk_assert(!anyDuplicated(data.frame(subject = raw$USUBJID, time = time)),
            "Duplicate subject/time concentration records")
  # Match make_adnca.R's read.csv -> as.character -> as.numeric -> paste path.
  legacy <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
  legacy_time <- suppressWarnings(as.numeric(as.character(legacy$TIME_H)))
  keys <- paste(as.character(legacy$USUBJID), legacy_time, sep = "\r")
  pk_assert(all(is.finite(legacy_time)) && !anyDuplicated(keys),
            paste("Automatic NCA cannot preserve distinct subject/time keys through legacy conversion.",
                  "Provide ADPP calculated from the original times; no bundle was saved."))
  invisible(TRUE)
}

pk_validate_analysis_time <- function(conc) {
  if (!all(c("SSNCAFL", "TAD_H") %in% names(conc))) return(invisible(NULL))
  selected <- conc[pk_flag(conc$SSNCAFL), , drop = FALSE]
  pk_assert(!anyNA(selected$TAD_H), "Selected steady_state records have missing TAD_H")
  pk_assert(!anyDuplicated(selected[c("USUBJID", "TAD_H")]),
            "Duplicate subject/TAD_H in the selected analysis interval")
  for (id in unique(selected$USUBJID)) {
    d <- selected[selected$USUBJID == id, , drop = FALSE]
    origin <- d$TIME_H - d$TAD_H
    pk_assert(all(is.finite(origin)), paste("Non-finite analysis-dose origin for", id))
    # Bound subtraction roundoff using the magnitudes of its operands, not a clinical time window.
    tolerance <- 8 * .Machine$double.eps * pmax(abs(d$TIME_H), abs(d$TAD_H), abs(origin), abs(origin[1]))
    pk_assert(all(abs(origin - origin[1]) <= tolerance), paste("Inconsistent analysis-dose origin for", id))
  }
  invisible(NULL)
}

pk_load_bundle <- function(path) {
  files <- file.path(path, c("ADNCA.csv", "ADPP.csv"))
  conc <- pk_csv(files[1])
  params <- pk_csv(files[2])
  pk_assert(nrow(conc) > 0 && nrow(params) > 0, "Both ADNCA and ADPP must contain records")
  pk_assert(all(c("USUBJID", "TIME_H", "AVAL", "AVALU") %in% names(conc)),
            "ADNCA must contain concentration data: USUBJID, TIME_H, AVAL, AVALU. Parameters belong in ADPP.")
  pk_assert(all(c("USUBJID", "PARAMCD", "AVAL", "AVALU") %in% names(params)),
            "ADPP requires USUBJID, PARAMCD, AVAL, AVALU")
  for (d in list(conc, params)) {
    pk_assert(!anyNA(d$USUBJID) && all(nzchar(trimws(d$USUBJID))), "USUBJID must not be blank")
    pk_assert(!anyNA(d$AVALU) && all(nzchar(trimws(d$AVALU))), "AVALU must specify a unit for every record")
    if ("DOSE_MG" %in% names(d)) pk_dose_key(d$DOSE_MG)
    for (name in intersect(c("MDV", "BLQ", "SSNCAFL", "TROUGHFL", "ANL01FL"), names(d))) pk_flag(d[[name]], name)
  }
  if ("PARAMCD" %in% names(conc)) {
    pk_assert(all(!is.na(conc$PARAMCD) & conc$PARAMCD == "CONC"),
              "ADNCA PARAMCD must be CONC in this version; do not load PK parameters as concentrations")
  }
  pk_assert(!anyNA(params$PARAMCD) && all(nzchar(params$PARAMCD)), "ADPP PARAMCD is missing")
  pk_assert(!any(params$PARAMCD == "CONC"), "Concentration records belong in ADNCA, not ADPP")
  conc$TIME_H <- pk_number(conc$TIME_H, "ADNCA TIME_H")
  conc$AVAL <- pk_number(conc$AVAL, "ADNCA AVAL", TRUE)
  params$AVAL <- pk_number(params$AVAL, "ADPP AVAL", TRUE)
  pk_assert(!any(conc$AVAL < 0, na.rm = TRUE), "Negative concentration in ADNCA")
  if ("TAD_H" %in% names(conc)) conc$TAD_H <- pk_number(conc$TAD_H, "TAD_H", TRUE)
  pk_assert(!anyDuplicated(conc[c("USUBJID", "TIME_H")]),
            "Duplicate subject/time concentration records; split analytes or occasions into separate bundles")
  pk_validate_analysis_time(conc)
  pk_assert(!anyDuplicated(params[c("USUBJID", "PARAMCD")]),
            "Duplicate subject/parameter records; split analytes or occasions into separate bundles")
  pk_assert(setequal(unique(conc$USUBJID), unique(params$USUBJID)),
            "ADNCA and ADPP subject sets differ")
  for (name in c("STUDYID", "ANALYTE")) pk_identity_label(list(ADNCA = conc, ADPP = params), name, unique(conc$USUBJID))
  for (name in intersect(c("STUDYID", "ARM", "SEX", "DOSE_MG", "ROUTE", "AGE", "WT"), union(names(conc), names(params)))) {
    for (id in unique(conc$USUBJID)) {
      values <- unlist(lapply(list(conc, params), function(d) {
        if (name %in% names(d)) d[d$USUBJID == id, name] else character()
      }), use.names = FALSE)
      values <- unique(if (name %in% c("DOSE_MG", "AGE", "WT")) pk_dose_key(values, name) else values)
      pk_assert(!anyNA(values) || all(is.na(values)), paste("Partially missing subject attribute", name, "for", id))
      values <- values[!is.na(values)]
      pk_assert(length(values) <= 1, paste(name, "is inconsistent for", id))
    }
  }
  hashes <- stats::setNames(lapply(files, pk_hash), basename(files))
  manifest_path <- file.path(path, "BUNDLE.json")
  manifest <- if (file.exists(manifest_path)) jsonlite::fromJSON(manifest_path, simplifyVector = FALSE) else NULL
  if (!is.null(manifest)) pk_assert(identical(manifest$version, 2L), "Legacy review bundle: prepare a new bundle with the repaired importer")
  if (!is.null(manifest$files)) {
    pk_assert(identical(hashes, manifest$files), "Bundle data changed after preparation; create a new bundle")
  }
  if (!is.null(manifest)) hashes[["BUNDLE.json"]] <- pk_hash(manifest_path)
  list(ADNCA = conc, ADPP = params, hashes = hashes, manifest = manifest)
}

pk_inspect <- function(bundle) {
  d <- pk_load_bundle(bundle)
  eligible <- if ("ANL01FL" %in% names(d$ADPP)) pk_flag(d$ADPP$ANL01FL, "ANL01FL") else rep(TRUE, nrow(d$ADPP))
  list(bundle = normalizePath(bundle), subjects = I(sort(unique(d$ADNCA$USUBJID))),
       n_subjects = length(unique(d$ADNCA$USUBJID)), concentration_rows = nrow(d$ADNCA),
       parameter_rows = nrow(d$ADPP), parameters = I(sort(unique(d$ADPP$PARAMCD))),
       eligible_parameters = I(sort(unique(d$ADPP$PARAMCD[eligible]))),
       group_columns = I(intersect(c("ARM", "SEX", "DOSE_MG", "ROUTE"), intersect(names(d$ADNCA), names(d$ADPP)))),
       profiles = I(c("all", if ("SSNCAFL" %in% names(d$ADNCA) && any(pk_flag(d$ADNCA$SSNCAFL))) "steady_state",
                    if ("TROUGHFL" %in% names(d$ADNCA) && any(pk_flag(d$ADNCA$TROUGHFL))) "trough")),
       input_hashes = d$hashes)
}

pk_read_config <- function(path) {
  cfg <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  pk_validate_history(cfg)
  cfg
}

pk_validate_history <- function(cfg) {
  previous <- NULL
  for (event in cfg$audit) {
    pk_assert(all(vapply(event[c("actor", "reason", "channel", "at_utc")], function(x) is.character(x) && length(x) == 1 && nzchar(x), logical(1))), "Incomplete view change history")
    if (!is.null(previous)) pk_assert(identical(event$before, previous) && event$after$revision == previous$revision + 1L,
                                     "Inconsistent view change history")
    previous <- event$after
  }
  if (length(cfg$audit)) pk_assert(identical(previous, cfg[setdiff(names(cfg), "audit")]), "Settings do not match the recorded change history")
  invisible(cfg)
}

pk_strata_columns <- function(data) {
  setdiff(names(data), c("USUBJID", "SUBJID", "ID", "AVAL", "AVALU", "TIME_H", "TAD_H", "PARAMCD", "NONE",
    grep("^\\.", names(data), value = TRUE)))
}
pk_validate_config <- function(cfg, data) {
  fields <- c("version", "revision", "title", "bundle", "input_hashes", "profile", "x", "scale", "statistic", "interval", "show_individual", "group_by", "subjects", "parameters", "include_mdv", "blq", "digits", "include_listings")
  extended <- identical(cfg$version, 2L)
  if (extended) fields <- c(fields, "facet_by")
  pk_assert(setequal(setdiff(names(cfg), "audit"), fields), "Unknown or missing configuration field")
  pk_validate_history(cfg)
  pk_assert(identical(cfg$version, 1L) || identical(cfg$version, 2L), "Unsupported configuration version")
  pk_assert(is.numeric(cfg$revision) && length(cfg$revision) == 1 && cfg$revision >= 1 && cfg$revision == floor(cfg$revision), "Invalid revision")
  pk_assert(is.character(cfg$title) && length(cfg$title) == 1 && nchar(cfg$title) >= 1 && nchar(cfg$title) <= 160, "Title must be 1 to 160 characters")
  pk_assert(identical(cfg$input_hashes, data$hashes), "Input checksum differs from saved view; create a new view for the changed data")
  options <- list(profile = c("all", "steady_state", "trough"), x = c("TIME_H", "TAD_H"),
                  scale = c("linear", "log10"), statistic = c("mean", "median", "geometric_mean"),
                  interval = c("none", "sd", if (extended) "se", "p05_p95"), blq = c("exclude", "as_recorded"))
  for (name in names(options)) pk_assert(is.character(cfg[[name]]) && length(cfg[[name]]) == 1 && cfg[[name]] %in% options[[name]], paste("Invalid", name))
  for (name in c("show_individual", "include_mdv", "include_listings")) pk_assert(is.logical(cfg[[name]]) && length(cfg[[name]]) == 1 && !is.na(cfg[[name]]), paste("Invalid", name))
  pk_assert(is.numeric(cfg$digits) && length(cfg$digits) == 1 && cfg$digits %in% 1:6, "digits must be an integer from 1 to 6")
  for (name in c("subjects", "parameters")) {
    pk_assert(is.list(cfg[[name]]) && all(vapply(cfg[[name]], function(x) is.character(x) && length(x) == 1, logical(1))), paste(name, "must be a string array"))
    pk_assert(!anyDuplicated(unlist(cfg[[name]])), paste("Duplicate", name))
  }
  pk_assert(length(cfg$parameters) > 0, "Select at least one ADPP parameter")
  pk_assert(all(unlist(cfg$parameters) %in% data$ADPP$PARAMCD), "Unknown ADPP parameter")
  pk_assert(all(unlist(cfg$subjects) %in% data$ADNCA$USUBJID), "Unknown subject")
  pk_assert(is.character(cfg$group_by) && length(cfg$group_by) >= 1 && !anyNA(cfg$group_by) &&
    !anyDuplicated(cfg$group_by) && all(cfg$group_by %in% c("NONE", if (extended)
      intersect(pk_strata_columns(data$ADNCA), pk_strata_columns(data$ADPP)) else c("ARM", "SEX", "DOSE_MG", "ROUTE"))) &&
    (!"NONE" %in% cfg$group_by || length(cfg$group_by) == 1), "Invalid group_by")
  if (!identical(cfg$group_by, "NONE")) pk_assert(all(vapply(data[c("ADNCA", "ADPP")], function(d) all(cfg$group_by %in% names(d)), logical(1))), "Grouping column must exist in both datasets")
  if (extended) {
    pk_assert(is.character(cfg$facet_by) && length(cfg$facet_by) <= 2 && !anyNA(cfg$facet_by) &&
      !anyDuplicated(cfg$facet_by) && all(cfg$facet_by %in% pk_strata_columns(data$ADNCA)), "Invalid facet_by")
    # Additional grouping columns must describe the same subject in both inputs.
    for (column in setdiff(cfg$group_by, c("NONE", "ARM", "SEX", "DOSE_MG", "ROUTE"))) {
      for (id in unique(data$ADNCA$USUBJID)) {
        values <- unlist(lapply(data[c("ADNCA", "ADPP")], function(d) as.character(d[[column]][d$USUBJID == id])), use.names = FALSE)
        pk_assert(length(unique(values)) <= 1, paste("Inconsistent grouping column", column, "across subject records"))
      }
    }
  }
  pk_assert(!cfg$interval %in% c("sd", "se") || cfg$statistic == "mean", "SD intervals require the arithmetic mean; choose none or p05_p95 with other statistics")
  pk_assert(cfg$x != "TAD_H" || cfg$profile == "steady_state", "TAD_H requires the steady_state profile")
  if (cfg$profile != "all") {
    flag <- if (cfg$profile == "steady_state") "SSNCAFL" else "TROUGHFL"
    pk_assert(flag %in% names(data$ADNCA) && any(pk_flag(data$ADNCA[[flag]])), paste("No", cfg$profile, "records in this bundle"))
  }
  if (cfg$x == "TAD_H") pk_assert("TAD_H" %in% names(data$ADNCA), "TAD_H is unavailable")
  invisible(cfg)
}

pk_update_config <- function(path, changes, revision, actor, reason) {
  python <- Sys.getenv("PK_REVIEW_PYTHON", file.path(pk_root, ".venv", "bin", "python"))
  pk_assert(file.exists(python), "Set PK_REVIEW_PYTHON to the review Python interpreter")
  request <- tempfile(fileext = ".json")
  log <- tempfile()
  errors <- tempfile()
  on.exit(unlink(c(request, log, errors)))
  jsonlite::write_json(list(changes = changes, expected_revision = revision, actor = actor, reason = reason, channel = "gui"),
                       request, auto_unbox = TRUE, null = "null", digits = NA)
  status <- suppressWarnings(system2(python, c(shQuote(file.path(pk_root, "tools", "pk_review.py")), "update", shQuote(normalizePath(path)),
                                               "--request", shQuote(request)), stdout = log, stderr = errors))
  pk_assert(status == 0, paste(readLines(errors, warn = FALSE), collapse = "\n"))
  saved <- jsonlite::fromJSON(log, simplifyVector = FALSE)$config
  pk_assert(!is.null(saved) && saved$revision == revision + 1L, "Invalid update response")
  pk_validate_history(saved)
  invisible(saved)
}
