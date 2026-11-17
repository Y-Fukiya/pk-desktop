desktop_nca_contract <- desktop_read(file.path(pk_root, "apps/pk-desktop/nca_mcp_contract.json"))

desktop_public_validation <- function(message) {
  stop(structure(list(message = message, call = NULL, code = "E_VALIDATION"),
    class = c("pkapp_public_error", "pkapp_error", "error", "condition")))
}

desktop_schema_valid <- function(value, schema, definitions, depth = 0L) {
  if (depth > 70L) return(FALSE)
  valid <- function(x, rule) desktop_schema_valid(x, rule, definitions, depth + 1L)
  if (!is.null(schema[["$ref"]])) return(valid(value, definitions[[sub("^#/\\$defs/", "", schema[["$ref"]])]]))
  if (!is.null(schema$oneOf) && sum(vapply(schema$oneOf, function(rule) valid(value, rule), logical(1))) != 1L) return(FALSE)
  if (!is.null(schema$allOf) && !all(vapply(schema$allOf, function(rule) valid(value, rule), logical(1)))) return(FALSE)
  if (!is.null(schema$`if`)) {
    branch <- if (valid(value, schema$`if`)) schema$then else schema$`else`
    if (!is.null(branch) && !valid(value, branch)) return(FALSE)
  }
  if (!is.null(schema$type)) {
    scalar <- is.atomic(value) && length(value) == 1L && !is.na(value)
    matches <- vapply(unlist(schema$type), function(type) switch(type,
      object = is.list(value) && (length(value) == 0L || !is.null(names(value))),
      array = is.list(value) && is.null(names(value)),
      string = scalar && is.character(value), number = scalar && is.numeric(value) && is.finite(value),
      integer = scalar && is.numeric(value) && is.finite(value) && value == floor(value),
      boolean = scalar && is.logical(value), `null` = is.null(value), FALSE), logical(1))
    if (!any(matches)) return(FALSE)
  }
  if ("const" %in% names(schema) && !identical(value, schema$const)) return(FALSE)
  if (!is.null(schema$enum) && !any(vapply(schema$enum, function(x) identical(value, x), logical(1)))) return(FALSE)
  if (is.character(value) && length(value) == 1L) {
    if (!is.null(schema$minLength) && nchar(value) < schema$minLength) return(FALSE)
    if (!is.null(schema$pattern) && !grepl(schema$pattern, value, perl = TRUE)) return(FALSE)
    if (!is.null(schema$maxLength) && nchar(value) > schema$maxLength) return(FALSE)
  }
  if (is.numeric(value) && length(value) == 1L && !is.na(value)) {
    if (!is.null(schema$minimum) && value < schema$minimum) return(FALSE)
    if (!is.null(schema$maximum) && value > schema$maximum) return(FALSE)
    if (!is.null(schema$exclusiveMinimum) && value <= schema$exclusiveMinimum) return(FALSE)
    if (!is.null(schema$exclusiveMaximum) && value >= schema$exclusiveMaximum) return(FALSE)
  }
  if (is.list(value)) {
    if (!is.null(schema$required) && !all(unlist(schema$required) %in% names(value))) return(FALSE)
    if (!is.null(schema$minProperties) && length(value) < schema$minProperties) return(FALSE)
    if (!is.null(schema$propertyNames) && !all(vapply(names(value), function(key) valid(key, schema$propertyNames), logical(1)))) return(FALSE)
    if (identical(schema$additionalProperties, FALSE) && !all(names(value) %in% names(schema$properties))) return(FALSE)
    if (is.list(schema$additionalProperties)) {
      extra <- setdiff(names(value), names(schema$properties))
      if (!all(vapply(extra, function(key) valid(value[[key]], schema$additionalProperties), logical(1)))) return(FALSE)
    }
    for (key in intersect(names(value), names(schema$properties))) if (!valid(value[[key]], schema$properties[[key]])) return(FALSE)
    if (!is.null(schema$minItems) && length(value) < schema$minItems) return(FALSE)
    if (!is.null(schema$maxItems) && length(value) > schema$maxItems) return(FALSE)
    if (isTRUE(schema$uniqueItems) && anyDuplicated(vapply(value, desktop_json, character(1)))) return(FALSE)
    if (!is.null(schema$items) && !all(vapply(value, function(x) valid(x, schema$items), logical(1)))) return(FALSE)
  }
  TRUE
}

desktop_validate_prepare_steps <- function(steps) {
  if (!is.list(steps) || !is.null(names(steps)) || length(steps) > 100L)
    desktop_public_validation("steps must be an array with at most 100 operations.")
  definitions <- desktop_nca_contract$preparation_steps[["$defs"]]
  for (step in steps) {
    op <- if (is.list(step)) step$op else NULL
    if (!is.list(step) || !is.character(op) || length(op) != 1L || is.na(op) || !op %in% setdiff(names(definitions), "predicate"))
      desktop_public_validation("step.op must be filter, select, rename, convert, sort, distinct, where, elapsed_time, or blq_rules.")
    if (!desktop_schema_valid(step, definitions[[op]], definitions)) {
      message <- if (op == "filter") "filter: use column, operator (eq/ne/in/ge/le/gt/lt/missing/present), and values as a non-empty array. Comparisons other than in require one value; missing/present use no values." else
        paste0(op, ": invalid arguments; use the published types and required fields: ", paste(names(definitions[[op]]$properties), collapse = ", "), ".")
      desktop_public_validation(message)
    }
  }
  invisible(TRUE)
}

desktop_prepare_checked <- function(data, steps) {
  before <- nrow(data); trace <- list()
  guidance <- c(filter = "Check that column exists and comparison values have compatible types.",
    select = "Use existing, unique column names in columns.", rename = "Use an existing column and a new, non-empty, unique name.",
    convert = "Use an existing column and character/numeric; non-numeric values cannot be converted to numeric.",
    sort = "Use existing, unique columns and a boolean decreasing flag.", distinct = "Use existing, unique columns.",
    where = "Check predicate column names, argument counts, types and depth.",
    elapsed_time = "Check date columns, datetime modes, UTC offset and a new output column; invalid_datetime=missing preserves other rows.",
    blq_rules = "Check source columns, distinct time/profile roles, flags, all required rules, LLOQ and unused NCA output columns.")
  for (step in steps) {
    value <- tryCatch(pkapp_prepare(data, list(step)), error = function(e)
      desktop_public_validation(paste0(step$op, ": ", guidance[[step$op]])))
    data <- value$data; trace <- c(trace, value$trace)
  }
  list(data = data, counts = c(input = before, result = nrow(data)), trace = trace)
}

desktop_source <- function(engine, source) {
  if (source %in% c("ADNCA", "ADPP")) engine$data[[source]] else desktop_table_get(engine, source)
}
desktop_describe <- function(engine, source) {
  data <- desktop_source(engine, source)
  list(source = source, rows = nrow(data), columns = as.list(names(data)),
    types = as.list(vapply(data, function(x) class(x)[1], character(1))),
    missing = as.list(vapply(data, function(x) sum(is.na(x)), integer(1))))
}
desktop_allowed_bundle <- function(engine, path) {
  desktop_check(is.character(path) && length(path) == 1, "E_PATH_DENIED", "Project-local bundle required")
  path <- normalizePath(path, mustWork = TRUE)
  desktop_check(startsWith(path, paste0(engine$project, .Platform$file.sep)), "E_PATH_DENIED", "Place the bundle inside the active project")
  for (name in c("ADNCA.csv", "ADPP.csv", "BUNDLE.json")) {
    file <- file.path(path, name)
    if (file.exists(file)) desktop_check(startsWith(normalizePath(file), paste0(engine$project, .Platform$file.sep)),
      "E_PATH_DENIED", "Input symlinks must stay inside the active project")
  }
  path
}
desktop_prepare_api <- function(engine, args) {
  desktop_revision(engine, args$base_revision, args$project_id)
  data <- desktop_source(engine, args$source)
  value <- if (identical(engine$mode, "nca")) desktop_prepare_checked(data, args$steps) else NULL
  desktop_table_save(engine, args$name, list(source = data),
    list(kind = "prepare", steps = args$steps), "mcp", args$actor, frozen = value,
    provenance = if (identical(engine$mode, "nca")) list(identity = desktop_nca_identity_metadata(args, "mcp")) else NULL)
}
desktop_join_api <- function(engine, args) {
  desktop_revision(engine, args$base_revision, args$project_id)
  desktop_table_save(engine, args$name,
    list(left = desktop_source(engine, args$left), right = desktop_source(engine, args$right)),
    list(kind = "join", by = args$by, columns = args$columns, join = args$join, right_name = args$right), "mcp", args$actor,
    provenance = if (identical(engine$mode, "nca")) list(identity = desktop_nca_identity_metadata(args, "mcp")) else NULL)
}
pkapp_save_preparation <- function(project, data, steps, name) {
  desktop_table_save(desktop_r_engine(project), name, list(source = data), list(kind = "prepare", steps = steps), "r", "R user")
}
pkapp_tables <- function(project) desktop_tables(desktop_r_engine(project))
pkapp_table <- function(project, id) desktop_table_get(desktop_r_engine(project), id)

desktop_validate_assembly <- function(specification) {
  schema <- desktop_nca_contract$assembly_specification
  if (!desktop_schema_valid(specification, schema, schema[["$defs"]]))
    desktop_public_validation("Invalid assembly specification; check the published schema and required mappings, keys, policies, and clock.")
  invisible(TRUE)
}

desktop_assemble_nca <- function(engine, args, channel = "mcp") {
  desktop_nca_idle(engine)
  desktop_nca_revision(engine, args$base_revision, args$project_id)
  desktop_nca_identity(args)
  desktop_validate_assembly(args$specification)
  ids <- args[c("conc", "pc", "ex")]
  valid_id <- function(id) is.character(id) && length(id) == 1L && !is.na(id) && grepl("^table-[a-f0-9]{24}$", id)
  desktop_check(all(vapply(ids, valid_id, logical(1))), "E_VALIDATION", "Assembly sources must be saved table IDs")
  desktop_check(is.character(args$name) && length(args$name) == 1L && !is.na(args$name) && nzchar(trimws(args$name)) && nchar(args$name) <= 100L, "E_VALIDATION", "Assembly name must contain 1 to 100 characters")
  inputs <- lapply(ids, function(id) desktop_table_get(engine, id))
  value <- tryCatch(pkapp_assemble_nca(inputs$conc, inputs$pc, inputs$ex, args$specification), error = function(e)
    desktop_public_validation("Assembly failed; check source columns, keys, policies, and actual datetimes in the local UI."))
  provenance <- list(source_ids = ids, source_hashes = lapply(inputs, desktop_hash),
    identity = desktop_nca_identity_metadata(args, channel), reason = args$reason)
  saved <- desktop_table_save(engine, args$name, inputs, list(kind = "nca_assembly", specification = value$specification),
    channel, args$actor, frozen = value, provenance = provenance)
  before <- engine$nca
  engine$nca$revision <- engine$nca$revision + 1L
  tryCatch(desktop_nca_record(engine, "nca_table_operation", args, channel,
    list(operation = "assemble_nca", table_id = saved$id, source_ids = ids, source_hashes = provenance$source_hashes)),
    error = function(e) { engine$nca <- before; stop(e) })
  engine$nca_disclosure <- NULL
  desktop_nca_notify(engine)
  saved$revision <- engine$nca$revision
  saved
}
