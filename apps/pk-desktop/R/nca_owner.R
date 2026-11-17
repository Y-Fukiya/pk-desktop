desktop_nca_state_hash <- function(state) {
  state$state_hash <- NULL
  desktop_hash(state)
}
desktop_nca_write_state <- function(engine, state) {
  state$state_hash <- desktop_nca_state_hash(state)
  desktop_atomic(state, file.path(engine$project, "nca-state.json"))
  engine$nca <- state
  invisible(state)
}
desktop_nca_identity_metadata <- function(args, channel) {
  client <- if (identical(channel, "mcp") && is.list(args[["_mcp_client"]])) args[["_mcp_client"]] else list()
  scalar <- function(x) if (is.character(x) && length(x) == 1L && !is.na(x) && nchar(x) <= 200L) x else NULL
  list(declared_actor = args$actor, os_user = unname(Sys.info()[["user"]]), os_pid = Sys.getpid(),
    client_name = scalar(client$name), client_version = scalar(client$version),
    client_identity_source = if (identical(channel, "mcp")) "mcp_initialization_self_reported" else "local_ui",
    authenticated_human_identity = FALSE)
}
desktop_nca_notify <- function(engine) {
  engine$nca_signal(shiny::isolate(engine$nca_signal()) + 1L)
  invisible(NULL)
}
desktop_nca_busy <- function(engine) !is.null(engine$nca_job) && identical(engine$nca_job$status, "running")
desktop_nca_idle <- function(engine) {
  desktop_check(!isTRUE(engine$nca_closing), "E_CLOSED", "The NCA owner is closing")
  desktop_check(!desktop_nca_busy(engine), "E_BUSY", "NCA is running; wait for the current job to finish")
}
desktop_nca_changes <- function(before, source, specification) {
  old <- c(list(source = before$source), before$specification)
  new <- c(list(source = source), specification)
  fields <- union(names(old), names(new))
  changed <- fields[!vapply(fields, function(field) identical(desktop_hash(old[[field]]), desktop_hash(new[[field]])), logical(1))]
  lapply(changed, function(field) list(field = field, before = old[[field]], after = new[[field]]))
}
desktop_nca_record <- function(engine, action, args, channel, details = list()) {
  state <- engine$nca
  event <- c(list(at = desktop_time(), action = action, channel = channel, actor = args$actor,
    reason = args$reason, identity = desktop_nca_identity_metadata(args, channel), project_id = state$project_id, revision = state$revision,
    source = state$source, specification = state$specification), details)
  state$audit <- c(state$audit, list(event))
  desktop_nca_write_state(engine, state)
}
desktop_nca_init <- function(engine) {
  engine$mode <- "nca"
  path <- file.path(engine$project, "nca-state.json")
  if (file.exists(path)) {
    state <- tryCatch(desktop_read(path), error = function(e) NULL)
    desktop_check(is.list(state) && identical(state$format, "pkapp-nca-1") &&
      identical(state$state_hash, desktop_nca_state_hash(state)) &&
      is.character(state$project_id) && length(state$project_id) == 1L && grepl("^[a-f0-9]{64}$", state$project_id) &&
      is.numeric(state$revision) && length(state$revision) == 1L && state$revision >= 1 && state$revision == floor(state$revision) &&
      is.list(state$audit) && length(state$audit) > 0 &&
      (is.null(state$source) || (is.character(state$source) && length(state$source) == 1L)) &&
      (is.null(state$specification) || is.list(state$specification)) &&
      identical(is.null(state$source), is.null(state$specification)) &&
      identical(as.numeric(tail(state$audit, 1)[[1]]$revision), as.numeric(state$revision)) &&
      identical(tail(state$audit, 1)[[1]]$project_id, state$project_id) &&
      identical(tail(state$audit, 1)[[1]]$source, state$source) &&
      identical(desktop_hash(tail(state$audit, 1)[[1]]$specification), desktop_hash(state$specification)),
      "E_VALIDATION", "Invalid NCA state; restore a verified project")
    if (!is.null(state$source)) {
      valid <- tryCatch({
        data <- desktop_table_get(engine, state$source)
        identical(desktop_hash(desktop_nca_specification(data, state$specification)), desktop_hash(state$specification))
      }, error = function(e) FALSE)
      desktop_check(valid, "E_INPUT_CHANGED", "NCA state or its source is invalid; restore a verified project")
    }
  } else {
    state <- list(format = "pkapp-nca-1", project_id = desktop_hash(list(engine$project, desktop_time(), tempfile())),
      revision = 1L, source = NULL, specification = NULL,
      audit = list())
    engine$nca <- state
    desktop_nca_record(engine, "create", list(actor = "local user", reason = "Create project"), "system")
    state <- engine$nca
  }
  engine$nca <- state
  engine$nca_signal <- shiny::reactiveVal(state$revision)
  engine$nca_result <- NULL
  engine$nca_results <- list()
  engine$nca_failed <- FALSE
  engine$nca_disclosure <- NULL
  engine$nca_job <- NULL
  engine$nca_jobs <- list()
  engine$nca_worker <- NULL
  engine$nca_closing <- FALSE
  engine$nca_job_timeout <- 300
}
desktop_nca_revision <- function(engine, revision, project_id) {
  desktop_check(identical(project_id, engine$nca$project_id), "E_PROJECT_CONFLICT", "Active project changed; read status again")
  desktop_check(is.numeric(revision) && length(revision) == 1L && identical(as.numeric(revision), as.numeric(engine$nca$revision)),
    "E_REVISION_CONFLICT", "Settings changed; fetch current NCA conditions and reconcile")
}
desktop_nca_identity <- function(args) {
  desktop_check(is.character(args$actor) && length(args$actor) == 1L && !is.na(args$actor) && nzchar(trimws(args$actor)) && nchar(args$actor) <= 120 &&
    is.character(args$reason) && length(args$reason) == 1L && !is.na(args$reason) && nzchar(trimws(args$reason)) && nchar(args$reason) <= 1000,
    "E_VALIDATION", "actor and reason are required")
}
desktop_nca_hash <- function(source, data, spec) desktop_hash(list(source = source, data = data, specification = spec))
desktop_nca_get <- function(engine) {
  s <- engine$nca
  matches <- Filter(function(r) desktop_nca_current(engine, r), rev(engine$nca_results))
  current <- if (length(matches)) matches[[1]] else NULL
  list(project_id = s$project_id, revision = s$revision, source = s$source, specification = s$specification,
    result_id = if (!is.null(current)) current$id else NULL,
    result_status = if (desktop_nca_busy(engine)) "running" else if (engine$nca_failed) "failed" else if (!is.null(current)) "current" else if (is.null(engine$nca_result)) "none" else "stale",
    disclosure = desktop_nca_disclosure(engine, if (!is.null(current)) current$id else NULL),
    job = engine$nca_job)
}
desktop_nca_current <- function(engine, r = engine$nca_result) {
  if (is.null(r) || isTRUE(engine$nca_failed) || is.null(engine$nca$source)) return(FALSE)
  tryCatch(identical(r$hash, desktop_nca_hash(engine$nca$source, desktop_table_get(engine, engine$nca$source), engine$nca$specification)), error = function(e) FALSE)
}
desktop_nca_configure <- function(engine, args, channel = "mcp") {
  desktop_nca_idle(engine)
  desktop_nca_revision(engine, args$base_revision, args$project_id); desktop_nca_identity(args)
  desktop_check(is.character(args$source) && length(args$source) == 1L && grepl("^table-[a-f0-9]{24}$", args$source), "E_VALIDATION", "source must be a saved table ID")
  data <- desktop_table_get(engine, args$source)
  desktop_check(is.list(args$specification) && "c0_method" %in% names(args$specification) &&
    is.character(args$specification$c0_method) && length(args$specification$c0_method) == 1L && args$specification$c0_method %in% c("none", "zero", "predose"),
    "E_VALIDATION", "specification.c0_method must explicitly be none, zero, or predose")
  spec <- tryCatch(desktop_nca_specification(data, args$specification), error = function(e) {
    if (inherits(e, "pkapp_public_error")) stop(e)
    desktop_error("E_VALIDATION", "Invalid NCA specification; check field names, columns, units, C0, and prepared BLQ rules")
  })
  state <- engine$nca
  if (identical(state$source, args$source) && identical(desktop_hash(state$specification), desktop_hash(spec))) {
    engine$nca_disclosure <- NULL
    desktop_nca_notify(engine)
    return(desktop_nca_get(engine))
  }
  state$revision <- state$revision + 1L; state$source <- args$source; state$specification <- spec
  before <- engine$nca
  engine$nca <- state
  tryCatch(desktop_nca_record(engine, "configure", args, channel,
    list(changes = desktop_nca_changes(before, args$source, spec))), error = function(e) { engine$nca <- before; stop(e) })
  engine$nca_disclosure <- NULL
  desktop_nca_notify(engine)
  desktop_nca_get(engine)
}
desktop_nca_begin_run <- function(engine, args, channel) {
  desktop_nca_idle(engine)
  desktop_nca_revision(engine, args$base_revision, args$project_id); desktop_nca_identity(args)
  engine$nca_disclosure <- NULL
  engine$nca_failed <- TRUE
  desktop_nca_notify(engine)
  desktop_require_code()
  desktop_check(!is.null(engine$nca$source) && !is.null(engine$nca$specification), "E_VALIDATION", "Configure NCA before running")
  state <- engine$nca
  data <- desktop_table_get(engine, state$source)
  list(state = state[c("project_id", "revision", "source", "specification")], data = data,
    hash = desktop_nca_hash(state$source, data, state$specification), args = args, channel = channel)
}
desktop_nca_publish <- function(engine, snapshot, value) {
  state <- snapshot$state
  desktop_require_code()
  desktop_nca_revision(engine, state$revision, state$project_id)
  desktop_check(identical(snapshot$hash, desktop_nca_hash(engine$nca$source,
    desktop_table_get(engine, engine$nca$source), engine$nca$specification)),
    "E_INPUT_CHANGED", "NCA input changed during calculation")
  id <- paste0("nca-", substr(desktop_hash(list(tempfile(), desktop_time())), 1, 24))
  args <- snapshot$args; channel <- snapshot$channel
  execution <- list(result_id = id, input_spec_hash = snapshot$hash, actor = args$actor, reason = args$reason,
    channel = channel, identity = desktop_nca_identity_metadata(args, channel), at = desktop_time(), revision = state$revision)
  desktop_nca_record(engine, "run", args, channel, list(result_id = id, input_spec_hash = snapshot$hash))
  engine$nca_result <- list(id = id, hash = snapshot$hash, revision = state$revision, value = value, input = snapshot$data, execution = execution)
  engine$nca_results[[id]] <- engine$nca_result
  engine$nca_failed <- FALSE
  desktop_nca_notify(engine)
  desktop_nca_public(engine, id, state$project_id)
}
desktop_nca_run_shared <- function(engine, args, channel = "mcp") {
  snapshot <- desktop_nca_begin_run(engine, args, channel)
  value <- tryCatch(pkapp_nca(snapshot$data, snapshot$state$specification), error = function(e)
    desktop_error("E_NCA_FAILED", "NCA failed; inspect the input and conditions in the local UI"))
  desktop_nca_publish(engine, snapshot, value)
}
desktop_nca_result <- function(engine, id, project_id) {
  desktop_check(identical(project_id, engine$nca$project_id), "E_PROJECT_CONFLICT", "Active project changed; read status again")
  r <- if (is.character(id) && length(id) == 1L && !is.na(id) && grepl("^nca-[a-f0-9]{24}$", id)) engine$nca_results[[id]] else NULL
  desktop_check(!is.null(r) && identical(id, r$id), "E_NOT_FOUND", "Unknown result_id")
  r
}
desktop_nca_disclosure <- function(engine, result_id) {
  grant <- engine$nca_disclosure
  r <- if (is.character(result_id) && length(result_id) == 1L && !is.na(result_id)) engine$nca_results[[result_id]] else NULL
  allowed <- !is.null(grant) && !is.null(r) && identical(grant$result_id, result_id) &&
    identical(grant$input_spec_hash, r$hash) && desktop_nca_current(engine, r)
  list(mode = if (allowed) "numeric_summary" else "counts_only", allowed = allowed, result_id = result_id)
}
desktop_nca_set_disclosure <- function(engine, result_id, allow, channel = "ui") {
  desktop_check(identical(channel, "ui"), "E_VALIDATION", "Numeric disclosure can only be changed in the local UI")
  desktop_check(is.logical(allow) && length(allow) == 1L && !is.na(allow), "E_VALIDATION", "allow must be true or false")
  r <- desktop_nca_result(engine, result_id, engine$nca$project_id)
  desktop_check(desktop_nca_current(engine, r) && identical(engine$nca_result$id, result_id),
    "E_STALE_RESULT", "Result is stale; load the current result before changing disclosure")
  args <- list(actor = "local user", reason = if (allow) "Allow numeric summary for this result" else "Revoke numeric summary for this result")
  desktop_nca_record(engine, if (allow) "disclosure_grant" else "disclosure_revoke", args, "ui",
    list(result_id = result_id, input_spec_hash = r$hash))
  engine$nca_disclosure <- if (allow) list(result_id = result_id, input_spec_hash = r$hash) else NULL
  desktop_nca_notify(engine)
  desktop_nca_disclosure(engine, result_id)
}
desktop_nca_public <- function(engine, id, project_id) {
  r <- desktop_nca_result(engine, id, project_id)
  disclosure <- desktop_nca_disclosure(engine, id)
  d <- r$value$data
  units <- if ("AVALU" %in% names(d)) as.character(d$AVALU) else rep("", nrow(d))
  keys <- unique(data.frame(parameter = as.character(d$PARAMCD), unit = units, stringsAsFactors = FALSE))
  summaries <- unlist(lapply(seq_len(nrow(keys)), function(i) {
    idx <- !is.na(d$PARAMCD) & d$PARAMCD == keys$parameter[i] & ((is.na(units) & is.na(keys$unit[i])) | (!is.na(units) & !is.na(keys$unit[i]) & units == keys$unit[i]))
    selected <- d[idx, , drop = FALSE]
    if (!disclosure$allowed) {
      x <- selected$AVAL[is.finite(selected$AVAL)]
      return(list(list(parameter = keys$parameter[i], unit = keys$unit[i], n = length(x), missing = nrow(selected) - length(x))))
    }
    # Returned summaries must not combine period/analyte or policy-assigned values.
    groups <- unlist(r$value$specification$groups, use.names = FALSE)
    mapping <- r$value$trace$output_key_mapping
    desktop_check(is.list(mapping) && all(groups %in% names(mapping)), "E_VALIDATION", "NCA analysis identity metadata is missing.")
    strata <- unname(unlist(mapping[groups], use.names = FALSE))
    desktop_check(!anyDuplicated(strata) && all(strata %in% names(selected)), "E_VALIDATION", "NCA grouping columns do not match the result.")
    state_column <- ".探索状態"
    while (state_column %in% names(selected)) state_column <- paste0(state_column, "_")
    selected[[state_column]] <- desktop_explore_class(selected)
    partitions <- desktop_explore_groups(selected, c(strata, state_column))
    lapply(partitions, function(part) {
      rows <- selected[part, , drop = FALSE]
      x <- rows$AVAL[is.finite(rows$AVAL)]
      list(parameter = keys$parameter[i], unit = keys$unit[i], strata = as.list(rows[1, strata, drop = FALSE]),
        result_status = rows[[state_column]][1], n = length(x), missing = nrow(rows) - length(x),
        mean = if (length(x)) mean(x) else NULL, min = if (length(x)) min(x) else NULL, max = if (length(x)) max(x) else NULL)
    })
  }), recursive = FALSE)
  counts <- function(x) as.list(table(factor(x, exclude = NULL)))
  public <- list(result_id = r$id, revision = r$revision, is_current = desktop_nca_current(engine, r), input_spec_hash = r$hash,
    specification = r$value$specification, counts = as.list(r$value$counts), parameters = summaries, disclosure = disclosure,
    profile_status = counts(vapply(r$value$trace$groups, function(x) x$status %||% "unknown", character(1))),
    qc_status = counts(vapply(r$value$trace$groups, function(x) x$predose_qc$status %||% "unknown", character(1))),
    qc_reason = counts(vapply(r$value$trace$groups, function(x) x$predose_qc$reason %||% "", character(1))),
    parameter_status = counts(d$PARAM_STATUS), pknca_options = r$value$trace$pknca_options,
    pknca_version = as.character(utils::packageVersion("PKNCA")))
  if (!is.null(r$value$trace$terminal)) {
    terminal <- lapply(r$value$trace$groups, function(x) x$terminal)
    public$terminal_status <- counts(vapply(terminal, function(x) x$status %||% "unknown", character(1)))
    reasons <- unlist(lapply(terminal, function(x) c(x$reason, x$quality_reasons)), use.names = FALSE)
    public$terminal_reason <- counts(reasons)
  }
  public
}
desktop_nca_table_operation <- function(engine, operation, args) {
  saved <- switch(operation, import_table = desktop_import_table_api(engine, args),
    prepare = desktop_prepare_api(engine, args), join = desktop_join_api(engine, args))
  audit_args <- args
  if (is.null(audit_args$reason)) audit_args$reason <- paste("MCP", operation)
  desktop_nca_record(engine, operation, audit_args, "mcp", list(table_id = saved$id,
    preparation_source = args$source, left_source = args$left, right_source = args$right))
  saved
}
desktop_nca_dispatch <- function(engine, operation, args = list()) {
  switch(operation,
    status = c(list(mode = "nca", app_version = desktop_version, contract_version = desktop_nca_contract$contract_version, capabilities = as.list(c("nca", "tables", "exploration", "modules"))), desktop_nca_get(engine)[c("project_id", "revision")]),
    modules = desktop_modules_list(engine),
    add_module = desktop_module_mutate(engine, args),
    remove_module = desktop_module_mutate(engine, args, remove = TRUE),
    explore_get = desktop_explore_get(engine),
    explore_set = desktop_explore_set(engine, args),
    nca_get_spec = desktop_nca_get(engine),
    nca_configure = desktop_nca_configure(engine, args),
    nca_run = desktop_nca_start_job(engine, args),
    nca_get_job = desktop_nca_get_job(engine, args$job_id, args$project_id),
    nca_get_result = desktop_nca_public(engine, args$result_id, args$project_id),
    nca_save_result = {
      desktop_nca_revision(engine, args$base_revision, args$project_id); desktop_nca_identity(args)
      r <- desktop_nca_result(engine, args$result_id, args$project_id)
      desktop_check(desktop_nca_current(engine, r), "E_STALE_RESULT", "Result is stale; run NCA again before saving")
      saved <- desktop_table_save(engine, args$name, list(source = r$input), list(kind = "nca", specification = r$value$specification), "mcp", args$actor, frozen = r$value,
        provenance = list(execution = r$execution, saved_by = args$actor, save_reason = args$reason,
          saved_identity = desktop_nca_identity_metadata(args, "mcp")))
      desktop_nca_record(engine, "save", args, "mcp", list(result_id = r$id, table_id = saved$id, input_spec_hash = r$hash))
      saved
    },
    describe = desktop_describe(engine, args$source), tables = desktop_tables(engine),
    assemble_nca = desktop_assemble_nca(engine, args),
    annotate_nca = desktop_annotate_nca(engine, args),
    prepare_nca_covariates = desktop_prepare_nca_covariates(engine, args),
    import_table = desktop_nca_table_operation(engine, operation, args),
    prepare = desktop_nca_table_operation(engine, operation, args),
    join = desktop_nca_table_operation(engine, operation, args),
    desktop_error("E_VALIDATION", "Unknown NCA operation"))
}
