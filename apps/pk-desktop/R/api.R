desktop_dispatch <- function(engine, operation, args = list()) {
  switch(operation,
    status = list(app_version = desktop_version, project_id = engine$spec$project_id, revision = engine$spec$revision,
                  n_subjects = engine$info$n_subjects, parameters = as.list(engine$info$eligible_parameters),
                  group_columns = as.list(engine$info$group_columns), facet_columns = as.list(engine$info$facet_columns), profiles = as.list(engine$info$profiles)),
    describe = desktop_describe(engine, args$source),
    tables = desktop_tables(engine),
    modules = desktop_modules_list(engine),
    add_module = desktop_module_mutate(engine, args),
    remove_module = desktop_module_mutate(engine, args, remove = TRUE),
    import_table = desktop_import_table_api(engine, args),
    validate_tables = desktop_validate_tables_api(engine, args),
    apply_tables = desktop_apply_tables_api(engine, args),
    prepare = desktop_prepare_api(engine, args),
    join = desktop_join_api(engine, args),
    load_data = desktop_load_bundle(engine, desktop_allowed_bundle(engine, args$bundle), args$base_revision,
      args$project_id, "mcp", args$actor, args$reason),
    spec = list(spec = engine$spec, analysis_hash = desktop_analysis_hash(engine$spec)),
    update = desktop_apply(engine, args$section, args$values, args$base_revision, args$project_id,
                            "mcp", args$actor, args$reason),
    run = desktop_run(engine, args$base_revision, args$project_id, "mcp"),
    result = desktop_result(engine, args$run_id),
    runs = desktop_runs(engine),
    run_manifest = desktop_verify_run(desktop_run_folder(engine, args$run_id)),
    replay_run = desktop_replay_run(engine, args$run_id, args$base_revision, args$project_id, "mcp"),
    export_report = desktop_export_report(engine, args$run_id, args$project_id, "mcp", args$actor, args$reason),
    job = desktop_get_job(engine, args$job_id),
    jobs = desktop_list_jobs(engine),
    cancel_job = desktop_cancel_job(engine, args$job_id, args$project_id),
    desktop_error("E_VALIDATION", "Unknown operation"))
}
desktop_http_app <- function(engine, runtime) {
  respond <- function(status, value) {
    body <- desktop_json(value)
    list(status = status, headers = list("Content-Type" = "application/json; charset=utf-8",
      "Cache-Control" = "no-store", "Content-Length" = as.character(nchar(body, type = "bytes"))), body = body)
  }
  list(call = function(req) {
    if (!identical(req$HTTP_AUTHORIZATION, paste("Bearer", runtime$token)) || !is.null(req$HTTP_ORIGIN))
      return(respond(403L, list(ok = FALSE, error = list(code = "E_AUTH", message = "Authentication required"))))
    operation <- ""
    tryCatch({
      if (identical(req$PATH_INFO, "/health") && identical(req$REQUEST_METHOD, "GET")) {
        return(respond(200L, list(ok = TRUE, instance_id = runtime$instance_id, pid = Sys.getpid(), R = as.character(getRversion()),
          app_version = desktop_version, contract_version = if (identical(runtime$mode, "nca")) desktop_nca_contract$contract_version else NULL, lockfile_sha256 = desktop_lock_hash, mode = runtime$mode %||% "summary", project = engine$project,
          capabilities = if (identical(runtime$mode, "nca")) as.list(c("nca", "tables", "exploration", "modules")) else as.list(c("summary", "tables")))))
      }
      if (identical(req$PATH_INFO, "/shutdown") && identical(req$REQUEST_METHOD, "POST")) {
        later::later(function() shiny::stopApp(), delay = 0.05)
        return(respond(200L, list(ok = TRUE)))
      }
      if (identical(runtime$mode, "nca")) desktop_check(identical(req$HTTP_X_PKAPP_CONTRACT, desktop_nca_contract$contract_version),
        "E_VERSION", "MCP contract differs; update the registration to this app release and reconnect.")
      desktop_check(identical(req$REQUEST_METHOD, "POST"), "E_VALIDATION", "POST required")
      desktop_check(startsWith(req$CONTENT_TYPE %||% "", "application/json"), "E_VALIDATION", "JSON required")
      size <- suppressWarnings(as.numeric(req$CONTENT_LENGTH))
      desktop_check(length(size) == 1 && is.finite(size) && size > 0 && size <= 262144, "E_VALIDATION", "Invalid body size")
      body <- req$rook.input$read()
      desktop_check(length(body) <= 262144, "E_VALIDATION", "Request too large")
      body_text <- rawToChar(body)
      if (identical(runtime$mode, "nca") && !startsWith(trimws(body_text), "{"))
        desktop_public_validation("Request body must be a JSON object.")
      args <- tryCatch(jsonlite::fromJSON(body_text, simplifyVector = FALSE), error = function(e)
        desktop_public_validation("Request body must be valid JSON."))
      desktop_check(is.list(args), "E_VALIDATION", "JSON object required")
      operation <- sub("^/v1/", "", req$PATH_INFO)
      desktop_check(startsWith(req$PATH_INFO, "/v1/"), "E_VALIDATION", "Unknown endpoint")
      if (identical(runtime$mode, "nca") && identical(operation, "prepare")) desktop_validate_prepare_steps(args$steps)
      if (identical(runtime$mode, "nca") && identical(operation, "assemble_nca")) desktop_validate_assembly(args$specification)
      if (identical(runtime$mode, "nca") && identical(operation, "annotate_nca")) desktop_validate_annotation(args$specification)
      if (identical(runtime$mode, "nca") && identical(operation, "prepare_nca_covariates")) desktop_validate_covariates(args$specification)
      respond(200L, list(ok = TRUE, result = if (identical(runtime$mode, "nca")) desktop_nca_dispatch(engine, operation, args) else desktop_dispatch(engine, operation, args)))
    }, pkapp_error = function(e) {
      fixed_owner <- operation %in% c("nca_get_spec", "nca_configure", "nca_run", "nca_get_job", "nca_get_result", "nca_save_result") &&
        e$code %in% c("E_NOT_FOUND", "E_STALE_RESULT", "E_BUSY", "E_NCA_FAILED", "E_TIMEOUT", "E_CLOSED", "E_INPUT_CHANGED", "E_CODE_CHANGED", "E_VALIDATION")
      public <- inherits(e, "pkapp_public_error") || e$code %in% c("E_VERSION", "E_REVISION_CONFLICT", "E_PROJECT_CONFLICT", "E_PATH_DENIED") || fixed_owner
      message <- if (!identical(runtime$mode, "nca") || public) conditionMessage(e) else "Operation failed; inspect local UI."
      respond(400L, list(ok = FALSE, error = list(code = e$code, message = message),
        revision = if (identical(runtime$mode, "nca")) engine$nca$revision else engine$spec$revision))
    },
       error = function(e) {
         message("Engine request failed: ", conditionMessage(e))
         respond(400L, list(ok = FALSE, error = list(code = "E_R_ERROR", message = "Operation failed; inspect the local R log.")))
       })
  })
}
