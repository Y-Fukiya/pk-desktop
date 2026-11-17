# Rscript entry point: interactive teal application or a pure-R batch report.
local({
  cli_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  if (length(cli_file) != 1) stop("Use Rscript --vanilla apps/pk-desktop/launch.R, or source pkapp.R in RStudio", call. = FALSE)
  source(file.path(dirname(normalizePath(cli_file)), "pkapp.R"), local = TRUE)
  arguments <- commandArgs(trailingOnly = TRUE)
  if ("--help" %in% arguments) {
    cat(paste(
      "PK Desktop R — no Tauri required",
      "  --mode gui|batch|nca   Default: gui; nca needs no data bundle",
      "  --project DIR          Project output directory",
      "  --bundle DIR           Folder containing ADNCA.csv and ADPP.csv",
      "  --port N               GUI port, default 3877",
      "  --api-port N           MCP bridge port, default 3878",
      "  --no-browser           Print the authenticated local URL",
      "Batch settings:",
      "  --group-by NONE|SEX|ARM|DOSE_MG|ROUTE",
      "  --profile all|steady_state|trough",
      "  --x TIME_H|TAD_H",
      "  --parameters CODE1,CODE2  Select codes present in ADPP",
      "  --title TEXT",
      sep = "\n"), "\n")
    return(invisible(NULL))
  }
  options <- list()
  no_browser <- FALSE
  known <- c("mode", "project", "bundle", "port", "api-port", "group-by", "profile", "x", "parameters", "title")
  while (length(arguments)) {
    flag <- arguments[[1]]
    if (identical(flag, "--no-browser")) {
      if (no_browser) stop("Repeated --no-browser", call. = FALSE)
      no_browser <- TRUE
      arguments <- arguments[-1]
      next
    }
    key <- sub("^--", "", flag)
    if (!startsWith(flag, "--") || !key %in% known || length(arguments) < 2 || key %in% names(options))
      stop("Unknown, repeated or incomplete option: ", flag, call. = FALSE)
    options[[key]] <- arguments[[2]]
    arguments <- arguments[-c(1, 2)]
  }
  default <- function(key, fallback) if (is.null(options[[key]])) fallback else options[[key]]
  mode <- default("mode", "gui")
  if (!mode %in% c("gui", "batch", "nca")) stop("--mode must be gui, batch or nca", call. = FALSE)
  project <- default("project", file.path(pkapp_root(), "outputs/pk-desktop", paste0("r-", mode)))
  bundle <- default("bundle", file.path(pkapp_root(), "outputs/pk-review/bundles/reviewed-repeated"))
  if (mode == "nca") {
    if (any(c("bundle", "group-by", "profile", "x", "parameters", "title") %in% names(options)))
      stop("NCA mode accepts --project, --port, --api-port and --no-browser; select data and conditions on screen", call. = FALSE)
    pkapp_launch_nca(project, port = as.numeric(default("port", "3879")), api_port = as.numeric(default("api-port", "3880")), launch_browser = !no_browser)
  } else if (mode == "gui") {
    if (any(c("group-by", "profile", "x", "parameters", "title") %in% names(options)))
      stop("Batch settings require --mode batch; edit GUI settings in teal or through MCP", call. = FALSE)
    pkapp_launch(project, bundle, port = as.numeric(default("port", "3877")), api_port = as.numeric(default("api-port", "3878")),
      launch_browser = !no_browser)
  } else {
    if (any(c("port", "api-port") %in% names(options)) || no_browser) stop("GUI options cannot be used in batch mode", call. = FALSE)
    p <- pkapp_open(project, bundle)
    on.exit(pkapp_close(p), add = TRUE)
    group <- default("group-by", "NONE")
    invisible(pkapp_update(p, "conc_plot", list(group_by = if (group == "NONE") character() else group,
      profile = default("profile", "all"), x = default("x", "TIME_H")), pkapp_spec(p)$revision,
      reason = "Rscript batch analysis conditions", actor = "Rscript"))
    if (!is.null(options$parameters)) invisible(pkapp_update(p, "param_summary",
      list(paramcd = strsplit(options$parameters, ",", fixed = TRUE)[[1]]), pkapp_spec(p)$revision,
      reason = "Rscript ADPP selection", actor = "Rscript"))
    if (!is.null(options$title)) invisible(pkapp_update(p, "report", list(title = options$title), pkapp_spec(p)$revision,
      reason = "Rscript report title", actor = "Rscript"))
    run <- pkapp_run(p)
    report <- pkapp_report(p, run$run_id, reason = "Rscript batch Word report", actor = "Rscript")
    cat(jsonlite::toJSON(list(project = normalizePath(project), run_id = run$run_id, revision = run$revision,
      analysis_hash = run$analysis_hash, counts = run$counts, report = report), auto_unbox = TRUE, pretty = TRUE, digits = NA), "\n")
  }
})
