# General setup: other dependencies use CRAN, but the scientific backend is pinned.
# Use restore.R for the complete recorded environment. --check makes no changes.
(function() {
  if (getRversion() < "4.4.0") stop("R 4.4以降を使ってください（現在のteal依存関係の要件）。", call. = FALSE)
  frames <- vapply(sys.frames(), function(f) if (is.character(f$ofile)) f$ofile else "", character(1))
  cli_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  file <- if (any(nzchar(frames))) tail(frames[nzchar(frames)], 1) else cli_file[[1]]
  root <- normalizePath(file.path(dirname(file), "..", ".."), mustWork = TRUE)
  args <- commandArgs(trailingOnly = TRUE)
  if (length(setdiff(args, c("--check", "--with-parquet", "--sync-only", "--with-modules")))) stop("Options: --check, --with-parquet, --sync-only, --with-modules", call. = FALSE)
  check_only <- "--check" %in% args
  lib <- file.path(root, ".runtime", "R-library")
  if (!check_only) dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
  backend_version <- "0.12.1"
  installed_backend <- tryCatch(as.character(utils::packageVersion("PKNCA")), error = function(e) NA_character_)
  backend_matches <- identical(installed_backend, backend_version)
  packages <- c("digest", "jsonlite", "jsonvalidate", "shiny", "bslib", "ggplot2", "ragg", "callr", "officer", "flextable",
    "httpuv", "later", "zip", "teal", "teal.data", "teal.transform", "datamods", "PKNCA", "openssl", "ps", "haven", "testthat")
  if ("--with-parquet" %in% args) packages <- c(packages, "nanoparquet", "bit64")
  if ("--with-modules" %in% args) packages <- c(packages, "teal.modules.general")
  sync_only <- "--sync-only" %in% args
  if (sync_only) packages <- setdiff(packages, "callr")
  source(file.path(root, "apps/pk-desktop/R/dependencies.R"), local = TRUE)
  # Do not load a mismatched PKNCA namespace before replacing its on-disk package.
  status <- desktop_dependency_status(setdiff(packages, "PKNCA"))
  print(status, row.names = FALSE)
  message("PKNCA: installed=", installed_backend, "; required=", backend_version)
  if (check_only && (!all(status$available) || !backend_matches)) {
    stop("Dependencies are unavailable or PKNCA differs from 0.12.1. Run install.R without --check in a fresh R process; inspect the status above.", call. = FALSE)
  }
  if (!check_only) {
    missing <- status$package[!status$installed]
    backend_archive <- NULL
    if (!backend_matches) {
      if ("PKNCA" %in% loadedNamespaces()) stop("PKNCA is already loaded with another version. Restart R and run install.R again.", call. = FALSE)
      # Both URLs name the exact release; a current CRAN release may not be archived yet.
      candidates <- c(
        "https://cran.r-project.org/src/contrib/PKNCA_0.12.1.tar.gz",
        "https://cran.r-project.org/src/contrib/Archive/PKNCA/PKNCA_0.12.1.tar.gz")
      staging <- tempfile("pkdesktop-pknca-source-")
      dir.create(staging)
      on.exit(unlink(staging, recursive = TRUE), add = TRUE)
      backend_archive <- file.path(staging, "PKNCA_0.12.1.tar.gz")
      downloaded <- FALSE
      failures <- character()
      for (candidate in candidates) {
        unlink(backend_archive)
        attempt <- tryCatch({
          code <- suppressWarnings(utils::download.file(candidate, backend_archive, mode = "wb", quiet = TRUE))
          if (!identical(as.integer(code), 0L) || !file.exists(backend_archive)) stop("download did not complete")
          NULL
        }, error = conditionMessage)
        if (is.null(attempt)) {
          downloaded <- TRUE
          break
        }
        failures <- c(failures, paste(candidate, attempt, sep = ": "))
      }
      if (!downloaded) stop("Could not download exact PKNCA 0.12.1 from current contrib or Archive. No package installation was attempted.\n",
        paste(failures, collapse = "\n"), call. = FALSE)
      description_name <- "PKNCA/DESCRIPTION"
      contents <- tryCatch(utils::untar(backend_archive, list = TRUE), error = function(e) character())
      if (sum(contents == description_name) != 1L) stop("Downloaded source lacks a unique PKNCA/DESCRIPTION; refusing installation.", call. = FALSE)
      extracted <- utils::untar(backend_archive, files = description_name, exdir = staging)
      description_path <- file.path(staging, description_name)
      if (!identical(as.integer(extracted), 0L) || !file.exists(description_path)) stop("Could not verify the downloaded PKNCA DESCRIPTION; refusing installation.", call. = FALSE)
      metadata <- read.dcf(description_path, fields = c("Package", "Version"))
      if (nrow(metadata) != 1L || !identical(unname(metadata[1L, "Package"]), "PKNCA") ||
          !identical(unname(metadata[1L, "Version"]), backend_version)) {
        stop("Downloaded source is not PKNCA 0.12.1; no package installation was attempted.", call. = FALSE)
      }
      message("Verified PKNCA 0.12.1 source: ", candidate)
      # Imports of the pinned PKNCA release; installing a local source archive does not resolve them itself.
      imports <- c("checkmate", "dplyr", "digest", "nlme", "purrr", "rlang", "tidyr", "tibble", "lifecycle")
      missing <- unique(c(missing, imports[!vapply(imports, function(p) length(find.package(p, quiet = TRUE)) > 0, logical(1))]))
    }
    if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org", lib = lib)
    if (!backend_matches) install.packages(backend_archive, repos = NULL, type = "source", lib = lib)
  }
  final_status <- desktop_dependency_status(packages)
  backend <- final_status[final_status$package == "PKNCA", ]
  if (!all(final_status$available) || !identical(backend$version, backend_version)) {
    print(final_status, row.names = FALSE)
    stop("Dependency verification failed (PKNCA must be exactly 0.12.1).", call. = FALSE)
  }
  message(if (sync_only) "Synchronous R dependencies and PKNCA 0.12.1 are ready; worker readiness was not checked." else "R dependencies and pinned PKNCA 0.12.1 are ready.")
  message("Other package versions are not pinned by this installer. Use restore.R for the recorded environment.")
})()
