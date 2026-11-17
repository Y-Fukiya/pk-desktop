# Explicit locked setup only. --check verifies without writing or downloading.
(function() {
  frames <- vapply(sys.frames(), function(f) if (is.character(f$ofile)) f$ofile else "", character(1))
  cli <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  file <- if (any(nzchar(frames))) tail(frames[nzchar(frames)], 1) else cli[[1]]
  root <- normalizePath(file.path(dirname(file), "..", ".."), mustWork = TRUE)
  args <- commandArgs(trailingOnly = TRUE)
  if (length(setdiff(args, "--check"))) stop("Options: --check", call. = FALSE)
  check_only <- "--check" %in% args
  lib <- file.path(root, ".runtime", "R-library")
  if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
  # Bootstrap only the recorded renv version, never the current CRAN release.
  renv_version <- "1.2.3"
  installed_renv <- tryCatch(as.character(utils::packageVersion("renv")), error = function(e) NA_character_)
  if (!identical(installed_renv, renv_version)) {
    if (check_only) stop("Locked verification requires renv 1.2.3; --check does not install it.", call. = FALSE)
    if ("renv" %in% loadedNamespaces()) stop("Restart R before restoring a different renv version.", call. = FALSE)
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(lib, .libPaths()))
    install.packages("https://cran.r-project.org/src/contrib/Archive/renv/renv_1.2.3.tar.gz", repos = NULL, type = "source", lib = lib)
  }
  if (!requireNamespace("renv", quietly = TRUE) || as.character(utils::packageVersion("renv")) != renv_version) stop("renv bootstrap verification failed.", call. = FALSE)
  lockfile <- file.path(root, "apps/pk-desktop/renv.lock")
  lock <- renv::lockfile_read(lockfile)
  if (as.character(getRversion()) != lock$R$Version) stop("The recorded environment uses R ", lock$R$Version,
    ". Use that R version for locked restore/verification. General installation with R >= 4.4 uses install.R.", call. = FALSE)
  if (lock$Packages$renv$Version != renv_version || lock$Packages$PKNCA$Version != "0.12.1") stop("Lock metadata differs from the supported bootstrap/backend versions.", call. = FALSE)
  if (!check_only) {
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(lib, .libPaths()))
    renv::restore(project = root, library = lib, lockfile = lockfile, prompt = FALSE)
  }
  source(file.path(root, "apps/pk-desktop/R/dependencies.R"), local = TRUE)
  status <- desktop_dependency_status(names(lock$Packages))
  expected <- vapply(lock$Packages, function(x) x$Version, character(1))
  status$expected <- unname(expected[status$package])
  same_version <- vapply(seq_len(nrow(status)), function(i) {
    !is.na(status$version[[i]]) && utils::compareVersion(status$version[[i]], status$expected[[i]]) == 0
  }, logical(1))
  mismatch <- !status$available | !same_version
  if (any(mismatch)) {
    print(status[mismatch, ], row.names = FALSE)
    stop("Locked verification failed; inspect package errors above.", call. = FALSE)
  }
  message("Verified R ", lock$R$Version, " and ", nrow(status), " locked package versions. No clean restore, native binary, or worker acceptance is implied by --check.")
})()
