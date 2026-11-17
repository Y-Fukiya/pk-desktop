# Source this file from any working directory. Only the public pkapp_* API is exported.
(function(target) {
  files <- vapply(sys.frames(), function(frame) {
    path <- frame$ofile
    if (is.character(path) && length(path) == 1) path else ""
  }, character(1))
  files <- files[nzchar(files)]
  if (!length(files)) stop("Load this API with source('/absolute/path/apps/pk-desktop/pkapp.R')", call. = FALSE)
  root <- normalizePath(file.path(dirname(tail(files, 1)), "..", ".."), mustWork = TRUE)
  lib <- file.path(root, ".runtime", "R-library")
  if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
  if (!requireNamespace("digest", quietly = TRUE)) stop("Install the R dependencies with apps/pk-desktop/install.R first", call. = FALSE)
  previous <- Sys.getenv("PK_REVIEW_ROOT", unset = NA_character_)
  on.exit(if (is.na(previous)) Sys.unsetenv("PK_REVIEW_ROOT") else Sys.setenv(PK_REVIEW_ROOT = previous), add = TRUE)
  Sys.setenv(PK_REVIEW_ROOT = root)
  core <- new.env(parent = globalenv())
  for (file in c("engine.R", "api.R", "app.R", "r_api.R")) sys.source(file.path(root, "apps", "pk-desktop", "R", file), envir = core)
  for (name in ls(core, pattern = "^pkapp_")) assign(name, core[[name]], envir = target)
  invisible(NULL)
})(environment())
