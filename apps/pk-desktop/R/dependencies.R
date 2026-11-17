desktop_dependency_status <- function(packages) {
  rows <- lapply(packages, function(package) {
    path <- tryCatch(find.package(package, quiet = TRUE), error = function(e) character())
    installed <- length(path) > 0
    error <- tryCatch({ loadNamespace(package); "" }, error = function(e) conditionMessage(e))
    data.frame(package = package, installed = installed, available = !nzchar(error),
      version = if (installed) as.character(utils::packageVersion(package)) else NA_character_, error = error,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
