# Recalculate summaries from the frozen input and source, without the original workspace.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)][1])
directory <- dirname(normalizePath(script))
manifest <- jsonlite::fromJSON(file.path(directory, "REPORT_MANIFEST.json"), simplifyVector = FALSE)
stopifnot(as.character(getRversion()) == manifest$software$R)
for (p in names(manifest$software$packages)) {
  if (!requireNamespace(p, quietly = TRUE) || as.character(utils::packageVersion(p)) != manifest$software$packages[[p]]) stop("Package version differs: ", p)
}
for (file in names(manifest$outputs)) {
  if (!identical(digest::digest(file = file.path(directory, file), algo = "sha256"), manifest$outputs[[file]])) stop("Artifact checksum differs: ", file)
}
for (file in names(manifest$code_hashes)) {
  stopifnot(identical(digest::digest(file = file.path(directory, "source", file), algo = "sha256"), manifest$code_hashes[[file]]))
}
Sys.setenv(PK_REVIEW_ROOT = file.path(directory, "source"))
source(file.path(directory, "source", "apps", "pk-review", "bootstrap.R"))
cfg <- pk_read_config(file.path(directory, "view.json"))
stopifnot(identical(pk_hash(file.path(directory, "view.json")), manifest$config_sha256))
bundle <- pk_load_bundle(file.path(directory, "input"))
result <- pk_analyse(bundle, cfg)
for (name in c("concentration_summary", "parameter_summary")) {
  actual <- result[[name]]
  expected <- pk_csv(file.path(directory, paste0(name, ".csv")))
  if (!identical(names(actual), names(expected))) stop(name, " columns differ")
  numeric_columns <- names(actual)[vapply(actual, is.numeric, logical(1))]
  for (column in numeric_columns) expected[[column]] <- pk_number(expected[[column]], paste(name, column), allow_na = TRUE)
  keys <- intersect(c(".GROUP", "AVALU", ".TIME", "PARAMCD"), names(actual))
  if (nrow(actual) != nrow(expected) || anyDuplicated(actual[keys]) || anyDuplicated(expected[keys])) stop(name, " keys differ")
  # Compare complete keys in a deterministic order, independent of LC_COLLATE.
  sort_keys <- function(data) lapply(data[keys], function(x) {
    if (!is.character(x)) return(x)
    x <- enc2utf8(x); Encoding(x) <- "UTF-8"; x
  })
  actual <- actual[do.call(order, c(unname(sort_keys(actual)), list(method = "radix"))), , drop = FALSE]
  expected <- expected[do.call(order, c(unname(sort_keys(expected)), list(method = "radix"))), , drop = FALSE]
  if (!all(vapply(keys, function(key) identical(actual[[key]], expected[[key]]), logical(1)))) stop(name, " keys differ")
  check <- all.equal(actual, expected, tolerance = 1e-12, check.attributes = FALSE)
  if (!isTRUE(check)) stop(name, " differs: ", paste(check, collapse = "; "))
}
cat("Frozen inputs and source reproduced both full-precision summary tables.\n")
