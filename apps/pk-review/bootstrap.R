pk_root <- Sys.getenv("PK_REVIEW_ROOT", unset = getwd())
pk_app <- file.path(pk_root, "apps", "pk-review")
if (dir.exists(file.path(pk_root, ".runtime", "R-library"))) {
  .libPaths(c(file.path(pk_root, ".runtime", "R-library"), .libPaths()))
}
if (!nzchar(Sys.getenv("R_CACHE_ROOTPATH"))) Sys.setenv(R_CACHE_ROOTPATH = file.path(pk_root, ".runtime", "r-cache"))
for (filename in c("data.R", "analysis.R", "report.R")) source(file.path(pk_app, "R", filename), local = TRUE)
pk_code_files <- c(list.files(pk_app, pattern = "\\.(R|Rmd|py|docx|txt)$", recursive = TRUE, full.names = TRUE),
                   file.path(pk_root, "tools", c("pk_review.py", "make_adnca.R")))
pk_code_files <- sort(pk_code_files[!grepl("/tests/", pk_code_files, fixed = TRUE)])
pk_code_hashes <- stats::setNames(lapply(pk_code_files, pk_hash), substring(pk_code_files, nchar(pk_root) + 2L))
