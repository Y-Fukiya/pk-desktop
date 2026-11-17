# Offline installer checks: fake the library/install calls and serve a tiny local source archive.
# Run from the source root: Rscript --vanilla apps/pk-desktop/release/tests/runtime_setup.R
.libPaths(c(".runtime/R-library", .libPaths()))
library(testthat)
script <- normalizePath("apps/pk-desktop/install.R")
current_url <- "https://cran.r-project.org/src/contrib/PKNCA_0.12.1.tar.gz"
archive_url <- "https://cran.r-project.org/src/contrib/Archive/PKNCA/PKNCA_0.12.1.tar.gz"
run_install <- function(version = "0.12.1", args = character(), missing = character(), loaded = FALSE,
                        responses = "ok", source_version = "0.12.1", source_package = "PKNCA", description = TRUE) {
  fixture <- tempfile("pk-source-fixture-")
  dir.create(file.path(fixture, "PKNCA"), recursive = TRUE)
  on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
  entry <- if (description) "PKNCA/DESCRIPTION" else "PKNCA/README"
  writeLines(c(paste("Package:", source_package), paste("Version:", source_version)), file.path(fixture, entry))
  archive <- file.path(fixture, "fixture.tar.gz")
  local({
    withr::local_dir(fixture)
    utils::tar(archive, files = entry, compression = "gzip", tar = "internal")
  })
  state <- new.env(parent = emptyenv())
  state$version <- version; state$installs <- list(); state$downloads <- character(); state$mkdir <- 0L
  state$temp_downloads <- character()
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (pkg == "PKNCA") {
        if (is.na(state$version)) stop("not installed")
        return(package_version(state$version))
      }
      package_version("1.0.0")
    },
    download.file = function(url, destfile, ...) {
      state$downloads <- c(state$downloads, url)
      state$temp_downloads <- c(state$temp_downloads, destfile)
      response <- responses[[min(length(state$downloads), length(responses))]]
      if (response == "error") stop("mock HTTP 404")
      if (response == "nonzero") { writeLines("partial download", destfile); return(1L) }
      stopifnot(file.copy(archive, destfile))
      0L
    }, .package = "utils")
  env <- new.env(parent = globalenv())
  env$commandArgs <- function(trailingOnly = FALSE) if (trailingOnly) args else paste0("--file=", script)
  env$getRversion <- function() package_version("4.6.1")
  env$.libPaths <- function(...) character()
  env$dir.create <- function(path, ...) {
    state$mkdir <- state$mkdir + 1L
    if (basename(path) == "R-library") return(invisible(TRUE))
    base::dir.create(path, ...)
  }
  env$find.package <- function(...) "/mock/library/package"
  env$loadedNamespaces <- function() if (loaded) "PKNCA" else character()
  env$install.packages <- function(pkgs, repos, lib, type = NULL, ...) {
    state$installs[[length(state$installs) + 1L]] <- list(pkgs = pkgs, repos = repos, type = type)
    if (is.null(repos)) {
      stopifnot(file.exists(pkgs), identical(basename(pkgs), "PKNCA_0.12.1.tar.gz"))
      state$version <- "0.12.1"
    }
    missing <<- setdiff(missing, pkgs)
  }
  env$source <- function(file, local, ...) {
    stopifnot(basename(file) == "dependencies.R")
    assign("desktop_dependency_status", function(packages) {
      data.frame(package = packages, installed = !packages %in% missing, available = !packages %in% missing,
        version = ifelse(packages == "PKNCA", state$version, "1.0.0"), error = "", stringsAsFactors = FALSE)
    }, envir = parent.frame())
  }
  state$error <- tryCatch({capture.output(base::source(script, local = env)); NULL}, error = identity)
  state
}
expect_pinned_source_install <- function(result) {
  expect_null(result$error)
  expect_length(result$installs, 1L)
  expect_identical(basename(result$installs[[1L]]$pkgs), "PKNCA_0.12.1.tar.gz")
  expect_null(result$installs[[1L]]$repos)
  expect_identical(result$installs[[1L]]$type, "source")
  expect_false(any(file.exists(result$temp_downloads)))
}
test_that("check verifies exact scientific backend without writes or downloads", {
  ready <- run_install(args = "--check")
  expect_null(ready$error); expect_length(ready$downloads, 0L); expect_length(ready$installs, 0L); expect_equal(ready$mkdir, 0L)
  wrong <- run_install("0.13.0", "--check")
  expect_match(conditionMessage(wrong$error), "PKNCA differs")
  expect_length(wrong$downloads, 0L); expect_length(wrong$installs, 0L); expect_equal(wrong$mkdir, 0L)
})
test_that("missing or mismatched backend uses the exact current-contrib release", {
  for (v in c(NA_character_, "0.13.0", "0.11.0")) {
    result <- run_install(v)
    expect_pinned_source_install(result)
    expect_identical(result$downloads, current_url)
  }
  ready <- run_install()
  expect_null(ready$error); expect_length(ready$downloads, 0L); expect_length(ready$installs, 0L)
})
test_that("unavailable current release falls back to the same archived version", {
  for (failure in c("error", "nonzero")) {
    result <- run_install("0.13.0", responses = c(failure, "ok"))
    expect_pinned_source_install(result)
    expect_identical(result$downloads, c(current_url, archive_url))
  }
})
test_that("failure of both fixed URLs stops without installing any packages", {
  result <- run_install("0.13.0", responses = c("error", "nonzero"), missing = "shiny")
  expect_match(conditionMessage(result$error), "Could not download exact PKNCA 0.12.1", fixed = TRUE)
  expect_identical(result$downloads, c(current_url, archive_url))
  expect_length(result$installs, 0L)
  expect_false(any(file.exists(result$temp_downloads)))
})
test_that("downloaded DESCRIPTION must identify the exact package and version", {
  for (metadata in list(list(source_version = "0.13.0"), list(source_package = "OtherPackage"))) {
    result <- do.call(run_install, c(list(version = "0.13.0", missing = "shiny"), metadata))
    expect_match(conditionMessage(result$error), "Downloaded source is not PKNCA 0.12.1", fixed = TRUE)
    expect_identical(result$downloads, current_url)
    expect_length(result$installs, 0L)
    expect_false(any(file.exists(result$temp_downloads)))
  }
})
test_that("an archive without the required DESCRIPTION cannot be installed", {
  result <- run_install("0.13.0", description = FALSE)
  expect_match(conditionMessage(result$error), "lacks a unique PKNCA/DESCRIPTION", fixed = TRUE)
  expect_length(result$installs, 0L)
  expect_false(any(file.exists(result$temp_downloads)))
})
test_that("other missing dependencies do not unpin PKNCA", {
  result <- run_install("0.13.0", missing = "shiny")
  expect_null(result$error); expect_length(result$installs, 2L)
  expect_identical(result$installs[[1L]]$pkgs, "shiny")
  expect_identical(basename(result$installs[[2L]]$pkgs), "PKNCA_0.12.1.tar.gz")
  expect_false(any(vapply(result$installs, function(x) "PKNCA" %in% x$pkgs, logical(1))))
})
test_that("loaded wrong backend requires a new process instead of unsafe replacement", {
  result <- run_install("0.13.0", loaded = TRUE)
  expect_match(conditionMessage(result$error), "already loaded")
  expect_length(result$downloads, 0L); expect_length(result$installs, 0L)
})
