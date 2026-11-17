# Rscript diagnose.R /absolute/path/to/diagnostic.json
frames <- vapply(sys.frames(), function(f) if (is.character(f$ofile)) f$ofile else "", character(1))
cli <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
file <- if (any(nzchar(frames))) tail(frames[nzchar(frames)], 1) else cli[[1]]
source(file.path(dirname(file), "pkapp.R"))
result <- list(worker = pkapp_worker_check(), dependencies = pkapp_dependencies())
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply one diagnostic JSON output path", call. = FALSE)
if (file.exists(args[[1]])) stop("Diagnostic output already exists; choose a new filename", call. = FALSE)
jsonlite::write_json(result, args[[1]], pretty = TRUE, auto_unbox = TRUE, null = "null")
cat("Diagnostic saved. Worker available:", result$worker$available, "\n")
if (!isTRUE(result$worker$available) || !all(result$dependencies$available)) quit(status = 1L)
