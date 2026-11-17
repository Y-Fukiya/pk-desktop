args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || length(args) > 4L) stop("Usage: Rscript nca013_benchmark.R BASELINE_SOURCE NEW_OUTPUT_JSON [PROFILES=500] [REPEATS=5]")
baseline <- normalizePath(args[1], mustWork = TRUE)
destination <- args[2]
if (file.exists(destination)) stop("Choose a new output path")
profiles <- if (length(args) >= 3L) as.integer(args[3]) else 500L
repeats <- if (length(args) >= 4L) as.integer(args[4]) else 5L
stopifnot(length(profiles) == 1L, is.finite(profiles), profiles > 0L,
  length(repeats) == 1L, is.finite(repeats), repeats >= 5L)
load_api <- function(path) {
  api <- new.env(parent = globalenv())
  source(file.path(path, "apps/pk-desktop/pkapp.R"), local = api)
  api
}
apis <- list(before = load_api(baseline), after = load_api(getwd()))
time <- c(-.25, 0, .25, .5, .75, 1, 1.5, 2, 3, 4, 6, 8, 12, 16, 24, 36)
concentration <- c("<0.2", 0, 1, 3, 6, 8, 7, 6, 5, 4, 2, 1.5, .7, .3, "<0.2", "<0.2")
data <- data.frame(ID = rep(sprintf("%04d", seq_len(profiles)), each = 16L), TIME = rep(time, profiles),
  CONC = rep(concentration, profiles), BLQ = rep(c(1, rep(0, 13), 1, 1), profiles), LLOQ = .2, UNIT = "ng/mL")
step <- list(op = "blq_rules", mode = "positional", profile_columns = "ID", concentration = "CONC",
  blq_column = "BLQ", time = "TIME", zero_time = "post", pre_rule = "zero", first_rule = "zero",
  middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude", lloq_source = "column", lloq_column = "LLOQ")
spec <- list(id = "ID", time = "TIME", concentration = "NCA_CONC", blq_column = "NCA_BLQ",
  mdv_column = "NCA_MDV", blq = "as_recorded", unit_column = "UNIT", auc_method = "linear", c0_method = "zero")
for (api in apis) {
  warm <- api$pkapp_prepare(data[1:16, ], list(step))$data
  invisible(api$pkapp_nca(warm, spec))
}
runs <- list()
reference <- NULL
load_before <- system("uptime", intern = TRUE)
for (iteration in seq_len(repeats)) for (side in names(apis)) {
  api <- apis[[side]]
  gc()
  prep_seconds <- unname(system.time(prepared <- api$pkapp_prepare(data, list(step))$data)[["elapsed"]])
  nca_seconds <- unname(system.time(result <- api$pkapp_nca(prepared, spec))[["elapsed"]])
  stopifnot(nrow(prepared) == profiles * 16L, length(unique(prepared$ID)) == profiles,
    nrow(result$data) == profiles * 3L, all(is.finite(result$data$AVAL)),
    all(result$data$AVAL[result$data$PARAMCD == "CMAX"] == 8),
    all(result$data$AVAL[result$data$PARAMCD == "TMAX"] == 1))
  if (is.null(reference)) reference <- list(prepared = prepared, result = result$data)
  stopifnot(identical(prepared, reference$prepared), identical(result$data, reference$result))
  runs[[length(runs) + 1L]] <- data.frame(iteration, side, preparation_s = prep_seconds, nca_s = nca_seconds,
    combined_s = prep_seconds + nca_seconds, input_rows = nrow(prepared), result_rows = nrow(result$data), errors = 0L)
  cat(side, iteration, "preparation", prep_seconds, "NCA", nca_seconds, "seconds; output identical\n")
}
runs <- do.call(rbind, runs)
summary <- do.call(rbind, lapply(names(apis), function(side) {
  do.call(rbind, lapply(c("preparation_s", "nca_s", "combined_s"), function(metric) {
    values <- runs[runs$side == side, metric]
    data.frame(side, metric, median = median(values), minimum = min(values), maximum = max(values), runs = length(values))
  }))
}))
report <- list(scope = "Serial public API preparation plus prepared NCA. No browser or file loading in timed regions.",
  profiles = profiles, observations_per_profile = 16L, R = R.version.string, PKNCA = as.character(packageVersion("PKNCA")),
  jsonlite = as.character(packageVersion("jsonlite")), platform = R.version$platform,
  baseline_source = baseline, candidate_source = normalizePath(getwd()), load_before = load_before,
  load_after = system("uptime", intern = TRUE), exact_prepared_and_result_equality = TRUE, runs = runs, summary = summary)
writeLines(jsonlite::toJSON(report, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, digits = NA), destination)
print(summary, row.names = FALSE)
