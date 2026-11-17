options(digits = 17)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
out <- dirname(normalizePath(script))
stopifnot(as.character(utils::packageVersion("PKNCA")) == "0.12.1")
input <- data.frame(ID = as.character(datasets::Theoph$Subject),
  TIME = datasets::Theoph$Time, CONC = datasets::Theoph$conc,
  UNIT = "mg/L", stringsAsFactors = FALSE)
manual_auc <- function(time, conc, method) {
  stopifnot(length(time) == length(conc), length(time) > 1, all(diff(time) > 0))
  left <- head(conc, -1); right <- tail(conc, -1); dt <- diff(time)
  area <- dt * (left + right) / 2
  if (method == "lin up/log down") {
    use_log <- left > right & right > 0
    area[use_log] <- dt[use_log] * (left[use_log] - right[use_log]) /
      log(left[use_log] / right[use_log])
  }
  sum(area)
}
expected <- list()
for (id in unique(input$ID)) {
  curve <- input[input$ID == id, ]
  curve <- curve[order(curve$TIME), ]
  stopifnot(curve$TIME[1] == 0)
  tlast <- max(curve$TIME[curve$CONC > 0])
  to_last <- curve[curve$TIME <= tlast, ]
  for (method in c("linear", "lin up/log down")) {
    independent <- manual_auc(to_last$TIME, to_last$CONC, method)
    reference <- PKNCA::pk.calc.auc.last(to_last$CONC, to_last$TIME,
      interval = c(0, tlast), options = list(auc.method = method, conc.blq = "keep"))
    stopifnot(isTRUE(all.equal(as.numeric(reference), independent, tolerance = 1e-12)))
    expected[[length(expected) + 1L]] <- data.frame(ID = id, AUC_METHOD = method,
      CMAX = max(curve$CONC), TMAX = min(curve$TIME[curve$CONC == max(curve$CONC)]),
      AUCLST = independent, TLAST_QUANT_H = tlast, PKNCA_AUCLST = as.numeric(reference))
  }
}
expected <- do.call(rbind, expected)
utils::write.csv(input, file.path(out, "theoph_input.csv"), row.names = FALSE)
utils::write.csv(expected, file.path(out, "theoph_expected.csv"), row.names = FALSE)
writeLines(c("Reference dataset: datasets::Theoph in R 4.6.1.",
  "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/Theoph.html",
  "132 original observations, 12 subjects; time in hours, concentration in mg/L.",
  "Manual independent adjacent-segment formulas are implemented in build_reference.R.",
  "Linear area = dt*(C1+C2)/2. For descending positive concentrations only,",
  "lin-up/log-down area = dt*(C1-C2)/log(C1/C2); all other segments are linear.",
  "No app preprocessing or app NCA function participates in expected-value generation.",
  "All 24 manual AUC results agreed with installed PKNCA 0.12.1 within 1e-12 relative tolerance.",
  "This is an independent numerical cross-check, not Phoenix or regulatory validation.",
  paste("Generated with", R.version.string)), file.path(out, "REFERENCE.txt"))
cat("PASS: 12 public-data profiles x 2 integration methods, 24 independent AUC references.\n")
print(expected)
