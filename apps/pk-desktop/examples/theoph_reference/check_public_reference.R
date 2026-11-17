args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 1L)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
fixtures <- dirname(normalizePath(script))
root <- normalizePath(args[1])
setwd(root)
source("apps/pk-desktop/pkapp.R")
input <- utils::read.csv(file.path(fixtures, "theoph_input.csv"), colClasses = c(ID = "character"))
expected <- utils::read.csv(file.path(fixtures, "theoph_expected.csv"), colClasses = c(ID = "character"))
checks <- list()
for (method in unique(expected$AUC_METHOD)) {
  specification <- list(id = "ID", time = "TIME", concentration = "CONC", unit_column = "UNIT", auc_method = method)
  for (scenario in c("unchanged", "missing_only_C0", "shift_origin_72", "reversed_rows")) {
    data <- input
    s <- specification
    if (scenario == "missing_only_C0") s$c0_method <- "zero"
    if (scenario == "shift_origin_72") {
      data$TIME <- data$TIME + 72
      s$time_origin <- 72
      s$start <- 72
    }
    if (scenario == "reversed_rows") data <- data[rev(seq_len(nrow(data))), ]
    result <- pkapp_nca(data, s)
    stopifnot(all(result$data$PROFILE_STATUS == "ok"))
    for (id in unique(input$ID)) {
      oracle <- expected[expected$ID == id & expected$AUC_METHOD == method, ]
      calculated <- result$data[result$data$ID == id, ]
      stopifnot(nrow(oracle) == 1, nrow(calculated) == 3)
      for (parameter in c("CMAX", "TMAX", "AUCLST")) {
        actual <- calculated$AVAL[calculated$PARAMCD == parameter]
        reference <- oracle[[parameter]]
        stopifnot(isTRUE(all.equal(actual, reference, tolerance = 1e-12)))
        checks[[length(checks) + 1L]] <- data.frame(SCENARIO = scenario, METHOD = method,
          ID = id, PARAMCD = parameter, EXPECTED = reference, ACTUAL = actual,
          ABS_DIFFERENCE = abs(actual - reference))
      }
      stopifnot(all(abs(calculated$TLAST_QUANT_H - oracle$TLAST_QUANT_H) < 1e-12),
        all(abs(calculated$AUC_LAST_H - oracle$TLAST_QUANT_H) < 1e-12))
    }
    stopifnot(!any(result$trace$rows$IMPUTED))
  }
}
checks <- do.call(rbind, checks)
if (length(args) >= 2L) utils::write.csv(checks, args[2], row.names = FALSE)
cat(sprintf("PASS: %d parameter comparisons; 12 public profiles x 2 methods x 4 scenarios x 3 parameters.\n", nrow(checks)))
cat(sprintf("Maximum absolute difference: %.17g\n", max(checks$ABS_DIFFERENCE)))
cat("Tlast endpoints also agree; existing time-zero values were preserved; no synthetic points created.\n")
