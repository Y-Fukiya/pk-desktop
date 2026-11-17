args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1L]), mustWork = TRUE)
here <- dirname(script)
source(file.path(here, "..", "..", "pkapp.R"))
read <- function(name) pkapp_read_table(file.path(here, name))
prep <- jsonlite::fromJSON(file.path(here, "prepare_spec.json"), simplifyVector = FALSE)
spec <- jsonlite::fromJSON(file.path(here, "nca_spec.json"), simplifyVector = FALSE)
input <- pkapp_prepare_nca_covariates(read("oral_absorption.csv"), read("DM.csv"), read("VS.csv"), prep)
result <- pkapp_nca(input$data, spec)
stopifnot(result$data$AVAL[result$data$PARAMCD == "TMAX"] > 0,
  result$data$AVAL[result$data$PARAMCD == "CMAX"] > 0,
  !"oral_quantifiable_time_zero" %in% result$trace$groups[[1]]$warning)
print(result$data[c("PARAMCD", "AVAL", "AVALU", "PARAM_STATUS")])
