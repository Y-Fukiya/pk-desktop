args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1L]), mustWork = TRUE)
here <- dirname(script)
source(file.path(here, "..", "..", "pkapp.R"))
read <- function(name) pkapp_read_table(file.path(here, name))
prep <- jsonlite::fromJSON(file.path(here, "prepare_spec.json"), simplifyVector = FALSE)
spec <- jsonlite::fromJSON(file.path(here, "nca_spec.json"), simplifyVector = FALSE)
input <- pkapp_prepare_nca_covariates(read("synthetic.csv"), read("DM.csv"), read("VS.csv"), prep)
result <- pkapp_nca(input$data, spec)
expected <- read.csv(file.path(here, "expected.csv"), check.names = FALSE)
print(result$data[c("PARAMCD", "AVAL", "AVALU", "PARAM_STATUS")])
stopifnot(abs(result$data$AVAL[result$data$PARAMCD == "CLFO"] - 1) < 1e-10,
  abs(result$data$AVAL[result$data$PARAMCD == "VZFO"] - 10) < 1e-10,
  abs(result$data$AVAL[result$data$PARAMCD == "CLFOWT"] - .02) < 1e-10,
  abs(result$data$AVAL[result$data$PARAMCD == "VZFOBSA"] - 5) < 1e-10)
