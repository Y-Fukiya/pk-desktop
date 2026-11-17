source("apps/pk-desktop/pkapp.R")
folder <- "apps/pk-desktop/examples/nca_crossover"
read_json <- function(name) jsonlite::fromJSON(file.path(folder, name), simplifyVector = FALSE)
conc <- pkapp_read_table(file.path(folder, "conc.csv"))
pc <- pkapp_read_table(file.path(folder, "PC.csv"))
ex <- pkapp_read_table(file.path(folder, "EX.csv"))
assembled <- pkapp_assemble_nca(conc, pc, ex, read_json("assembly.json"))
prepared <- pkapp_prepare(assembled$data, list(list(op = "blq_rules", mode = "positional",
  profile_columns = c("STUDYID", "ID", "PERIOD", "ANALYTE"), concentration = "CONC", time = "NCA_TIME_H",
  blq_column = "BLQ", mdv_column = "MDV", zero_time = "pre", pre_rule = "zero", first_rule = "zero",
  middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude", lloq_source = "column", lloq_column = "LLOQ")))
specification <- read_json("nca.json")
result <- pkapp_nca(prepared$data, specification)
for (profile in read_json("expected.json")) {
  rows <- result$data[result$data$ID == profile$ID & result$data$PERIOD == profile$PERIOD, ]
  for (parameter in setdiff(names(profile), c("ID", "PERIOD")))
    stopifnot(isTRUE(all.equal(rows$AVAL[rows$PARAMCD == parameter], profile[[parameter]], tolerance = 1e-10)))
}
stopifnot(all(vapply(result$trace$groups, function(g) g$predose_qc$status == "pass_blq", logical(1))))
cat("PASS: two subjects, two periods, 28 closed-form parameter comparisons; predose QC pass_blq.\n")
print(result$data[result$data$PARAMCD %in% c("LAMZHL", "AUCIFO"), c("ID", "PERIOD", "PARAMCD", "AVAL")], row.names = FALSE)
