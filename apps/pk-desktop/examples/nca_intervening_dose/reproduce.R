source("apps/pk-desktop/examples/nca_crossover/reproduce.R")
reference <- result
extra <- ex[1, ]; extra$EXSEQ <- "RESCUE"; extra$EXSTDTC <- "2026-11-01T21:00+00:00"
ex <- rbind(ex, extra)
assembled <- pkapp_assemble_nca(conc, pc, ex, read_json("assembly.json"))
prepared <- pkapp_prepare(assembled$data, list(list(op = "blq_rules", mode = "positional",
  profile_columns = c("STUDYID", "ID", "PERIOD", "ANALYTE"), concentration = "CONC", time = "NCA_TIME_H",
  blq_column = "BLQ", mdv_column = "MDV", zero_time = "pre", pre_rule = "zero", first_rule = "zero",
  middle_rule = "exclude", last_rule = "exclude", all_blq_rule = "exclude", lloq_source = "column", lloq_column = "LLOQ")))
result <- pkapp_nca(prepared$data, specification)
affected <- result$data$ID == "001" & result$data$PERIOD == "1"
stopifnot(sum(affected) == 7L, all(is.na(result$data$AVAL[affected])),
  all(result$data$NCA_EXCLUDE[affected] == "intervening_dose"),
  all(result$data$PROFILE_STATUS[affected] == "excluded"),
  isTRUE(all.equal(result$data$AVAL[!affected], reference$data$AVAL[!affected], tolerance = 1e-12)))
group <- Filter(function(g) g$status == "excluded", result$trace$groups)
stopifnot(length(group) == 1L, group[[1]]$dose_review$next_h == 12,
  group[[1]]$dose_review$next_dose$ex_row == 5L,
  grepl("RESCUE", group[[1]]$dose_review$next_dose$key),
  identical(as.numeric(group[[1]]$dose_review$affected_rows$time_h), c(12, 24, 48)))
cat("PASS: one profile excluded (7 parameters), other three retain 21 analytic reference values.\n")
print(group[[1]]$dose_review)
