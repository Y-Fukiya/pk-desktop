source("apps/pk-desktop/pkapp.R")
folder <- "apps/pk-desktop/examples/nca_assembly"
specification <- jsonlite::fromJSON(file.path(folder, "specification.json"), simplifyVector = FALSE)
inputs <- list(conc = pkapp_read_table(file.path(folder, "conc.csv")),
  pc = pkapp_read_table(file.path(folder, "PC.sas7bdat")), ex = pkapp_read_table(file.path(folder, "EX.sas7bdat")))
result <- do.call(pkapp_assemble_nca, c(inputs, list(specification = specification)))
stopifnot(nrow(result$data) == 12L, length(unique(result$data$NCA_ASSEMBLY_PROFILE)) == 4L,
  all(result$data$NCA_TIME_H[c(1, 2, 7, 8)] == -.25), all(result$data$NCA_DOSE_COUNT == 4L),
  identical(result$data, do.call(pkapp_assemble_nca, c(inputs, list(specification = result$specification)))$data))
print(result$counts)
