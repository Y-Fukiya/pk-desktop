source("apps/pk-desktop/pkapp.R")
example <- "apps/pk-desktop/examples/nca_annotation"
data <- pkapp_read_table(file.path(example, "nca_input.csv"))
result <- pkapp_nca(data, list(id = "ID", groups = c("STUDYID", "PERIOD"), time = "TIME", concentration = "CONC",
  concentration_unit = "ng/mL", c0_method = "none", auc_method = "linear"))
dm <- pkapp_read_table(file.path(example, "DM.sas7bdat"))
vs <- pkapp_read_table(file.path(example, "VS.sas7bdat"))
specification <- jsonlite::fromJSON(file.path(example, "specification.json"), simplifyVector = FALSE)
annotated <- pkapp_annotate_nca(result, dm, vs, specification)
stopifnot(identical(annotated$data[names(result$data)], result$data), annotated$counts[["issues"]] == 0)
print(annotated$counts)
print(unique(annotated$data[c("ID", "STUDYID", "PERIOD", "DM_SEX", "VS_BMI", "VS_BMI_UNIT", "VS_BMI_DTC")]))
