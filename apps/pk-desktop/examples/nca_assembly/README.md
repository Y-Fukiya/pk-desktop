# Synthetic EX / PC / concentration example

PC.sas7bdat has six actual sample records without concentrations. EX.sas7bdat has four administrations across two periods. conc.csv contains twelve rows, with parent and metabolite per sample. All data are artificial.

specification.json selects the last dose within each ID/PERIOD. Predose is -0.25 h. Both SAS clock modes are sas_seconds with +09:00. Text ID001 is retained. Reference candidates use ID/PERIOD, while safety history/count/next-dose checks cover all four subject EX records across periods. No single-dose AUCinf is permitted for this fixture.

Run from source: Rscript apps/pk-desktop/examples/nca_assembly/reproduce.R. This checks row count, four profiles, signed times, four-record history and repeatability. See root ASSEMBLY_GUIDE.md for GUI mappings and NCA values.
