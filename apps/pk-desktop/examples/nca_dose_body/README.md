# Synthetic pre-NCA dose and body-size examples

For a single oral dose with an absorption phase, use `oral_absorption.csv` with the same DM, VS and specifications. It follows `12 * (exp(-0.1 * time) - exp(-1.2 * time))` mg/L and starts at zero. Run `oral_reproduce.R` to check the example.

The following `synthetic.csv` example is an **analytic exponential validation fixture**, not a realistic single oral absorption profile. Its positive time-zero value is retained and flagged for review.

All records are invented. The concentration curve is `10 * exp(-0.1 * time)` mg/L, the actual dose is 100 mg, selected weight is 50 kg and recorded BSA is 2 m². DM sex and recorded BMI are retained without becoming PK dependencies.

Run from any directory with the app's R dependencies available:

```sh
Rscript /path/to/source/apps/pk-desktop/examples/nca_dose_body/reproduce.R
```

For the UI, import `synthetic.csv`, `DM.csv` and `VS.csv`. In **NCA入力：DM・VS**, select `ID`, groups `STUDYID` and `PERIOD`, corresponding study and period keys, DM sex, and VS `TEST`/`VALUE`/`VUNIT` with `BASELINE=Y`. Confirm the selected values are the per-period baseline. Save this input, then use it in NCA with the choices in `nca_spec.json`. This example has no EX assembly and explicitly declares one actual reference dose per profile. Complete dose history and single-dose context are user declarations.

Expected CL/F is 1 L/h and Vz/F is 10 L. Per-kg values are 0.02 L/h/kg and 0.2 L/kg. Per-m² values are 0.5 L/h/m² and 5 L/m². Dose-normalized Cmax is 0.1 1/L; dose-normalized AUClast is `1-exp(-4.8)` h/L; dose-normalized AUCinf is 1 h/L. Base Cmax remains 10 mg/L.

`expected.csv` contains independent analytical values. New PARAMCDs are application codes. Mosteller is an explicitly selected alternative to recorded BSA; this fixture uses the recorded BSA. No reference weight, reference BSA or bioavailability is assumed.
