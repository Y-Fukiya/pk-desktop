# Source this file, then call load_pk_handoff("handoff.json") from the app directory.
load_pk_handoff <- function(path = "handoff.json") {
  handoff <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  root <- handoff$root
  env <- new.env(parent = environment())
  source(file.path(root, "apps/pk-review/R/data.R"), local = env)
  source(file.path(root, "apps/pk-review/R/analysis.R"), local = env)
  env$pk_assert(identical(handoff$view_sha256, env$pk_hash(handoff$view)),
                "Saved PK settings changed; create a new TealFlow handoff")
  cfg <- env$pk_read_config(handoff$view)
  env$pk_assert(identical(cfg$revision, handoff$revision), "PK revision differs from handoff")
  bundle <- env$pk_load_bundle(handoff$bundle)
  env$pk_assert(identical(bundle$hashes, handoff$input_hashes), "PK inputs differ from handoff")
  result <- env$pk_analyse(bundle, cfg)
  list(ADNCA = result$concentrations, ADPP = result$parameters,
       config = cfg, counts = result$counts, warnings = result$warnings,
       concentration_summary = result$concentration_summary,
       parameter_summary = result$parameter_summary)
}
