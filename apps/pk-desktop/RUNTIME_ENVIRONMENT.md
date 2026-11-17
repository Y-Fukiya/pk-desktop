# PK Desktop 0.61.0-alpha: runtime and source distribution

The English UI remains a preview; diagnostic, graph and data-display coverage is incomplete. Test the environment and workflows before use.

The recorded validation environment is **R 4.6.1 with PKNCA 0.12.1**. The 168 package records in `renv.lock` were captured from the installed validation library. They replace the old R 4.4.3 lock, which did not describe the environment used to run the tests.

## Installation versus exact restoration

| Command, from the source root | Scope |
| --- | --- |
| `Rscript --vanilla apps/pk-desktop/install.R --check` | Read-only loadability check and exact PKNCA 0.12.1 check. Requires R >= 4.4 for the full teal app. |
| `Rscript --vanilla apps/pk-desktop/install.R` | Installs missing general dependencies from CRAN, replaces a missing or different PKNCA using only the exact 0.12.1 source release from current contrib or Archive, and verifies both source metadata and the installed backend version. Other packages are not pinned by this command. |
| `Rscript --vanilla apps/pk-desktop/restore.R --check` | Read-only check for the exact R version recorded in the lock and every recorded package version. Does not install anything. |
| `Rscript --vanilla apps/pk-desktop/restore.R` | Uses pinned renv 1.2.3 to restore the lock. Requires the recorded R 4.6.1, then verifies every version and package load. |

When PKNCA must be installed, setup first tries `https://cran.r-project.org/src/contrib/PKNCA_0.12.1.tar.gz`, then the exact-version `Archive/PKNCA/PKNCA_0.12.1.tar.gz` URL if the first download fails. It never resolves an unversioned latest release. Before installing any packages, setup reads the downloaded archive’s `PKNCA/DESCRIPTION` and requires Package `PKNCA` and Version `0.12.1`. A missing or mismatched DESCRIPTION stops installation; failure of both downloads is reported explicitly. Temporary downloads are removed on exit. This metadata check does not substitute for an independently recorded source checksum.

Run setup in a fresh R process. A loaded wrong PKNCA/renv namespace cannot safely be replaced in place, so setup stops and requests a restart. Package installation uses the project `.runtime/R-library`. A symlink at that path is followed by R: maintainers must not run installation against a shared test library unless modification is intended.

R >= 4.4 is the installation minimum for full teal functionality, not a claim that every R or dependency combination was validated. The strict restore policy is intentional: use the recorded R version to replay the reference environment, or the general installer for another supported R version and rerun acceptance checks. NCA replay continues to reject a PKNCA version different from the saved computation.

The lock records installed versions and their reported source metadata. The check confirms availability and matching versions; it does not prove a clean network restore. Source tarballs, compilers, system libraries and platform binaries have not been independently archived. In particular, the reference library reports Rcpp 1.1.1-1.1 from CRAN; availability of that exact source must be checked during a clean restore. Optional `nanoparquet` and `teal.modules.general` are not in the validated lock; the general installer accepts `--with-parquet` and `--with-modules` for those features. Python dependencies for optional MCP adapters are separate from the R lock.

## Focused source distribution

`release/package_source.py` builds a fresh source distribution with an explicit file allowlist. It keeps the full normal app, NCA app, R API, report generation/templates, optional MCP/TealFlow adapters, native wrapper source, application tests, launcher scripts, schemas and the synthetic `reviewed-repeated` demo bundle. The demo ADPP contains historical precomputed parameters; opening it does not rerun NCA.

It excludes the 37-drug development library, unrelated harness tools, old project release notes, cached/generated outputs, runtime libraries, virtual environments and compiled native binaries. `tools/pk_review.py` and `tools/make_adnca.R` remain because bootstrap fingerprints them and optional imports/reports use them. The normal app therefore remains available; this is not an NCA-only distribution.

The developer workspace retains the complete harness and scientific fixtures. Run `make harness-check` there. The focused distribution includes `release/verify_release.py`, its [reproduction guide](release/VERIFY_RELEASE.md), `check_macos.R`, and the application R tests. The focused verifier runs the shipped NCA/preprocessing tests and reference examples in fresh, bounded child processes and records individual logs and a JSON result. It does not contain the repository-wide Makefile or claim that the 37-drug harness runs from the smaller archive. Mac GUI, native wrapper/binary, signing, notarization, clean-machine installation and optional network integrations require separate acceptance.

The macOS launch and check commands preserve an existing UTF-8 locale. Under a C or other non-UTF-8 locale, they select and verify an installed UTF-8 locale before starting R. If no usable UTF-8 locale is available, they stop with a diagnostic. This affects only the launched process environment; it does not alter OS settings.

The packaging command refuses overwrite and symlinks in selected source paths, checks the four application version labels, records every packaged path/size/SHA-256/mode in `PACKAGE_CONTENTS.json`, preserves executable `.command` files, and verifies the resulting ZIP contents. ZIP timestamps and entry order are fixed for deterministic output. Pass each top-level release document/example/launcher explicitly with `--include`; no unrelated root content is copied implicitly.

Example (run from the developer source):

```sh
python3 apps/pk-desktop/release/package_source.py \
  --source /path/to/completed/source \
  --release-root /path/to/completed/release \
  --include README.md --include Start-NCA.command \
  --destination /path/to/new-stage/PKDesktop-R-0.61.0-alpha \
  --archive /path/to/new-output/PKDesktop-R-0.61.0-alpha.zip \
  --version 0.61.0-alpha
```

Build the archive only after changes and checks are complete. The packaging helper verifies structure and integrity, not scientific correctness or live GUI behavior.
