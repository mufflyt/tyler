# mysterycall 1.3.0

Released 2026-05-08.

## 💥 Breaking changes (with backward compatibility)

* Package renamed from `tyler` to `mysterycall`. `library(tyler)` will no longer
  work; use `library(mysterycall)`.
* All 100 exported functions now carry the `mysterycall_` prefix (e.g.,
  `mysterycall_geocode()`, `mysterycall_search_and_process_npi()`).
  The previous `tyler_` prefix names are retained as deprecated shims that
  emit a warning and forward to the new name. The even-older unprefixed names
  (e.g., `check_normality()`) continue to work as double-deprecated shims.

## ✨ New functions

**Data quality and validation**

* `mysterycall_validate_phone()` — validates US phone numbers against NANP
  structural rules (NPA/NXX first-digit constraints, N11 service codes) and
  optionally checks that the area code belongs to the provider's reported
  practice state via a bundled lookup table. Returns a tidy data frame with
  `phone_e164_valid`, `phone_npa`, `phone_state_from_npa`,
  `phone_area_code_matches_state`, and `phone_validity_flag` columns.
  Fully vectorised; lookup table is lazy-loaded and cached per session.

* `mysterycall_parse_physician_name()` — converts free-text physician name
  strings (board certification data, NPPES, CMS sources) into structured
  first / middle / last / suffix / title fields with confidence scoring
  (`high` / `medium` / `low`) and warning flags. Handles DO credential vs.
  Vietnamese surname disambiguation (`"Robert Smith DO"` → suffix `"DO"`;
  `"Linda Do"` → last name `"Do"`), three-part comma format
  (`"Smith, John, Jr."`), hyphenated names, and name particles.

* `mysterycall_validate_parsed_names()` — extends parse output with quality
  flags: `has_first`, `has_last`, `is_valid`, `last_is_credential`,
  `last_is_suffix`, `last_too_short`, `middle_has_particle`, `quality_issue`.

* `mysterycall_format_physician_name()` — reassembles parsed components into
  `"last_first"`, `"first_last"`, or `"formal"` display strings; vectorised.

* `mysterycall_test_name_parser()` — runs a 13-case edge-case accuracy suite
  and prints a per-case report to the console. An extended 30-case benchmark
  corpus is available in `inst/extdata/name_benchmark_corpus.csv`; the
  evaluation script is in `data-raw/benchmark_name_parser.R`.

**Safe join wrappers**

* `mysterycall_safe_left_join()` — wraps `dplyr::left_join()` with key-type
  harmonisation, right-side uniqueness assertion, coverage threshold
  enforcement (`min_coverage`, default 0.98), row-multiplication guard
  (`max_duplication`, default 1.02×), and optional CSV audit report.

* `mysterycall_safe_inner_join()` — wraps `dplyr::inner_join()` with the same
  guards; default `min_coverage = 0.90`.

* `mysterycall_safe_semi_join()` — wraps `dplyr::semi_join()` with a
  keep-rate threshold; default `min_coverage = 0.50`.

* `mysterycall_safe_anti_join()` — wraps `dplyr::anti_join()` with an
  over-exclusion cap (`max_matched`, default 1.0).

* `mysterycall_assert_unique_keys()` — asserts that specified columns form a
  unique key; optionally deduplicates (first row kept) instead of erroring.

**Package infrastructure**

* `humaniformat` moved from `Suggests` → `Imports`; the runtime
  `requireNamespace()` guard has been removed.
* 119 new `test_that` blocks across three new test files
  (`test-validate-phone.R`, `test-parse-physician-name.R`,
  `test-join-safety.R`).
* Five new vignettes: `data-quality`, `statistical-analysis`,
  `provider-classification`, `workflow-orchestration`, `table-generation`.
* `CITATION.cff` updated with full abstract, keywords, and affiliation.
* `CONTRIBUTING.md` expanded to a full developer guide.

## ✅ rOpenSci compliance

* Replaced all 34 `\dontrun{}` blocks with `@examplesIf interactive()`.
* Replaced 18 live `print()` calls with `message()` or `invisible()`.
* Moved `sf` from `Imports` to `Suggests`; added `requireNamespace()` guards
  in 8 geospatial functions to avoid forcing GDAL/GEOS/PROJ on all users.
* Bumped minimum R version from 3.5.0 to 4.1.0.
* Removed duplicate `Maintainer:` field from `DESCRIPTION`.
* Added GitHub issue/PR templates and `repostatus.org` badge.

---

# mysterycall 1.2.2

Released 2026-05-04.

## 🐛 Bug fixes

* `library(tyler)` no longer crashes R or causes system memory exhaustion.
  Seven heavy packages (`ggmap`, `ggspatial`, `hereR`, `leaflet`,
  `leaflet.extras`, `lme4`, `censusapi`) were moved from `Imports` to
  `Suggests` so their compiled spatial libraries (GDAL, GEOS, PROJ) are
  loaded **only when the relevant function is first called**, not on package
  attach. Two packages declared in `Imports` but never called (`tigris`,
  `effects`) were removed entirely.

* `create_isochrones()` no longer accumulates memoized results in RAM
  indefinitely. The internal memoization object is now exposed through
  `mysterycall_clear_isochrone_cache()`. Call it after processing a large
  batch to reclaim memory.

* `create_isochrones_for_dataframe()` and `create_individual_isochrone_plots()`
  previously called `beepr::beep()` unconditionally even though `beepr` is a
  suggested package. Both calls are now guarded with `requireNamespace()`.

## ✨ New features

* `mysterycall_search_taxonomy()` gains three new arguments:
  - **`states`** — loops over each state and deduplicates on NPI, bypassing
    the NPI API's hard 1,200-record-per-query cap. Pass all 50 state
    abbreviations to perform a complete national search.
  - **`city`** — optional city filter passed directly to `npi::npi_search()`.
  - **`limit`** — controls records per API call (max 1,200).

* All mapping and geospatial functions now emit a clear `stop()` message with
  the exact `install.packages()` command if a required optional package is
  not installed.

---

# mysterycall 1.2.1

Released 2025-10-23.

## 📝 Documentation

* Released to align all metadata artifacts with the package website and
  codemeta specification.
* Introduced an **Imotive News & Changelog** vignette centralising release notes.
* Documented how `mysterycall_run_workflow()` coordinates roster creation,
  validation, call preparation, and QA for Imotive projects.

## ✨ New features

* `mysterycall_not_contacted_states()` now ignores rows without affirmative
  contact outcomes and reports the number of unique physicians reached.

## 🗑️ Deprecated

* `search_npi()` → use `mysterycall_search_and_process_npi()`
* `test_and_process_isochrones()` → use `mysterycall_isochrones_for_df()`
* `process_and_save_isochrones()` → use `mysterycall_isochrones_for_df()`

---

# mysterycall 0.0.0.9000

## 🌱 Initial development

* Added `NEWS.md` to track changes.
* Verified R-CMD-check workflows on macOS, Windows, and Ubuntu.
* Moved `provider` to `Suggests`; added runtime checks throughout.
* Refactored `mysterycall_genderize()` to use the Genderize.io API, removing
  the dependency on the non-CRAN `genderdata` package.
* Added `mysterycall_geocode()` to simplify geocoding lists of addresses.
* Added vignette skeleton on aggregating provider data.
