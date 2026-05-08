# tyler 1.2.2

Released 2026-05-04.

## Bug fixes

* `library(tyler)` no longer crashes R or causes system memory exhaustion.
  Seven heavy packages (`ggmap`, `ggspatial`, `hereR`, `leaflet`,
  `leaflet.extras`, `lme4`, `censusapi`) were moved from `Imports` to
  `Suggests` so their compiled spatial libraries (GDAL, GEOS, PROJ) are
  loaded **only when the relevant function is first called**, not on package
  attach. Two packages declared in `Imports` but never called (`tigris`,
  `effects`) were removed entirely.

* `create_isochrones()` no longer accumulates memoized results in RAM
  indefinitely. The internal memoization object is now exposed through a new
  exported function `tyler_clear_isochrone_cache()` that releases the cache on
  demand. Call it after processing a large batch to reclaim memory.

* `create_isochrones_for_dataframe()` and `create_individual_isochrone_plots()`
  previously called `beepr::beep()` unconditionally even though `beepr` is a
  suggested package. Both calls are now guarded with `requireNamespace()`.

## New features

* `search_by_taxonomy()` gains three new arguments:
  - **`states`** — a character vector of two-letter state abbreviations. When
    supplied the function loops over each state and deduplicates on NPI,
    bypassing the NPI API's hard 1,200-record-per-query cap that previously
    caused national searches to return only providers whose names start with
    "A". Pass all 50 state abbreviations to perform a complete national search.
  - **`city`** — optional city filter passed directly to `npi::npi_search()`.
  - **`limit`** — controls records per API call (max 1,200, the API ceiling).

* All mapping and geospatial functions that depend on now-optional packages
  (`ggmap`, `ggspatial`, `hereR`, `leaflet`, `lme4`, `censusapi`, `easyr`)
  now emit a clear `stop()` message with the exact `install.packages()` command
  if the package is not installed.

# tyler 1.2.1

* Released on 2025-10-23 to align all metadata artifacts with the
  package website and codemeta specification.

* Introduced an **Imotive News & Changelog** vignette that centralizes release
  notes for the mystery caller workflow.
* Documented how `run_mystery_caller_workflow()` coordinates roster creation,
  validation, call preparation, and QA for Imotive projects.
* Highlighted supporting helpers (`retrieve_clinician_data()`,
  `genderize_physicians()`, `split_and_save()`, and
  `states_where_physicians_were_NOT_contacted()`) inside the new vignette to
  surface relevant improvements for field teams.
* Enhanced `states_where_physicians_were_NOT_contacted()` to ignore rows without
  affirmative contact outcomes and report the number of unique physicians who
  were successfully reached.
* Deprecated legacy helpers (`search_npi()`, `test_and_process_isochrones()`,
  and `process_and_save_isochrones()`) in favor of the consolidated
  `search_and_process_npi()` and `create_isochrones_for_dataframe()` workflow.

# tyler 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Verified and tested GitHub Actions workflows:
  * Confirmed R-CMD-check workflow runs on multiple platforms (macOS, Windows, Ubuntu).
  * Verified pkgdown-deploy workflow with `workflow_dispatch` for manual triggering.
  * Workflows properly configured to run on push/PR to main/master branches.
* Ensured the optional `provider` package is listed under Suggests and no longer
  imported in the namespace.
* Updated CRAN compliance:
  * Moved `provider` to Suggests and added runtime checks.
  * Removed automatic installation of `genderdata` and `provider` packages.
  * Refactored `genderize_physicians()` to use the Genderize.io API, removing the
    dependency on the non-CRAN `genderdata` package.
  * Excluded `To_amany.R` and `install_log.txt` from the build.
* Added `geocode_unique_addresses()` to simplify geocoding lists of addresses.
* Added a vignette skeleton on aggregating provider data and updated pkgdown configuration.
* Refactored naming and clarified API usage across various helper functions.
* Improved GitHub Actions workflows with dependency caching and clearer test output.
