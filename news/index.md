# Changelog

## mysterycall 1.3.0

Released 2026-05-08.

### 💥 Breaking changes (with backward compatibility)

- Package renamed from `tyler` to `mysterycall`.
  [`library(tyler)`](https://rdrr.io/r/base/library.html) will no longer
  work; use
  [`library(mysterycall)`](https://mufflyt.github.io/mysterycall/).
- All 100 exported functions now carry the `mysterycall_` prefix (e.g.,
  [`mysterycall_geocode()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_geocode.md),
  [`mysterycall_search_and_process_npi()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_search_and_process_npi.md)).
  The previous `tyler_` prefix names are retained as deprecated shims
  that emit a warning and forward to the new name. The even-older
  unprefixed names (e.g.,
  [`check_normality()`](https://mufflyt.github.io/mysterycall/reference/mysterycall-deprecated.md))
  continue to work as double-deprecated shims.

### ✅ rOpenSci compliance

- Replaced all 34 `\dontrun{}` blocks with `@examplesIf interactive()`.
- Replaced 18 live [`print()`](https://rdrr.io/r/base/print.html) calls
  with [`message()`](https://rdrr.io/r/base/message.html) or
  [`invisible()`](https://rdrr.io/r/base/invisible.html).
- Moved `sf` from `Imports` to `Suggests`; added
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) guards in
  8 geospatial functions to avoid forcing GDAL/GEOS/PROJ on all users.
- Bumped minimum R version from 3.5.0 to 4.1.0.
- Removed duplicate `Maintainer:` field from `DESCRIPTION`.
- Added GitHub issue/PR templates and `repostatus.org` badge.

------------------------------------------------------------------------

## mysterycall 1.2.2

Released 2026-05-04.

### 🐛 Bug fixes

- [`library(tyler)`](https://rdrr.io/r/base/library.html) no longer
  crashes R or causes system memory exhaustion. Seven heavy packages
  (`ggmap`, `ggspatial`, `hereR`, `leaflet`, `leaflet.extras`, `lme4`,
  `censusapi`) were moved from `Imports` to `Suggests` so their compiled
  spatial libraries (GDAL, GEOS, PROJ) are loaded **only when the
  relevant function is first called**, not on package attach. Two
  packages declared in `Imports` but never called (`tigris`, `effects`)
  were removed entirely.

- [`create_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall-deprecated.md)
  no longer accumulates memoized results in RAM indefinitely. The
  internal memoization object is now exposed through
  [`mysterycall_clear_isochrone_cache()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_clear_isochrone_cache.md).
  Call it after processing a large batch to reclaim memory.

- [`create_isochrones_for_dataframe()`](https://mufflyt.github.io/mysterycall/reference/mysterycall-deprecated.md)
  and
  [`create_individual_isochrone_plots()`](https://mufflyt.github.io/mysterycall/reference/mysterycall-deprecated.md)
  previously called
  [`beepr::beep()`](https://rdrr.io/pkg/beepr/man/beep.html)
  unconditionally even though `beepr` is a suggested package. Both calls
  are now guarded with
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

### ✨ New features

- [`mysterycall_search_taxonomy()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_search_taxonomy.md)
  gains three new arguments:
  - **`states`** — loops over each state and deduplicates on NPI,
    bypassing the NPI API’s hard 1,200-record-per-query cap. Pass all 50
    state abbreviations to perform a complete national search.
  - **`city`** — optional city filter passed directly to
    [`npi::npi_search()`](https://docs.ropensci.org/npi/reference/npi_search.html).
  - **`limit`** — controls records per API call (max 1,200).
- All mapping and geospatial functions now emit a clear
  [`stop()`](https://rdrr.io/r/base/stop.html) message with the exact
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  command if a required optional package is not installed.

------------------------------------------------------------------------

## mysterycall 1.2.1

Released 2025-10-23.

### 📝 Documentation

- Released to align all metadata artifacts with the package website and
  codemeta specification.
- Introduced an **Imotive News & Changelog** vignette centralising
  release notes.
- Documented how
  [`mysterycall_run_workflow()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_run_workflow.md)
  coordinates roster creation, validation, call preparation, and QA for
  Imotive projects.

### ✨ New features

- [`mysterycall_not_contacted_states()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_not_contacted_states.md)
  now ignores rows without affirmative contact outcomes and reports the
  number of unique physicians reached.

### 🗑️ Deprecated

- [`search_npi()`](https://mufflyt.github.io/mysterycall/reference/mysterycall-deprecated.md)
  → use
  [`mysterycall_search_and_process_npi()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_search_and_process_npi.md)
- [`test_and_process_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall-deprecated.md)
  → use
  [`mysterycall_isochrones_for_df()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_isochrones_for_df.md)
- [`process_and_save_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall-deprecated.md)
  → use
  [`mysterycall_isochrones_for_df()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_isochrones_for_df.md)

------------------------------------------------------------------------

## mysterycall 0.0.0.9000

### 🌱 Initial development

- Added `NEWS.md` to track changes.
- Verified R-CMD-check workflows on macOS, Windows, and Ubuntu.
- Moved `provider` to `Suggests`; added runtime checks throughout.
- Refactored
  [`mysterycall_genderize()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_genderize.md)
  to use the Genderize.io API, removing the dependency on the non-CRAN
  `genderdata` package.
- Added
  [`mysterycall_geocode()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_geocode.md)
  to simplify geocoding lists of addresses.
- Added vignette skeleton on aggregating provider data.
