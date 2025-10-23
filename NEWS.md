# tyler 0.0.0.9001

* Introduced an **Imotive News & Changelog** vignette that centralises release
  notes for the mystery caller workflow.
* Documented how `run_mystery_caller_workflow()` coordinates roster creation,
  validation, call preparation, and QA for Imotive projects.
* Highlighted supporting helpers (`retrieve_clinician_data()`,
  `genderize_physicians()`, `split_and_save()`, and
  `states_where_physicians_were_NOT_contacted()`) inside the new vignette to
  surface relevant improvements for field teams.
* Deprecated legacy helpers (`search_npi()`, `test_and_process_isochrones()`,
  and `process_and_save_isochrones()`) in favour of the consolidated
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
