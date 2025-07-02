# tyler 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Updated CRAN compliance:
  * Moved `provider` to Suggests and added runtime checks.
  * Removed automatic installation of `genderdata` and `provider` packages.
  * Excluded `To_amany.R` and `install_log.txt` from the build.
* Added `geocode_unique_addresses()` to simplify geocoding lists of addresses.
* Added a vignette skeleton on aggregating provider data and updated pkgdown configuration.
* Refactored naming and clarified API usage across various helper functions.
* Improved GitHub Actions workflows with dependency caching and clearer test output.
