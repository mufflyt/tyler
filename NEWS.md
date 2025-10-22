# tyler 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
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
