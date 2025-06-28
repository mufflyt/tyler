# tyler 1.2.0

* Expanded dataset functions with ACGME residency program listings and ACOG
  district information.
* Introduced `search_by_taxonomy()` and `search_and_process_npi()` helpers for
  retrieving provider NPIs and demographics.
* Added geocoding utilities and mapping tools including
  `geocode_unique_addresses()`, `create_isochrones()`, and overlap calculations.
* Implemented wrappers around the `censusapi` package via `get_census_data()` to
  pull block-group information for isochrone analysis.
* Created numerous plotting helpers for scatter, line, and dot maps.
* Improved testing coverage across all functions using the `testthat` framework.
* Package documentation now includes detailed vignettes on census data queries
  and NPI lookups.
* Provided sample `physicians` dataset with geocoded provider coordinates for demonstration.
* Added `create_block_group_overlap_map()` to visualize drive-time coverage with census block groups.
* Introduced `validate_and_remove_invalid_npi()` for cleaning NPI lists before analysis.
* HERE API integration enables drive-time isochrones via `create_isochrones_for_dataframe()`.


# tyler 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
