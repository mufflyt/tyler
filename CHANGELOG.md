# Changelog

All notable changes to this project will be documented in this file.

## [1.2.0] - 2024-07-20
### Added
- New `search_by_taxonomy()` function for pulling provider lists from the NPI registry.
- Expanded data helpers including residency programs and district information.
- Geocoding and mapping utilities like `geocode_unique_addresses()` and `create_isochrones()`.
- Wrappers for US Census queries via `get_census_data()`.
- Helper plots for density, line, and scatter charts.
- Added `physicians` dataset with sample geocoded NPI data for examples.
- New `create_block_group_overlap_map()` for interactive visualizations of drive-time coverage.
- Introduced `validate_and_remove_invalid_npi()` to clean provider lists before retrieval.
- Batch isochrone generation via `create_isochrones_for_dataframe()` using the HERE API.

### Changed
- Improved processing of NPIs through `search_and_process_npi()`.
- Better splitting of data frames for caller assignments.
- Updated `get_census_data()` for clearer variable output.
- Refactored `split_and_save` to keep lab assistant names consistent across runs.
- Plotting helpers now support custom color palettes and scales.

### Fixed
- Export issue for `geocode_unique_addresses()` in the namespace.
- Numerous small bugs uncovered by the expanded test suite.
- Fixed dataset documentation for `acgme` and `ACOG_Districts`.

## [0.0.0.9000] - 2023-09-11
- Initial package structure with basic utilities.
