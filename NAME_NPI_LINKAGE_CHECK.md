# Name–NPI Linkage Check

Date: 2026-05-13

## Scope reviewed
- `R/search_and_process_npi.R`
- `R/validate_and_remove_invalid_npi.R`
- `tests/testthat/test-search_and_process_npi.R`
- `tests/testthat/test-validate_and_remove_invalid_npi.R`
- Recent QA/docs summaries in repository root.

## Findings
1. **Linkage bug identified in `search_and_process_npi()`:** the `search_term` field was being built from returned NPI registry provider names (`basic_first_name/basic_last_name`) instead of the original query names. This can break resume behavior and auditability for name→NPI lookups when registry names differ from query input.
2. `validate_and_remove_invalid_npi()` correctly validates format + checksum via `npi::npi_is_valid`, but this only validates NPI integrity, not query-name linkage.

## Fix applied
- Updated `search_and_process_npi()` to preserve query linkage explicitly with:
  - `query_first_name`
  - `query_last_name`
  - `search_term` now set from the original query string
- Added test coverage to verify these fields are preserved and stable.

## Runtime validation status
- Could not execute R tests in this container because `Rscript` is unavailable.
