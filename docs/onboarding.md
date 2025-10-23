# Mystery Caller Workflow Onboarding Guide

Welcome to the **tyler** project! This guide is meant to get new collaborators productive quickly by explaining how we orchestrate a mystery caller study. It covers the core functions you will touch most frequently, the context in which they are used, and tips for avoiding common pitfalls.

> ℹ️ **Tip:** Many of these helpers read or write files. Always work from a project directory (via `usethis::proj_activate()` or opening the RStudio project) so relative paths resolve correctly.

## 1. Building the Provider Roster

### `search_by_taxonomy()`
Use this to pull fresh NPI registry results filtered by NUCC taxonomy descriptions.

- **Input:** Character vector of taxonomy descriptions (e.g., `c("Gynecologic Oncology", "Maternal & Fetal Medicine")`).
- **Output:** Tibble of provider details (NPI, name, taxonomy, location, etc.).
- **When to run:** At the start of a project to seed the roster with everyone matching a specialty description.
- **Workflow tips:**
  - Batch related subspecialties together and immediately deduplicate with `dplyr::distinct(npi, .keep_all = TRUE)` if combining manual lists.
  - The API paginates 200 records at a time; the helper fetches sequentially so allow a few minutes for large pulls.

### `search_and_process_npi()`
Augments the roster with NPIs found by physician name.

- **Input:** Data frame with `first` and `last` columns or a CSV path. Optional filters (credentials, country) live in the function arguments.
- **Output:** Tibble with validated NPIs and metadata.
- **When to run:** After receiving spreadsheets of “must call” physicians who may not appear in taxonomy results.
- **Workflow tips:**
  - Clean casing/spaces before calling so first/last name parsing succeeds.
  - Pass `limit` > 5 when you expect many providers with the same name.
  - Store timestamped results in `data/` for reproducibility.

## 2. Validating and Enriching NPIs

### `validate_and_remove_invalid_npi()`
Ensures every roster entry carries a clean 10-digit NPI.

- **Input:** Data frame or CSV path containing an `npi` column.
- **Output:** Filtered data frame with `npi_is_valid = TRUE`.
- **When to run:** Immediately after combining taxonomy/name pulls and before any downstream API lookups.
- **Workflow tips:**
  - The function strips punctuation automatically, so feed in raw NPIs from spreadsheets.
  - Save the returned tibble; you will pass it into later steps rather than re-reading from disk.

### `retrieve_clinician_data()`
Fetches detailed demographics from the CMS Care Compare dataset.

- **Input:** CSV of NPIs (or data frame) produced by the validation step.
- **Output:** Tidy tibble with PAC IDs, organisation info, gender, training details, etc.
- **When to run:** After validation, if you need facility affiliations or demographic columns to inform stratification.
- **Workflow tips:**
  - Expect `NULL` for NPIs absent from the CMS dataset; handle these cases explicitly in analysis scripts.
  - Cache downloads locally because the remote dataset is large.

### `genderize_physicians()`
Backfills missing physician gender information through the Genderize.io API.

- **Input:** CSV path with a `first_name` column.
- **Output:** CSV saved alongside probability and sample-size columns; invisibly returns the augmented data.
- **When to run:** Before reporting, once you have consolidated the roster and deduplicated names.
- **Workflow tips:**
  - The helper deduplicates first names before calling the API—ensure your dataset already normalises accent marks.
  - Budget API usage (1,000 daily requests on the free tier). Break large batches into chunks if necessary.

## 3. Preparing Phase 1 Calls

### `clean_phase_1_results()`
Normalises the initial roster for REDCap uploads and caller workbooks.

- **Input:** Data frame exported from screening spreadsheets (Excel/CSV).
- **Output:** Clean tibble with REDCap-ready `for_redcap` column plus helper fields like `doctor_id`, `insurance`, and `random_id`. A timestamped CSV is written to the output directory.
- **When to run:** Once you have a consolidated roster and before splitting workbooks for callers.
- **Workflow tips:**
  - Confirm the input has `names`, `practice_name`, `phone_number`, and `state_name` columns; the function stops early if they are missing.
  - The helper duplicates rows to balance Blue Cross/Medicaid calls—do not run it twice on the same data.
  - Review the console logs for warnings about missing NPIs or column coercions.

### `split_and_save()`
Distributes Phase 1 workbooks across caller teams.

- **Input:** Cleaned Phase 1 tibble or CSV path, output directory, vector of lab assistant names.
- **Output:** Timestamped Excel workbooks (one complete roster + one per assistant).
- **When to run:** Immediately after `clean_phase_1_results()`.
- **Workflow tips:**
  - Provide at least two names; the function randomises assignments within insurance groupings.
  - Default insurance order prioritises Medicaid—override `insurance_order` if project requirements differ.
  - The helper plays a confirmation sound (`beepr::beep`)—keep volume on if you want audible confirmation.

## 4. Phase 2 Follow-up

### `clean_phase_2_data()`
Standardises call outcome spreadsheets from callers.

- **Input:** Data frame or CSV path, plus vectors of substrings (`required_strings`) and desired names (`standard_names`).
- **Output:** Cleaned tibble with consistent column names and a timestamped CSV written to the working directory.
- **When to run:** After receiving Phase 2 logs from callers.
- **Workflow tips:**
  - Adjust `required_strings` when spreadsheets evolve—each entry maps to the first column containing the substring.
  - Inspect warnings about unmatched substrings; they often indicate typos in the raw workbook.

### `states_where_physicians_were_NOT_contacted()`
Summarises Phase 2 coverage gaps.

- **Input:** Phase 2 tibble (needs a `state` column) and optional vector of all state abbreviations.
- **Output:** Tibble listing states with zero completed contacts.
- **When to run:** After cleaning Phase 2 data to guide reallocation decisions.
- **Workflow tips:**
  - Pass the full list of expected states so the summary reports on missing data rather than just observed states.

### `save_quality_check_table()`
(Defined within `QualityCheck.R`) generates a QA extract for manual review.

- **Input:** Clean Phase 2 tibble and a file path for the CSV output.
- **Output:** CSV summarising key fields for QA (duplicates, missing NPIs, etc.).
- **When to run:** Before closing Phase 2 to document data hygiene.
- **Workflow tips:**
  - Create the output directory first or supply a path under `output/quality_checks/`.
  - Pair with `states_where_physicians_were_NOT_contacted()` for a full status update.

## 5. Optional Analytics Helpers

### `get_census_data()`
Pulls American Community Survey (ACS) demographics for block groups.

- **Input:** Data frame of geographies (with GEOIDs) or a vector of GEOIDs, plus API settings.
- **Output:** Tibble containing requested ACS variables.
- **When to run:** When enriching provider catchment areas prior to analytics or reporting.
- **Workflow tips:**
  - Set your Census API key via `census_api_key()` before calling.
  - Request only the variables you need; the ACS API throttles large pulls.

### `create_isochrones()` / `create_isochrones_for_dataframe()`
Generates drive-time polygons using the Mapbox API.

- **Input:** Single latitude/longitude pair (`create_isochrones`) or a data frame of provider locations (`create_isochrones_for_dataframe`).
- **Output:** GeoJSON files and `sf` objects representing drive-time areas.
- **When to run:** During spatial accessibility analysis once provider coordinates are final.
- **Workflow tips:**
  - Supply your Mapbox token via `Sys.setenv(MAPBOX_PUBLIC_TOKEN = "...")`.
  - Keep travel times ≤ 120 minutes to stay within API limits.

### `calculate_intersection_overlap_and_save()`
Computes overlaps between isochrones and census block groups.

- **Input:** Isochrone `sf` object and block group polygons.
- **Output:** CSV summarising overlap metrics.
- **When to run:** After generating isochrones and retrieving census polygons.
- **Workflow tips:**
  - Ensure both datasets share a common CRS (EPSG:4326) before intersecting.
  - Large spatial joins can be slow—test with a subset before running statewide analyses.

## 6. One-Click Orchestration

### `run_mystery_caller_workflow()`
Bundles the entire end-to-end process.

- **Input:**
  - `taxonomy_terms`, `name_data` for roster creation.
  - `phase1_data`, `lab_assistant_names`, `output_directory` for call preparation.
  - `phase2_data`, `quality_check_path` for follow-up.
  - Optional `npi_search_args` and `all_states` to customise behaviour.
- **Output:** Named list containing intermediate artefacts (roster, validated roster, cleaned Phase 1 & Phase 2 data, coverage summary, QA table).
- **When to run:** After you have all inputs ready and want a reproducible, scripted run of the entire workflow.
- **Workflow tips:**
  - Start in a clean R session so package options and API tokens are set explicitly.
  - Inspect the returned list and the timestamped files to verify each stage before notifying stakeholders.
  - Wrap the call in `tryCatch()` if you are iterating quickly; some API steps can fail due to network hiccups.

## 7. Troubleshooting Checklist

- Double-check API credentials (`MAPBOX_PUBLIC_TOKEN`, Census key, Genderize key if using a paid plan).
- Keep an eye on the console logs—many helpers print actionable warnings.
- Store intermediate CSVs in `data/` or `inst/extdata/` with timestamps so runs are auditable.
- When something fails, rerun the smallest helper possible before retrying the full workflow.

Welcome aboard! Reach out in the project Slack channel `#mystery-caller` if you hit blockers or discover opportunities to streamline the pipeline.
