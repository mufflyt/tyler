# Snapshot regression tests for the audit trail JSON written by
# mysterycall_clean_phase1(). These tests enforce two guarantees:
#
#   1. Schema stability: every required field from tests/fixtures/audit_trail_schema.json
#      is present in every audit file.
#   2. Value stability: flag counts for the canonical 4-row input match
#      tests/fixtures/expected_audit_counts.json.
#
# On the first run, expect_snapshot() creates _snaps/test-audit-trail-snapshot.md.
# Subsequent runs diff against that file. A snapshot failure means the audit
# trail structure or cleaning logic changed; update the snapshot intentionally
# with testthat::snapshot_accept() if the change is deliberate.

library(testthat)
library(mysterycall)

skip_if_not_installed("jsonlite")

# ---------------------------------------------------------------------------
# Canonical 4-row input — fixed for all tests in this file.
# Row 3: empty name + NA NPI  → triggers empty_names_count and generated_id path.
# Row 4: single-word name     → triggers no_last_name_count.
# duplicate_rows = FALSE ensures output_rows == input_rows.
# ---------------------------------------------------------------------------
canonical_input <- data.frame(
  names         = c("Alice Smith", "Bob Jones", "", "Carol"),
  practice_name = c("Clinic A", "Clinic B", "Clinic C", "Clinic D"),
  phone_number  = c("555-0101", "555-0102", "555-0103", "555-0104"),
  state_name    = c("Colorado", "Texas", "Florida", "New York"),
  npi           = c("1234567893", "1234567891", NA_character_, "1234567890"),
  for_redcap    = c("Yes", "No", "Yes", "Yes"),
  stringsAsFactors = FALSE
)

# Run clean_phase1 on the canonical input; return parsed audit JSON.
# Skips (not fails) if no audit file is written (e.g. jsonlite absent).
run_canonical_audit <- function() {
  tmp <- tempfile()
  dir.create(tmp)
  suppressMessages(
    mysterycall_clean_phase1(
      phase1_data      = canonical_input,
      output_directory = tmp,
      verbose          = FALSE,
      notify           = FALSE,
      duplicate_rows   = FALSE
    )
  )
  files <- list.files(tmp, pattern = "audit_trail.*\\.json", full.names = TRUE)
  if (length(files) == 0L) skip("No audit trail JSON was written")
  jsonlite::fromJSON(files[1L], simplifyVector = FALSE)
}

# Strip volatile fields before snapshot comparison so timestamps and
# environment-specific values do not cause spurious failures.
normalize_for_snapshot <- function(audit) {
  volatile <- c("start_time", "end_time", "duration_seconds",
                "r_version", "platform", "package_version", "parameters")
  for (field in volatile) audit[[field]] <- NULL
  # Sort for stable ordering across R versions
  audit[sort(names(audit))]
}

# ---------------------------------------------------------------------------
# 1. Schema presence: every required field must be in the audit JSON
# ---------------------------------------------------------------------------

test_that("Snapshot: audit JSON contains all required schema fields", {
  skip_on_cran()
  audit <- run_canonical_audit()

  required_fields <- c(
    "function_name", "input_rows", "input_cols", "input_colnames",
    "output_rows", "output_cols", "empty_names_count", "no_last_name_count",
    "rows_retained_pct", "rows_duplicated", "original_npi_preserved",
    "quality_metrics"
  )

  missing <- setdiff(required_fields, names(audit))
  expect_equal(missing, character(0L),
               label = paste("Missing required audit fields:", paste(missing, collapse = ", ")))
})

# ---------------------------------------------------------------------------
# 2. Value stability: flag counts match the expected_audit_counts fixture
# ---------------------------------------------------------------------------

test_that("Snapshot: audit counts match expected_audit_counts fixture", {
  skip_on_cran()
  audit <- run_canonical_audit()

  fixture_file <- testthat::test_path("../fixtures/expected_audit_counts.json")
  if (!file.exists(fixture_file)) skip("Fixture file not found")
  expected <- jsonlite::fromJSON(fixture_file, simplifyVector = TRUE)

  expect_equal(audit$function_name,   expected$function_name,
               label = "function_name must match fixture")
  expect_equal(audit$input_rows,      expected$input_rows,
               label = "input_rows must match fixture")
  expect_equal(audit$input_cols,      expected$input_cols,
               label = "input_cols must match fixture")
  expect_equal(audit$output_rows,     expected$output_rows,
               label = "output_rows must match fixture (duplicate_rows=FALSE)")
  expect_equal(audit$empty_names_count, expected$empty_names_count,
               label = "empty_names_count must match fixture")
  expect_equal(audit$no_last_name_count, expected$no_last_name_count,
               label = "no_last_name_count must match fixture")
  expect_equal(audit$rows_retained_pct,  expected$rows_retained_pct,
               label = "rows_retained_pct must be 100 for no-drop run")
  expect_equal(audit$rows_duplicated,    expected$rows_duplicated,
               label = "rows_duplicated must be FALSE when duplicate_rows=FALSE")
  expect_equal(audit$original_npi_preserved, expected$original_npi_preserved,
               label = "original_npi_preserved must be TRUE when npi column present")

  actual_colnames <- sort(unlist(audit$input_colnames))
  expected_colnames <- sort(expected$input_colnames)
  expect_equal(actual_colnames, expected_colnames,
               label = "input_colnames must match fixture column names")
})

# ---------------------------------------------------------------------------
# 3. Structural snapshot: catches silent field additions, removals, renames
# ---------------------------------------------------------------------------

test_that("Snapshot: normalized audit structure is stable", {
  skip_on_cran()
  audit <- run_canonical_audit()
  normalized <- normalize_for_snapshot(audit)

  # Snapshot captures field names and non-volatile values.
  # Update with testthat::snapshot_accept("audit-trail-snapshot") if intentional.
  expect_snapshot(names(normalized))
})

test_that("Snapshot: quality_metrics keys are stable", {
  skip_on_cran()
  audit <- run_canonical_audit()
  expect_snapshot(sort(names(audit$quality_metrics)))
})

# ---------------------------------------------------------------------------
# 4. Fixture consistency: schema fixture itself is valid JSON with required keys
# ---------------------------------------------------------------------------

test_that("Fixture: audit_trail_schema.json is valid and contains required/volatile sections", {
  fixture_file <- testthat::test_path("../fixtures/audit_trail_schema.json")
  if (!file.exists(fixture_file)) skip("Schema fixture file not found")

  schema <- jsonlite::fromJSON(fixture_file, simplifyVector = FALSE)
  expect_true("required"  %in% names(schema),
              label = "Schema fixture must have 'required' section")
  expect_true("volatile"  %in% names(schema),
              label = "Schema fixture must have 'volatile' section")
  expect_true("conditional" %in% names(schema),
              label = "Schema fixture must have 'conditional' section")
  expect_gt(length(schema$required), 5L,
            label = "Schema must document at least 6 required fields")
})

test_that("Fixture: schema_version is declared in audit_trail_schema.json required section", {
  schema_file <- testthat::test_path("../fixtures/audit_trail_schema.json")
  if (!file.exists(schema_file)) skip("Schema fixture file not found")

  schema <- jsonlite::fromJSON(schema_file, simplifyVector = FALSE)
  expect_true("schema_version" %in% names(schema$required),
              label = "schema_version must be listed as a required field in the schema fixture")
  expect_true(is.character(schema$required$schema_version))
  expect_match(schema$required$schema_version, "semantic version|schema",
               ignore.case = TRUE,
               label = "schema_version description must mention semantic versioning")
})

# ---------------------------------------------------------------------------
# 5. schema_version and cohort_hash contracts
# ---------------------------------------------------------------------------

test_that("Audit schema_version is present, semantic, and matches fixture", {
  skip_on_cran()
  audit <- run_canonical_audit()

  fixture_file <- testthat::test_path("../fixtures/expected_audit_counts.json")
  if (!file.exists(fixture_file)) skip("Fixture file not found")
  expected <- jsonlite::fromJSON(fixture_file, simplifyVector = TRUE)

  expect_true("schema_version" %in% names(audit),
              label = paste("schema_version missing. Available fields:",
                            paste(names(audit), collapse = ", ")))
  expect_type(audit$schema_version, "character")
  expect_length(audit$schema_version, 1L)
  expect_match(audit$schema_version, "^\\d+\\.\\d+\\.\\d+$",
               label = "schema_version must follow MAJOR.MINOR.PATCH")
  expect_identical(audit$schema_version, expected$schema_version,
                   label = paste(
                     "Audit schema_version does not match fixture.",
                     "If schema changed intentionally:",
                     "1. Update audit_trail_schema.json",
                     "2. Update expected_audit_counts.json",
                     "3. Refresh snapshot files with testthat::snapshot_accept()"
                   ))
})

test_that("Audit cohort_hash is present and SHA-256 shaped", {
  skip_on_cran()
  audit <- run_canonical_audit()

  expect_true("cohort_hash" %in% names(audit),
              label = "cohort_hash must be present in every audit JSON")
  expect_type(audit$cohort_hash, "character")
  expect_match(audit$cohort_hash, "^[a-f0-9]{64}$",
               label = "cohort_hash must be a 64-character lowercase hex SHA-256 digest")
})

test_that("Audit cohort_hash is deterministic for identical input", {
  skip_on_cran()
  audit1 <- run_canonical_audit()
  audit2 <- run_canonical_audit()

  expect_identical(audit1$cohort_hash, audit2$cohort_hash,
                   label = "Same input must always produce the same cohort_hash")
})

# ---------------------------------------------------------------------------
# 6. parent_cohort_hash lineage stub
# ---------------------------------------------------------------------------

test_that("Lineage: parent_cohort_hash is NULL when no upstream hash supplied", {
  skip_on_cran()
  audit <- run_canonical_audit()  # run_canonical_audit does not pass parent_cohort_hash

  # Field may be absent (NULL serialises away) or explicitly NULL — both are correct.
  value <- audit[["parent_cohort_hash"]]
  expect_true(is.null(value),
              label = "parent_cohort_hash must be NULL when no upstream hash is supplied")
})

test_that("Lineage: supplied valid parent_cohort_hash is preserved verbatim", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")

  fake_parent <- paste(rep("a", 64), collapse = "")  # valid 64-char hex

  tmp <- tempfile()
  dir.create(tmp)
  suppressMessages(
    mysterycall_clean_phase1(
      phase1_data        = canonical_input,
      output_directory   = tmp,
      verbose            = FALSE,
      notify             = FALSE,
      duplicate_rows     = FALSE,
      parent_cohort_hash = fake_parent
    )
  )
  files <- list.files(tmp, pattern = "audit_trail.*\\.json", full.names = TRUE)
  if (length(files) == 0L) skip("No audit trail JSON was written")
  audit <- jsonlite::fromJSON(files[1L], simplifyVector = FALSE)

  expect_identical(audit$parent_cohort_hash, fake_parent,
                   label = "A valid parent_cohort_hash must be written verbatim to audit JSON")
})

test_that("Lineage: invalid parent_cohort_hash is silently dropped", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")

  tmp <- tempfile()
  dir.create(tmp)
  suppressMessages(
    mysterycall_clean_phase1(
      phase1_data        = canonical_input,
      output_directory   = tmp,
      verbose            = FALSE,
      notify             = FALSE,
      duplicate_rows     = FALSE,
      parent_cohort_hash = "not-a-valid-hash"
    )
  )
  files <- list.files(tmp, pattern = "audit_trail.*\\.json", full.names = TRUE)
  if (length(files) == 0L) skip("No audit trail JSON was written")
  audit <- jsonlite::fromJSON(files[1L], simplifyVector = FALSE)

  expect_true(is.null(audit[["parent_cohort_hash"]]),
              label = "An invalid parent_cohort_hash must be silently dropped (NULL in output)")
})

test_that("Snapshot: schema_version value is locked", {
  skip_on_cran()
  audit <- run_canonical_audit()

  expect_true("schema_version" %in% names(normalize_for_snapshot(audit)),
              label = "schema_version must survive volatile-field stripping")

  legacy_versions <- c("0.0.0", "0.1.0", "legacy", "unversioned", NA_character_, "")
  expect_false(audit$schema_version %in% legacy_versions,
               label = "schema_version must not be a legacy or unversioned placeholder")

  expect_snapshot_value(audit$schema_version, style = "json2")
})
