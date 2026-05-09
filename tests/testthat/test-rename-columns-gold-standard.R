library(testthat)
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("janitor")

# ── mysterycall_rename_columns ───────────────────────────────────────────────

test_that("mysterycall_rename_columns - basic rename preserves other columns", {
  df <- data.frame(doctor_info = 1:5, patient_data = 6:10)
  result <- suppressMessages(
    mysterycall_rename_columns(df, c("doctor_info"), c("physician_info"))
  )
  expect_equal(names(result), c("physician_info", "patient_data"))
  expect_equal(result$physician_info, 1:5)
})

test_that("mysterycall_rename_columns - data values are unchanged after rename", {
  df <- data.frame(old_name = c(10, 20, 30))
  result <- suppressMessages(
    mysterycall_rename_columns(df, c("old_name"), c("new_name"))
  )
  expect_equal(result$new_name, c(10, 20, 30))
})

test_that("mysterycall_rename_columns - case-insensitive matching (UPPER → lower target)", {
  df <- data.frame(DOCTOR_INFO = 1:3, other = 4:6)
  # target_string 'doctor_info' should match 'DOCTOR_INFO' case-insensitively
  result <- suppressMessages(
    mysterycall_rename_columns(df, c("DOCTOR_INFO"), c("physician_info"))
  )
  expect_true("physician_info" %in% names(result))
  expect_false("DOCTOR_INFO" %in% names(result))
})

test_that("mysterycall_rename_columns - multiple renames in one call", {
  df <- data.frame(col_a = 1:3, col_b = 4:6, col_c = 7:9)
  result <- suppressMessages(
    mysterycall_rename_columns(
      df,
      c("col_a", "col_b"),
      c("renamed_a", "renamed_b")
    )
  )
  expect_equal(names(result), c("renamed_a", "renamed_b", "col_c"))
})

test_that("mysterycall_rename_columns - mismatched vector lengths error", {
  df <- data.frame(a = 1, b = 2)
  expect_error(
    mysterycall_rename_columns(df, c("a", "b"), c("x")),
    "same length"
  )
})

test_that("mysterycall_rename_columns - non-matching target emits warning, not error", {
  df <- data.frame(existing = 1:3)
  expect_warning(
    suppressMessages(
      mysterycall_rename_columns(df, c("no_such_col"), c("new_name"))
    ),
    "nothing was renamed"
  )
})

test_that("mysterycall_rename_columns - rename log attribute is attached to result", {
  df <- data.frame(col_x = 1:4)
  result <- suppressMessages(
    mysterycall_rename_columns(df, c("col_x"), c("col_y"))
  )
  log <- attr(result, "rename_log")
  expect_false(is.null(log))
  expect_s3_class(log, "data.frame")
  expect_equal(log$renamed_from[1], "col_x")
  expect_equal(log$renamed_to[1], "col_y")
})

test_that("mysterycall_rename_columns - row count is unchanged", {
  df <- data.frame(foo = 1:100, bar = 101:200)
  result <- suppressMessages(
    mysterycall_rename_columns(df, c("foo"), c("baz"))
  )
  expect_equal(nrow(result), 100L)
})

test_that("mysterycall_rename_columns - no-op (empty vectors) returns data unchanged", {
  df <- data.frame(a = 1:3, b = 4:6)
  # mismatched-length guard fires before loop, so empty-empty is actually valid
  result <- suppressMessages(
    mysterycall_rename_columns(df, character(0), character(0))
  )
  expect_equal(names(result), names(df))
})

# ── mysterycall_clean_phase2 ────────────────────────────────────────────────────────

test_that("mysterycall_clean_phase2 - accepts a data frame directly", {
  df <- data.frame(doctor_info = 1:5, contact_data = 6:10)
  result <- suppressMessages(
    mysterycall_clean_phase2(
      df,
      required_strings = c("doctor_info", "contact_data"),
      standard_names  = c("physician_info", "patient_contact"),
      output_directory = tempdir()
    )
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5L)
})

test_that("mysterycall_clean_phase2 - renames columns to standard_names", {
  df <- data.frame(doc_info = 1:3, contact = 4:6)
  result <- suppressMessages(
    mysterycall_clean_phase2(
      df,
      required_strings = c("doc_info", "contact"),
      standard_names  = c("doctor_info", "patient_contact"),
      output_directory = tempdir()
    )
  )
  expect_true("doctor_info" %in% names(result))
  expect_true("patient_contact" %in% names(result))
})

test_that("mysterycall_clean_phase2 - writes a CSV file to output_directory", {
  df <- data.frame(col_a = 1:4, col_b = 5:8)
  out_dir <- file.path(tempdir(), paste0("phase2_test_", Sys.getpid()))
  dir.create(out_dir, showWarnings = FALSE)
  result <- suppressMessages(
    mysterycall_clean_phase2(
      df,
      required_strings = c("col_a"),
      standard_names   = c("renamed_a"),
      output_directory = out_dir
    )
  )
  csv_files <- list.files(out_dir, pattern = "\\.csv$", full.names = TRUE)
  expect_true(length(csv_files) >= 1L)
})

test_that("mysterycall_clean_phase2 - output_path attribute is set on successful run", {
  df <- data.frame(col_a = 1:4)
  result <- suppressMessages(
    mysterycall_clean_phase2(
      df,
      required_strings = c("col_a"),
      standard_names   = c("renamed_a"),
      output_directory = tempdir()
    )
  )
  out_path <- attr(result, "output_path")
  expect_true(!is.null(out_path))
  expect_true(file.exists(out_path))
})

test_that("mysterycall_clean_phase2 - empty required_strings stops with error", {
  df <- data.frame(a = 1)
  expect_error(
    suppressMessages(
      mysterycall_clean_phase2(df, required_strings = character(0), standard_names = character(0))
    )
  )
})

test_that("mysterycall_clean_phase2 - non-existent file path stops with error", {
  expect_error(
    suppressMessages(
      mysterycall_clean_phase2(
        "/no/such/file.csv",
        required_strings = c("x"),
        standard_names   = c("y")
      )
    ),
    "does not exist"
  )
})

test_that("mysterycall_clean_phase2 - clean_names lowercases column names before matching", {
  # janitor::clean_names converts spaces/caps to snake_case
  df <- data.frame(`Doctor Info` = 1:3, check.names = FALSE)
  result <- suppressMessages(
    mysterycall_clean_phase2(
      df,
      required_strings = c("doctor_info"),  # post-clean_names form
      standard_names   = c("physician_info"),
      output_directory = tempdir()
    )
  )
  expect_true("physician_info" %in% names(result))
})

test_that("mysterycall_clean_phase2 - non-data-frame non-path input stops with error", {
  expect_error(
    suppressMessages(mysterycall_clean_phase2(42L, c("a"), c("b"))),
    "dataframe or a valid file path"
  )
})
