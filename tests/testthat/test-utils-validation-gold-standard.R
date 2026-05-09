library(testthat)

# Internal validators: validate_dataframe and validate_required_columns.
# These are called by many exported functions, so their behavior is a
# load-bearing contract.  All expected values manually verified.

# ── validate_dataframe ────────────────────────────────────────────────────────

test_that("validate_dataframe - accepts a regular data frame silently", {
  df <- data.frame(a = 1:5, b = letters[1:5])
  expect_no_error(mysterycall:::validate_dataframe(df, name = "test"))
})

test_that("validate_dataframe - returns the input invisibly", {
  df <- data.frame(x = 1:3)
  result <- mysterycall:::validate_dataframe(df, name = "test")
  expect_identical(result, df)
})

test_that("validate_dataframe - NULL input with allow_null=FALSE stops", {
  expect_error(mysterycall:::validate_dataframe(NULL, name = "roster"), "must be a data frame")
})

test_that("validate_dataframe - NULL input with allow_null=TRUE passes", {
  expect_no_error(mysterycall:::validate_dataframe(NULL, allow_null = TRUE))
})

test_that("validate_dataframe - non-data-frame stops with class info", {
  expect_error(mysterycall:::validate_dataframe(list(a = 1), name = "x"), "data frame")
  expect_error(mysterycall:::validate_dataframe(1:10, name = "x"), "data frame")
  expect_error(mysterycall:::validate_dataframe("string", name = "x"), "data frame")
})

test_that("validate_dataframe - zero-row data frame is accepted by default", {
  df <- data.frame(x = integer(0))
  expect_no_error(mysterycall:::validate_dataframe(df, allow_zero_rows = TRUE))
})

test_that("validate_dataframe - zero-row data frame with allow_zero_rows=FALSE stops", {
  df <- data.frame(x = integer(0))
  expect_error(
    mysterycall:::validate_dataframe(df, name = "empty", allow_zero_rows = FALSE),
    "at least one row"
  )
})

test_that("validate_dataframe - tibble is accepted (inherits data.frame)", {
  skip_if_not_installed("tibble")
  tbl <- tibble::tibble(a = 1:3)
  expect_no_error(mysterycall:::validate_dataframe(tbl, name = "tbl"))
})

test_that("validate_dataframe - error message includes the supplied name", {
  expect_error(
    mysterycall:::validate_dataframe(NULL, name = "my_roster"),
    "my_roster"
  )
})

test_that("validate_dataframe - works with a 1-column data frame", {
  df <- data.frame(npi = c("1234567890"))
  expect_identical(mysterycall:::validate_dataframe(df, name = "npi_df"), df)
})

# ── validate_required_columns ─────────────────────────────────────────────────

test_that("validate_required_columns - all required columns present passes", {
  df <- data.frame(npi = 1, name = "a", state = "CO")
  expect_no_error(
    mysterycall:::validate_required_columns(df, required = c("npi", "name", "state"))
  )
})

test_that("validate_required_columns - returns the input data frame", {
  df <- data.frame(a = 1, b = 2)
  result <- mysterycall:::validate_required_columns(df, required = c("a", "b"))
  expect_identical(result, df)
})

test_that("validate_required_columns - missing column stops with column name", {
  df <- data.frame(npi = 1)
  expect_error(
    mysterycall:::validate_required_columns(df, required = c("npi", "state")),
    "state"
  )
})

test_that("validate_required_columns - multiple missing columns all listed in error", {
  df <- data.frame(x = 1)
  err <- tryCatch(
    mysterycall:::validate_required_columns(df, required = c("a", "b", "c")),
    error = function(e) e$message
  )
  expect_true(grepl("a", err))
  expect_true(grepl("b", err))
  expect_true(grepl("c", err))
})

test_that("validate_required_columns - NULL required returns invisibly", {
  df <- data.frame(x = 1)
  expect_no_error(mysterycall:::validate_required_columns(df, required = NULL))
})

test_that("validate_required_columns - empty required vector returns invisibly", {
  df <- data.frame(x = 1)
  expect_no_error(mysterycall:::validate_required_columns(df, required = character(0)))
})

test_that("validate_required_columns - error message names the data object", {
  df <- data.frame(a = 1)
  expect_error(
    mysterycall:::validate_required_columns(df, required = "missing_col", name = "physician_roster"),
    "physician_roster"
  )
})

test_that("validate_required_columns - case-sensitive: 'NPI' != 'npi'", {
  df <- data.frame(NPI = 1)
  expect_error(
    mysterycall:::validate_required_columns(df, required = "npi"),
    "npi"
  )
})

test_that("validate_required_columns - extra unrequired columns are ignored", {
  df <- data.frame(a = 1, b = 2, c = 3, d = 4)
  expect_no_error(
    mysterycall:::validate_required_columns(df, required = c("a", "b"))
  )
})


test_that("validate_required_columns - includes available columns in error", {
  df <- data.frame(npi = 1, state = "CO", specialty = "OBGYN")
  err <- tryCatch(
    mysterycall:::validate_required_columns(df, required = c("npi", "zip_code")),
    error = function(e) e$message
  )

  expect_match(err, "Available columns")
  expect_match(err, "npi")
  expect_match(err, "state")
  expect_match(err, "specialty")
})

test_that("validate_required_columns - typo suggestions are included when close match exists", {
  df <- data.frame(provider_npi = 1, provider_name = "A")
  err <- tryCatch(
    mysterycall:::validate_required_columns(df, required = "provider_np"),
    error = function(e) e$message
  )

  expect_match(err, "did you mean `provider_npi`\\?", perl = TRUE)
})
