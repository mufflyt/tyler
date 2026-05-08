library(testthat)

# Deprecated functions must:
#   1. Emit a deprecation warning via .Deprecated()
#   2. Either stop (for removed functionality) or delegate correctly
# All behavior verified against source in R/deprecated.R.

test_that("test_and_process_isochrones - emits deprecation warning", {
  expect_warning(
    tryCatch(
      test_and_process_isochrones(data.frame()),
      error = function(e) NULL   # stop() after .Deprecated() — catch silently
    ),
    regexp = "deprecated|Deprecated",
    ignore.case = TRUE
  )
})

test_that("test_and_process_isochrones - stops with deprecation error message", {
  expect_error(
    suppressWarnings(test_and_process_isochrones(data.frame())),
    "deprecated"
  )
})

test_that("process_and_save_isochrones - emits deprecation warning", {
  expect_warning(
    tryCatch(
      process_and_save_isochrones(data.frame()),
      error = function(e) NULL
    ),
    regexp = "deprecated|Deprecated",
    ignore.case = TRUE
  )
})

test_that("process_and_save_isochrones - stops with deprecation error message", {
  expect_error(
    suppressWarnings(process_and_save_isochrones(data.frame())),
    "deprecated"
  )
})

test_that("process_and_save_isochrones - chunk_size argument is accepted silently", {
  # Even with chunk_size, the function should deprecate-warn then stop
  expect_error(
    suppressWarnings(process_and_save_isochrones(data.frame(), chunk_size = 10)),
    "deprecated"
  )
})

test_that("search_npi - emits deprecation warning when called", {
  skip_on_cran()
  # search_npi delegates to search_and_process_npi, which makes API calls.
  # Test only that the deprecation warning fires before any downstream work.
  df <- data.frame(first = "Jane", last = "Smith")
  # The deprecation warning fires immediately; the subsequent error (missing NPI
  # or network) is irrelevant — we just need the warning.
  expect_warning(
    tryCatch(search_npi(df), error = function(e) NULL),
    regexp = "deprecated|search_and_process_npi",
    ignore.case = TRUE
  )
})

test_that("search_npi - stops if input lacks first/last columns", {
  df <- data.frame(npi = "1234567890")   # missing first and last
  expect_error(
    suppressWarnings(search_npi(df)),
    "first|last"
  )
})

test_that("search_npi - stops on non-data-frame non-path input", {
  expect_error(
    suppressWarnings(search_npi(42L)),
    "data frame or a file path"
  )
})

test_that("test_and_process_isochrones - error message names the replacement function", {
  err <- tryCatch(
    suppressWarnings(test_and_process_isochrones(data.frame())),
    error = function(e) e$message
  )
  expect_true(grepl("create_isochrones_for_dataframe", err))
})

test_that("process_and_save_isochrones - error message names the replacement function", {
  err <- tryCatch(
    suppressWarnings(process_and_save_isochrones(data.frame())),
    error = function(e) e$message
  )
  expect_true(grepl("create_isochrones_for_dataframe", err))
})
