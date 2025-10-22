library(testthat)
library(mockery)

sample_data <- data.frame(
  first = c("John", "Jane", "Alex"),
  last = c("Doe", "Smith", "Brown"),
  stringsAsFactors = FALSE
)

mock_npi_search <- function(first_name, last_name, enumeration_type, country_code, limit) {
  if (first_name == "John" && last_name == "Doe") {
    list(
      basic = data.frame(first_name = "John", last_name = "Doe", basic_credential = "MD", stringsAsFactors = FALSE),
      taxonomies = data.frame(taxonomies_desc = c("Family Medicine", "Internal Medicine"), stringsAsFactors = FALSE)
    )
  } else if (first_name == "Jane" && last_name == "Smith") {
    list(
      basic = data.frame(first_name = "Jane", last_name = "Smith", basic_credential = "DO", stringsAsFactors = FALSE),
      taxonomies = data.frame(taxonomies_desc = c("Pediatrics"), stringsAsFactors = FALSE)
    )
  } else if (first_name == "Alex" && last_name == "Brown") {
    list(
      basic = data.frame(first_name = "Alex", last_name = "Brown", basic_credential = "MD", stringsAsFactors = FALSE),
      taxonomies = data.frame(taxonomies_desc = c("Surgery"), stringsAsFactors = FALSE)
    )
  } else {
    list(
      basic = data.frame(first_name = first_name, last_name = last_name, basic_credential = "MD", stringsAsFactors = FALSE),
      taxonomies = data.frame(taxonomies_desc = c("Unknown"), stringsAsFactors = FALSE)
    )
  }
}

mock_npi_flatten <- function(npi_obj, cols) {
  if (is.null(npi_obj)) {
    return(NULL)
  }
  cbind(npi_obj$basic, npi_obj$taxonomies)
}

create_temp_dir <- function() {
  dir <- tempfile("npi-test-")
  unlink(dir, recursive = TRUE)
  dir
}

expect_quiet <- function(expr) {
  old <- options(tyler.quiet = TRUE)
  on.exit(options(old), add = TRUE)
  force(expr)
}

# 1
test_that("Processes data frame input correctly", {
  stub(search_and_process_npi, 'npi::npi_search', mock_npi_search)
  stub(search_and_process_npi, 'npi::npi_flatten', mock_npi_flatten)
  result <- expect_quiet(search_and_process_npi(sample_data, dest_dir = tempdir(), quiet = TRUE))
  expect_true(nrow(result) >= 3)
  expect_true("taxonomies_desc" %in% names(result))
})

# 2
test_that("Handles empty input data frame", {
  empty_data <- data.frame(first = character(), last = character(), stringsAsFactors = FALSE)
  result <- expect_quiet(search_and_process_npi(empty_data, quiet = TRUE))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# 3
test_that("Handles invalid NPIs gracefully", {
  stub(search_and_process_npi, 'npi::npi_search', function(...) NULL)
  stub(search_and_process_npi, 'npi::npi_flatten', function(...) NULL)
  result <- expect_quiet(search_and_process_npi(sample_data[1, , drop = FALSE], quiet = TRUE))
  expect_equal(nrow(result), 0)
})

# 4
test_that("Processes various names correctly", {
  stub(search_and_process_npi, 'npi::npi_search', mock_npi_search)
  stub(search_and_process_npi, 'npi::npi_flatten', mock_npi_flatten)
  for (i in seq_len(nrow(sample_data))) {
    result <- expect_quiet(search_and_process_npi(sample_data[i, , drop = FALSE], quiet = TRUE))
    expect_true(nrow(result) >= 1)
  }
})

# 5
test_that("Handles large datasets efficiently", {
  stub(search_and_process_npi, 'npi::npi_search', mock_npi_search)
  stub(search_and_process_npi, 'npi::npi_flatten', mock_npi_flatten)
  large_data <- sample_data[rep(seq_len(nrow(sample_data)), times = 50), ]
  result <- expect_quiet(search_and_process_npi(large_data, quiet = TRUE))
  expect_true(nrow(result) >= nrow(sample_data))
})

# 6
test_that("Requires data frame input", {
  expect_error(search_and_process_npi(list(a = 1)), "data frame")
})

# 7
test_that("Requires first and last columns", {
  expect_error(search_and_process_npi(data.frame(first = "John")), "must contain 'first' and 'last'")
})

# 8
test_that("Validates limit parameter", {
  expect_error(search_and_process_npi(sample_data, limit = 0), "positive")
})

# 9
test_that("Validates save_chunk_size", {
  expect_error(search_and_process_npi(sample_data, save_chunk_size = 0), "positive")
})

# 10
test_that("Creates destination directory when missing", {
  stub(search_and_process_npi, 'npi::npi_search', mock_npi_search)
  stub(search_and_process_npi, 'npi::npi_flatten', mock_npi_flatten)
  temp_dir <- create_temp_dir()
  result <- expect_quiet(search_and_process_npi(sample_data[1, , drop = FALSE], dest_dir = temp_dir, quiet = TRUE))
  expect_true(dir.exists(temp_dir))
  expect_s3_class(result, "data.frame")
})

# 11
test_that("Filters by credentials when available", {
  stub(search_and_process_npi, 'npi::npi_search', mock_npi_search)
  stub(search_and_process_npi, 'npi::npi_flatten', mock_npi_flatten)
  res <- expect_quiet(search_and_process_npi(sample_data[1, , drop = FALSE], filter_credentials = "DO", quiet = TRUE))
  expect_equal(nrow(res), 0)
})

# 12
test_that("Errors when destination cannot be created", {
  bad_dir <- tempfile("npi-bad")
  file.create(bad_dir)
  expect_true(file.exists(bad_dir))
  expect_error(search_and_process_npi(sample_data[1, , drop = FALSE], dest_dir = bad_dir), "Unable to create")
})
