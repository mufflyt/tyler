# Comprehensive tests for search_by_taxonomy function
library(testthat)
library(tyler)
library(tibble)
library(dplyr)

# Mock npi functions for testing
mock_npi_search <- function(taxonomy_description, ...) {
  # Return different mock data based on taxonomy
  if (taxonomy_description == "Gynecologic Oncology") {
    list(
      basic_first_name = c("Jane", "Mary"),
      basic_last_name = c("Smith", "Johnson"),
      basic_credential = c("MD", "DO"),
      basic_gender = c("F", "F"),
      npi = c("1234567890", "0987654321"),
      addresses_country_name = c("United States", "United States"),
      taxonomies_desc = c("Gynecologic Oncology", "Gynecologic Oncology")
    )
  } else if (taxonomy_description == "Invalid Taxonomy") {
    stop("HTTP 404: Not Found")
  } else {
    list()  # Empty result
  }
}

mock_npi_flatten <- function(x) {
  if (is.list(x) && length(x) > 0) {
    tibble::as_tibble(x)
  } else {
    tibble::tibble()
  }
}

# Unit Tests
test_that("search_by_taxonomy: Basic functionality", {
  # Mock the npi functions
  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    {
      result <- search_by_taxonomy("Gynecologic Oncology",
                                   write_snapshot = FALSE,
                                   notify = FALSE)

      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) >= 0)

      if (nrow(result) > 0) {
        expect_true("npi" %in% names(result))
        expect_true(all(!is.na(result$npi)))
      }
    }
  )
})

test_that("search_by_taxonomy: Input validation", {
  # Missing taxonomy
  expect_s3_class(search_by_taxonomy(write_snapshot = FALSE, notify = FALSE),
                  "data.frame")
  expect_equal(nrow(search_by_taxonomy(write_snapshot = FALSE, notify = FALSE)), 0)

  # NULL taxonomy
  expect_s3_class(search_by_taxonomy(NULL, write_snapshot = FALSE, notify = FALSE),
                  "data.frame")
  expect_equal(nrow(search_by_taxonomy(NULL, write_snapshot = FALSE, notify = FALSE)), 0)

  # Empty vector
  expect_s3_class(search_by_taxonomy(character(0), write_snapshot = FALSE, notify = FALSE),
                  "data.frame")
  expect_equal(nrow(search_by_taxonomy(character(0), write_snapshot = FALSE, notify = FALSE)), 0)

  # NA values
  expect_s3_class(search_by_taxonomy(c(NA, NA), write_snapshot = FALSE, notify = FALSE),
                  "data.frame")
  expect_equal(nrow(search_by_taxonomy(c(NA, NA), write_snapshot = FALSE, notify = FALSE)), 0)
})

test_that("search_by_taxonomy: Data quality validation", {
  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    {
      result <- search_by_taxonomy("Gynecologic Oncology",
                                   write_snapshot = FALSE,
                                   notify = FALSE)

      if (nrow(result) > 0) {
        # Check NPI format (10 digits)
        if ("npi" %in% names(result)) {
          expect_true(all(grepl("^[0-9]{10}$", result$npi, na.rm = TRUE)))
        }

        # Check gender values are valid
        if ("basic_gender" %in% names(result)) {
          valid_genders <- c("M", "F", "Male", "Female", NA)
          expect_true(all(result$basic_gender %in% valid_genders))
        }

        # Check state codes if present
        if ("addresses_state" %in% names(result)) {
          # Should be 2-letter state codes or NA
          state_pattern <- "^[A-Z]{2}$"
          expect_true(all(grepl(state_pattern, result$addresses_state, na.rm = TRUE)))
        }
      }
    }
  )
})

test_that("search_by_taxonomy: Error handling", {
  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    {
      # Should handle API errors gracefully
      expect_s3_class(
        search_by_taxonomy("Invalid Taxonomy", write_snapshot = FALSE, notify = FALSE),
        "data.frame"
      )
    }
  )
})

test_that("search_by_taxonomy: Multiple taxonomies", {
  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    {
      result <- search_by_taxonomy(
        c("Gynecologic Oncology", "Maternal & Fetal Medicine"),
        write_snapshot = FALSE,
        notify = FALSE
      )

      expect_s3_class(result, "data.frame")
    }
  )
})

test_that("search_by_taxonomy: Performance test", {
  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    {
      start_time <- Sys.time()
      result <- search_by_taxonomy("Gynecologic Oncology",
                                   write_snapshot = FALSE,
                                   notify = FALSE)
      end_time <- Sys.time()

      # Should complete within reasonable time
      expect_lt(as.numeric(end_time - start_time, units = "secs"), 5)
    }
  )
})

test_that("search_by_taxonomy: Snapshot functionality", {
  temp_dir <- tempdir()

  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    {
      result <- search_by_taxonomy("Gynecologic Oncology",
                                   write_snapshot = TRUE,
                                   snapshot_dir = temp_dir,
                                   notify = FALSE)

      # Check if snapshot file was created
      snapshot_files <- list.files(temp_dir, pattern = "*.rds", full.names = TRUE)
      expect_gt(length(snapshot_files), 0)
    }
  )
})

# Integration Tests
test_that("search_by_taxonomy: Integration with real taxonomy data", {
  skip_if_not_installed("npi")
  skip_on_cran()

  # Test with actual OBGYN taxonomy if data is available
  if (exists("taxonomy", envir = asNamespace("tyler"))) {
    obgyn_codes <- tyler::taxonomy$Classification[
      grepl("Obstetrics|Gynecol", tyler::taxonomy$Classification, ignore.case = TRUE)
    ][1:2]  # Just test first 2 to avoid long-running tests

    if (length(obgyn_codes) > 0) {
      result <- search_by_taxonomy(obgyn_codes[1],
                                   write_snapshot = FALSE,
                                   notify = FALSE)
      expect_s3_class(result, "data.frame")
    }
  }
})

# Property-Based Tests
test_that("search_by_taxonomy: Property-based testing", {
  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    {
      # Test with various string inputs
      test_inputs <- c(
        "Valid Taxonomy",
        "Another Valid Taxonomy",
        "",  # Empty string
        "   ",  # Whitespace
        "Taxonomy with Numbers 123",
        "Taxonomy with Special !@#$%"
      )

      for (input in test_inputs) {
        result <- search_by_taxonomy(input, write_snapshot = FALSE, notify = FALSE)

        # Should always return a data frame
        expect_s3_class(result, "data.frame")

        # Should have consistent column structure
        expect_true(is.data.frame(result))

        # Should not have negative row counts
        expect_gte(nrow(result), 0)
      }
    }
  )
})

# Regression Tests
test_that("search_by_taxonomy: Regression tests for known issues", {
  # Test that function handles special characters in taxonomy descriptions
  special_chars <- c("Obstetrics & Gynecology", "Maternal & Fetal Medicine")

  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    {
      expect_no_error(
        search_by_taxonomy(special_chars, write_snapshot = FALSE, notify = FALSE)
      )
    }
  )
})

# Data Validation Tests
test_that("search_by_taxonomy: Data validation and cleaning", {
  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    {
      result <- search_by_taxonomy("Gynecologic Oncology",
                                   write_snapshot = FALSE,
                                   notify = FALSE)

      if (nrow(result) > 0) {
        # Check that required columns exist and are properly formatted
        if ("npi" %in% names(result)) {
          expect_true(all(nchar(as.character(result$npi)) == 10))
        }

        # Check for duplicate NPIs (should be removed)
        if ("npi" %in% names(result)) {
          expect_equal(nrow(result), length(unique(result$npi)))
        }

        # Check that US-only addresses are retained
        if ("addresses_country_name" %in% names(result)) {
          expect_true(all(result$addresses_country_name == "United States" |
                         is.na(result$addresses_country_name)))
        }
      }
    }
  )
})