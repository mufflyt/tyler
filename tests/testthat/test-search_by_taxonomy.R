library(testthat)
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("mockery")
library(dplyr)
library(mockery)

# Test cases
test_that("Handles no matching data", {
  cat("Running test: Handles no matching data\n")
  taxonomy <- "Nonexistent Taxonomy"

  # Stub the npi_search and npi_flatten functions
  stub(search_by_taxonomy, 'npi::npi_search', function(...) {
    cat("Mock npi_search called\n")
    NULL
  })

  stub(search_by_taxonomy, 'npi::npi_flatten', function(result) {
    cat("Mock npi_flatten called\n")
    result
  })

  result <- search_by_taxonomy(taxonomy)

  expect_equal(nrow(result), 0)
})

test_that("Filters and renames taxonomy results", {
  taxonomy <- "Gynecologic Oncology"
  mockery::stub(search_by_taxonomy, 'npi::npi_search', function(...) list(id = 1))
  mockery::stub(search_by_taxonomy, 'npi::npi_flatten', function(...) {
    data.frame(
      npi = c("1234567890", "9876543210"),
      basic_first_name = c("Ada", "Maria"),
      basic_last_name = c("Lovelace", "Curie"),
      basic_middle_name = c(NA, NA),
      basic_credential = c("MD", "PA"),
      addresses_country_name = c("United States", "United States"),
      taxonomies_desc = c(taxonomy, taxonomy),
      stringsAsFactors = FALSE
    )
  })

  result <- search_by_taxonomy(taxonomy)
  expect_equal(nrow(result), 1)
  expect_true("first_name" %in% names(result))
  expect_equal(result$first_name, "Ada")
})
