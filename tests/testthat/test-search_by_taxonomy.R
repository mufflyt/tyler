library(testthat)
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
