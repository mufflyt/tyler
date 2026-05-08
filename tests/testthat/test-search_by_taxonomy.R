library(testthat)
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("npi")
library(dplyr)

test_that("Handles no matching data", {
  with_mocked_bindings(
    npi_search = function(...) NULL,
    npi_flatten = function(result, ...) result,
    .package = "npi",
    code = {
      result <- search_by_taxonomy("Nonexistent Taxonomy", write_snapshot = FALSE, notify = FALSE)
      expect_equal(nrow(result), 0)
    }
  )
})

test_that("Filters and renames taxonomy results", {
  taxonomy <- "Gynecologic Oncology"
  with_mocked_bindings(
    npi_search = function(...) list(id = 1),
    npi_flatten = function(...) data.frame(
      npi = c("1922051358", "1750344388"),
      basic_first_name = c("Ada", "Maria"),
      basic_last_name = c("Lovelace", "Curie"),
      basic_middle_name = c(NA, NA),
      basic_credential = c("MD", "PA"),
      addresses_country_name = c("United States", "United States"),
      taxonomies_desc = c(taxonomy, taxonomy),
      stringsAsFactors = FALSE
    ),
    .package = "npi",
    code = {
      result <- search_by_taxonomy(taxonomy, write_snapshot = FALSE, notify = FALSE)
      expect_true(nrow(result) >= 1)
      expect_true("first_name" %in% names(result))
      expect_true("Ada" %in% result$first_name)
    }
  )
})
