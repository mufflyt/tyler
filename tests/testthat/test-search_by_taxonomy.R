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
      result <- tyler_search_taxonomy("Nonexistent Taxonomy", write_snapshot = FALSE, notify = FALSE)
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
      result <- tyler_search_taxonomy(taxonomy, write_snapshot = FALSE, notify = FALSE)
      expect_true(nrow(result) >= 1)
      expect_true("first_name" %in% names(result))
      expect_true("Ada" %in% result$first_name)
    }
  )
})

test_that("Handles saboteur payload missing optional columns", {
  taxonomy <- "Gynecologic Oncology"
  with_mocked_bindings(
    npi_search = function(...) list(id = 1),
    npi_flatten = function(...) {
      data.frame(
        npi = c("1234567890"),
        basic_first_name = c("Ada"),
        basic_last_name = c("Lovelace"),
        addresses_country_name = c("United States"),
        taxonomies_desc = c(taxonomy),
        stringsAsFactors = FALSE
      )
    },
    .package = "npi",
    code = {
      result <- tyler_search_taxonomy(taxonomy, write_snapshot = FALSE, notify = FALSE)
      expect_equal(nrow(result), 1)
      expect_true("first_name" %in% names(result))
      expect_true("middle_name" %in% names(result))
    }
  )
})

test_that("Handles saboteur taxonomy strings containing regex metacharacters", {
  taxonomy <- "OB/GYN (MFM)+"
  with_mocked_bindings(
    npi_search = function(...) list(id = 1),
    npi_flatten = function(...) {
      data.frame(
        npi = c("1234567890"),
        basic_first_name = c("Ada"),
        basic_last_name = c("Lovelace"),
        basic_middle_name = c(NA_character_),
        basic_credential = c("MD"),
        addresses_country_name = c("United States"),
        taxonomies_desc = c("ob/gyn (mfm)+"),
        stringsAsFactors = FALSE
      )
    },
    .package = "npi",
    code = {
      result <- tyler_search_taxonomy(taxonomy, write_snapshot = FALSE, notify = FALSE)
      expect_equal(nrow(result), 1)
      expect_equal(result$search_term, taxonomy)
    }
  )
})
