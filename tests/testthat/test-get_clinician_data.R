library(testthat)
testthat::skip_if_not_installed("readr")
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("provider")
testthat::skip_if_not_installed("purrr")
testthat::skip_if_not_installed("tidyr")
library(readr)
library(dplyr)

create_temp_csv <- function(data, file_name = "temp_npi_data.csv") {
  temp_file <- file.path(tempdir(), file_name)
  write_csv(data, temp_file)
  temp_file
}

sample_data_valid <- data.frame(
  npi = c(1234567890, 2345678901, 3456789012, 4567890123, 5678901234),
  stringsAsFactors = FALSE
)
sample_data_invalid <- data.frame(
  npi = c("12345", "23456789012", "ABCDE12345", NA, ""),
  stringsAsFactors = FALSE
)
sample_data_mixed <- data.frame(
  npi = c(1234567890, "23456789012", 3456789012, NA, ""),
  stringsAsFactors = FALSE
)

mock_clinicians <- function(npi, ...) {
  if (npi == 1234567890) {
    data.frame(name = "John Doe", specialty = "Cardiology")
  } else if (npi == 3456789012) {
    data.frame(name = "Jane Smith", specialty = "Neurology")
  } else {
    NULL
  }
}

mock_validate_and_remove_invalid_npi <- function(df, ...) {
  df %>%
    dplyr::filter(.data$npi %in% c(1234567890, 3456789012, "1234567890", "3456789012")) %>%
    dplyr::mutate(npi = as.character(.data$npi), npi_is_valid = TRUE)
}

run_with_mocks <- function(expr) {
  with_mocked_bindings(
    clinicians = mock_clinicians,
    .package = "provider",
    code = with_mocked_bindings(
      tyler_validate_npi = mock_validate_and_remove_invalid_npi,
      .package = "tyler",
      code = expr
    )
  )
}

test_that("Retrieves clinician data for valid NPIs", {
  temp_csv <- create_temp_csv(sample_data_valid)
  run_with_mocks({
    result <- tyler_get_clinician_data(temp_csv)
    expect_equal(nrow(result), 2)
    expect_true("name" %in% colnames(result))
    expect_true("specialty" %in% colnames(result))
    expect_equal(result$name, c("John Doe", "Jane Smith"))
  })
})

test_that("Handles invalid NPIs", {
  temp_csv <- create_temp_csv(sample_data_invalid)
  run_with_mocks({
    result <- tyler_get_clinician_data(temp_csv)
    expect_equal(nrow(result), 0)
  })
})

test_that("Handles mixed valid and invalid NPIs", {
  temp_csv <- create_temp_csv(sample_data_mixed)
  run_with_mocks({
    result <- tyler_get_clinician_data(temp_csv)
    expect_equal(nrow(result), 2)
    expect_true("name" %in% colnames(result))
    expect_true("specialty" %in% colnames(result))
    expect_equal(result$name, c("John Doe", "Jane Smith"))
  })
})

test_that("Handles dataframe input correctly", {
  run_with_mocks({
    result <- tyler_get_clinician_data(sample_data_mixed)
    expect_equal(nrow(result), 2)
    expect_true("name" %in% colnames(result))
    expect_true("specialty" %in% colnames(result))
    expect_equal(result$name, c("John Doe", "Jane Smith"))
  })
})
