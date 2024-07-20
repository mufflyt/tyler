library(testthat)
library(dplyr)

test_that("remove_near_zero_var removes near-zero variance variables correctly", {
  # Create a sample data frame
  data_frame <- data.frame(
    A = rep(1, 10),
    B = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
    C = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )

  # Expected data frame after removal of near-zero variance variables
  expected_data_frame <- data_frame[, c("B", "C"), drop = FALSE]

  # Apply the function
  result_data_frame <- remove_near_zero_var(data_frame)

  # Check the structure and content of the resulting data frame
  expect_equal(nrow(result_data_frame), nrow(expected_data_frame))
  expect_equal(ncol(result_data_frame), ncol(expected_data_frame))
  expect_equal(names(result_data_frame), names(expected_data_frame))
})
