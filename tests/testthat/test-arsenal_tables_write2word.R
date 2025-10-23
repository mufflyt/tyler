library(testthat)
testthat::skip_if_not_installed("arsenal")
library(arsenal)

# Define the test cases
test_that("errors are thrown for invalid inputs", {
  expect_error(arsenal_tables_write2word(123, "filename"), "Error: 'object' must be a data frame object.")
  expect_error(arsenal_tables_write2word(data.frame(), 123), "Error: 'filename' must be a character string.")
})
