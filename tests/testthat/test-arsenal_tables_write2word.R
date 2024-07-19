library(testthat)
library(arsenal)

# Setup and cleanup functions to manage the test environment
setup <- function() {
  test_dir <- tempdir()  # Creates a unique temporary directory for testing
  tables_dir <- file.path(test_dir, "tables")
  if (!dir.exists(tables_dir)) {
    dir.create(tables_dir, recursive = TRUE)  # Ensures the 'tables' directory exists
  }
  on.exit({
    setwd(old_dir)  # Restores the original working directory when the test ends
    unlink(tables_dir, recursive = TRUE)  # Removes the 'tables' directory
  }, add = TRUE)
  old_dir <- setwd(test_dir)  # Changes the working directory to the test directory
  list(test_dir = test_dir, tables_dir = tables_dir)
}

cleanup <- function(path) {
  unlink(path, recursive = TRUE)  # Deletes the directory created during setup
}

# Define the test cases
test_that("errors are thrown for invalid inputs", {
  expect_error(arsenal_tables_write2word(123, "filename"), "Error: 'object' must be a data frame object.")
  expect_error(arsenal_tables_write2word(data.frame(), 123), "Error: 'filename' must be a character string.")
})
