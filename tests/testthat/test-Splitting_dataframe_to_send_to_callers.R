library(testthat)
library(openxlsx)
library(dplyr)
library(fs)

# Test if the function stops when provided with an invalid path
test_that("Invalid file path handling", {
  expect_error(
    split_and_save(data_or_path = "nonexistent/path/data.csv", output_directory = tempdir(), lab_assistant_names = c("Alice", "Bob")),
    "Error reading the input file"
  )
})

# Test if the function stops when only one lab assistant name is provided
test_that("Single lab assistant name handling", {
  data <- data.frame(
    for_redcap = 1:4,
    id = 1:4,
    stringsAsFactors = FALSE
  )
  expect_error(
    split_and_save(data_or_path = data, output_directory = tempdir(), lab_assistant_names = c("Alice")),
    "Please provide at least two lab assistant names for the splits."
  )
})

# Test if the function stops when required columns are missing
test_that("Missing required columns handling", {
  data <- data.frame(
    not_for_redcap = 1:4,
    not_id = 1:4,
    stringsAsFactors = FALSE
  )
  expect_error(
    split_and_save(data_or_path = data, output_directory = tempdir(), lab_assistant_names = c("Alice", "Bob")),
    "The input data is missing the following columns: for_redcap, id"
  )
})

# Test the function with a minimal correct dataframe and check outputs
test_that("Proper output file creation", {
  # Create a temporary directory for output
  output_dir <- tempdir()

  # Ensure directory is empty
  unlink(list.files(output_dir, full.names = TRUE), recursive = TRUE)

  # Create a sample dataframe
  data <- data.frame(
    for_redcap = 1:6,
    id = 1:6,
    stringsAsFactors = FALSE
  )

  # Test with two lab assistant names
  lab_assistant_names <- c("Alice", "Bob")

  # Run function
  suppressMessages(suppressWarnings({
    split_and_save(
      data_or_path = data,
      output_directory = output_dir,
      lab_assistant_names = lab_assistant_names
    )
  }))

  # Check if the expected number of files is created
  expected_files <- length(lab_assistant_names) + 1  # Each lab assistant plus one complete file
  output_files <- list.files(output_dir, pattern = "\\.xlsx$", full.names = TRUE)

  expect_equal(length(output_files), expected_files)

  # Check if the files include the correct number of rows
  for (file in output_files) {
    sheet_data <- openxlsx::read.xlsx(file)
    expect_true(nrow(sheet_data) >= 1)
  }
})

# Test correct assignment of lab assistants
test_that("Correct assignment of lab assistants", {
  # Setup
  output_dir <- tempdir()

  # Ensure directory is empty
  unlink(list.files(output_dir, full.names = TRUE), recursive = TRUE)

  # Sample data simulating multiple entries
  data <- data.frame(
    for_redcap = 1:6,
    id = 1:6,
    stringsAsFactors = FALSE
  )

  lab_assistant_names <- c("Alice", "Bob")

  # Run function
  suppressMessages(suppressWarnings({
    split_and_save(
      data_or_path = data,
      output_directory = output_dir,
      lab_assistant_names = lab_assistant_names
    )
  }))

  # Check file creation and contents
  output_files <- list.files(output_dir, pattern = "\\.xlsx$", full.names = TRUE)
  expect_length(output_files, length(lab_assistant_names) + 1) # +1 for the complete file

  for (file in output_files) {
    if (!grepl("complete_non_split_version", file)) {
      sheet_data <- openxlsx::read.xlsx(file)
      # Extract the lab assistant name from the file name
      lab_name <- gsub("_.*$", "", basename(file))

      # Log the lab assistant name and the contents of the sheet_data
      cat("Checking file:", file, "\n")
      cat("Expected lab assistant:", lab_name, "\n")
      cat("Lab assistants assigned in file:", unique(sheet_data$lab_assistant_assigned), "\n")

      # Test if all entries are assigned to the correct lab assistant
      expect_true(all(sheet_data$lab_assistant_assigned == lab_name),
                  info = paste("All entries in", file, "should be assigned to", lab_name))
    }
  }
})
