library(testthat)
library(ggplot2)
library(emmeans)
library(dplyr)

message("Testing plot_and_save_emmeans function")

# Setup a basic model for testing
data(mtcars)
test_model <- lm(mpg ~ cyl + gear, data = mtcars)

# Test for input validation
test_that("input validation works correctly", {
  expect_error(plot_and_save_emmeans("not a model", "cyl", "cyl", tempdir()),
               "model_object must be a valid model class object")
  expect_error(plot_and_save_emmeans(test_model, list("cyl"), "cyl", tempdir()),
               "specs must be a single character string")
  expect_error(plot_and_save_emmeans(test_model, "cyl", 123, tempdir()),
               "variable_of_interest must be a single character string")
  expect_error(plot_and_save_emmeans(test_model, "cyl", "cyl", "nonexistent/directory"),
               "Output directory does not exist")
})

# Test for correct output structure
test_that("function returns correct output structure", {
  result <- plot_and_save_emmeans(test_model, "cyl", "cyl", tempdir())
  expect_true("data.frame" %in% class(result$data))
  expect_true("gg" %in% class(result$plot))
})

# Test for plot file creation
test_that("function creates a plot file", {
  output_dir <- tempdir()
  plot_and_save_emmeans(test_model, "cyl", "cyl", output_dir)
  plot_files <- list.files(output_dir, pattern = "png$", full.names = TRUE)
  expect_length(plot_files, 1)  # Expect exactly one plot file
  expect_true(file.exists(plot_files[1]))
})

# Optional: Test error handling from emmeans
test_that("function handles emmeans errors correctly", {
  # Modify the model or specs to trigger an error in emmeans
  bad_model <- lm(mpg ~ factor(cyl) * gear, data = mtcars)
  expect_error(plot_and_save_emmeans(bad_model, "cyl", "cyl", tempdir()), "Error in calculating emmeans")
})

# Cleanup test files if needed
unlink(list.files(tempdir(), pattern = "png$", full.names = TRUE), force = TRUE)
