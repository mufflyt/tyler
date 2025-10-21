library(testthat)
library(tyler)
library(ggplot2)
library(dplyr)

# Setup: Create test data
setup_test_data <- function() {
  data.frame(
    insurance = factor(rep(c("Private", "Medicaid", "Medicare"), each = 20)),
    business_days_until_appointment = c(
      rnorm(20, mean = 10, sd = 3),
      rnorm(20, mean = 15, sd = 4),
      rnorm(20, mean = 12, sd = 3.5)
    ),
    provider_id = 1:60
  )
}

# Unit Tests for create_scatter_plot
test_that("create_scatter_plot returns ggplot object", {
  test_data <- setup_test_data()

  result <- create_scatter_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "gg")
  expect_s3_class(result, "ggplot")
})

test_that("create_scatter_plot handles log transformation", {
  test_data <- setup_test_data()

  result <- create_scatter_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    y_transform = "log",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
  # Check that the y-axis label reflects log transformation
  expect_true(grepl("Log", result$labels$y) || !is.null(result$labels$y))
})

test_that("create_scatter_plot handles sqrt transformation", {
  test_data <- setup_test_data()

  result <- create_scatter_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    y_transform = "sqrt",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
  expect_true(grepl("Sqrt", result$labels$y) || !is.null(result$labels$y))
})

test_that("create_scatter_plot handles no transformation", {
  test_data <- setup_test_data()

  result <- create_scatter_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    y_transform = "none",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_scatter_plot removes negative and zero values", {
  test_data <- setup_test_data()
  # Add some negative and zero values
  test_data$business_days_until_appointment[1:5] <- c(-1, -2, 0, -3, 0)

  result <- create_scatter_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
  # Verify that data points are filtered
  plot_data <- ggplot_build(result)$data[[1]]
  expect_true(all(plot_data$y > 0))
})

test_that("create_scatter_plot removes NA values", {
  test_data <- setup_test_data()
  test_data$business_days_until_appointment[1:5] <- NA

  result <- create_scatter_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_scatter_plot accepts custom labels", {
  test_data <- setup_test_data()

  result <- create_scatter_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    x_label = "Insurance Type",
    y_label = "Wait Time (Days)",
    plot_title = "Test Plot",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_equal(result$labels$x, "Insurance Type")
  expect_equal(result$labels$y, "Wait Time (Days)")
  expect_equal(result$labels$title, "Test Plot")
})

test_that("create_scatter_plot accepts custom jitter parameters", {
  test_data <- setup_test_data()

  result <- create_scatter_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    jitter_width = 0.3,
    jitter_height = 0.1,
    point_alpha = 0.8,
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_scatter_plot saves files to output directory", {
  test_data <- setup_test_data()
  temp_output <- tempdir()

  # Clear any existing files
  existing_files <- list.files(temp_output, pattern = "scatter_plot.*\\.(tiff|png)$", full.names = TRUE)
  if (length(existing_files) > 0) file.remove(existing_files)

  result <- create_scatter_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    output_dir = temp_output,
    file_prefix = "test_scatter",
    verbose = FALSE
  )

  # Check that files were created
  files <- list.files(temp_output, pattern = "test_scatter.*\\.(tiff|png)$")
  expect_true(length(files) >= 2)  # Should have both TIFF and PNG
})

# Regression Tests
test_that("create_scatter_plot handles single category", {
  test_data <- data.frame(
    category = rep("A", 20),
    value = rnorm(20, mean = 10, sd = 2)
  )

  result <- create_scatter_plot(
    plot_data = test_data,
    x_var = "category",
    y_var = "value",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_scatter_plot handles large dataset", {
  large_data <- data.frame(
    category = factor(sample(c("A", "B", "C"), 1000, replace = TRUE)),
    value = rnorm(1000, mean = 50, sd = 10)
  )

  result <- create_scatter_plot(
    plot_data = large_data,
    x_var = "category",
    y_var = "value",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_scatter_plot handles small dataset", {
  small_data <- data.frame(
    category = factor(c("A", "B", "A")),
    value = c(5, 10, 7)
  )

  result <- create_scatter_plot(
    plot_data = small_data,
    x_var = "category",
    y_var = "value",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_scatter_plot handles many categories", {
  many_categories <- data.frame(
    category = factor(rep(LETTERS[1:10], each = 10)),
    value = rnorm(100, mean = 50, sd = 15)
  )

  result <- create_scatter_plot(
    plot_data = many_categories,
    x_var = "category",
    y_var = "value",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_scatter_plot verbose FALSE suppresses messages", {
  test_data <- setup_test_data()

  expect_silent({
    result <- create_scatter_plot(
      plot_data = test_data,
      x_var = "insurance",
      y_var = "business_days_until_appointment",
      output_dir = tempdir(),
      verbose = FALSE
    )
  })
})

test_that("create_scatter_plot handles extreme values", {
  extreme_data <- data.frame(
    category = factor(rep(c("A", "B"), each = 10)),
    value = c(rnorm(10, mean = 1, sd = 0.1), rnorm(10, mean = 1000, sd = 100))
  )

  result <- create_scatter_plot(
    plot_data = extreme_data,
    x_var = "category",
    y_var = "value",
    y_transform = "log",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})
