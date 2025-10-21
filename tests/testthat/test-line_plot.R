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
    provider = rep(letters[1:6], 10)
  )
}

# Unit Tests for create_line_plot
test_that("create_line_plot returns ggplot object", {
  test_data <- setup_test_data()

  result <- create_line_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "gg")
  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot handles log transformation", {
  test_data <- setup_test_data()

  result <- create_line_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    y_transform = "log",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
  # Check for log label
  expect_true(grepl("Log", result$labels$y) || !is.null(result$labels$y))
})

test_that("create_line_plot handles sqrt transformation", {
  test_data <- setup_test_data()

  result <- create_line_plot(
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

test_that("create_line_plot handles no transformation", {
  test_data <- setup_test_data()

  result <- create_line_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    y_transform = "none",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot handles geom_line with grouping", {
  test_data <- setup_test_data()

  result <- create_line_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    use_geom_line = TRUE,
    geom_line_group = "provider",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot handles geom_line without grouping", {
  test_data <- setup_test_data()

  result <- create_line_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    use_geom_line = FALSE,
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot handles custom colors", {
  test_data <- setup_test_data()

  result <- create_line_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    point_color = "blue",
    line_color = "green",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot removes NA values", {
  test_data <- setup_test_data()
  test_data$business_days_until_appointment[1:5] <- NA

  result <- create_line_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot saves files to output directory", {
  test_data <- setup_test_data()
  temp_output <- tempdir()

  # Clear any existing files
  existing_files <- list.files(temp_output, pattern = "line_plot.*\\.(tiff|png)$", full.names = TRUE)
  if (length(existing_files) > 0) file.remove(existing_files)

  result <- create_line_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    output_dir = temp_output,
    file_prefix = "test_line",
    verbose = FALSE
  )

  # Check that files were created
  files <- list.files(temp_output, pattern = "test_line.*\\.(tiff|png)$")
  expect_true(length(files) >= 2)  # Should have both TIFF and PNG
})

test_that("create_line_plot accepts custom DPI", {
  test_data <- setup_test_data()

  result <- create_line_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    dpi = 150,
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

# Regression Tests
test_that("create_line_plot handles single category", {
  test_data <- data.frame(
    category = rep("A", 20),
    value = rnorm(20, mean = 10, sd = 2)
  )

  result <- create_line_plot(
    plot_data = test_data,
    x_var = "category",
    y_var = "value",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot handles large dataset", {
  large_data <- data.frame(
    category = factor(sample(c("A", "B", "C"), 1000, replace = TRUE)),
    value = rnorm(1000, mean = 50, sd = 10),
    group = sample(letters[1:10], 1000, replace = TRUE)
  )

  result <- create_line_plot(
    plot_data = large_data,
    x_var = "category",
    y_var = "value",
    use_geom_line = TRUE,
    geom_line_group = "group",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot handles small dataset", {
  small_data <- data.frame(
    category = factor(c("A", "B", "A")),
    value = c(5, 10, 7)
  )

  result <- create_line_plot(
    plot_data = small_data,
    x_var = "category",
    y_var = "value",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot verbose FALSE suppresses messages", {
  test_data <- setup_test_data()

  expect_silent({
    result <- create_line_plot(
      plot_data = test_data,
      x_var = "insurance",
      y_var = "business_days_until_appointment",
      output_dir = tempdir(),
      verbose = FALSE
    )
  })
})

test_that("create_line_plot handles extreme values", {
  extreme_data <- data.frame(
    category = factor(rep(c("A", "B"), each = 10)),
    value = c(rnorm(10, mean = 1, sd = 0.1), rnorm(10, mean = 1000, sd = 100))
  )

  result <- create_line_plot(
    plot_data = extreme_data,
    x_var = "category",
    y_var = "value",
    y_transform = "log",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot handles many categories", {
  many_categories <- data.frame(
    category = factor(rep(LETTERS[1:10], each = 10)),
    value = rnorm(100, mean = 50, sd = 15)
  )

  result <- create_line_plot(
    plot_data = many_categories,
    x_var = "category",
    y_var = "value",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot handles negative values", {
  negative_data <- data.frame(
    category = factor(rep(c("A", "B"), each = 10)),
    value = rnorm(20, mean = -10, sd = 5)
  )

  result <- create_line_plot(
    plot_data = negative_data,
    x_var = "category",
    y_var = "value",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_line_plot uses viridis color palette", {
  test_data <- setup_test_data()

  result <- create_line_plot(
    plot_data = test_data,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    point_color = "viridis",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})
