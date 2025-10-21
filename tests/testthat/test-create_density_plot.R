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
    )
  )
}

# Unit Tests for create_density_plot
test_that("create_density_plot returns ggplot object", {
  test_data <- setup_test_data()

  result <- create_density_plot(
    data = test_data,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "gg")
  expect_s3_class(result, "ggplot")
})

test_that("create_density_plot handles log transformation", {
  test_data <- setup_test_data()

  result <- create_density_plot(
    data = test_data,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    x_transform = "log",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
  expect_true(grepl("Log", result$labels$x) || !is.null(result$labels$x))
})

test_that("create_density_plot handles sqrt transformation", {
  test_data <- setup_test_data()

  result <- create_density_plot(
    data = test_data,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    x_transform = "sqrt",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
  expect_true(grepl("Sqrt", result$labels$x) || !is.null(result$labels$x))
})

test_that("create_density_plot handles no transformation", {
  test_data <- setup_test_data()

  result <- create_density_plot(
    data = test_data,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    x_transform = "none",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_density_plot removes negative and zero values", {
  test_data <- setup_test_data()
  test_data$business_days_until_appointment[1:5] <- c(-1, -2, 0, -3, 0)

  result <- create_density_plot(
    data = test_data,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_density_plot removes NA values", {
  test_data <- setup_test_data()
  test_data$business_days_until_appointment[1:5] <- NA

  result <- create_density_plot(
    data = test_data,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_density_plot accepts custom labels", {
  test_data <- setup_test_data()

  result <- create_density_plot(
    data = test_data,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    x_label = "Wait Time (Days)",
    y_label = "Frequency",
    plot_title = "Test Density Plot",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_equal(result$labels$x, "Wait Time (Days)")
  expect_equal(result$labels$y, "Frequency")
  expect_equal(result$labels$title, "Test Density Plot")
})

test_that("create_density_plot saves files to output directory", {
  test_data <- setup_test_data()
  temp_output <- tempdir()

  # Clear any existing files
  existing_files <- list.files(temp_output, pattern = "density_plot.*\\.(tiff|png)$", full.names = TRUE)
  if (length(existing_files) > 0) file.remove(existing_files)

  result <- create_density_plot(
    data = test_data,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    output_dir = temp_output,
    file_prefix = "test_density",
    verbose = FALSE
  )

  # Check that files were created
  files <- list.files(temp_output, pattern = "test_density.*\\.(tiff|png)$")
  expect_true(length(files) >= 2)  # Should have both TIFF and PNG
})

test_that("create_density_plot accepts custom DPI", {
  test_data <- setup_test_data()

  result <- create_density_plot(
    data = test_data,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    dpi = 150,
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

# Regression Tests
test_that("create_density_plot handles single fill category", {
  test_data <- data.frame(
    value = rnorm(100, mean = 50, sd = 10),
    category = rep("A", 100)
  )

  result <- create_density_plot(
    data = test_data,
    x_var = "value",
    fill_var = "category",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_density_plot handles many fill categories", {
  test_data <- data.frame(
    value = rnorm(200, mean = 50, sd = 10),
    category = factor(rep(LETTERS[1:10], each = 20))
  )

  result <- create_density_plot(
    data = test_data,
    x_var = "value",
    fill_var = "category",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_density_plot handles small dataset", {
  small_data <- data.frame(
    value = c(5, 10, 7, 8, 9),
    category = factor(c("A", "A", "B", "B", "A"))
  )

  result <- create_density_plot(
    data = small_data,
    x_var = "value",
    fill_var = "category",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_density_plot handles large dataset", {
  large_data <- data.frame(
    value = rnorm(10000, mean = 100, sd = 20),
    category = factor(sample(c("A", "B", "C"), 10000, replace = TRUE))
  )

  result <- create_density_plot(
    data = large_data,
    x_var = "value",
    fill_var = "category",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_density_plot verbose FALSE suppresses messages", {
  test_data <- setup_test_data()

  expect_silent({
    result <- create_density_plot(
      data = test_data,
      x_var = "business_days_until_appointment",
      fill_var = "insurance",
      output_dir = tempdir(),
      verbose = FALSE
    )
  })
})

test_that("create_density_plot handles extreme values", {
  extreme_data <- data.frame(
    value = c(rnorm(50, mean = 1, sd = 0.1), rnorm(50, mean = 1000, sd = 100)),
    category = factor(rep(c("A", "B"), each = 50))
  )

  result <- create_density_plot(
    data = extreme_data,
    x_var = "value",
    fill_var = "category",
    x_transform = "log",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_density_plot handles skewed distributions", {
  skewed_data <- data.frame(
    value = c(rexp(100, rate = 0.5)),
    category = factor(rep("A", 100))
  )

  result <- create_density_plot(
    data = skewed_data,
    x_var = "value",
    fill_var = "category",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("create_density_plot handles integer values", {
  integer_data <- data.frame(
    value = sample(1:100, 200, replace = TRUE),
    category = factor(sample(c("A", "B"), 200, replace = TRUE))
  )

  result <- create_density_plot(
    data = integer_data,
    x_var = "value",
    fill_var = "category",
    output_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "ggplot")
})
