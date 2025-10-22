library(testthat)
library(tyler)
library(dplyr)
library(mockery)

# End-to-End Tests for Complete Workflows

# Workflow 1: Complete NPI Processing Pipeline
test_that("E2E: Complete NPI validation and processing workflow", {
  # Create test NPI data
  test_npis <- data.frame(
    npi = c(1234567890, 9876543210, NA, 1111111111, 123),
    provider_name = c("Dr. Smith", "Dr. Jones", "Dr. Missing", "Dr. Brown", "Dr. Invalid"),
    specialty = c("Cardiology", "Neurology", "OBGYN", "OBGYN", "Pediatrics"),
    city = c("New York", "Boston", "Chicago", "Denver", "Seattle"),
    stringsAsFactors = FALSE
  )

  # Mock NPI validation
  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', function(x) {
    x %in% c("1234567890", "9876543210", "1111111111")
  })

  # Step 1: Validate NPIs
  validated_data <- validate_and_remove_invalid_npi(test_npis)

  expect_s3_class(validated_data, "data.frame")
  expect_true("npi_is_valid" %in% colnames(validated_data))
  expect_true(nrow(validated_data) >= 2)
  expect_false(any(is.na(validated_data$npi)))

  # Step 2: Calculate proportions by specialty
  specialty_props <- calculate_proportion(validated_data, specialty)

  expect_s3_class(specialty_props, "data.frame")
  expect_equal(sum(specialty_props$percent), 100, tolerance = 0.1)

  # Step 3: Get most common specialty
  most_common <- calcpercentages(validated_data, "specialty")

  expect_s3_class(most_common, "data.frame")
  expect_equal(nrow(most_common), 1)
})

# Workflow 2: Data Quality Assessment Pipeline
test_that("E2E: Data quality assessment workflow", {
  # Create test dataset with quality issues
  messy_data <- data.frame(
    provider_id = c(1:100),
    category = c(
      rep("Valid", 70),
      rep(NA, 20),
      rep("", 5),
      rep("Outlier", 5)
    ),
    wait_time = c(
      rnorm(70, mean = 10, sd = 3),
      rep(NA, 20),
      rep(0, 5),
      c(1000, 2000, 3000, 4000, 5000)
    ),
    stringsAsFactors = FALSE
  )

  # Step 1: Remove missing values
  clean_data <- messy_data %>%
    filter(!is.na(category) & category != "")

  expect_true(nrow(clean_data) < nrow(messy_data))
  expect_false(any(is.na(clean_data$category)))

  # Step 2: Calculate category proportions
  category_dist <- calculate_proportion(clean_data, category)

  expect_s3_class(category_dist, "data.frame")
  expect_equal(sum(category_dist$n), nrow(clean_data))

  # Step 3: Format percentages for reporting
  formatted_pcts <- format_pct(category_dist$percent / 100, my_digits = 1)

  expect_type(formatted_pcts, "character")
  expect_length(formatted_pcts, nrow(category_dist))

  # Step 4: Check normality of wait times (excluding NAs and zeros)
  valid_wait_data <- messy_data %>%
    filter(!is.na(wait_time) & wait_time > 0)

  if (nrow(valid_wait_data) >= 3) {
    normality_result <- suppressMessages({
      invisible(capture.output(
        result <- check_normality(valid_wait_data, "wait_time"),
        file = nullfile()
      ))
      result
    })

    expect_type(normality_result, "list")
    expect_true(length(normality_result) == 2)
  }
})

# Workflow 3: Visualization Pipeline
test_that("E2E: Complete visualization workflow", {
  # Create comprehensive test dataset
  viz_data <- data.frame(
    insurance = factor(rep(c("Private", "Medicaid", "Medicare"), each = 30)),
    wait_time = c(
      rnorm(30, mean = 8, sd = 2),
      rnorm(30, mean = 15, sd = 5),
      rnorm(30, mean = 12, sd = 3)
    ),
    provider = rep(paste0("Provider_", 1:9), 10),
    stringsAsFactors = FALSE
  )

  temp_dir <- tempdir()

  # Step 1: Create scatter plot
  scatter <- create_scatter_plot(
    plot_data = viz_data,
    x_var = "insurance",
    y_var = "wait_time",
    y_transform = "log",
    output_dir = temp_dir,
    file_prefix = "e2e_scatter",
    verbose = FALSE
  )

  expect_s3_class(scatter, "ggplot")

  # Step 2: Create density plot
  density <- create_density_plot(
    data = viz_data,
    x_var = "wait_time",
    fill_var = "insurance",
    x_transform = "sqrt",
    output_dir = temp_dir,
    file_prefix = "e2e_density",
    verbose = FALSE
  )

  expect_s3_class(density, "ggplot")

  # Step 3: Create line plot
  line <- create_line_plot(
    plot_data = viz_data,
    x_var = "insurance",
    y_var = "wait_time",
    use_geom_line = TRUE,
    geom_line_group = "provider",
    output_dir = temp_dir,
    file_prefix = "e2e_line",
    verbose = FALSE
  )

  expect_s3_class(line, "ggplot")

  # Verify all plots were created and saved
  scatter_files <- list.files(temp_dir, pattern = "e2e_scatter.*\\.(tiff|png)$")
  density_files <- list.files(temp_dir, pattern = "e2e_density.*\\.(tiff|png)$")
  line_files <- list.files(temp_dir, pattern = "e2e_line.*\\.(tiff|png)$")

  expect_true(length(scatter_files) >= 2)
  expect_true(length(density_files) >= 2)
  expect_true(length(line_files) >= 2)
})

# Workflow 4: Statistical Summary Pipeline
test_that("E2E: Statistical analysis workflow", {
  # Create analysis dataset
  analysis_data <- data.frame(
    group = factor(rep(c("Treatment", "Control"), each = 50)),
    outcome = c(
      rnorm(50, mean = 100, sd = 15),
      rnorm(50, mean = 95, sd = 15)
    ),
    age = sample(25:75, 100, replace = TRUE),
    gender = factor(sample(c("M", "F"), 100, replace = TRUE))
  )

  # Step 1: Calculate group proportions
  group_props <- calculate_proportion(analysis_data, group)

  expect_s3_class(group_props, "data.frame")
  expect_equal(sum(group_props$percent), 100, tolerance = 0.1)

  # Step 2: Get most common gender
  gender_summary <- calcpercentages(analysis_data, "gender")

  expect_s3_class(gender_summary, "data.frame")

  # Step 3: Check normality of outcome
  normality <- suppressMessages({
    invisible(capture.output(
      result <- check_normality(analysis_data, "outcome"),
      file = nullfile()
    ))
    result
  })

  expect_type(normality, "list")

  # Step 4: Format summary statistics
  if ("mean" %in% names(normality)) {
    mean_formatted <- format_pct(normality$mean / 100, my_digits = 2)
    sd_formatted <- format_pct(normality$sd / 100, my_digits = 2)

    expect_type(mean_formatted, "character")
    expect_type(sd_formatted, "character")
  }

  # Step 5: Analyze by subgroups
  treatment_props <- analysis_data %>%
    filter(group == "Treatment") %>%
    calculate_proportion(gender)

  control_props <- analysis_data %>%
    filter(group == "Control") %>%
    calculate_proportion(gender)

  expect_s3_class(treatment_props, "data.frame")
  expect_s3_class(control_props, "data.frame")
})

# Workflow 5: Data Transformation and Reporting
test_that("E2E: Data transformation and reporting workflow", {
  # Create raw data
  raw_data <- data.frame(
    id = 1:200,
    insurance_type = sample(c("Private", "Medicaid", "Medicare", "Uninsured"), 200, replace = TRUE),
    appointment_wait = rpois(200, lambda = 10),
    contacted = sample(c(TRUE, FALSE), 200, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Step 1: Filter contacted only
  contacted_data <- raw_data %>%
    filter(contacted == TRUE)

  expect_true(nrow(contacted_data) <= nrow(raw_data))

  # Step 2: Calculate insurance distribution
  insurance_dist <- calculate_proportion(contacted_data, insurance_type)

  expect_equal(sum(insurance_dist$percent), 100, tolerance = 0.1)

  # Step 3: Get most common insurance
  most_common_insurance <- calcpercentages(contacted_data, "insurance_type")

  expect_s3_class(most_common_insurance, "data.frame")

  # Step 4: Format for report
  insurance_dist <- insurance_dist %>%
    mutate(
      formatted_pct = format_pct(percent / 100, my_digits = 1),
      formatted_n = format_pct(n, my_digits = 0)
    )

  expect_true("formatted_pct" %in% colnames(insurance_dist))
  expect_true("formatted_n" %in% colnames(insurance_dist))

  # Step 5: Create visualization
  if (nrow(contacted_data) > 0) {
    viz <- create_scatter_plot(
      plot_data = contacted_data,
      x_var = "insurance_type",
      y_var = "appointment_wait",
      y_transform = "sqrt",
      output_dir = tempdir(),
      file_prefix = "e2e_final",
      verbose = FALSE
    )

    expect_s3_class(viz, "ggplot")
  }
})

# Workflow 6: Validation and Cleaning Pipeline
test_that("E2E: Complete data validation and cleaning workflow", {
  # Create messy NPI dataset
  messy_npi_data <- data.frame(
    npi = c(1234567890, NA, 9876543210, 123, "", 1111111111, 2222222222),
    name = c("A", "B", "C", "D", "E", "F", "G"),
    specialty = c("OBGYN", "OBGYN", "Cardiology", "OBGYN", "Neurology", "OBGYN", "Cardiology"),
    state = c("CO", "NY", "CA", "TX", "FL", "CO", "CA"),
    stringsAsFactors = FALSE
  )

  # Step 1: Validate NPIs
  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', function(x) {
    x %in% c("1234567890", "9876543210", "1111111111", "2222222222")
  })

  clean_npis <- validate_and_remove_invalid_npi(messy_npi_data)

  expect_s3_class(clean_npis, "data.frame")
  expect_true(nrow(clean_npis) < nrow(messy_npi_data))

  # Step 2: Analyze specialty distribution
  specialty_analysis <- calculate_proportion(clean_npis, specialty)

  expect_s3_class(specialty_analysis, "data.frame")
  expect_equal(sum(specialty_analysis$percent), 100, tolerance = 0.1)

  # Step 3: Get most common specialty
  top_specialty <- calcpercentages(clean_npis, "specialty")

  expect_equal(nrow(top_specialty), 1)

  # Step 4: Analyze by state
  state_counts <- calculate_proportion(clean_npis, state)

  expect_s3_class(state_counts, "data.frame")

  # Step 5: Create final summary
  summary_stats <- list(
    total_records = nrow(clean_npis),
    total_specialties = nrow(specialty_analysis),
    total_states = nrow(state_counts),
    most_common_specialty = top_specialty$specialty[1],
    pct_most_common = format_pct(top_specialty$percent[1] / 100, my_digits = 1)
  )

  expect_type(summary_stats, "list")
  expect_equal(length(summary_stats), 5)
  expect_true(summary_stats$total_records > 0)
})

# Workflow 7: Multi-transformation Visualization Workflow
test_that("E2E: Multi-transformation visualization workflow", {
  # Create skewed data requiring transformation
  skewed_data <- data.frame(
    category = factor(rep(c("A", "B", "C"), each = 40)),
    value = c(
      rexp(40, rate = 0.1),
      rexp(40, rate = 0.2),
      rexp(40, rate = 0.15)
    )
  )

  temp_output <- tempdir()

  # Step 1: Visualize without transformation
  plot_none <- create_scatter_plot(
    plot_data = skewed_data,
    x_var = "category",
    y_var = "value",
    y_transform = "none",
    output_dir = temp_output,
    file_prefix = "e2e_none",
    verbose = FALSE
  )

  # Step 2: Visualize with log transformation
  plot_log <- create_scatter_plot(
    plot_data = skewed_data,
    x_var = "category",
    y_var = "value",
    y_transform = "log",
    output_dir = temp_output,
    file_prefix = "e2e_log",
    verbose = FALSE
  )

  # Step 3: Visualize with sqrt transformation
  plot_sqrt <- create_scatter_plot(
    plot_data = skewed_data,
    x_var = "category",
    y_var = "value",
    y_transform = "sqrt",
    output_dir = temp_output,
    file_prefix = "e2e_sqrt",
    verbose = FALSE
  )

  # All plots should be valid ggplot objects
  expect_s3_class(plot_none, "ggplot")
  expect_s3_class(plot_log, "ggplot")
  expect_s3_class(plot_sqrt, "ggplot")

  # Step 4: Create density plots for comparison
  density_log <- create_density_plot(
    data = skewed_data,
    x_var = "value",
    fill_var = "category",
    x_transform = "log",
    output_dir = temp_output,
    file_prefix = "e2e_density_log",
    verbose = FALSE
  )

  expect_s3_class(density_log, "ggplot")

  # Step 5: Check data distribution normality
  normality_result <- suppressMessages({
    invisible(capture.output(
      result <- check_normality(skewed_data, "value"),
      file = nullfile()
    ))
    result
  })

  expect_type(normality_result, "list")
  # Exponential data should likely be non-normal
})
