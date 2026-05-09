library(testthat)
testthat::skip_if_not_installed("ggplot2")
testthat::skip_if_not_installed("viridis")
testthat::skip_if_not_installed("dplyr")

# Shared test fixture reused across all plot function tests
WAIT_DATA <- data.frame(
  insurance = rep(c("Medicaid", "Commercial", "Medicare"), each = 10),
  days      = c(
    abs(rnorm(10, mean = 15, sd = 5)),
    abs(rnorm(10, mean = 8, sd = 3)),
    abs(rnorm(10, mean = 20, sd = 7))
  ) + 0.01  # ensure > 0 for log/sqrt transforms
)
OUT_DIR <- file.path(tempdir(), paste0("mysterycall_plot_tests_", Sys.getpid()))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ── mysterycall_plot_density ───────────────────────────────────────────────────────

test_that("mysterycall_plot_density - returns a ggplot object", {
  result <- suppressMessages(
    mysterycall_plot_density(
      data        = WAIT_DATA,
      x_var       = "days",
      fill_var    = "insurance",
      x_transform = "none",
      dpi         = 50,
      output_dir  = OUT_DIR,
      file_prefix = "density_none",
      verbose     = FALSE
    )
  )
  expect_s3_class(result, "ggplot")
})

test_that("mysterycall_plot_density - log transform returns ggplot", {
  result <- suppressMessages(
    mysterycall_plot_density(
      data        = WAIT_DATA,
      x_var       = "days",
      fill_var    = "insurance",
      x_transform = "log",
      dpi         = 50,
      output_dir  = OUT_DIR,
      file_prefix = "density_log",
      verbose     = FALSE
    )
  )
  expect_s3_class(result, "ggplot")
})

test_that("mysterycall_plot_density - sqrt transform returns ggplot", {
  result <- suppressMessages(
    mysterycall_plot_density(
      data        = WAIT_DATA,
      x_var       = "days",
      fill_var    = "insurance",
      x_transform = "sqrt",
      dpi         = 50,
      output_dir  = OUT_DIR,
      file_prefix = "density_sqrt",
      verbose     = FALSE
    )
  )
  expect_s3_class(result, "ggplot")
})

test_that("mysterycall_plot_density - saves PNG and TIFF files", {
  prefix <- paste0("density_save_", Sys.getpid())
  suppressMessages(
    mysterycall_plot_density(
      data        = WAIT_DATA,
      x_var       = "days",
      fill_var    = "insurance",
      dpi         = 50,
      output_dir  = OUT_DIR,
      file_prefix = prefix,
      verbose     = FALSE
    )
  )
  png_files  <- Sys.glob(file.path(OUT_DIR, paste0(prefix, "*.png")))
  tiff_files <- Sys.glob(file.path(OUT_DIR, paste0(prefix, "*.tiff")))
  expect_true(length(png_files)  >= 1L, info = "No PNG file saved")
  expect_true(length(tiff_files) >= 1L, info = "No TIFF file saved")
})

test_that("mysterycall_plot_density - filters out zero/negative x values without error", {
  df_with_zeros <- rbind(WAIT_DATA, data.frame(insurance = "Medicaid", days = 0))
  expect_no_error(suppressMessages(
    mysterycall_plot_density(df_with_zeros, "days", "insurance",
                        dpi = 50, output_dir = OUT_DIR, verbose = FALSE)
  ))
})

test_that("mysterycall_plot_density - custom axis labels are accepted", {
  result <- suppressMessages(
    mysterycall_plot_density(
      data        = WAIT_DATA,
      x_var       = "days",
      fill_var    = "insurance",
      x_label     = "Waiting Time (days)",
      y_label     = "Probability Density",
      plot_title  = "Wait Time Distribution",
      dpi         = 50,
      output_dir  = OUT_DIR,
      verbose     = FALSE
    )
  )
  expect_s3_class(result, "ggplot")
})

# ── mysterycall_plot_scatter ───────────────────────────────────────────────────────

test_that("mysterycall_plot_scatter - returns a ggplot object", {
  result <- suppressMessages(
    mysterycall_plot_scatter(
      plot_data   = WAIT_DATA,
      x_var       = "insurance",
      y_var       = "days",
      y_transform = "none",
      dpi         = 50,
      output_dir  = OUT_DIR,
      file_prefix = "scatter_none",
      verbose     = FALSE
    )
  )
  expect_s3_class(result, "ggplot")
})

test_that("mysterycall_plot_scatter - log transform returns ggplot", {
  result <- suppressMessages(
    mysterycall_plot_scatter(
      plot_data   = WAIT_DATA,
      x_var       = "insurance",
      y_var       = "days",
      y_transform = "log",
      dpi         = 50,
      output_dir  = OUT_DIR,
      file_prefix = "scatter_log",
      verbose     = FALSE
    )
  )
  expect_s3_class(result, "ggplot")
})

test_that("mysterycall_plot_scatter - saves PNG and TIFF to output_dir", {
  prefix <- paste0("scatter_save_", Sys.getpid())
  suppressMessages(
    mysterycall_plot_scatter(
      plot_data   = WAIT_DATA,
      x_var       = "insurance",
      y_var       = "days",
      dpi         = 50,
      output_dir  = OUT_DIR,
      file_prefix = prefix,
      verbose     = FALSE
    )
  )
  png_files  <- Sys.glob(file.path(OUT_DIR, paste0(prefix, "*.png")))
  tiff_files <- Sys.glob(file.path(OUT_DIR, paste0(prefix, "*.tiff")))
  expect_true(length(png_files)  >= 1L)
  expect_true(length(tiff_files) >= 1L)
})

test_that("mysterycall_plot_scatter - jitter_width and point_alpha are accepted", {
  expect_no_error(suppressMessages(
    mysterycall_plot_scatter(
      plot_data    = WAIT_DATA,
      x_var        = "insurance",
      y_var        = "days",
      jitter_width = 0.5,
      point_alpha  = 0.3,
      dpi          = 50,
      output_dir   = OUT_DIR,
      verbose      = FALSE
    )
  ))
})

test_that("mysterycall_plot_scatter - sqrt transform returns ggplot", {
  result <- suppressMessages(
    mysterycall_plot_scatter(
      plot_data   = WAIT_DATA,
      x_var       = "insurance",
      y_var       = "days",
      y_transform = "sqrt",
      dpi         = 50,
      output_dir  = OUT_DIR,
      file_prefix = "scatter_sqrt",
      verbose     = FALSE
    )
  )
  expect_s3_class(result, "ggplot")
})

# ── mysterycall_plot_line ──────────────────────────────────────────────────────────

test_that("mysterycall_plot_line - returns a ggplot object", {
  result <- suppressMessages(
    mysterycall_plot_line(
      plot_data   = WAIT_DATA,
      x_var       = "insurance",
      y_var       = "days",
      y_transform = "none",
      dpi         = 50,
      output_dir  = OUT_DIR,
      file_prefix = "line_none",
      verbose     = FALSE
    )
  )
  expect_s3_class(result, "ggplot")
})

test_that("mysterycall_plot_line - log transform accepted without error", {
  expect_no_error(suppressMessages(
    mysterycall_plot_line(
      plot_data   = WAIT_DATA,
      x_var       = "insurance",
      y_var       = "days",
      y_transform = "log",
      dpi         = 50,
      output_dir  = OUT_DIR,
      verbose     = FALSE
    )
  ))
})

test_that("mysterycall_plot_line - saves PNG and TIFF to output_dir", {
  prefix <- paste0("line_save_", Sys.getpid())
  suppressMessages(
    mysterycall_plot_line(
      plot_data   = WAIT_DATA,
      x_var       = "insurance",
      y_var       = "days",
      dpi         = 50,
      output_dir  = OUT_DIR,
      file_prefix = prefix,
      verbose     = FALSE
    )
  )
  png_files  <- Sys.glob(file.path(OUT_DIR, paste0(prefix, "*.png")))
  tiff_files <- Sys.glob(file.path(OUT_DIR, paste0(prefix, "*.tiff")))
  expect_true(length(png_files)  >= 1L)
  expect_true(length(tiff_files) >= 1L)
})

test_that("mysterycall_plot_line - use_geom_line with group column does not error", {
  df <- WAIT_DATA
  df$group_col <- rep(c("Group1", "Group2", "Group3"), 10)
  expect_no_error(suppressMessages(
    mysterycall_plot_line(
      plot_data       = df,
      x_var           = "insurance",
      y_var           = "days",
      use_geom_line   = TRUE,
      geom_line_group = "group_col",
      dpi             = 50,
      output_dir      = OUT_DIR,
      verbose         = FALSE
    )
  ))
})
