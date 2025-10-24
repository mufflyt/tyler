library(testthat)
testthat::skip_if_not_installed("tibble")
testthat::skip_if_not_installed("ggplot2")

sample_census <- tibble::tibble(
  statefp = c("01", "01", "02"),
  countyfp = c("001", "003", "013"),
  tractce = c("000100", "000200", "000300"),
  block_group = c("1", "2", "1"),
  geoid = c("010010001001", "010010002002", "020130003001"),
  B01001_001E = c(1000, 1200, 800),
  B01001_002E = c(480, 570, 360),
  B01001_026E = c(520, 630, 440),
  B01001_027E = c(30, 35, 25),
  B01001_028E = c(35, 40, 30),
  B01001_029E = c(40, 45, 35),
  B01001_030E = c(45, 50, 38),
  B01001_031E = c(46, 51, 39),
  B01001_032E = c(47, 52, 40),
  B01001_033E = c(48, 53, 41),
  B01001_034E = c(49, 54, 42),
  B01001_035E = c(50, 55, 43),
  B01001_036E = c(51, 56, 44),
  B01001_037E = c(52, 57, 45),
  B01001_038E = c(53, 58, 46),
  B01001_039E = c(54, 59, 47),
  B01001_040E = c(55, 60, 48),
  B01001_041E = c(56, 61, 49),
  B01001_042E = c(57, 62, 50),
  B01001_043E = c(58, 63, 51),
  B01001_044E = c(59, 64, 52),
  B01001_045E = c(60, 65, 53),
  B01001_046E = c(61, 66, 54),
  B01001_047E = c(62, 67, 55),
  B01001_048E = c(63, 68, 56),
  B01001_049E = c(64, 69, 57)
)

test_that("summarize_census_data aggregates by grouping variables", {
  result <- summarize_census_data(sample_census, group_vars = "statefp")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)

  state01 <- result[result$statefp == "01", ]
  expect_equal(state01$total_population, 2200)
  expect_equal(state01$female_population, 1150)
  expect_equal(state01$male_population, 1050)
  expect_equal(state01$block_group_count, 2L)
  expect_equal(state01$reproductive_age_female, 927)
  expect_equal(state01$female_share, 1150 / 2200)
  expect_equal(state01$reproductive_age_female_share, 927 / 1150)
})

test_that("summarize_census_data can summarise without grouping", {
  result <- summarize_census_data(sample_census, group_vars = character(0))
  expect_equal(nrow(result), 1L)
  expect_equal(result$total_population, sum(sample_census$B01001_001E))
  expect_equal(result$female_population, sum(sample_census$B01001_026E))
  expect_equal(result$male_population, sum(sample_census$B01001_002E))
  expect_equal(result$reproductive_age_female, 927 + 378)
})

test_that("summarize_census_data validates required columns", {
  expect_error(
    summarize_census_data(sample_census[, setdiff(names(sample_census), "B01001_026E")]),
    "Missing required columns"
  )
})

test_that("plot_census_age_distribution returns a ggplot and saves files", {
  output_dir <- file.path(tempdir(), "census_plot_test")
  if (dir.exists(output_dir)) {
    unlink(output_dir, recursive = TRUE)
  }

  plot_obj <- plot_census_age_distribution(
    sample_census,
    group_var = "statefp",
    output_dir = output_dir,
    file_prefix = "demo",
    dpi = 50,
    verbose = FALSE
  )

  expect_s3_class(plot_obj, "ggplot")
  expect_true(dir.exists(output_dir))
  png_files <- Sys.glob(file.path(output_dir, "demo_*.png"))
  tiff_files <- Sys.glob(file.path(output_dir, "demo_*.tiff"))
  expect_true(length(png_files) >= 1L)
  expect_true(length(tiff_files) >= 1L)
})
