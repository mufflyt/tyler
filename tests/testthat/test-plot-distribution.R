# ── mysterycall_plot_distribution ────────────────────────────────────────────

test_that("plot_distribution: returns named list of two ggplots", {
  skip_if_not_installed("ggplot2")
  res <- mysterycall_plot_distribution(rpois(100, 10), title = "Wait days")
  expect_type(res, "list")
  expect_named(res, c("raw", "sqrt_transformed"))
  expect_s3_class(res$raw,             "ggplot")
  expect_s3_class(res$sqrt_transformed,"ggplot")
})

test_that("plot_distribution: NA values are silently excluded", {
  skip_if_not_installed("ggplot2")
  x   <- c(1, 2, NA, 4, 5)
  res <- mysterycall_plot_distribution(x, title = "x")
  expect_s3_class(res$raw, "ggplot")
})

test_that("plot_distribution: non-numeric input errors", {
  expect_error(mysterycall_plot_distribution(c("a", "b")), "numeric")
})

test_that("plot_distribution: custom title appears in plot", {
  skip_if_not_installed("ggplot2")
  res <- mysterycall_plot_distribution(1:50, title = "MyTitle")
  expect_match(res$raw$labels$title, "MyTitle")
})

test_that("plot_distribution: sqrt panel has sqrt in y label", {
  skip_if_not_installed("ggplot2")
  res <- mysterycall_plot_distribution(1:50, title = "x")
  expect_match(res$sqrt_transformed$labels$y, "sqrt", ignore.case = TRUE)
})
