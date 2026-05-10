# ── mysterycall_plot_emmeans_full ─────────────────────────────────────────────

test_that("plot_emmeans_full: returns invisible list with data and plot", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ggplot2")
  m   <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
  res <- mysterycall_plot_emmeans_full(m, specs = "cyl", variable = "cyl",
                                        type = "response")
  expect_type(res, "list")
  expect_named(res, c("data", "plot"))
  expect_s3_class(res$plot, "ggplot")
  expect_s3_class(res$data, "data.frame")
})

test_that("plot_emmeans_full: data frame has rows", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ggplot2")
  m   <- lm(mpg ~ factor(cyl), data = mtcars)
  res <- mysterycall_plot_emmeans_full(m, "cyl", "cyl")
  expect_gt(nrow(res$data), 0L)
})

test_that("plot_emmeans_full: use_color=FALSE returns ggplot", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ggplot2")
  m   <- lm(mpg ~ factor(cyl), data = mtcars)
  res <- mysterycall_plot_emmeans_full(m, "cyl", "cyl", use_color = FALSE)
  expect_s3_class(res$plot, "ggplot")
})

test_that("plot_emmeans_full: two-spec grouping returns ggplot", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ggplot2")
  mt      <- mtcars
  mt$cyl_f <- factor(mt$cyl)
  mt$am_f  <- factor(mt$am, labels = c("Auto","Manual"))
  m   <- lm(mpg ~ cyl_f * am_f, data = mt)
  res <- mysterycall_plot_emmeans_full(m, c("cyl_f","am_f"), "cyl_f")
  expect_s3_class(res$plot, "ggplot")
})

test_that("plot_emmeans_full: save_path writes a file", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ggplot2")
  m    <- lm(mpg ~ factor(cyl), data = mtcars)
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path))
  mysterycall_plot_emmeans_full(m, "cyl", "cyl", save_path = path)
  expect_true(file.exists(path))
})

test_that("plot_emmeans_full: non-character variable errors", {
  skip_if_not_installed("emmeans")
  m <- lm(mpg ~ wt, data = mtcars)
  expect_error(mysterycall_plot_emmeans_full(m, "wt", 123), "character")
})
