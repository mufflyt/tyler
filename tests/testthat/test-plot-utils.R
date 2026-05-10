# ── mysterycall_save_plot ─────────────────────────────────────────────────────

test_that("save_plot: writes a PNG file and returns path invisibly", {
  skip_if_not_installed("ggplot2")
  p    <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  path <- tempfile(fileext = ".png")
  ret  <- mysterycall_save_plot(p, path)
  expect_true(file.exists(path))
  expect_equal(ret, path)
  unlink(path)
})

test_that("save_plot: creates directory if it does not exist", {
  skip_if_not_installed("ggplot2")
  p      <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  tmpdir <- file.path(tempdir(), paste0("mc_test_", Sys.getpid()))
  path   <- file.path(tmpdir, "plot.png")
  on.exit(unlink(tmpdir, recursive = TRUE))
  mysterycall_save_plot(p, path)
  expect_true(file.exists(path))
})

test_that("save_plot: non-ggplot errors", {
  expect_error(mysterycall_save_plot(list(), tempfile(fileext=".png")), "ggplot")
})

test_that("save_plot: empty path errors", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  expect_error(mysterycall_save_plot(p, ""), "non-empty")
})

test_that("save_plot: PDF extension works", {
  skip_if_not_installed("ggplot2")
  p    <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  path <- tempfile(fileext = ".pdf")
  mysterycall_save_plot(p, path)
  expect_true(file.exists(path))
  unlink(path)
})
