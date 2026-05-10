test_that("returns data.frame with correct columns", {
  df <- data.frame(accepted = rbinom(100, 1, 0.6))
  out <- mysterycall_bootstrap_ci(df, "accepted", seed = 1)
  expected_cols <- c("group", "n", "estimate", "lower_ci", "upper_ci", "n_boot")
  expect_true(all(expected_cols %in% names(out)))
  expect_s3_class(out, "data.frame")
})

test_that("returns Overall row when group_col is NULL", {
  df <- data.frame(accepted = rbinom(50, 1, 0.5))
  out <- mysterycall_bootstrap_ci(df, "accepted", seed = 2)
  expect_equal(nrow(out), 1L)
  expect_equal(out$group, "Overall")
})

test_that("estimate is close to true proportion", {
  set.seed(99)
  df <- data.frame(accepted = rbinom(500, 1, 0.7))
  out <- mysterycall_bootstrap_ci(df, "accepted", n_boot = 3000L, seed = 99)
  expect_true(abs(out$estimate - 0.7) < 0.05)
})

test_that("CI contains the observed estimate", {
  set.seed(7)
  df <- data.frame(accepted = rbinom(200, 1, 0.4))
  out <- mysterycall_bootstrap_ci(df, "accepted", seed = 7)
  expect_true(out$lower_ci <= out$estimate)
  expect_true(out$upper_ci >= out$estimate)
})

test_that("seed makes results reproducible", {
  df <- data.frame(accepted = rbinom(100, 1, 0.5))
  out1 <- mysterycall_bootstrap_ci(df, "accepted", seed = 42)
  out2 <- mysterycall_bootstrap_ci(df, "accepted", seed = 42)
  expect_equal(out1$lower_ci, out2$lower_ci)
  expect_equal(out1$upper_ci, out2$upper_ci)
})

test_that("group_col splits correctly", {
  df <- data.frame(
    ins      = c(rep("A", 50), rep("B", 50)),
    accepted = c(rep(1, 50), rep(0, 50))
  )
  out <- mysterycall_bootstrap_ci(df, "accepted", group_col = "ins", seed = 1)
  expect_equal(nrow(out), 2L)
  expect_true("A" %in% out$group)
  expect_true("B" %in% out$group)
  a_est <- out$estimate[out$group == "A"]
  b_est <- out$estimate[out$group == "B"]
  expect_equal(a_est, 1.0)
  expect_equal(b_est, 0.0)
})

test_that("n_boot=NULL causes an error", {
  df <- data.frame(accepted = rbinom(50, 1, 0.5))
  expect_error(
    mysterycall_bootstrap_ci(df, "accepted", n_boot = NULL),
    regexp = "NULL"
  )
})

test_that("n_boot < 100 causes an error", {
  df <- data.frame(accepted = rbinom(50, 1, 0.5))
  expect_error(
    mysterycall_bootstrap_ci(df, "accepted", n_boot = 50L),
    regexp = ">= 100"
  )
})

test_that("stat='mean' works on numeric outcome", {
  df <- data.frame(wait = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  out <- mysterycall_bootstrap_ci(df, "wait", stat = "mean", seed = 5)
  expect_equal(out$estimate, mean(df$wait))
  expect_true(!is.na(out$lower_ci))
})

test_that("stat='median' works on numeric outcome", {
  df <- data.frame(wait = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  out <- mysterycall_bootstrap_ci(df, "wait", stat = "median", seed = 6)
  expect_equal(out$estimate, stats::median(df$wait))
  expect_true(!is.na(out$lower_ci))
})
