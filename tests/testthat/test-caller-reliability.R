test_that("kappa for binary data returns list with correct names", {
  df <- data.frame(
    caller  = c("A","A","A","B","B","B"),
    pair    = c(1, 2, 3, 1, 2, 3),
    outcome = c(1, 0, 1, 1, 0, 0)
  )
  res <- mysterycall_caller_reliability(df, "caller", "outcome", pair_col = "pair", type = "kappa")
  expect_named(res, c("type","statistic","lower_ci","upper_ci","n_pairs","interpretation"))
})

test_that("kappa statistic is in [-1, 1]", {
  df <- data.frame(
    caller  = c("A","A","A","B","B","B"),
    pair    = c(1, 2, 3, 1, 2, 3),
    outcome = c(1, 0, 1, 1, 0, 0)
  )
  res <- mysterycall_caller_reliability(df, "caller", "outcome", pair_col = "pair", type = "kappa")
  expect_gte(res$statistic, -1)
  expect_lte(res$statistic,  1)
})

test_that("perfect agreement gives kappa near 1", {
  df <- data.frame(
    caller  = c("A","A","A","B","B","B"),
    pair    = c(1, 2, 3, 1, 2, 3),
    outcome = c(1, 0, 1, 1, 0, 1)
  )
  res <- mysterycall_caller_reliability(df, "caller", "outcome", pair_col = "pair", type = "kappa")
  expect_equal(res$statistic, 1, tolerance = 1e-6)
})

test_that("no agreement gives kappa near 0 or negative", {
  # Opposite ratings every time
  df <- data.frame(
    caller  = c("A","A","A","B","B","B"),
    pair    = c(1, 2, 3, 1, 2, 3),
    outcome = c(1, 0, 1, 0, 1, 0)
  )
  res <- mysterycall_caller_reliability(df, "caller", "outcome", pair_col = "pair", type = "kappa")
  expect_lte(res$statistic, 0.1)
})

test_that("gold_col works for two-rater comparison", {
  df <- data.frame(
    caller  = rep("A", 5),
    outcome = c(1, 1, 0, 0, 1),
    gold    = c(1, 0, 0, 1, 1)
  )
  res <- mysterycall_caller_reliability(df, "caller", "outcome", gold_col = "gold", type = "kappa")
  expect_equal(res$type, "kappa")
  expect_equal(res$n_pairs, 5L)
})

test_that("icc for continuous data returns numeric statistic", {
  set.seed(42)
  df <- data.frame(
    caller  = c(rep("A", 10), rep("B", 10)),
    pair    = c(1:10, 1:10),
    outcome = c(rnorm(10, 5, 1), rnorm(10, 5, 1))
  )
  res <- mysterycall_caller_reliability(df, "caller", "outcome", pair_col = "pair", type = "icc")
  expect_equal(res$type, "icc")
  expect_true(is.numeric(res$statistic))
})

test_that("interpretation matches Landis & Koch scale", {
  df <- data.frame(
    caller  = c("A","A","A","B","B","B"),
    pair    = c(1, 2, 3, 1, 2, 3),
    outcome = c(1, 0, 1, 1, 0, 1)
  )
  res <- mysterycall_caller_reliability(df, "caller", "outcome", pair_col = "pair", type = "kappa")
  expect_true(res$interpretation %in% c("poor","fair","moderate","good","excellent"))
})

test_that("poor kappa (< 0.20) returns interpretation 'poor'", {
  # Use gold_col to create near-zero agreement
  set.seed(1)
  # Build data where agreement is by chance only
  df <- data.frame(
    caller  = rep("A", 20),
    outcome = rep(c(1,0), 10),
    gold    = rep(c(0,1), 10)
  )
  res <- mysterycall_caller_reliability(df, "caller", "outcome", gold_col = "gold", type = "kappa")
  expect_equal(res$interpretation, "poor")
})

test_that("print method does not error", {
  df <- data.frame(
    caller  = c("A","A","B","B"),
    pair    = c(1, 2, 1, 2),
    outcome = c(1, 0, 1, 0)
  )
  res <- mysterycall_caller_reliability(df, "caller", "outcome", pair_col = "pair", type = "kappa")
  expect_output(print(res), "mysterycall_caller_reliability")
})

test_that("errors when caller_col missing", {
  df <- data.frame(outcome = c(1, 0))
  expect_error(
    mysterycall_caller_reliability(df, "caller", "outcome"),
    "caller_col"
  )
})

test_that("errors when outcome_col missing", {
  df <- data.frame(caller = c("A","B"))
  expect_error(
    mysterycall_caller_reliability(df, "caller", "outcome"),
    "outcome_col"
  )
})
