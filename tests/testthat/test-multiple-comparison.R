test_that("returns data.frame with correct columns", {
  pvals <- c(0.001, 0.03, 0.2, 0.5, 0.04)
  out <- mysterycall_multiple_comparison_adjust(pvals)
  expected_cols <- c("comparison", "p_raw", "p_adjusted", "significant", "stars")
  expect_true(all(expected_cols %in% names(out)))
  expect_s3_class(out, "data.frame")
})

test_that("p_adjusted <= 1 for bonferroni (p_adj = p_raw * n, capped at 1)", {
  pvals <- c(0.01, 0.05, 0.2)
  out <- mysterycall_multiple_comparison_adjust(pvals, method = "bonferroni")
  # bonferroni p_adj = p_raw * n, so p_adjusted >= p_raw
  expect_true(all(out$p_adjusted >= out$p_raw))
  expect_true(all(out$p_adjusted <= 1))
})

test_that("significant column is logical", {
  pvals <- c(0.001, 0.5)
  out <- mysterycall_multiple_comparison_adjust(pvals)
  expect_type(out$significant, "logical")
})

test_that("significant flags match alpha threshold", {
  pvals <- c(0.001, 0.5)
  out <- mysterycall_multiple_comparison_adjust(pvals, alpha = 0.05)
  expect_true(out$significant[1])
  expect_false(out$significant[2])
})

test_that("stars column has correct values", {
  # use a small vector with known bonferroni adjustments
  pvals <- c(0.0001, 0.005, 0.04, 0.2)
  # with n=4 bonferroni: 0.0004, 0.02, 0.16, 0.8 -> ***, *, ns, ns
  out <- mysterycall_multiple_comparison_adjust(pvals, method = "bonferroni")
  expect_equal(out$stars[1], "***")
  expect_equal(out$stars[2], "*")
  expect_equal(out$stars[3], "ns")
  expect_equal(out$stars[4], "ns")
})

test_that("fdr is an alias for BH", {
  pvals <- c(0.01, 0.04, 0.2)
  out_fdr <- mysterycall_multiple_comparison_adjust(pvals, method = "fdr")
  out_bh  <- mysterycall_multiple_comparison_adjust(pvals, method = "BH")
  expect_equal(out_fdr$p_adjusted, out_bh$p_adjusted)
})

test_that("works with data.frame input and p_col", {
  df <- data.frame(
    comparison = c("A vs B", "A vs C", "B vs C"),
    p          = c(0.01, 0.04, 0.3)
  )
  out <- mysterycall_multiple_comparison_adjust(
    df, p_col = "p", label_col = "comparison"
  )
  expect_equal(out$comparison, df$comparison)
  expect_equal(out$p_raw, df$p)
})

test_that("label_col is used for comparison column", {
  df <- data.frame(label = c("X", "Y"), pv = c(0.01, 0.5))
  out <- mysterycall_multiple_comparison_adjust(df, p_col = "pv", label_col = "label")
  expect_equal(out$comparison, c("X", "Y"))
})

test_that("errors when p-values are outside [0, 1]", {
  expect_error(
    mysterycall_multiple_comparison_adjust(c(0.5, -0.1, 0.2)),
    regexp = "\\[0, 1\\]"
  )
  expect_error(
    mysterycall_multiple_comparison_adjust(c(0.5, 1.1)),
    regexp = "\\[0, 1\\]"
  )
})

test_that("errors when p_col not supplied for data.frame input", {
  df <- data.frame(p = c(0.01, 0.5))
  expect_error(
    mysterycall_multiple_comparison_adjust(df),
    regexp = "p_col"
  )
})
