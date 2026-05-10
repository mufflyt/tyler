test_that("returns data.frame with correct class", {
  df <- data.frame(
    ins      = c("A", "A", "B", "B", "C", "C"),
    accepted = c(1, 0, 1, 1, 0, 0)
  )
  out <- mysterycall_disparities_table(df, "accepted", "ins")
  expect_s3_class(out, "mysterycall_disparities_table")
  expect_s3_class(out, "data.frame")
})

test_that("has all expected columns", {
  df <- data.frame(
    ins      = c("A", "A", "B", "B"),
    accepted = c(1, 0, 1, 1)
  )
  out <- mysterycall_disparities_table(df, "accepted", "ins")
  expected_cols <- c(
    "group", "n", "n_accepted", "rate",
    "lower_ci", "upper_ci", "abs_diff", "rel_risk",
    "rr_lower", "rr_upper", "p_value", "p_value_fmt"
  )
  expect_true(all(expected_cols %in% names(out)))
})

test_that("reference group row has abs_diff=0 and rel_risk=1", {
  df <- data.frame(
    ins      = c("A", "A", "B", "B", "C"),
    accepted = c(1, 0, 1, 1, 0)
  )
  out <- mysterycall_disparities_table(df, "accepted", "ins", ref_group = "A")
  ref_row <- out[out$group == "A", ]
  expect_equal(ref_row$abs_diff, 0)
  expect_equal(ref_row$rel_risk, 1)
  expect_true(is.na(ref_row$p_value))
  expect_true(is.na(ref_row$rr_lower))
  expect_true(is.na(ref_row$rr_upper))
})

test_that("rate is n_accepted / n", {
  df <- data.frame(
    ins      = c("A", "A", "A", "A", "B", "B"),
    accepted = c(1, 1, 1, 0, 1, 0)
  )
  out <- mysterycall_disparities_table(df, "accepted", "ins")
  a_row <- out[out$group == "A", ]
  expect_equal(a_row$n, 4L)
  expect_equal(a_row$n_accepted, 3L)
  expect_equal(a_row$rate, 0.75)
})

test_that("wilson ci_method works", {
  df <- data.frame(
    ins      = c("A", "A", "B", "B"),
    accepted = c(1, 0, 1, 1)
  )
  out <- mysterycall_disparities_table(df, "accepted", "ins", ci_method = "wilson")
  expect_true(all(out$lower_ci <= out$rate, na.rm = TRUE))
  expect_true(all(out$upper_ci >= out$rate, na.rm = TRUE))
})

test_that("exact ci_method works", {
  df <- data.frame(
    ins      = c("A", "A", "B", "B"),
    accepted = c(1, 0, 1, 1)
  )
  out <- mysterycall_disparities_table(df, "accepted", "ins", ci_method = "exact")
  expect_true(all(out$lower_ci <= out$rate, na.rm = TRUE))
  expect_true(all(out$upper_ci >= out$rate, na.rm = TRUE))
})

test_that("wald ci_method works", {
  df <- data.frame(
    ins      = c("A", "A", "A", "A", "B", "B", "B", "B"),
    accepted = c(1, 0, 1, 0, 1, 1, 1, 0)
  )
  out <- mysterycall_disparities_table(df, "accepted", "ins", ci_method = "wald")
  expect_true(all(!is.na(out$lower_ci)))
  expect_true(all(!is.na(out$upper_ci)))
})

test_that("custom ref_group is respected", {
  df <- data.frame(
    ins      = c("A", "A", "B", "B", "C", "C"),
    accepted = c(1, 0, 1, 1, 0, 0)
  )
  out <- mysterycall_disparities_table(df, "accepted", "ins", ref_group = "B")
  ref_row <- out[out$group == "B", ]
  expect_equal(ref_row$abs_diff, 0)
  expect_equal(ref_row$rel_risk, 1)
  # B has rate 1.0; A rate 0.5, so A rel_risk should be ~0.5
  a_row <- out[out$group == "A", ]
  expect_equal(a_row$rel_risk, 0.5)
})

test_that("abs_diff values are in percentage points", {
  df <- data.frame(
    ins      = c("A", "A", "A", "A", "B", "B", "B", "B"),
    accepted = c(1, 1, 1, 1, 0, 0, 0, 0)
  )
  out <- mysterycall_disparities_table(df, "accepted", "ins", ref_group = "A")
  b_row <- out[out$group == "B", ]
  expect_equal(b_row$abs_diff, -100)
})

test_that("errors on non-binary outcome", {
  df <- data.frame(
    ins      = c("A", "B"),
    accepted = c(1, 2)
  )
  expect_error(
    mysterycall_disparities_table(df, "accepted", "ins"),
    regexp = "binary"
  )
})

test_that("errors on missing outcome column", {
  df <- data.frame(ins = c("A", "B"), accepted = c(1, 0))
  expect_error(
    mysterycall_disparities_table(df, "MISSING", "ins"),
    regexp = "not found"
  )
})

test_that("errors on missing group column", {
  df <- data.frame(ins = c("A", "B"), accepted = c(1, 0))
  expect_error(
    mysterycall_disparities_table(df, "accepted", "MISSING"),
    regexp = "not found"
  )
})

test_that("print method does not error", {
  df <- data.frame(
    ins      = c("A", "A", "B", "B", "C", "C"),
    accepted = c(1, 0, 1, 1, 0, 0)
  )
  out <- mysterycall_disparities_table(df, "accepted", "ins")
  expect_output(print(out))
})
