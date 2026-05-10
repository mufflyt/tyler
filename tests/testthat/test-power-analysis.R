# ── mysterycall_cochran_n ────────────────────────────────────────────────────

test_that("cochran_n: known result — N=369, e=0.05 ≈ 192", {
  res <- mysterycall_cochran_n(N = 369)
  expect_equal(res$n, 192L)
})

test_that("cochran_n: known result — N=215, e=0.05 ≈ 140", {
  res <- mysterycall_cochran_n(N = 215)
  expect_equal(res$n, 140L)
})

test_that("cochran_n: n <= N always", {
  for (N in c(50, 200, 500, 2000)) {
    expect_lte(mysterycall_cochran_n(N)$n, N)
  }
})

test_that("cochran_n: returns named list", {
  res <- mysterycall_cochran_n(200)
  expect_named(res, c("n", "N", "margin_of_error", "effective_margin"))
})

test_that("cochran_n: effective_margin is numeric in (0,1)", {
  res <- mysterycall_cochran_n(200)
  expect_true(is.numeric(res$effective_margin))
  expect_true(res$effective_margin > 0 && res$effective_margin < 1)
})

test_that("cochran_n: smaller margin_of_error increases n", {
  n_wide   <- mysterycall_cochran_n(500, margin_of_error = 0.10)$n
  n_narrow <- mysterycall_cochran_n(500, margin_of_error = 0.05)$n
  expect_gt(n_narrow, n_wide)
})

test_that("cochran_n: non-positive N errors", {
  expect_error(mysterycall_cochran_n(0), "positive")
  expect_error(mysterycall_cochran_n(-10), "positive")
})

test_that("cochran_n: margin_of_error out of (0,1) errors", {
  expect_error(mysterycall_cochran_n(200, margin_of_error = 0), "0 and 1")
  expect_error(mysterycall_cochran_n(200, margin_of_error = 1), "0 and 1")
})

test_that("cochran_n: non-scalar N errors", {
  expect_error(mysterycall_cochran_n(c(200, 300)), "single")
})

# ── mysterycall_poisson_power ─────────────────────────────────────────────────

test_that("poisson_power: returns named list with required elements", {
  res <- suppressMessages(mysterycall_poisson_power(irr = 1.40, lambda_ref = 14))
  expect_named(res, c("n_per_arm", "n_total", "n_total_calls",
                       "irr", "lambda_ref", "lambda_trt",
                       "alpha", "power", "design_effect"),
               ignore.order = TRUE)
})

test_that("poisson_power: n_per_arm is positive integer", {
  res <- suppressMessages(mysterycall_poisson_power(irr = 1.40, lambda_ref = 14))
  expect_true(is.numeric(res$n_per_arm))
  expect_gt(res$n_per_arm, 0)
  expect_equal(res$n_per_arm, ceiling(res$n_per_arm))
})

test_that("poisson_power: lambda_trt = irr * lambda_ref", {
  res <- suppressMessages(mysterycall_poisson_power(irr = 1.40, lambda_ref = 14))
  expect_equal(res$lambda_trt, 1.40 * 14, tolerance = 1e-8)
})

test_that("poisson_power: both_arms=TRUE → n_total = n_per_arm", {
  res <- suppressMessages(mysterycall_poisson_power(irr = 1.40, lambda_ref = 14, both_arms = TRUE))
  expect_equal(res$n_total, res$n_per_arm)
})

test_that("poisson_power: both_arms=FALSE → n_total = 2 * n_per_arm", {
  res <- suppressMessages(mysterycall_poisson_power(irr = 1.40, lambda_ref = 14, both_arms = FALSE))
  expect_equal(res$n_total, res$n_per_arm * 2L)
})

test_that("poisson_power: higher power requires more subjects", {
  r80 <- suppressMessages(mysterycall_poisson_power(irr = 1.40, lambda_ref = 14, power = 0.80))
  r90 <- suppressMessages(mysterycall_poisson_power(irr = 1.40, lambda_ref = 14, power = 0.90))
  expect_gt(r90$n_per_arm, r80$n_per_arm)
})

test_that("poisson_power: smaller IRR (closer to 1) requires more subjects", {
  r140 <- suppressMessages(mysterycall_poisson_power(irr = 1.40, lambda_ref = 14))
  r120 <- suppressMessages(mysterycall_poisson_power(irr = 1.20, lambda_ref = 14))
  expect_gt(r120$n_per_arm, r140$n_per_arm)
})

test_that("poisson_power: IRR < 1 also accepted (protective direction)", {
  res <- suppressMessages(mysterycall_poisson_power(irr = 0.70, lambda_ref = 14))
  expect_gt(res$n_per_arm, 0)
})

test_that("poisson_power: icc > 0 inflates n_per_arm", {
  r0   <- suppressMessages(mysterycall_poisson_power(irr = 1.40, lambda_ref = 14, icc = 0))
  r05  <- suppressMessages(mysterycall_poisson_power(irr = 1.40, lambda_ref = 14, icc = 0.05))
  expect_gte(r05$n_per_arm, r0$n_per_arm)
})

test_that("poisson_power: design_effect = 1 when icc = 0", {
  res <- suppressMessages(mysterycall_poisson_power(irr = 1.40, lambda_ref = 14, icc = 0))
  expect_equal(res$design_effect, 1)
})

test_that("poisson_power: IRR = 1 errors", {
  expect_error(mysterycall_poisson_power(irr = 1, lambda_ref = 14), "not equal to 1")
})

test_that("poisson_power: negative lambda_ref errors", {
  expect_error(mysterycall_poisson_power(irr = 1.4, lambda_ref = -5), "positive")
})

test_that("poisson_power: alpha out of (0,1) errors", {
  expect_error(mysterycall_poisson_power(irr = 1.4, lambda_ref = 14, alpha = 0), "(0, 1)")
  expect_error(mysterycall_poisson_power(irr = 1.4, lambda_ref = 14, alpha = 1), "(0, 1)")
})

test_that("poisson_power: power out of (0,1) errors", {
  expect_error(mysterycall_poisson_power(irr = 1.4, lambda_ref = 14, power = 0), "(0, 1)")
  expect_error(mysterycall_poisson_power(irr = 1.4, lambda_ref = 14, power = 1), "(0, 1)")
})

test_that("poisson_power: icc >= 1 errors", {
  expect_error(mysterycall_poisson_power(irr = 1.4, lambda_ref = 14, icc = 1), "\\[0, 1\\)")
})
