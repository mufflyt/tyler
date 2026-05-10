test_that("wait_time_summary: ungrouped returns correct stats", {
  df <- data.frame(wait_days = c(2, 5, 10, 20, 8, NA))
  res <- mysterycall_wait_time_summary(df)

  expect_named(res, c("summary", "test", "p_value", "test_name", "interpretation"))
  expect_null(res$test)
  expect_true(is.na(res$p_value))
  expect_equal(res$test_name, "none")

  s <- res$summary
  expect_equal(s$n, 5L)
  expect_equal(s$n_missing, 1L)
  expect_equal(s$median, stats::median(c(2, 5, 10, 20, 8)))
  expect_equal(s$min, 2)
  expect_equal(s$max, 20)
})

test_that("wait_time_summary: two groups runs Wilcoxon, returns p-value", {
  df <- data.frame(
    wait_days = c(3, 7, 14, 21, 5, 10, 30, 2),
    insurance = c("Medicaid", "Medicaid", "Medicaid", "Medicaid",
                  "BCBS", "BCBS", "BCBS", "BCBS")
  )
  res <- mysterycall_wait_time_summary(df, group_by = "insurance")

  expect_s3_class(res$summary, "tbl_df")
  expect_equal(nrow(res$summary), 2L)
  expect_true("insurance" %in% names(res$summary))
  expect_true(grepl("Wilcoxon", res$test_name))
  expect_true(is.numeric(res$p_value) && !is.na(res$p_value))
  expect_true(nzchar(res$interpretation))
})

test_that("wait_time_summary: three groups runs Kruskal-Wallis", {
  df <- data.frame(
    wait_days = c(3, 7, 14, 5, 10, 30, 1, 4, 8),
    insurance = c("Medicaid", "Medicaid", "Medicaid",
                  "BCBS", "BCBS", "BCBS",
                  "Medicare", "Medicare", "Medicare")
  )
  res <- mysterycall_wait_time_summary(df, group_by = "insurance")

  expect_true(grepl("Kruskal", res$test_name))
  expect_equal(nrow(res$summary), 3L)
})

test_that("wait_time_summary: rejects non-numeric wait column", {
  df <- data.frame(wait_days = c("a", "b", "c"))
  expect_error(mysterycall_wait_time_summary(df), "`wait_days`")
})

test_that("wait_time_summary: rejects missing wait column", {
  df <- data.frame(x = 1:3)
  expect_error(mysterycall_wait_time_summary(df), "wait_days")
})

test_that("wait_time_summary: rejects invalid conf_level", {
  df <- data.frame(wait_days = c(1, 2, 3))
  expect_error(mysterycall_wait_time_summary(df, conf_level = 1.5), "conf_level")
  expect_error(mysterycall_wait_time_summary(df, conf_level = 0),   "conf_level")
})

test_that("wait_time_summary: all-NA wait column handled gracefully", {
  df <- data.frame(wait_days = c(NA_real_, NA_real_),
                   insurance = c("A", "B"))
  res <- mysterycall_wait_time_summary(df, group_by = "insurance")
  expect_true(all(is.na(res$summary$median)))
})

# ── acceptance_rate ──────────────────────────────────────────────────────────

test_that("acceptance_rate: ungrouped logical column", {
  df <- data.frame(contact_office = c(TRUE, FALSE, TRUE, TRUE, NA))
  res <- mysterycall_acceptance_rate(df)

  expect_named(res, c("summary", "test", "p_value", "test_name", "interpretation"))
  expect_null(res$test)
  s <- res$summary
  expect_equal(s$n_total,    4L)  # NA excluded
  expect_equal(s$n_missing,  1L)
  expect_equal(s$n_accepted, 3L)
  expect_equal(s$n_rejected, 1L)
  expect_equal(s$rate, 3 / 4)
  expect_true(s$ci_lower > 0 && s$ci_upper < 1)
})

test_that("acceptance_rate: character Yes/No coercion", {
  df <- data.frame(contact_office = c("Yes", "No", "yes", "YES", "no"))
  res <- mysterycall_acceptance_rate(df)
  expect_equal(res$summary$n_accepted, 3L)
})

test_that("acceptance_rate: two groups triggers chi-square or Fisher", {
  df <- data.frame(
    contact_office = c("Yes", "No", "Yes", "Yes",
                       "No",  "No", "Yes", "No"),
    insurance      = c("Medicaid", "Medicaid", "Medicaid", "Medicaid",
                       "BCBS",     "BCBS",     "BCBS",     "BCBS")
  )
  res <- mysterycall_acceptance_rate(df, group_by = "insurance")

  expect_s3_class(res$summary, "tbl_df")
  expect_equal(nrow(res$summary), 2L)
  expect_true("insurance" %in% names(res$summary))
  expect_true(grepl("chi-square|Fisher", res$test_name))
  expect_true(is.numeric(res$p_value) && !is.na(res$p_value))
})

test_that("acceptance_rate: small cells trigger Fisher's exact for 2 groups", {
  df <- data.frame(
    contact_office = c("Yes", "No", "Yes", "No"),
    insurance      = c("A",   "A",  "B",   "B")
  )
  res <- mysterycall_acceptance_rate(df, group_by = "insurance", min_cell = 5L)
  expect_equal(res$test_name, "Fisher's exact")
})

test_that("acceptance_rate: three groups uses chi-square", {
  df <- data.frame(
    contact_office = rep(c("Yes", "No", "Yes"), times = 3),
    insurance      = rep(c("Medicaid", "BCBS", "Medicare"), each = 3)
  )
  res <- mysterycall_acceptance_rate(df, group_by = "insurance")
  expect_equal(nrow(res$summary), 3L)
  expect_true(grepl("chi-square", res$test_name))
})

test_that("acceptance_rate: rejects invalid conf_level", {
  df <- data.frame(contact_office = c(TRUE, FALSE))
  expect_error(mysterycall_acceptance_rate(df, conf_level = 2), "conf_level")
})

test_that("acceptance_rate: rejects missing accepted_col", {
  df <- data.frame(x = 1:3)
  expect_error(mysterycall_acceptance_rate(df), "contact_office")
})

test_that("acceptance_rate: all accepted gives rate 1 with valid CI", {
  df <- data.frame(contact_office = c("Yes", "Yes", "Yes"))
  res <- mysterycall_acceptance_rate(df)
  expect_equal(res$summary$rate, 1)
  expect_true(res$summary$ci_lower < 1)
})

test_that("acceptance_rate: all rejected gives rate 0 with valid CI", {
  df <- data.frame(contact_office = c("No", "No", "No"))
  res <- mysterycall_acceptance_rate(df)
  expect_equal(res$summary$rate, 0)
  expect_true(res$summary$ci_upper > 0)
})

test_that("acceptance_rate: interpretation string is non-empty", {
  df <- data.frame(
    contact_office = c("Yes", "No", "Yes", "No"),
    insurance      = c("A", "A", "B", "B")
  )
  res <- mysterycall_acceptance_rate(df, group_by = "insurance")
  expect_true(nzchar(res$interpretation))
  expect_true(grepl("%", res$interpretation))
})
