# ── mysterycall_check_duplicates ──────────────────────────────────────────────

test_that("check_duplicates: flags physicians over max_calls", {
  df  <- data.frame(id = c("A","A","A","B","B","C"), x = 1:6)
  res <- mysterycall_check_duplicates(df, "id", max_calls = 2L)
  expect_true(all(res$id == "A"))
  expect_equal(attr(res, "n_flagged"), 1L)
})

test_that("check_duplicates: no duplicates → empty data frame", {
  df  <- data.frame(id = c("A","B","C"), x = 1:3)
  res <- mysterycall_check_duplicates(df, "id", max_calls = 2L)
  expect_equal(nrow(res), 0L)
})

test_that("check_duplicates: n_calls column present and correct", {
  df  <- data.frame(id = c("A","A","A","B","B"), x = 1:5)
  res <- mysterycall_check_duplicates(df, "id", max_calls = 2L)
  expect_true("n_calls" %in% names(res))
  expect_equal(unique(res$n_calls), 3L)
})

test_that("check_duplicates: missing id_col errors", {
  df <- data.frame(x = 1:3)
  expect_error(mysterycall_check_duplicates(df, "id"), "not found")
})

test_that("check_duplicates: non-data-frame errors", {
  expect_error(mysterycall_check_duplicates(list(), "id"), "data frame")
})

# ── mysterycall_stratified_sample ─────────────────────────────────────────────

test_that("stratified_sample: returns n_per_group rows per group", {
  df  <- data.frame(
    g = rep(c("A","B","C"), c(100, 80, 60)),
    x = rnorm(240)
  )
  res <- mysterycall_stratified_sample(df, "g", n_per_group = 20L, seed = 1L)
  counts <- table(res$g)
  expect_true(all(counts == 20L))
})

test_that("stratified_sample: small group returned in full", {
  df  <- data.frame(g = c(rep("A", 50), rep("B", 5)), x = rnorm(55))
  res <- mysterycall_stratified_sample(df, "g", n_per_group = 20L, seed = 1L)
  expect_equal(sum(res$g == "B"), 5L)
})

test_that("stratified_sample: seed produces reproducible output", {
  df   <- data.frame(g = rep(c("A","B"), 50), x = rnorm(100))
  res1 <- mysterycall_stratified_sample(df, "g", 10L, seed = 42L)
  res2 <- mysterycall_stratified_sample(df, "g", 10L, seed = 42L)
  expect_identical(res1, res2)
})

test_that("stratified_sample: rows in original order", {
  df  <- data.frame(g = rep("A", 50), row_num = 1:50)
  res <- mysterycall_stratified_sample(df, "g", 10L, seed = 1L)
  expect_true(all(diff(res$row_num) > 0))
})

test_that("stratified_sample: missing group_col errors", {
  df <- data.frame(x = 1:5)
  expect_error(mysterycall_stratified_sample(df, "g", 2L), "not found")
})

test_that("stratified_sample: non-positive n_per_group errors", {
  df <- data.frame(g = rep("A", 5), x = 1:5)
  expect_error(mysterycall_stratified_sample(df, "g", 0L), "positive")
})

# ── mysterycall_prepare_table1_vars ──────────────────────────────────────────

test_that("prepare_table1_vars: adds age_category from age_col", {
  df  <- data.frame(age = c(28, 35, 55))
  res <- mysterycall_prepare_table1_vars(df, age_col = "age")
  expect_true("age_category" %in% names(res))
  expect_equal(res$age_category, c("<30", "30-39", "50-59"))
})

test_that("prepare_table1_vars: imputes age from grad_year_col", {
  df  <- data.frame(gy = c(1990, 2000))
  res <- mysterycall_prepare_table1_vars(df, grad_year_col = "gy", ref_year = 2026L)
  expect_true("age_imputed"  %in% names(res))
  expect_true("age_category" %in% names(res))
})

test_that("prepare_table1_vars: gender_std normalises variants", {
  df  <- data.frame(g = c("Female", "M", "male", "f", "FEMALE"))
  res <- mysterycall_prepare_table1_vars(df, gender_col = "g")
  expect_equal(res$gender_std, c("Female","Male","Male","Female","Female"))
})

test_that("prepare_table1_vars: unknown gender → Unknown", {
  df  <- data.frame(g = c("NB", NA, ""))
  res <- mysterycall_prepare_table1_vars(df, gender_col = "g")
  expect_true(all(res$gender_std == "Unknown"))
})

test_that("prepare_table1_vars: missing columns are silently skipped", {
  df  <- data.frame(x = 1:3)
  res <- mysterycall_prepare_table1_vars(df, age_col = "age")
  expect_false("age_category" %in% names(res))
})

test_that("prepare_table1_vars: non-data-frame errors", {
  expect_error(mysterycall_prepare_table1_vars(list()), "data frame")
})

test_that("prepare_table1_vars: setting_col passed through as setting_std", {
  df  <- data.frame(setting = c("Academic","Private Practice"))
  res <- mysterycall_prepare_table1_vars(df, setting_col = "setting")
  expect_equal(res$setting_std, df$setting)
})
