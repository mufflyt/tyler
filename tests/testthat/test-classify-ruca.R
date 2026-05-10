# ── mysterycall_classify_ruca ─────────────────────────────────────────────────

test_that("classify_ruca: codes 1-3 → Urban", {
  expect_equal(mysterycall_classify_ruca(c(1, 2, 3)), c("Urban", "Urban", "Urban"))
})

test_that("classify_ruca: codes 4-6 → Suburban", {
  expect_equal(mysterycall_classify_ruca(c(4, 5, 6)), c("Suburban", "Suburban", "Suburban"))
})

test_that("classify_ruca: codes > 6 → Rural", {
  expect_equal(mysterycall_classify_ruca(c(7, 10, 10.6)), c("Rural", "Rural", "Rural"))
})

test_that("classify_ruca: NA → 'Unknown'", {
  expect_equal(mysterycall_classify_ruca(NA_real_), "Unknown")
})

test_that("classify_ruca: mixed vector", {
  expect_equal(
    mysterycall_classify_ruca(c(1, 4, 7, NA)),
    c("Urban", "Suburban", "Rural", "Unknown")
  )
})

test_that("classify_ruca: custom na_label", {
  expect_equal(mysterycall_classify_ruca(NA_real_, na_label = "Missing"), "Missing")
})

test_that("classify_ruca: custom labels", {
  res <- mysterycall_classify_ruca(c(1, 5, 8), labels = c("Metro", "Semi", "Country"))
  expect_equal(res, c("Metro", "Semi", "Country"))
})

test_that("classify_ruca: as_factor returns ordered factor", {
  res <- mysterycall_classify_ruca(c(1, 5, 8, NA), as_factor = TRUE)
  expect_s3_class(res, "factor")
  expect_true(is.ordered(res))
})

test_that("classify_ruca: factor levels include na_label", {
  res <- mysterycall_classify_ruca(c(1, NA), as_factor = TRUE)
  expect_true("Unknown" %in% levels(res))
})

test_that("classify_ruca: non-numeric input errors", {
  expect_error(mysterycall_classify_ruca(c("1", "2")), "numeric")
})

test_that("classify_ruca: suburban_max <= urban_max errors", {
  expect_error(mysterycall_classify_ruca(1:5, urban_max = 6, suburban_max = 3), "suburban_max")
})

test_that("classify_ruca: labels wrong length errors", {
  expect_error(mysterycall_classify_ruca(1, labels = c("Urban", "Rural")), "length 3")
})

test_that("classify_ruca: boundary at urban_max is Urban", {
  expect_equal(mysterycall_classify_ruca(3, urban_max = 3), "Urban")
})

test_that("classify_ruca: one above urban_max is Suburban", {
  expect_equal(mysterycall_classify_ruca(4, urban_max = 3), "Suburban")
})

test_that("classify_ruca: boundary at suburban_max is Suburban", {
  expect_equal(mysterycall_classify_ruca(6, suburban_max = 6), "Suburban")
})

test_that("classify_ruca: one above suburban_max is Rural", {
  expect_equal(mysterycall_classify_ruca(7, suburban_max = 6), "Rural")
})
