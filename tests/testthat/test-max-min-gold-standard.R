library(testthat)

# ── tyler_max_table gold-standard tests ─────────────────────────────────────────────
# All expected values computed by hand or via independent Rscript before writing.

test_that("tyler_max_table - gold standard: B is most frequent in c(A,B,A,C,B,B)", {
  vec <- factor(c("A", "B", "A", "C", "B", "B"))
  # B=3, A=2, C=1 → most frequent is B
  expect_equal(tyler_max_table(vec), "B")
})

test_that("tyler_max_table - mult=TRUE returns only B when B is strictly most frequent", {
  vec <- factor(c("A", "B", "A", "C", "B", "B"))
  expect_equal(tyler_max_table(vec, mult = TRUE), "B")
})

test_that("tyler_max_table - three-way tie returns all three with mult=TRUE", {
  vec <- factor(c("A", "B", "C", "A", "B", "C"))
  # A=2, B=2, C=2
  result <- tyler_max_table(vec, mult = TRUE)
  expect_setequal(result, c("A", "B", "C"))
})

test_that("tyler_max_table - two-way tie returns alphabetically first without mult", {
  vec <- factor(c("X", "Y", "X", "Y"))
  # X=2, Y=2 → which.max returns first maximum → X (levels are sorted alphabetically)
  expect_equal(tyler_max_table(vec), "X")
})

test_that("tyler_max_table - single-element vector returns that element", {
  vec <- factor("Z")
  expect_equal(tyler_max_table(vec), "Z")
  expect_equal(tyler_max_table(vec, mult = TRUE), "Z")
})

test_that("tyler_max_table - empty factor returns character(0)", {
  expect_equal(tyler_max_table(factor(character(0))), character(0))
  expect_equal(tyler_max_table(factor(character(0)), mult = TRUE), character(0))
})

test_that("tyler_max_table - converts non-factor character vector to factor", {
  vec <- c("cat", "dog", "cat", "bird", "cat")
  expect_equal(tyler_max_table(vec), "cat")
})

test_that("tyler_max_table - numeric vector is coerced to factor correctly", {
  vec <- c(1, 2, 1, 3, 1, 2)
  # 1 appears 3 times → most frequent
  result <- tyler_max_table(vec)
  expect_equal(result, "1")
})

test_that("tyler_max_table - all same value returns that value", {
  vec <- factor(rep("alpha", 50))
  expect_equal(tyler_max_table(vec), "alpha")
  expect_equal(tyler_max_table(vec, mult = TRUE), "alpha")
})

test_that("tyler_max_table - taxonomy Classification: gold standard Clinic/Center n=63", {
  e <- new.env()
  data("taxonomy", package = "tyler", envir = e)
  result <- tyler_max_table(e$taxonomy$Classification)
  # Verified externally: Clinic/Center appears 63 times, more than any other
  expect_equal(result, "Clinic/Center")
})

test_that("tyler_max_table - with mult=TRUE, taxonomy most common is still Clinic/Center", {
  e <- new.env()
  data("taxonomy", package = "tyler", envir = e)
  result <- tyler_max_table(e$taxonomy$Classification, mult = TRUE)
  expect_true("Clinic/Center" %in% result)
})

test_that("tyler_max_table - result is always a character vector", {
  vec <- factor(c("A", "B", "A"))
  expect_type(tyler_max_table(vec), "character")
  expect_type(tyler_max_table(vec, mult = TRUE), "character")
})

# ── tyler_min_table gold-standard tests ─────────────────────────────────────────────

test_that("tyler_min_table - gold standard: C is least frequent in c(A,B,A,C,B,B)", {
  vec <- factor(c("A", "B", "A", "C", "B", "B"))
  # A=2, B=3, C=1 → least frequent is C
  expect_equal(tyler_min_table(vec), "C")
})

test_that("tyler_min_table - mult=TRUE returns C when C is strictly least frequent", {
  vec <- factor(c("A", "B", "A", "C", "B", "B"))
  expect_equal(tyler_min_table(vec, mult = TRUE), "C")
})

test_that("tyler_min_table - three-way tie returns all three with mult=TRUE", {
  vec <- factor(c("A", "B", "C", "A", "B", "C"))
  # A=2, B=2, C=2 → all tied for minimum
  result <- tyler_min_table(vec, mult = TRUE)
  expect_setequal(result, c("A", "B", "C"))
})

test_that("tyler_min_table - empty factor returns character(0)", {
  expect_equal(tyler_min_table(factor(character(0))), character(0))
})

test_that("tyler_min_table - non-factor input is coerced to factor and returns least frequent", {
  # The function coerces non-factors to factor before processing
  result <- tyler_min_table(c("a", "b", "a"))
  expect_equal(result, "b")  # "b" appears once, "a" appears twice
})

test_that("tyler_min_table - single element returns that element", {
  vec <- factor("Q")
  expect_equal(tyler_min_table(vec), "Q")
  expect_equal(tyler_min_table(vec, mult = TRUE), "Q")
})

test_that("tyler_min_table - all same: min and max are the same", {
  vec <- factor(rep("beta", 10))
  expect_equal(tyler_min_table(vec), tyler_max_table(vec))
})

test_that("tyler_min_table - taxonomy Classification: Acupuncturist appears just once (gold standard)", {
  e <- new.env()
  data("taxonomy", package = "tyler", envir = e)
  result <- tyler_min_table(factor(e$taxonomy$Classification), mult = TRUE)
  # Verified externally: multiple classifications appear exactly 1 time
  # Acupuncturist is one of them
  expect_true("Acupuncturist" %in% result)
  # All returned values should appear just once
  cls_counts <- table(e$taxonomy$Classification)
  min_count <- min(cls_counts)
  expect_equal(min_count, 1L)
  expect_true(all(cls_counts[result] == min_count))
})

test_that("tyler_min_table - result is always a character vector", {
  vec <- factor(c("A", "B", "A"))
  expect_type(tyler_min_table(vec), "character")
})

# ── tyler_max_table / tyler_min_table invariant relationship ───────────────────────────────

test_that("tyler_max_table result never appears in tyler_min_table result when distribution is unequal", {
  set.seed(99)
  # Generate unequal distribution so no ties span both max and min
  vec <- factor(c(rep("dominant", 20), rep("rare", 2), rep("mid", 8)))
  mx <- tyler_max_table(vec)
  mn <- tyler_min_table(vec, mult = TRUE)
  expect_false(mx %in% mn)
})

test_that("tyler_max_table and tyler_min_table agree when all levels equally frequent", {
  vec <- factor(c("A", "B", "C", "A", "B", "C"))
  mx <- tyler_max_table(vec, mult = TRUE)
  mn <- tyler_min_table(vec, mult = TRUE)
  expect_setequal(mx, mn)
})
