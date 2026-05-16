# tests/testthat/test-join-safety.R
#
# Unit tests for:
#   mysterycall_assert_unique_keys()
#   mysterycall_safe_left_join()
#   mysterycall_safe_inner_join()
#   mysterycall_safe_semi_join()
#   mysterycall_safe_anti_join()

library(testthat)

# ── fixtures ──────────────────────────────────────────────────────────────────

# Three-row left table (physicians roster).
.left3 <- function() {
  data.frame(
    npi       = c("A", "B", "C"),
    specialty = c("OB", "GYN", "REI"),
    stringsAsFactors = FALSE
  )
}

# Two-row right table; matches "A" and "B" only (C is unmatched).
.right2 <- function() {
  data.frame(
    npi   = c("A", "B"),
    state = c("CO", "TX"),
    stringsAsFactors = FALSE
  )
}

# Right table with a duplicate key.
.right_dup <- function() {
  data.frame(
    npi   = c("A", "A", "B"),
    state = c("CO", "NY", "TX"),
    stringsAsFactors = FALSE
  )
}

# Helper: run left join without coverage guard (min_coverage = 0) and no report.
.slj <- function(left, right, by = "npi",
                 expect_unique_right = TRUE,
                 min_coverage = 0,
                 max_duplication = NULL) {
  suppressMessages(
    mysterycall_safe_left_join(
      left, right, by = by,
      expect_unique_right = expect_unique_right,
      min_coverage        = min_coverage,
      max_duplication     = max_duplication,
      write_report        = FALSE
    )
  )
}

# Helper: unset an env var, run expr, restore on exit.
.with_env <- function(key, value, expr) {
  old <- Sys.getenv(key, unset = "")
  Sys.setenv(key)
  on.exit({
    if (nzchar(old)) do.call(Sys.setenv, setNames(list(old), key))
    else Sys.unsetenv(key)
  }, add = TRUE)
  Sys.setenv(key)
  do.call(Sys.setenv, setNames(list(value), key))
  force(expr)
}

# ── 1. mysterycall_assert_unique_keys ─────────────────────────────────────────

test_that("assert_unique_keys: unique keys → returns .data invisibly", {
  df <- data.frame(npi = c("A", "B", "C"), stringsAsFactors = FALSE)
  result <- mysterycall_assert_unique_keys(df, "npi")
  expect_equal(result, df)
})

test_that("assert_unique_keys: duplicate keys, dedupe=FALSE → informative error", {
  df <- data.frame(npi = c("A", "B", "A"), value = 1:3,
                   stringsAsFactors = FALSE)
  expect_error(
    mysterycall_assert_unique_keys(df, "npi", dedupe = FALSE),
    "expected unique key"
  )
})

test_that("assert_unique_keys: duplicate keys error cites the column name", {
  df <- data.frame(npi = c("A", "B", "A"), stringsAsFactors = FALSE)
  expect_error(
    mysterycall_assert_unique_keys(df, "npi"),
    "npi"
  )
})

test_that("assert_unique_keys: dedupe=TRUE removes duplicates, keeps first row", {
  df <- data.frame(npi = c("A", "B", "A"), value = c(10L, 20L, 99L),
                   stringsAsFactors = FALSE)
  result <- suppressMessages(mysterycall_assert_unique_keys(df, "npi", dedupe = TRUE))
  expect_equal(nrow(result), 2L)
  # "A" row from position 1 (value=10) should be kept, not position 3 (value=99)
  expect_equal(result$value[result$npi == "A"], 10L)
})

test_that("assert_unique_keys: multi-column key checked jointly", {
  df <- data.frame(
    npi  = c("A", "A", "B"),
    year = c(2020L, 2021L, 2020L),
    val  = 1:3,
    stringsAsFactors = FALSE
  )
  # (npi, year) pairs are unique — should pass
  result <- mysterycall_assert_unique_keys(df, c("npi", "year"))
  expect_equal(nrow(result), 3L)
})

test_that("assert_unique_keys: multi-column duplicate → error", {
  df <- data.frame(
    npi  = c("A", "A"),
    year = c(2020L, 2020L),
    stringsAsFactors = FALSE
  )
  expect_error(
    mysterycall_assert_unique_keys(df, c("npi", "year")),
    "expected unique key"
  )
})

test_that("assert_unique_keys: key column not found → informative error", {
  df <- data.frame(npi = "A", stringsAsFactors = FALSE)
  expect_error(
    mysterycall_assert_unique_keys(df, "nonexistent_col"),
    "not found"
  )
})

test_that("assert_unique_keys: non-data-frame → error", {
  expect_error(
    mysterycall_assert_unique_keys("not a df", "npi"),
    "data frame"
  )
})

test_that("assert_unique_keys: empty key_cols → error", {
  df <- data.frame(npi = "A", stringsAsFactors = FALSE)
  expect_error(
    mysterycall_assert_unique_keys(df, character(0)),
    "non-empty"
  )
})

# ── 2. mysterycall_safe_left_join — basic correctness ─────────────────────────

test_that("safe_left_join: preserves all left rows (standard left-join semantics)", {
  result <- .slj(.left3(), .right2())
  expect_equal(nrow(result), 3L)
})

test_that("safe_left_join: matched rows have right-table columns", {
  result <- .slj(.left3(), .right2())
  expect_equal(result$state[result$npi == "A"], "CO")
  expect_equal(result$state[result$npi == "B"], "TX")
})

test_that("safe_left_join: unmatched left rows get NA in right columns", {
  result <- .slj(.left3(), .right2())
  expect_true(is.na(result$state[result$npi == "C"]))
})

test_that("safe_left_join: left columns are always present", {
  result <- .slj(.left3(), .right2())
  expect_true("specialty" %in% names(result))
  expect_equal(result$specialty[result$npi == "C"], "REI")
})

test_that("safe_left_join: 100% coverage — no error or warning", {
  left  <- data.frame(npi = c("A", "B"), stringsAsFactors = FALSE)
  right <- data.frame(npi = c("A", "B"), state = c("CO", "TX"),
                      stringsAsFactors = FALSE)
  expect_no_error(suppressMessages(
    mysterycall_safe_left_join(left, right, by = "npi",
                               min_coverage = 0.98,
                               write_report = FALSE)
  ))
})

# ── 3. mysterycall_safe_left_join — coverage enforcement ──────────────────────

test_that("safe_left_join: coverage below threshold → error with pct in message", {
  # 2 of 3 left rows match → coverage = 66.7%, below default 0.98
  expect_error(
    suppressMessages(
      mysterycall_safe_left_join(
        .left3(), .right2(), by = "npi",
        min_coverage = 0.98,
        write_report = FALSE
      )
    ),
    regexp = "coverage.*below|below.*threshold",
    ignore.case = TRUE
  )
})

test_that("safe_left_join: min_coverage=0 → never errors on coverage", {
  expect_no_error(.slj(.left3(), .right2(), min_coverage = 0))
})

test_that("safe_left_join: error message names left and right labels", {
  expect_error(
    suppressMessages(
      mysterycall_safe_left_join(
        .left3(), .right2(), by = "npi",
        label_left   = "physicians",
        label_right  = "demographics",
        min_coverage = 0.98,
        write_report = FALSE
      )
    ),
    regexp = "physicians"
  )
})

# ── 4. mysterycall_safe_left_join — right-side uniqueness ─────────────────────

test_that("safe_left_join: duplicate right key, expect_unique_right=TRUE → error", {
  expect_error(
    suppressMessages(
      mysterycall_safe_left_join(
        .left3(), .right_dup(), by = "npi",
        expect_unique_right = TRUE,
        min_coverage        = 0,
        write_report        = FALSE
      )
    ),
    regexp = "expected unique key"
  )
})

test_that("safe_left_join: duplicate right key, expect_unique_right=FALSE → allowed", {
  expect_no_error(suppressMessages(
    mysterycall_safe_left_join(
      .left3(), .right_dup(), by = "npi",
      expect_unique_right = FALSE,
      min_coverage        = 0,
      write_report        = FALSE
    )
  ))
})

# ── 5. mysterycall_safe_left_join — row-multiplication guard ──────────────────

test_that("safe_left_join: row multiplication beyond max_duplication → error", {
  # right has 2 rows for "A" → output has 4 rows from 3 input = 1.33x
  expect_error(
    suppressMessages(
      mysterycall_safe_left_join(
        .left3(), .right_dup(), by = "npi",
        expect_unique_right = FALSE,
        min_coverage        = 0,
        max_duplication     = 1.01,
        write_report        = FALSE
      )
    ),
    regexp = "duplication|exceed",
    ignore.case = TRUE
  )
})

# ── 6. mysterycall_safe_left_join — key-type harmonisation ────────────────────

test_that("safe_left_join: integer vs character key → coerced to character, join succeeds", {
  left  <- data.frame(npi = c(1L, 2L, 3L), val = letters[1:3],
                      stringsAsFactors = FALSE)
  right <- data.frame(npi = c("1", "2"), state = c("CO", "TX"),
                      stringsAsFactors = FALSE)
  result <- suppressMessages(
    mysterycall_safe_left_join(left, right, by = "npi",
                               min_coverage = 0,
                               write_report = FALSE)
  )
  expect_equal(nrow(result), 3L)
  expect_equal(result$state[result$val == "a"], "CO")
})

# ── 7. mysterycall_safe_left_join — env-var override ─────────────────────────

test_that("safe_left_join: JOIN_MIN_COVERAGE env var lowers threshold", {
  old <- Sys.getenv("JOIN_MIN_COVERAGE", unset = "")
  Sys.setenv(JOIN_MIN_COVERAGE = "0.50")
  on.exit({
    if (nzchar(old)) Sys.setenv(JOIN_MIN_COVERAGE = old)
    else Sys.unsetenv("JOIN_MIN_COVERAGE")
  }, add = TRUE)

  # 2/3 matched = 66.7% — should pass with 0.50 threshold
  expect_no_error(suppressMessages(
    mysterycall_safe_left_join(
      .left3(), .right2(), by = "npi",
      write_report = FALSE
    )
  ))
})

test_that("safe_left_join: explicit min_coverage overrides env var", {
  old <- Sys.getenv("JOIN_MIN_COVERAGE", unset = "")
  Sys.setenv(JOIN_MIN_COVERAGE = "0.10")   # very permissive env var
  on.exit({
    if (nzchar(old)) Sys.setenv(JOIN_MIN_COVERAGE = old)
    else Sys.unsetenv("JOIN_MIN_COVERAGE")
  }, add = TRUE)

  # explicit 0.98 should still error (66.7% < 98%)
  expect_error(
    suppressMessages(
      mysterycall_safe_left_join(.left3(), .right2(), by = "npi",
                                 min_coverage = 0.98,
                                 write_report = FALSE)
    ),
    regexp = "coverage.*below|below.*threshold",
    ignore.case = TRUE
  )
})

# ── 8. mysterycall_safe_left_join — input validation ─────────────────────────

test_that("safe_left_join: missing `by` → informative error", {
  expect_error(
    mysterycall_safe_left_join(.left3(), .right2(), by = character(0)),
    regexp = "`by` is required"
  )
})

test_that("safe_left_join: left is not a data frame → error", {
  expect_error(
    mysterycall_safe_left_join("not a df", .right2(), by = "npi"),
    regexp = "`left` must be a data frame"
  )
})

test_that("safe_left_join: right is not a data frame → error", {
  expect_error(
    mysterycall_safe_left_join(.left3(), list(npi = "A"), by = "npi"),
    regexp = "`right` must be a data frame"
  )
})

test_that("safe_left_join: label_left must be length-1 string", {
  expect_error(
    mysterycall_safe_left_join(.left3(), .right2(), by = "npi",
                               label_left = c("a", "b")),
    regexp = "`label_left` must be a string"
  )
})

# ── 9. mysterycall_safe_inner_join ────────────────────────────────────────────

test_that("safe_inner_join: returns only matched rows", {
  result <- suppressMessages(
    mysterycall_safe_inner_join(
      .left3(), .right2(), by = "npi",
      min_coverage = 0,
      write_report = FALSE
    )
  )
  expect_equal(nrow(result), 2L)
  expect_true(all(result$npi %in% c("A", "B")))
})

test_that("safe_inner_join: has both left and right columns", {
  result <- suppressMessages(
    mysterycall_safe_inner_join(
      .left3(), .right2(), by = "npi",
      min_coverage = 0,
      write_report = FALSE
    )
  )
  expect_true("specialty" %in% names(result))
  expect_true("state"     %in% names(result))
})

test_that("safe_inner_join: coverage below threshold → error", {
  expect_error(
    suppressMessages(
      mysterycall_safe_inner_join(
        .left3(), .right2(), by = "npi",
        min_coverage = 0.90,
        write_report = FALSE
      )
    ),
    regexp = "coverage.*below|below.*threshold",
    ignore.case = TRUE
  )
})

test_that("safe_inner_join: expect_unique_both=FALSE allows duplicate keys", {
  # right has duplicates; inner join should succeed with expect_unique_both=FALSE
  expect_no_error(suppressMessages(
    mysterycall_safe_inner_join(
      .left3(), .right_dup(), by = "npi",
      expect_unique_both = FALSE,
      min_coverage       = 0,
      write_report       = FALSE
    )
  ))
})

test_that("safe_inner_join: missing `by` → error", {
  expect_error(
    mysterycall_safe_inner_join(.left3(), .right2(), by = character(0)),
    regexp = "`by` is required"
  )
})

# ── 10. mysterycall_safe_semi_join ────────────────────────────────────────────

test_that("safe_semi_join: returns left rows that have a match, no right columns", {
  result <- suppressMessages(
    mysterycall_safe_semi_join(
      .left3(), .right2(), by = "npi",
      min_coverage = 0,
      write_report = FALSE
    )
  )
  expect_equal(nrow(result), 2L)
  # No right-side column "state" should appear
  expect_false("state" %in% names(result))
  # Left-side columns intact
  expect_true("specialty" %in% names(result))
})

test_that("safe_semi_join: keep rate below threshold → error", {
  # 2/3 kept = 66.7%
  expect_error(
    suppressMessages(
      mysterycall_safe_semi_join(
        .left3(), .right2(), by = "npi",
        min_coverage = 0.90,
        write_report = FALSE
      )
    ),
    regexp = "below.*threshold",
    ignore.case = TRUE
  )
})

test_that("safe_semi_join: min_coverage=0 → always passes", {
  expect_no_error(suppressMessages(
    mysterycall_safe_semi_join(
      .left3(), .right2(), by = "npi",
      min_coverage = 0,
      write_report = FALSE
    )
  ))
})

test_that("safe_semi_join: missing `by` → error", {
  expect_error(
    mysterycall_safe_semi_join(.left3(), .right2(), by = character(0)),
    "`by` is required"
  )
})

# ── 11. mysterycall_safe_anti_join ────────────────────────────────────────────

test_that("safe_anti_join: returns left rows with NO match in right", {
  result <- suppressMessages(
    mysterycall_safe_anti_join(
      .left3(), .right2(), by = "npi",
      write_report = FALSE
    )
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$npi, "C")
})

test_that("safe_anti_join: no right columns in output", {
  result <- suppressMessages(
    mysterycall_safe_anti_join(
      .left3(), .right2(), by = "npi",
      write_report = FALSE
    )
  )
  expect_false("state" %in% names(result))
})

test_that("safe_anti_join: max_matched exceeded → error", {
  # 2/3 matched (excluded) = 66.7% > max_matched 0.50
  expect_error(
    suppressMessages(
      mysterycall_safe_anti_join(
        .left3(), .right2(), by = "npi",
        max_matched  = 0.50,
        write_report = FALSE
      )
    ),
    regexp = "exceeds max_matched",
    ignore.case = TRUE
  )
})

test_that("safe_anti_join: max_matched=1.0 (default) → never errors on exclusion", {
  expect_no_error(suppressMessages(
    mysterycall_safe_anti_join(
      .left3(), .right2(), by = "npi",
      max_matched  = 1.0,
      write_report = FALSE
    )
  ))
})

test_that("safe_anti_join: missing `by` → error", {
  expect_error(
    mysterycall_safe_anti_join(.left3(), .right2(), by = character(0)),
    "`by` is required"
  )
})

# ── 12. Cross-column key mapping ──────────────────────────────────────────────

test_that("safe_left_join: named-vector `by` maps different column names", {
  left  <- data.frame(npi  = c("A", "B", "C"), specialty = c("OB", "GYN", "REI"),
                      stringsAsFactors = FALSE)
  right <- data.frame(provider_npi = c("A", "B"), state = c("CO", "TX"),
                      stringsAsFactors = FALSE)

  result <- suppressMessages(
    mysterycall_safe_left_join(
      left, right, by = c("npi" = "provider_npi"),
      min_coverage = 0,
      write_report = FALSE
    )
  )
  expect_equal(nrow(result), 3L)
  expect_equal(result$state[result$npi == "A"], "CO")
})

# ── 13. Empty-table edge cases ────────────────────────────────────────────────

test_that("safe_left_join: zero-row left table → zero-row output, no error", {
  left_empty <- data.frame(npi = character(0), specialty = character(0),
                           stringsAsFactors = FALSE)
  result <- suppressMessages(suppressWarnings(
    mysterycall_safe_left_join(left_empty, .right2(), by = "npi",
                               min_coverage = 0,
                               write_report = FALSE)
  ))
  expect_equal(nrow(result), 0L)
})

test_that("safe_inner_join: zero-row right table → zero-row output, warning emitted", {
  right_empty <- data.frame(npi = character(0), state = character(0),
                            stringsAsFactors = FALSE)
  expect_warning(
    suppressMessages(
      mysterycall_safe_inner_join(.left3(), right_empty, by = "npi",
                                  min_coverage = 0,
                                  write_report = FALSE)
    ),
    regexp = "0 rows"
  )
})
