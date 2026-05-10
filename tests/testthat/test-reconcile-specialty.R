# ── mysterycall_reconcile_specialty ──────────────────────────────────────────

make_spec_df <- function() {
  data.frame(
    primary   = c("Otolaryngology", "General", NA,          "Rhinology"),
    secondary = c(NA,              "Laryngology", "Rhinology", "General"),
    stringsAsFactors = FALSE
  )
}

test_that("reconcile_specialty: adds source and confidence columns", {
  df  <- make_spec_df()
  res <- mysterycall_reconcile_specialty(df, "primary", "secondary")
  expect_true("specialty_source"     %in% names(res))
  expect_true("specialty_confidence" %in% names(res))
})

test_that("reconcile_specialty: tier 1 rows → high confidence, source = primary_col", {
  df  <- make_spec_df()
  res <- mysterycall_reconcile_specialty(df, "primary", "secondary")
  t1  <- res$specialty_confidence == "high"
  expect_true(all(res$specialty_source[t1] == "primary"))
})

test_that("reconcile_specialty: tier 2 rows use secondary value", {
  df  <- make_spec_df()
  res <- mysterycall_reconcile_specialty(df, "primary", "secondary")
  # row 2: primary = "General" → tier 2; secondary = "Laryngology"
  expect_equal(res$primary[[2L]], "Laryngology")
  expect_equal(res$specialty_confidence[[2L]], "medium")
})

test_that("reconcile_specialty: tier 2 source column = secondary_col name", {
  df  <- make_spec_df()
  res <- mysterycall_reconcile_specialty(df, "primary", "secondary")
  expect_equal(res$specialty_source[[2L]], "secondary")
})

test_that("reconcile_specialty: tier 3 → low confidence and default label", {
  df <- data.frame(primary = NA_character_, secondary = NA_character_,
                   stringsAsFactors = FALSE)
  res <- mysterycall_reconcile_specialty(df, "primary", "secondary")
  expect_equal(res$specialty_confidence, "low")
  expect_equal(res$primary, "General")
})

test_that("reconcile_specialty: tier 1 without secondary_col works", {
  df  <- data.frame(primary = c("Otolaryngology", NA), stringsAsFactors = FALSE)
  res <- mysterycall_reconcile_specialty(df, "primary")
  expect_equal(res$specialty_confidence[[1L]], "high")
  expect_equal(res$specialty_confidence[[2L]], "low")
})

test_that("reconcile_specialty: custom default label respected", {
  df  <- data.frame(primary = c("Unknown", "Otolaryngology"), stringsAsFactors = FALSE)
  res <- mysterycall_reconcile_specialty(df, "primary", default = "Unknown")
  expect_equal(res$specialty_confidence[[1L]], "low")
  expect_equal(res$specialty_confidence[[2L]], "high")
})

test_that("reconcile_specialty: custom audit column names", {
  df  <- data.frame(primary = "Otolaryngology", stringsAsFactors = FALSE)
  res <- mysterycall_reconcile_specialty(df, "primary",
                                          source_col = "src",
                                          confidence_col = "conf")
  expect_true("src"  %in% names(res))
  expect_true("conf" %in% names(res))
})

test_that("reconcile_specialty: missing primary_col errors", {
  df <- data.frame(x = 1)
  expect_error(mysterycall_reconcile_specialty(df, "primary"), "not found")
})

test_that("reconcile_specialty: non-data-frame errors", {
  expect_error(mysterycall_reconcile_specialty(list(), "primary"), "data frame")
})

test_that("reconcile_specialty: output has same nrow as input", {
  df  <- make_spec_df()
  res <- mysterycall_reconcile_specialty(df, "primary", "secondary")
  expect_equal(nrow(res), nrow(df))
})
