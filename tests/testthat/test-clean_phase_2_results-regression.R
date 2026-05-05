library(testthat)

test_that("rename_columns_by_substring renames case-insensitive exact matches", {
  raw <- data.frame(
    PhysicianInformation = c("Jane"),
    stringsAsFactors = FALSE
  )

  renamed <- rename_columns_by_substring(
    raw,
    target_strings = "physicianinformation",
    new_names = "physician_info"
  )

  expect_true("physician_info" %in% names(renamed))
  expect_false("PhysicianInformation" %in% names(renamed))
})
