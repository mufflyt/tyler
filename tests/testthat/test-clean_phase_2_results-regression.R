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

test_that("rename_columns_by_substring falls back to substring matches", {
  raw <- data.frame(
    physician_information_notes = c("Jane"),
    stringsAsFactors = FALSE
  )

  renamed <- rename_columns_by_substring(
    raw,
    target_strings = "physician_information",
    new_names = "physician_info"
  )

  expect_true("physician_info" %in% names(renamed))
})

test_that("rename_columns_by_substring errors on ambiguous substring matches", {
  raw <- data.frame(
    physician_information_1 = c("A"),
    physician_information_2 = c("B"),
    stringsAsFactors = FALSE
  )

  expect_error(
    rename_columns_by_substring(
      raw,
      target_strings = "physician_information",
      new_names = "physician_info"
    ),
    "Multiple columns matched"
  )
})
