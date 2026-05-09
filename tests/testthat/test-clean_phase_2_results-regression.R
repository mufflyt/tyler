library(testthat)

test_that("tyler_rename_columns renames case-insensitive exact matches", {
  raw <- data.frame(
    PhysicianInformation = c("Jane"),
    stringsAsFactors = FALSE
  )

  renamed <- tyler_rename_columns(
    raw,
    target_strings = "physicianinformation",
    new_names = "physician_info"
  )

  expect_true("physician_info" %in% names(renamed))
  expect_false("PhysicianInformation" %in% names(renamed))
})

test_that("tyler_rename_columns falls back to substring matches", {
  raw <- data.frame(
    physician_information_notes = c("Jane"),
    stringsAsFactors = FALSE
  )

  renamed <- tyler_rename_columns(
    raw,
    target_strings = "physician_information",
    new_names = "physician_info"
  )

  expect_true("physician_info" %in% names(renamed))
})

test_that("tyler_rename_columns errors on ambiguous substring matches", {
  raw <- data.frame(
    physician_information_1 = c("A"),
    physician_information_2 = c("B"),
    stringsAsFactors = FALSE
  )

  expect_error(
    tyler_rename_columns(
      raw,
      target_strings = "physician_information",
      new_names = "physician_info"
    ),
    "Multiple columns matched"
  )
})
