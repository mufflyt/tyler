library(testthat)

test_that("mysterycall_rename_columns renames case-insensitive exact matches", {
  raw <- data.frame(
    PhysicianInformation = c("Jane"),
    stringsAsFactors = FALSE
  )

  renamed <- mysterycall_rename_columns(
    raw,
    target_strings = "physicianinformation",
    new_names = "physician_info"
  )

  expect_true("physician_info" %in% names(renamed))
  expect_false("PhysicianInformation" %in% names(renamed))
})

test_that("mysterycall_rename_columns falls back to substring matches", {
  raw <- data.frame(
    physician_information_notes = c("Jane"),
    stringsAsFactors = FALSE
  )

  renamed <- mysterycall_rename_columns(
    raw,
    target_strings = "physician_information",
    new_names = "physician_info"
  )

  expect_true("physician_info" %in% names(renamed))
})

test_that("mysterycall_rename_columns warns on ambiguous substring matches and keeps first", {
  raw <- data.frame(
    physician_information_1 = c("A"),
    physician_information_2 = c("B"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- mysterycall_rename_columns(
      raw,
      target_strings = "physician_information",
      new_names = "physician_info"
    ),
    "Multiple columns matched"
  )
  expect_true("physician_info" %in% names(result))
})
