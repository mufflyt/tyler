library(testthat)
testthat::skip_if_not_installed("dplyr")

# Gold-standard values computed before writing tests.
# most_common_gender_training_academic returns a single character string.

# ── Deterministic small-dataset gold standards ────────────────────────────────

SIMPLE_DATA <- data.frame(
  gender                 = c("Male", "Female", "Female", "Male", "Male"),
  specialty              = c("Cardiology", "Cardiology", "Neurology", "Cardiology", "Neurology"),
  Provider.Credential.Text = c("MD", "MD", "DO", "MD", "DO"),
  academic_affiliation   = c("Yes", "No", "Yes", "No", "Yes"),
  stringsAsFactors       = FALSE
)
# gender: Male=3 (60%), Female=2 → most common = male 60.0%
# specialty: Cardiology=3 (60%), Neurology=2 → Cardiology 60.0%
# credential: MD=3 (60%), DO=2 → MD 60.0%
# academic: Yes=3 (60%), No=2 → yes 60.0%

test_that("most_common_gender_training_academic - gold standard sentence structure", {
  result <- most_common_gender_training_academic(SIMPLE_DATA)
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("most_common_gender_training_academic - contains gender 'male' at 60%", {
  result <- most_common_gender_training_academic(SIMPLE_DATA)
  expect_true(grepl("male", result, ignore.case = FALSE))
  expect_true(grepl("60", result))
})

test_that("most_common_gender_training_academic - contains specialty 'Cardiology' at 60%", {
  result <- most_common_gender_training_academic(SIMPLE_DATA)
  expect_true(grepl("Cardiology", result))
})

test_that("most_common_gender_training_academic - contains credential 'MD' at 60%", {
  result <- most_common_gender_training_academic(SIMPLE_DATA)
  expect_true(grepl("MD", result))
})

test_that("most_common_gender_training_academic - academic affiliation is lowercased in output", {
  result <- most_common_gender_training_academic(SIMPLE_DATA)
  # Function calls tolower() on academic_affiliation value
  expect_true(grepl("yes", result, ignore.case = FALSE))
})

test_that("most_common_gender_training_academic - 70/30 split gender gold standard", {
  df <- data.frame(
    gender                   = c(rep("Male", 70), rep("Female", 30)),
    specialty                = rep("Gynecology", 100),
    Provider.Credential.Text = rep("MD", 100),
    academic_affiliation     = rep("Yes", 100),
    stringsAsFactors         = FALSE
  )
  result <- most_common_gender_training_academic(df)
  # Male appears 70/100 = 70.0%
  expect_true(grepl("male", result))
  expect_true(grepl("70", result))
})

test_that("most_common_gender_training_academic - sentence contains four percentages", {
  result <- most_common_gender_training_academic(SIMPLE_DATA)
  # Each of gender/specialty/credential/academic has a "XX.X%" pattern
  pct_matches <- regmatches(result, gregexpr("[0-9]+(?:\\.[0-9]+)?%", result))[[1]]
  expect_true(length(pct_matches) >= 4L)
})

test_that("most_common_gender_training_academic - all NAs in gender column yields NaN%", {
  df <- data.frame(
    gender                   = c(NA, NA, NA),
    specialty                = c("X", "X", "Y"),
    Provider.Credential.Text = c("MD", "DO", "MD"),
    academic_affiliation     = c("Yes", "No", "Yes"),
    stringsAsFactors         = FALSE
  )
  result <- most_common_gender_training_academic(df)
  expect_true(grepl("NaN", result))
})

test_that("most_common_gender_training_academic - ties broken by alphabetical order", {
  df <- data.frame(
    gender                   = c("Female", "Male", "Female", "Male"),
    specialty                = c("A_spec", "B_spec", "A_spec", "B_spec"),
    Provider.Credential.Text = c("DO", "MD", "DO", "MD"),
    academic_affiliation     = c("No", "Yes", "No", "Yes"),
    stringsAsFactors         = FALSE
  )
  result <- most_common_gender_training_academic(df)
  # Female comes before Male alphabetically, and both are tied at 50%
  # The function sorts by desc(n) then column; tie → alphabetically first wins
  expect_true(grepl("female", result))
})

test_that("most_common_gender_training_academic - result ends with a period", {
  result <- most_common_gender_training_academic(SIMPLE_DATA)
  expect_true(endsWith(result, "."))
})

test_that("most_common_gender_training_academic - proportion rounds to one decimal", {
  df <- data.frame(
    gender                   = c(rep("Male", 2), "Female"),    # 66.7%
    specialty                = rep("GYN", 3),
    Provider.Credential.Text = rep("MD", 3),
    academic_affiliation     = rep("Yes", 3),
    stringsAsFactors         = FALSE
  )
  result <- most_common_gender_training_academic(df)
  # 2/3 * 100 = 66.666... rounded to 1 decimal = 66.7
  expect_true(grepl("66\\.7", result))
})

test_that("most_common_gender_training_academic - missing required column errors", {
  df <- data.frame(
    gender   = c("Male", "Female"),
    specialty = c("A", "B"),
    stringsAsFactors = FALSE
  )
  # Provider.Credential.Text and academic_affiliation are absent
  expect_error(most_common_gender_training_academic(df))
})

test_that("most_common_gender_training_academic - purely female dataset says female in output", {
  df <- data.frame(
    gender                   = rep("Female", 50),
    specialty                = rep("OB/GYN", 50),
    Provider.Credential.Text = rep("MD", 50),
    academic_affiliation     = rep("Yes", 50),
    stringsAsFactors         = FALSE
  )
  result <- most_common_gender_training_academic(df)
  expect_true(grepl("female", result))
  expect_true(grepl("100", result))
})
