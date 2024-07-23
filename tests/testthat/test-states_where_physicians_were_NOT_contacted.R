library(testthat)
library(dplyr)

test_that("Function handles default all_states correctly", {
  # Test data
  filtered_data <- data.frame(state = c("California", "New York", "Texas"))

  # Expected result
  expected <- paste0(
    "A total of unique physicians were identified in the dataset and were successfully contacted (i.e., with a recorded wait time for an appointment) in ",
    3,
    " states including the District of Columbia. The excluded states include ",
    paste(
      c("Alabama", "Alaska", "Arizona", "Arkansas", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii",
        "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
        "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
        "New Mexico", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
        "South Carolina", "South Dakota", "Tennessee", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
        "Wisconsin", "Wyoming"), collapse = ", "), " and District of Columbia."
  )


# Run test
result <- states_where_physicians_were_NOT_contacted(filtered_data)
expect_equal(result, expected)
})

test_that("Function handles no states correctly", {
  # Test data
  filtered_data <- data.frame(state = character())

  # Expected result
  expected <- paste0(
    "A total of unique physicians were identified in the dataset and were successfully contacted (i.e., with a recorded wait time for an appointment) in ",
    0,
    " states including the District of Columbia. The excluded states include ",
    paste(c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
            "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
            "West Virginia", "Wisconsin", "Wyoming"), collapse = ", "), " and District of Columbia."
  )

  # Run test
  result <- states_where_physicians_were_NOT_contacted(filtered_data)
  expect_equal(result, expected)
})
