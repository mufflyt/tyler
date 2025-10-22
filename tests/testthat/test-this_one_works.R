library(testthat)

# The debugging helper should now only signal deprecation and return an empty
# tibble for backwards compatibility.
test_that("scrape_physicians_data_with_tor() is deprecated", {
  expect_warning(
    result <- scrape_physicians_data_with_tor(9045998, 9045999, 9150),
    class = "lifecycle_warning_deprecated"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
