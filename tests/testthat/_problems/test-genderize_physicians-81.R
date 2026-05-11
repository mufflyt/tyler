# Extracted from test-genderize_physicians.R:81

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
testthat::skip_if_not_installed("readr")
testthat::skip_if_not_installed("dplyr")
library(readr)
library(dplyr)
make_genderize_response <- function(payload, status = 200L) {
  body <- if (is.null(payload)) "null" else jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  structure(
    list(
      url = "https://api.genderize.io/",
      status_code = status,
      headers = list(),
      all_headers = list(),
      cookies = structure(list(), class = "set_cookies"),
      content = charToRaw(body),
      times = numeric()
    ),
    class = "response"
  )
}
expect_prediction_columns <- function(x) {
  expect_s3_class(x, "tbl_df")
  expect_setequal(colnames(x), c("first_name", "gender", "probability", "count"))
}

# test -------------------------------------------------------------------------
with_mocked_bindings(
    GET = function(url, query, ...) make_genderize_response(list(error = "Too Many Requests"), status = 429L),
    .package = "httr",
    code = expect_error(mysterycall:::genderize_fetch("Ava"), "Genderize.io request failed")
  )
