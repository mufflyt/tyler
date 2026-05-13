library(testthat)
testthat::skip_if_not_installed("readr")
testthat::skip_if_not_installed("npi")

# Sample data for testing
sample_data <- data.frame(
  first = c("John", "Jane", "Alex"),
  last = c("Doe", "Smith", "Brown"),
  stringsAsFactors = FALSE
)

mock_npi_search <- function(first_name, last_name, ...) {
  if (first_name == "John" && last_name == "Doe") {
    list(
      basic = data.frame(npi = "1234567890", first_name = "John", last_name = "Doe", stringsAsFactors = FALSE),
      taxonomies = data.frame(taxonomies_desc = c("Family Medicine", "Internal Medicine"), stringsAsFactors = FALSE)
    )
  } else if (first_name == "Jane" && last_name == "Smith") {
    list(
      basic = data.frame(npi = "2345678901", first_name = "Jane", last_name = "Smith", stringsAsFactors = FALSE),
      taxonomies = data.frame(taxonomies_desc = "Pediatrics", stringsAsFactors = FALSE)
    )
  } else if (first_name == "Alex" && last_name == "Brown") {
    list(
      basic = data.frame(npi = "3456789012", first_name = "Alex", last_name = "Brown", stringsAsFactors = FALSE),
      taxonomies = data.frame(taxonomies_desc = "Surgery", stringsAsFactors = FALSE)
    )
  } else {
    NULL
  }
}

mock_npi_flatten <- function(npi_obj, cols, ...) {
  if (!is.null(npi_obj)) {
    df <- cbind(npi_obj$basic, npi_obj$taxonomies)
    names(df)[names(df) == "first_name"] <- "basic_first_name"
    names(df)[names(df) == "last_name"] <- "basic_last_name"
    df$basic_credential <- "MD"
    df
  } else {
    NULL
  }
}

test_that("Processes data frame input correctly", {
  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    .package = "npi",
    code = {
      result <- search_and_process_npi(sample_data, notify = FALSE)
      expect_true(nrow(result) >= 3)
      expect_true("first_name" %in% colnames(result))
      expect_true("last_name" %in% colnames(result))
      expect_true("taxonomies_desc" %in% colnames(result))
    }
  )
})

test_that("Handles empty input data frame", {
  empty_data <- data.frame(first = character(), last = character(), stringsAsFactors = FALSE)
  result <- search_and_process_npi(empty_data, notify = FALSE)
  expect_equal(nrow(result), 0)
})

test_that("validates required columns", {
  expect_error(search_and_process_npi(data.frame(first = "A"), notify = FALSE), "last")
})

test_that("Handles invalid NPIs gracefully", {
  with_mocked_bindings(
    npi_search = function(...) NULL,
    npi_flatten = function(...) NULL,
    .package = "npi",
    code = {
      invalid_data <- data.frame(first = "Invalid", last = "Name", stringsAsFactors = FALSE)
      result <- search_and_process_npi(invalid_data, notify = FALSE)
      expect_equal(nrow(result), 0)
    }
  )
})

test_that("Accumulates results and resumes processing", {
  temp_accumulate <- file.path(tempdir(), "npi_accumulate.csv")
  temp_log <- file.path(tempdir(), "npi_progress.log")
  if (file.exists(temp_accumulate)) file.remove(temp_accumulate)
  if (file.exists(temp_log)) file.remove(temp_log)

  data <- data.frame(
    first = c("John", "Jane"),
    last = c("Doe", "Smith"),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    .package = "npi",
    code = {
      result_1 <- search_and_process_npi(
        data,
        accumulate_path = temp_accumulate,
        progress_log = temp_log,
        notify = FALSE
      )
      expect_true(file.exists(temp_accumulate))
      expect_true(file.exists(temp_log))
      accumulated <- readr::read_csv(temp_accumulate, show_col_types = FALSE)
      expect_true(nrow(accumulated) >= 2)

      result_2 <- search_and_process_npi(
        data[1, , drop = FALSE],
        accumulate_path = temp_accumulate,
        resume = TRUE,
        notify = FALSE
      )
      expect_equal(nrow(result_2), nrow(accumulated))
      expect_true(all(result_2$search_term %in% accumulated$search_term))
    }
  )
})

test_that("Processes various names correctly", {
  test_cases <- list(
    list(first = "John", last = "Doe"),
    list(first = "Jane", last = "Smith"),
    list(first = "Alex", last = "Brown"),
    list(first = "Invalid", last = "Name")
  )

  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    .package = "npi",
    code = {
      for (tc in test_cases) {
        data <- data.frame(first = tc$first, last = tc$last, stringsAsFactors = FALSE)
        result <- search_and_process_npi(data, notify = FALSE)
        if (tc$first == "Invalid") {
          expect_equal(nrow(result), 0)
        } else {
          expect_true(nrow(result) >= 1)
          expect_true("first_name" %in% colnames(result))
          expect_true("last_name" %in% colnames(result))
          expect_true("taxonomies_desc" %in% colnames(result))
        }
      }
    }
  )
})

test_that("Handles large datasets efficiently", {
  large_data <- data.frame(
    first = rep(c("John", "Jane", "Alex"), times = 100),
    last = rep(c("Doe", "Smith", "Brown"), times = 100),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    .package = "npi",
    code = {
      start_time <- Sys.time()
      result <- search_and_process_npi(large_data, notify = FALSE)
      expect_true(nrow(result) >= 300)
      expect_true("first_name" %in% colnames(result))
      expect_true("last_name" %in% colnames(result))
      expect_true("taxonomies_desc" %in% colnames(result))
      cat("Performance test duration:", as.numeric(Sys.time() - start_time), "\n")
    }
  )
})


test_that("Preserves query-to-NPI linkage fields", {
  with_mocked_bindings(
    npi_search = mock_npi_search,
    npi_flatten = mock_npi_flatten,
    .package = "npi",
    code = {
      data <- data.frame(first = "John", last = "Doe", stringsAsFactors = FALSE)
      result <- search_and_process_npi(data, notify = FALSE)
      expect_true(all(result$query_first_name == "John"))
      expect_true(all(result$query_last_name == "Doe"))
      expect_true(all(result$search_term == "John Doe"))
    }
  )
})
