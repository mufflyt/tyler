# Sample data for testing
sample_data <- data.frame(
  first = c("John", "Jane", "Alex"),
  last = c("Doe", "Smith", "Brown"),
  stringsAsFactors = FALSE
)

# Mock functions to simulate npi::npi_search and npi::npi_flatten
mock_npi_search <- function(first_name, last_name) {
  cat("Mock search for:", first_name, last_name, "\n")
  if (first_name == "John" && last_name == "Doe") {
    return(list(
      basic = data.frame(
        first_name = "John",
        last_name = "Doe",
        stringsAsFactors = FALSE
      ),
      taxonomies = data.frame(
        taxonomies_desc = c("Family Medicine", "Internal Medicine"),
        stringsAsFactors = FALSE
      )
    ))
  } else if (first_name == "Jane" && last_name == "Smith") {
    return(list(
      basic = data.frame(
        first_name = "Jane",
        last_name = "Smith",
        stringsAsFactors = FALSE
      ),
      taxonomies = data.frame(
        taxonomies_desc = c("Pediatrics"),
        stringsAsFactors = FALSE
      )
    ))
  } else if (first_name == "Alex" && last_name == "Brown") {
    return(list(
      basic = data.frame(
        first_name = "Alex",
        last_name = "Brown",
        stringsAsFactors = FALSE
      ),
      taxonomies = data.frame(
        taxonomies_desc = c("Surgery"),
        stringsAsFactors = FALSE
      )
    ))
  } else {
    return(NULL)
  }
}

mock_npi_flatten <- function(npi_obj, cols) {
  cat("Mock flatten for NPI object\n")
  if (!is.null(npi_obj)) {
    return(cbind(npi_obj$basic, npi_obj$taxonomies))
  } else {
    return(NULL)
  }
}

# Create a temporary CSV file for testing
create_temp_csv <- function(data, file_name = "temp_npi_data.csv") {
  temp_file <- file.path(tempdir(), file_name)
  write_csv(data, temp_file)
  return(temp_file)
}

# Tests
test_that("Processes data frame input correctly", {
  cat("Running test: Processes data frame input correctly\n")
  mockery::stub(search_and_process_npi, 'npi::npi_search', mock_npi_search)
  mockery::stub(search_and_process_npi, 'npi::npi_flatten', mock_npi_flatten)

  result <- search_and_process_npi(sample_data)

  expect_true(nrow(result) >= 3) # Should return results for at least 3 names, possibly more due to multiple matches
  expect_true("first_name" %in% colnames(result))
  expect_true("last_name" %in% colnames(result))
  expect_true("taxonomies_desc" %in% colnames(result))
})

test_that("Handles empty input data frame", {
  cat("Running test: Handles empty input data frame\n")
  empty_data <- data.frame(first = character(), last = character(), stringsAsFactors = FALSE)
  result <- search_and_process_npi(empty_data)

  expect_equal(nrow(result), 0)
})

test_that("Handles invalid NPIs gracefully", {
  cat("Running test: Handles invalid NPIs gracefully\n")
  invalid_data <- data.frame(first = c("Invalid"), last = c("Name"), stringsAsFactors = FALSE)
  mockery::stub(search_and_process_npi, 'npi::npi_search', function(first_name, last_name) {
    cat("Mock search for invalid NPI\n")
    return(NULL)
  })
  mockery::stub(search_and_process_npi, 'npi::npi_flatten', function(npi_obj, cols) {
    cat("Mock flatten for invalid NPI object\n")
    return(NULL)
  })
  result <- search_and_process_npi(invalid_data)

  expect_equal(nrow(result), 0)
})


# Parameterized test for different inputs
test_that("Processes various names correctly", {
  cat("Running test: Processes various names correctly\n")
  test_cases <- list(
    list(first = "John", last = "Doe"),
    list(first = "Jane", last = "Smith"),
    list(first = "Alex", last = "Brown"),
    list(first = "Invalid", last = "Name")
  )

  mockery::stub(search_and_process_npi, 'npi::npi_search', mock_npi_search)
  mockery::stub(search_and_process_npi, 'npi::npi_flatten', mock_npi_flatten)

  for (test_case in test_cases) {
    data <- data.frame(first = test_case$first, last = test_case$last, stringsAsFactors = FALSE)
    result <- search_and_process_npi(data)

    if (test_case$first == "Invalid") {
      expect_equal(nrow(result), 0)
    } else {
      expect_true(nrow(result) >= 1)
      expect_true("first_name" %in% colnames(result))
      expect_true("last_name" %in% colnames(result))
      expect_true("taxonomies_desc" %in% colnames(result))
    }
  }
})

# Performance test
test_that("Handles large datasets efficiently", {
  cat("Running test: Handles large datasets efficiently\n")
  large_data <- data.frame(
    first = rep(c("John", "Jane", "Alex"), times = 100),
    last = rep(c("Doe", "Smith", "Brown"), times = 100),
    stringsAsFactors = FALSE
  )

  mockery::stub(search_and_process_npi, 'npi::npi_search', mock_npi_search)
  mockery::stub(search_and_process_npi, 'npi::npi_flatten', mock_npi_flatten)

  start_time <- Sys.time()
  result <- search_and_process_npi(large_data)
  end_time <- Sys.time()

  expect_true(nrow(result) >= 300)
  expect_true("first_name" %in% colnames(result))
  expect_true("last_name" %in% colnames(result))
  expect_true("taxonomies_desc" %in% colnames(result))
  cat("Performance test duration:", end_time - start_time, "\n")
})

# Boundary and Edge Case Tests
test_that("Handles very long names", {
  cat("Running test: Handles very long names\n")
  long_names <- data.frame(
    first = strrep("A", 1000),
    last = strrep("B", 1000),
    stringsAsFactors = FALSE
  )
  mockery::stub(search_and_process_npi, 'npi::npi_search', function(first_name, last_name) {
    cat("Mock search for very long names\n")
    return(NULL)
  })
  result <- search_and_process_npi(long_names)

  expect_equal(nrow(result), 0)
})

test_that("Handles special characters in names", {
  cat("Running test: Handles special characters in names\n")
  special_chars_names <- data.frame(
    first = "J@hn!",
    last = "D#e$%",
    stringsAsFactors = FALSE
  )
  mockery::stub(search_and_process_npi, 'npi::npi_search', function(first_name, last_name) {
    cat("Mock search for special character names\n")
    return(NULL)
  })
  result <- search_and_process_npi(special_chars_names)

  expect_equal(nrow(result), 0)
})

test_that("Handles names with whitespace", {
  cat("Running test: Handles names with whitespace\n")
  whitespace_names <- data.frame(
    first = " John ",
    last = " Doe ",
    stringsAsFactors = FALSE
  )
  mockery::stub(search_and_process_npi, 'npi::npi_search', function(first_name, last_name) {
    cat("Mock search for whitespace names\n")
    if (first_name == " John " && last_name == " Doe ") {
      return(list(
        basic = data.frame(
          first_name = "John",
          last_name = "Doe",
          stringsAsFactors = FALSE
        ),
        taxonomies = data.frame(
          taxonomies_desc = c("Family Medicine", "Internal Medicine"),
          stringsAsFactors = FALSE
        )
      ))
    } else {
      return(NULL)
    }
  })
  result <- search_and_process_npi(whitespace_names)

  expect_true(nrow(result) > 0)
  expect_true("first_name" %in% colnames(result))
  expect_true("last_name" %in% colnames(result))
  expect_true("taxonomies_desc" %in% colnames(result))
})

test_that("Handles single character names", {
  cat("Running test: Handles single character names\n")
  single_char_names <- data.frame(
    first = "A",
    last = "B",
    stringsAsFactors = FALSE
  )
  mockery::stub(search_and_process_npi, 'npi::npi_search', function(first_name, last_name) {
    cat("Mock search for single character names\n")
    return(NULL)
  })
  result <- search_and_process_npi(single_char_names)

  expect_equal(nrow(result), 0)
})

test_that("Handles mixed case names", {
  cat("Running test: Handles mixed case names\n")
  mixed_case_names <- data.frame(
    first = "jOhN",
    last = "dOe",
    stringsAsFactors = FALSE
  )
  mockery::stub(search_and_process_npi, 'npi::npi_search', function(first_name, last_name) {
    cat("Mock search for mixed case names\n")
    if (first_name == "jOhN" && last_name == "dOe") {
      return(list(
        basic = data.frame(
          first_name = "John",
          last_name = "Doe",
          stringsAsFactors = FALSE
        ),
        taxonomies = data.frame(
          taxonomies_desc = c("Family Medicine", "Internal Medicine"),
          stringsAsFactors = FALSE
        )
      ))
    } else {
      return(NULL)
    }
  })
  result <- search_and_process_npi(mixed_case_names)

  expect_true(nrow(result) > 0)
  expect_true("first_name" %in% colnames(result))
  expect_true("last_name" %in% colnames(result))
  expect_true("taxonomies_desc" %in% colnames(result))
})
