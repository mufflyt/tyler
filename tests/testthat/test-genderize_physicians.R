library(testthat)
library(readr)
library(dplyr)
library(mockery)

make_genderize_response <- function(payload, status = 200L) {
  if (is.null(payload)) {
    body <- "null"
  } else {
    body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  }

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

test_that("genderize_fetch deduplicates exact first names and trims whitespace", {
  queries <- list()

  fake_get <- function(url, query, ...) {
    queries <<- c(queries, list(query))
    values <- unname(unlist(query, use.names = FALSE))
    payload <- lapply(values, function(value) {
      list(
        name = value,
        gender = ifelse(value == "Alice", "female", "male"),
        probability = 0.91,
        count = 123L
      )
    })

    make_genderize_response(payload)
  }

  fetch <- tyler:::genderize_fetch
  mockery::stub(fetch, "httr::GET", fake_get)

  result <- fetch(c(" Alice ", "Bob", "Alice", NA))

  expect_prediction_columns(result)
  expect_equal(nrow(result), 2L)
  expect_equal(sort(result$first_name), sort(c("Alice", "Bob")))
  expect_equal(queries[[1]][["name[0]"]], "Alice")
  expect_equal(queries[[1]][["name[1]"]], "Bob")
})

test_that("genderize_fetch handles single-object responses", {
  fake_get <- function(url, query, ...) {
    expect_equal(length(query), 1L)
    make_genderize_response(
      list(
        name = unname(query[[1]]),
        gender = "female",
        probability = 0.83,
        count = 55L
      ),
      status = 200L
    )
  }

  fetch <- tyler:::genderize_fetch
  mockery::stub(fetch, "httr::GET", fake_get)

  result <- fetch("Eve")
  expect_prediction_columns(result)
  expect_equal(result$first_name, "Eve")
  expect_equal(result$gender, "female")
  expect_equal(result$probability, 0.83)
  expect_equal(result$count, 55L)
})

test_that("genderize_fetch surfaces HTTP and API errors", {
  http_error_get <- function(url, query, ...) {
    make_genderize_response(list(error = "Too Many Requests"), status = 429L)
  }

  fetch_http <- tyler:::genderize_fetch
  mockery::stub(fetch_http, "httr::GET", http_error_get)

  expect_error(fetch_http(c("Ava")), "Genderize.io request failed")

  api_error_get <- function(url, query, ...) {
    make_genderize_response(list(error = "Daily limit reached"), status = 200L)
  }

  fetch_api <- tyler:::genderize_fetch
  mockery::stub(fetch_api, "httr::GET", api_error_get)

  expect_error(fetch_api(c("Mia")), "Genderize.io error: Daily limit reached")
})

test_that("genderize_fetch batches requests to improve performance", {
  call_count <- 0L
  requested <- list()

  fake_get <- function(url, query, ...) {
    call_count <<- call_count + 1L
    requested <<- c(requested, list(unname(unlist(query, use.names = FALSE))))
    payload <- lapply(unname(unlist(query, use.names = FALSE)), function(name) {
      list(name = name, gender = "female", probability = 0.9, count = 10L)
    })

    make_genderize_response(payload)
  }

  fetch <- tyler:::genderize_fetch
  mockery::stub(fetch, "httr::GET", fake_get)

  first_names <- sprintf("Name%02d", 1:25)
  result <- fetch(first_names)

  expect_equal(call_count, 3L)
  expect_equal(length(result$first_name), 25L)
  expect_true(all(vapply(requested, length, integer(1)) <= 10L))
})

test_that("genderize_physicians produces enriched output and preserves original data", {
  input <- tibble::tibble(
    first_name = c("Ada", "Grace", "Ada", "Edsger", NA_character_),
    last_name = c("Lovelace", "Hopper", "Smith", "Dijkstra", "Unknown")
  )

  temp_dir <- file.path(tempdir(), paste0("genderize-", as.integer(runif(1, 1, 1e6))))
  dir.create(temp_dir, showWarnings = FALSE)
  input_path <- file.path(temp_dir, "physicians.csv")
  readr::write_csv(input, input_path)

  predictions <- tibble::tibble(
    first_name = c("Ada", "Grace", "Edsger"),
    gender = c("female", "female", "male"),
    probability = c(0.99, 0.97, 0.88),
    count = c(120L, 80L, 40L)
  )

  fake_genderize <- function(first_names, ...) predictions

  gp <- genderize_physicians
  mockery::stub(gp, "genderize_fetch", fake_genderize)
  mockery::stub(gp, "beepr::beep", function(...) NULL)

  result <- gp(input_path, temp_dir)

  expect_equal(nrow(result), nrow(input))
  expect_setequal(colnames(result), c(colnames(input), colnames(predictions)[-1]))
  expect_equal(sum(!is.na(result$gender)), 3L)
  expect_equal(sum(is.na(result$gender)), 1L)

  output_files <- list.files(temp_dir, pattern = "^genderized_.*\\.csv$", full.names = TRUE)
  expect_true(length(output_files) == 1L)
  saved <- readr::read_csv(output_files[[1]], show_col_types = FALSE)
  expect_equal(result, saved)
})

test_that("genderize_physicians maintains historical match rates", {
  input <- tibble::tibble(first_name = c("Alan", "Barbara", "Charlie", "Dana"))
  predictions <- tibble::tibble(
    first_name = c("Alan", "Barbara", "Charlie"),
    gender = c("male", "female", NA_character_),
    probability = c(0.91, 0.92, NA_real_),
    count = c(50L, 65L, NA_integer_)
  )

  baseline_match_rate <- 0.5

  fake_genderize <- function(first_names, ...) predictions

  gp <- genderize_physicians
  mockery::stub(gp, "genderize_fetch", fake_genderize)
  mockery::stub(gp, "beepr::beep", function(...) NULL)

  temp_dir <- tempfile("genderize-regression")
  dir.create(temp_dir)
  input_path <- file.path(temp_dir, "physicians.csv")
  readr::write_csv(input, input_path)

  result <- gp(input_path, temp_dir)
  observed_match_rate <- mean(!is.na(result$gender))

  expect_gte(observed_match_rate, baseline_match_rate)
})

test_that("genderize_physicians enforces data validation on blank or malformed names", {
  input <- tibble::tibble(
    first_name = c("", "  ", "Lynn", NA_character_),
    last_name = c("One", "Two", "Three", "Four")
  )

  predictions <- tibble::tibble(
    first_name = "Lynn",
    gender = "female",
    probability = 0.95,
    count = 30L
  )

  fake_genderize <- function(first_names, ...) predictions

  gp <- genderize_physicians
  mockery::stub(gp, "genderize_fetch", fake_genderize)
  mockery::stub(gp, "beepr::beep", function(...) NULL)

  temp_dir <- tempfile("genderize-validation")
  dir.create(temp_dir)
  input_path <- file.path(temp_dir, "physicians.csv")
  readr::write_csv(input, input_path)

  result <- gp(input_path, temp_dir)

  trimmed <- trimws(result$first_name)
  blank_or_missing <- is.na(trimmed) | trimmed == ""

  expect_equal(sum(blank_or_missing), 2L)
  expect_equal(sum(!is.na(result$gender)), 1L)
  expect_true(all(result$gender[result$first_name == "Lynn"] == "female"))
})

test_that("genderize_physicians output remains compatible with downstream case_when logic", {
  input <- tibble::tibble(first_name = paste0("Name", 1:10))
  predictions <- tibble::tibble(
    first_name = paste0("Name", 1:10),
    gender = rep(c("male", "female"), length.out = 10),
    probability = rep(c(0.8, 0.85), length.out = 10),
    count = rep(20L, 10)
  )

  fake_genderize <- function(first_names, ...) predictions

  gp <- genderize_physicians
  mockery::stub(gp, "genderize_fetch", fake_genderize)
  mockery::stub(gp, "beepr::beep", function(...) NULL)

  temp_dir <- tempfile("genderize-property")
  dir.create(temp_dir)
  input_path <- file.path(temp_dir, "physicians.csv")
  readr::write_csv(input, input_path)

  result <- gp(input_path, temp_dir)

  allowed <- c("male", "female")
  genders <- na.omit(unique(result$gender))
  expect_true(all(genders %in% allowed))
})

test_that("genderize_physicians provides required columns for handoff workflows", {
  input <- tibble::tibble(first_name = c("Ruth", "Ida"), external_id = c("X1", "X2"))
  predictions <- tibble::tibble(
    first_name = c("Ruth", "Ida"),
    gender = c("female", "female"),
    probability = c(0.89, 0.9),
    count = c(44L, 32L)
  )

  fake_genderize <- function(first_names, ...) predictions

  gp <- genderize_physicians
  mockery::stub(gp, "genderize_fetch", fake_genderize)
  mockery::stub(gp, "beepr::beep", function(...) NULL)

  temp_dir <- tempfile("genderize-handoff")
  dir.create(temp_dir)
  input_path <- file.path(temp_dir, "physicians.csv")
  readr::write_csv(input, input_path)

  result <- gp(input_path, temp_dir)

  expect_true(all(c("probability", "count") %in% colnames(result)))
  expect_equal(result$external_id, input$external_id)
  expect_equal(result$gender, predictions$gender)
})

