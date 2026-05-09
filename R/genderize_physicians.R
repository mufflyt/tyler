#' Genderize Physicians Data
#'
#' This function reads a CSV file containing physician data, genderizes the first names,
#' and joins the gender information back to the original data. It then saves the
#' result to a new CSV file with a timestamp.
#'
#' @param data_or_path A data frame containing physician data, or a path to a
#'   roster file. CSV and Parquet formats are supported when a path is supplied.
#' @param output_dir The directory where the output file will be saved. Default
#'   is a session-specific folder inside [tempdir()].
#' @param output_format Output format for the saved roster. Either "csv" or
#'   "parquet" with "csv" as the default to preserve backwards compatibility.
#' @return A data frame with genderized information joined to the original data.
#'
#' The function queries the [Genderize.io](https://genderize.io) API for first
#' name gender predictions. First names are deduplicated before querying the
#' service to minimize repeated requests.
#'
#' @importFrom dplyr bind_rows distinct left_join mutate
#' @importFrom readr read_csv write_csv
#' @importFrom httr GET content http_error status_code timeout
#' @importFrom tibble tibble
#'
#' @examplesIf interactive()
#' result <- mysterycall_genderize("sample.csv")
#' result <- mysterycall_genderize(my_dataframe)  # also accepts a data frame directly
#'
#' @family gender
#' @export
mysterycall_genderize <- function(data_or_path, output_dir = NULL, output_format = c("csv", "parquet")) {
  output_format <- match.arg(output_format)

  if (is.data.frame(data_or_path)) {
    gender_Physicians <- data_or_path
    input_stem <- "roster"
    message(sprintf("Loaded %d row(s) for genderization from data frame.", nrow(gender_Physicians)))
  } else if (is.character(data_or_path) && length(data_or_path) == 1L && nzchar(data_or_path)) {
    if (!file.exists(data_or_path)) {
      stop(sprintf("Input file not found: %s", data_or_path), call. = FALSE)
    }
    input_format <- mysterycall_normalize_file_format(path = data_or_path)
    gender_Physicians <- mysterycall_read_table(data_or_path, format = input_format)
    input_stem <- tools::file_path_sans_ext(basename(data_or_path))
    message(sprintf("Loaded %d row(s) for genderization from %s.", nrow(gender_Physicians), data_or_path))
  } else {
    stop("`data_or_path` must be a data frame or a single file path (character scalar).", call. = FALSE)
  }

  validate_dataframe(gender_Physicians, name = "genderize input", allow_zero_rows = FALSE)

  if (!"first_name" %in% names(gender_Physicians)) {
    stop("Input data must include a 'first_name' column.", call. = FALSE)
  }

  gender_Physicians <- gender_Physicians %>%
    dplyr::mutate(first_name = trimws(.data$first_name))

  join_key <- tolower(gender_Physicians$first_name)

  # Get first names
  first_names <- gender_Physicians$first_name

  resolved_genders <- genderize_fetch(first_names)

  x <- resolved_genders %>%
    dplyr::mutate(join_first_name = tolower(.data$first_name)) %>%
    dplyr::distinct(.data$join_first_name, .keep_all = TRUE) %>%
    dplyr::select(-"first_name")

  # Bug #1 fix: Drop overlapping columns before join to prevent .x/.y suffixes
  # genderize_fetch() returns: first_name, gender, probability, count
  # We want the fresh API data, not any pre-existing columns with these names
  gender_Physicians_clean <- gender_Physicians %>%
    dplyr::select(-dplyr::any_of(c("gender", "probability", "count"))) %>%
    dplyr::mutate(join_first_name = join_key)

  # Rejoin with the original database
  y <- dplyr::left_join(gender_Physicians_clean, x, by = "join_first_name") %>%
    dplyr::select(-dplyr::any_of(c("join_first_name")))

  # Check for missing genders in the joined dataset
  missing_genders_joined <- sum(is.na(y$gender))

  # Generate a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

  if (is.null(output_dir)) {
    output_dir <- mysterycall_tempdir("mysterycall_genderize", create = TRUE)
  } else if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Create the output filename with timestamp
  output_extension <- if (identical(output_format, "parquet")) ".parquet" else ".csv"
  output_path <- file.path(output_dir, paste0("genderized_", timestamp, "_", input_stem, output_extension))

  # Write the result to disk
  mysterycall_write_table(y, output_path, format = output_format)

  # Print the number of missing genders in both datasets using plain-language summaries
  message(sprintf(
    "Identified %d unique first name(s); %d lacked gender predictions prior to joining.",
    nrow(x),
    sum(is.na(x$gender))
  ))
  message(sprintf(
    "After joining, %d record(s) still have missing gender values.",
    missing_genders_joined
  ))

  # Print the path and filename of the new CSV
  message(sprintf("Genderized roster saved to: %s", output_path))

  # Return the result
  if (isTRUE(interactive()) && requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  return(y)
}

genderize_fetch <- function(first_names, batch_size = 10, api_url = "https://api.genderize.io/") {
  checkmate::assert_count(batch_size, positive = TRUE, .var.name = "batch_size")
  checkmate::assert_string(api_url, min.chars = 1, .var.name = "api_url")
  if (is.null(first_names) || length(first_names) == 0) {
    return(tibble::tibble(
      first_name = character(),
      gender = character(),
      probability = numeric(),
      count = integer()
    ))
  }

  clean_names <- unique(stats::na.omit(trimws(as.character(first_names))))
  clean_names <- clean_names[nzchar(clean_names)]
  if (length(clean_names) == 0) {
    return(tibble::tibble(
      first_name = character(),
      gender = character(),
      probability = numeric(),
      count = integer()
    ))
  }

  batches <- split(clean_names, ceiling(seq_along(clean_names) / batch_size))

  total_batches <- length(batches)
  results <- vector("list", total_batches)
  for (i in seq_along(batches)) {
    name_batch <- batches[[i]]
    message(sprintf("Requesting gender predictions for batch %d of %d (%d name(s)).", i, total_batches, length(name_batch)))
    query <- stats::setNames(as.list(name_batch), paste0("name[", seq_along(name_batch) - 1, "]"))

    # Add retry logic for API requests
    max_retries <- 3
    retry_count <- 0
    response <- NULL

    while (retry_count < max_retries) {
      response <- tryCatch(
        httr::GET(api_url, query = query, httr::timeout(10)),
        error = function(e) {
          message(sprintf("Network error on attempt %d: %s", retry_count + 1L, conditionMessage(e)))
          NULL
        }
      )

      if (!is.null(response) && !httr::http_error(response)) {
        break  # Success, exit retry loop
      }

      retry_count <- retry_count + 1
      if (retry_count < max_retries) {
        wait_time <- 2^retry_count
        message(sprintf("Request failed, retrying in %d seconds (attempt %d/%d)...", wait_time, retry_count, max_retries))
        Sys.sleep(wait_time)
      }
    }

    if (is.null(response) || httr::http_error(response)) {
      status_str <- if (is.null(response)) "network error" else as.character(httr::status_code(response))
      warning(sprintf(
        "Genderize.io batch %d/%d failed after %d attempt(s) (status: %s); returning NA for %d name(s) in this batch.",
        i, total_batches, max_retries, status_str, length(name_batch)
      ), call. = FALSE)
      results[[i]] <- tibble::tibble(
        first_name = name_batch,
        gender      = NA_character_,
        probability = NA_real_,
        count       = NA_integer_
      )
      next
    }

    parsed <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")

    if (is.null(parsed)) {
      stop("Genderize.io returned an empty response.", call. = FALSE)
    }

    if (!is.null(parsed$error)) {
      stop("Genderize.io error: ", parsed$error, call. = FALSE)
    }

    entries <- if (!is.null(parsed$name)) list(parsed) else parsed

    results[[i]] <- tibble::tibble(
      first_name = vapply(entries, function(x) {
        if (is.null(x$name)) NA_character_ else x$name
      }, character(1), USE.NAMES = FALSE),
      gender = vapply(entries, function(x) {
        if (is.null(x$gender)) NA_character_ else x$gender
      }, character(1), USE.NAMES = FALSE),
      probability = vapply(entries, function(x) {
        if (is.null(x$probability)) NA_real_ else as.numeric(x$probability)
      }, numeric(1), USE.NAMES = FALSE),
      count = vapply(entries, function(x) {
        if (is.null(x$count)) NA_integer_ else as.integer(x$count)
      }, integer(1), USE.NAMES = FALSE)
    )
  }

  dplyr::bind_rows(results)
}
