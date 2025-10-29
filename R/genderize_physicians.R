#' Genderize Physicians Data
#'
#' This function reads a CSV file containing physician data, genderizes the first names,
#' and joins the gender information back to the original data. It then saves the
#' result to a new CSV file with a timestamp.
#'
#' @param input_csv The path to the input roster file. CSV and Parquet sources are
#'   supported.
#' @param output_dir The directory where the output file will be saved. Default
#'   is a session-specific folder inside [tempdir()].
#' @param output_format Output format for the saved roster. Either "csv" or
#'   "parquet" with "csv" as the default to preserve backwards compatibility.
#' @return A data frame with genderized information joined to the original data.
#'
#' The function queries the [Genderize.io](https://genderize.io) API for first
#' name gender predictions. First names are deduplicated before querying the
#' service to minimise repeated requests.
#'
#' @importFrom dplyr bind_rows distinct left_join mutate
#' @importFrom readr read_csv write_csv
#' @importFrom httr GET content http_error status_code timeout
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' result <- genderize_physicians("sample.csv")
#' }
#'
#' @family gender
#' @export
genderize_physicians <- function(input_csv, output_dir = NULL, output_format = c("csv", "parquet")) {
  output_format <- match.arg(output_format)
  if (!file.exists(input_csv)) {
    stop(sprintf("Input file not found: %s", input_csv), call. = FALSE)
  }

  input_format <- tyler_normalize_file_format(path = input_csv)

  # Read the data
  gender_Physicians <- tyler_read_table(input_csv, format = input_format)
  validate_dataframe(gender_Physicians, name = "genderize input", allow_zero_rows = FALSE)
  message(sprintf("Loaded %d row(s) for genderization from %s.", nrow(gender_Physicians), input_csv))

  if (!"first_name" %in% names(gender_Physicians)) {
    stop("Input data must include a 'first_name' column.")
  }

  gender_Physicians <- gender_Physicians %>%
    dplyr::mutate(first_name = trimws(first_name))

  # Get first names
  first_names <- gender_Physicians$first_name

  resolved_genders <- genderize_fetch(first_names)

  x <- resolved_genders %>%
    dplyr::distinct(first_name, .keep_all = TRUE)

  # Bug #1 fix: Drop overlapping columns before join to prevent .x/.y suffixes
  # genderize_fetch() returns: first_name, gender, probability, count
  # We want the fresh API data, not any pre-existing columns with these names
  gender_Physicians_clean <- gender_Physicians %>%
    dplyr::select(-dplyr::any_of(c("gender", "probability", "count")))

  # Rejoin with the original database
  y <- dplyr::left_join(gender_Physicians_clean, x, by = "first_name")

  # Check for missing genders in the joined dataset
  missing_genders_joined <- sum(is.na(y$gender))

  # Generate a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

  if (is.null(output_dir)) {
    output_dir <- tyler_tempdir("genderize_physicians", create = TRUE)
  } else if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Create the output filename with timestamp
  input_stem <- tools::file_path_sans_ext(basename(input_csv))
  output_extension <- if (identical(output_format, "parquet")) ".parquet" else ".csv"
  output_path <- file.path(output_dir, paste0("genderized_", timestamp, "_", input_stem, output_extension))

  # Write the result to disk
  tyler_write_table(y, output_path, format = output_format)

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
  if (requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  return(y)
}

genderize_fetch <- function(first_names, batch_size = 10, api_url = "https://api.genderize.io/") {
  if (is.null(first_names) || length(first_names) == 0) {
    return(tibble::tibble(
      first_name = character(),
      gender = character(),
      probability = numeric(),
      count = integer()
    ))
  }

  clean_names <- unique(stats::na.omit(trimws(first_names)))
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
      response <- httr::GET(api_url, query = query, httr::timeout(10))

      if (!httr::http_error(response)) {
        break  # Success, exit retry loop
      }

      retry_count <- retry_count + 1
      if (retry_count < max_retries) {
        wait_time <- retry_count * 2  # Exponential backoff
        message(sprintf("Request failed, retrying in %d seconds (attempt %d/%d)...", wait_time, retry_count, max_retries))
        Sys.sleep(wait_time)
      }
    }

    if (httr::http_error(response)) {
      stop("Genderize.io request failed after ", max_retries, " attempts with status ", httr::status_code(response), call. = FALSE)
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
