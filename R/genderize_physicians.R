#' Genderize Physicians Data
#'
#' This function reads a CSV file containing physician data, genderizes the first names,
#' and joins the gender information back to the original data. It then saves the
#' result to a new CSV file with a timestamp.
#'
#' @param input_csv The path to the input CSV file containing physician data.
#' @param output_dir The directory where the output CSV file will be saved. Default is the current working directory.
#' @param verbose Logical; if TRUE, prints status messages while processing. Default is FALSE.
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
genderize_physicians <- function(input_csv, output_dir = getwd(), verbose = FALSE) {
  # Read the data
  gender_Physicians <- readr::read_csv(input_csv, show_col_types = FALSE) %>%
    dplyr::mutate(first_name = trimws(first_name))

  # Get first names
  first_names <- gender_Physicians$first_name

  resolved_genders <- genderize_fetch(first_names, verbose = verbose)

  x <- resolved_genders %>%
    dplyr::distinct(first_name, .keep_all = TRUE)

  # Rejoin with the original database
  y <- dplyr::left_join(gender_Physicians, x, by = "first_name")

  # Check for missing genders in the joined dataset
  missing_genders_joined <- sum(is.na(y$gender))

  # Generate a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

  # Create the output CSV filename with timestamp
  output_csv <- file.path(output_dir, paste0("genderized_", timestamp, "_", basename(input_csv)))

  # Write the result to a CSV file
  readr::write_csv(y, output_csv)

  # Print the number of missing genders in both datasets
  if (isTRUE(verbose)) {
    message("Missing genders in original data: ", sum(is.na(x$gender)))
    message("Missing genders in joined data: ", missing_genders_joined)
    message("Result saved to: ", output_csv)
  }

  # Return the result
  beepr::beep(2)
  return(y)
}

genderize_fetch <- function(first_names, batch_size = 10, api_url = "https://api.genderize.io/", verbose = FALSE) {
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

  results <- lapply(batches, function(name_batch) {
    query <- stats::setNames(as.list(name_batch), paste0("name[", seq_along(name_batch) - 1, "]"))
    if (isTRUE(verbose)) {
      message("Requesting genders for batch of ", length(name_batch), " names")
    }
    response <- httr::GET(api_url, query = query, httr::timeout(10))

    if (httr::http_error(response)) {
      stop("Genderize.io request failed with status ", httr::status_code(response), call. = FALSE)
    }

    parsed <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")

    if (is.null(parsed)) {
      stop("Genderize.io returned an empty response.", call. = FALSE)
    }

    if (!is.null(parsed$error)) {
      stop("Genderize.io error: ", parsed$error, call. = FALSE)
    }

    entries <- if (!is.null(parsed$name)) list(parsed) else parsed

    tibble::tibble(
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
  })

  dplyr::bind_rows(results)
}
