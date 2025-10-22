#' Search and Process NPI Numbers
#'
#' This function takes an input data frame containing first and last names, performs NPI search and processing,
#' and filters results based on specified taxonomies. It supports customization for enumeration type, search limit,
#' and filtering credentials.
#'
#' @param data A data frame with columns 'first' and 'last' containing the names to search.
#' @param enumeration_type The enumeration type for NPI search (e.g., "ind", "org", "all"). Default is "ind".
#' @param limit The maximum number of search results to request for each name pair. Default is 5.
#' @param country_code Filter for only the "US".
#' @param filter_credentials A character vector containing the credentials to filter the NPI results. Default is c("MD", "DO").
#' @param save_chunk_size The number of results to save per chunk. Default is 10.
#' @param dest_dir Destination directory to save chunked results. Default is NULL (current working directory).
#' @param quiet Should progress output be suppressed? Default is `FALSE`.
#'
#' @return A data frame containing the processed NPI search results.
#'
#' @importFrom dplyr filter
#' @importFrom npi npi_search npi_flatten
#' @importFrom progress progress_bar
#' @importFrom purrr map2 keep
#' @importFrom data.table rbindlist
#' @importFrom readr write_csv
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @family npi
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(first = "John", last = "Doe")
#' results <- search_and_process_npi(df)
#' }
search_and_process_npi <- function(data,
                                   enumeration_type = "ind",
                                   limit = 5L,
                                   country_code = "US",
                                   filter_credentials = c("MD", "DO"),
                                   save_chunk_size = 10,
                                   dest_dir = NULL,
                                   quiet = getOption("tyler.quiet", FALSE)) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  required_cols <- c("first", "last")
  if (!all(required_cols %in% names(data))) {
    stop("Input data frame must contain 'first' and 'last' columns.")
  }

  if (!is.character(enumeration_type) || length(enumeration_type) != 1 || enumeration_type == "") {
    stop("`enumeration_type` must be a non-empty character string.")
  }

  if (length(limit) != 1 || is.na(limit) || limit <= 0) {
    stop("`limit` must be a single positive number.")
  }
  limit <- as.integer(limit)

  if (length(save_chunk_size) != 1 || is.na(save_chunk_size) || save_chunk_size <= 0) {
    stop("`save_chunk_size` must be a single positive number.")
  }
  save_chunk_size <- as.integer(save_chunk_size)

  if (is.null(dest_dir)) {
    dest_dir <- getwd()
  } else {
    dest_dir <- path.expand(dest_dir)
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(dest_dir)) {
    stop("Unable to create destination directory: ", dest_dir)
  }

  first_names <- as.character(data$first)
  last_names <- as.character(data$last)

  if (length(first_names) == 0) {
    return(tibble::tibble())
  }

  vc <- c("Allergy & Immunology", "Anesthesiology", "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine",
          "Obstetrics & Gynecology", "Ophthalmology", "Orthopaedic Surgery", "Pediatrics", "Psychiatry & Neurology", "Radiology", "Surgery", "Urology")
  bc <- c("Pathology", "Pediatrics", "Physical Medicine & Rehabilitation", "Plastic Surgery", "Preventive Medicine")

  search_npi <- function(first_name, last_name) {
    if (!quiet) {
      message("Searching NPI for: ", first_name, " ", last_name)
    }
    tryCatch({
      npi_obj <- npi::npi_search(
        first_name = first_name,
        last_name = last_name,
        enumeration_type = enumeration_type,
        country_code = country_code,
        limit = limit
      )
      if (!quiet) {
        message("NPI search object retrieved for: ", first_name, " ", last_name)
      }

      t <- npi::npi_flatten(npi_obj, cols = c("basic", "taxonomies"))
      if (!quiet) {
        message("NPI data flattened for: ", first_name, " ", last_name)
      }

      t <- dplyr::filter(t, taxonomies_desc %in% vc | taxonomies_desc %in% bc)
      if (!quiet) {
        message("NPI data filtered for: ", first_name, " ", last_name)
      }

      credential_cols <- intersect(c("basic_credential", "basic.credential"), names(t))
      if (length(credential_cols) > 0 && length(filter_credentials) > 0) {
        t <- dplyr::filter(t, .data[[credential_cols[1]]] %in% filter_credentials)
      }
      t
    }, error = function(e) {
      if (!quiet) {
        message("ERROR for ", first_name, " ", last_name, ": ", conditionMessage(e))
      }
      NULL
    })
  }

  total_names <- nrow(data)
  pb <- if (total_names > 0 && !quiet) progress::progress_bar$new(total = total_names) else NULL

  save_results <- function(result, file_prefix, directory) {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_name <- file.path(directory, paste0(file_prefix, "_chunk_", timestamp, ".csv"))
    readr::write_csv(result, file_name)
    if (!quiet) {
      message("Saved chunk results to: ", file_name)
    }
  }

  chunk_count <- 0

  out <- purrr::map2(first_names, last_names, function(first_name, last_name) {
    if (!is.null(pb)) pb$tick()
    result <- search_npi(first_name, last_name)

    if (!is.null(result) && nrow(result) > save_chunk_size) {
      chunk_count <<- chunk_count + 1
      save_results(result, "results_of_search_and_process_npi", dest_dir)
    }
    result
  })

  npi_data <- purrr::keep(out, is.data.frame)

  if (length(npi_data) == 0) {
    result <- tibble::tibble()
  } else {
    result <- data.table::rbindlist(npi_data, fill = TRUE)
  }

  if (requireNamespace("beepr", quietly = TRUE) && !quiet) {
    beepr::beep(2)
  }

  result
}
