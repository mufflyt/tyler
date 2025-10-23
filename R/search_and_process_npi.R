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
#' @param dest_dir Destination directory to save chunked results. Default is NULL (no files written).
#' @param accumulate_path Optional CSV path that accumulates every non-empty result.
#'   When provided, results are appended after each successful lookup.
#' @param resume Logical. If `TRUE` and `accumulate_path` already exists, names that
#'   have been processed previously (based on the `search_term` column) are skipped.
#' @param progress_log Optional file path used to log progress updates. The log file
#'   is appended to and created automatically if it does not exist.
#' @param notify Logical. If `TRUE`, play a notification sound when processing
#'   completes (requires the optional `beepr` package). Defaults to `TRUE`.
#' @param progress_callback Optional function invoked with a named list each
#'   time progress is updated. The list contains fields such as `event`,
#'   `timestamp`, `search_term`, `rows`, `index`, and `total`.
#' @param heartbeat_seconds Optional numeric value specifying how frequently
#'   (in seconds) to emit a "still processing" heartbeat update while iterating
#'   over names. Set to `NULL` to disable the heartbeat.
#' @param progress_log_format Either "text" (default) to append human readable
#'   lines to `progress_log` or "csv" to write structured entries with columns
#'   `timestamp`, `event`, `search_term`, `rows`, and `detail`.
#' @return A data frame containing the processed NPI search results.
#'
#' @importFrom dplyr filter mutate select rename distinct arrange bind_rows tibble
#' @importFrom npi npi_search npi_flatten
#' @importFrom progress progress_bar
#' @importFrom purrr map2 keep
#' @importFrom readr write_csv read_csv
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
                                   accumulate_path = NULL,
                                   resume = FALSE,
                                   progress_log = NULL,
                                   notify = TRUE,
                                   progress_callback = NULL,
                                   heartbeat_seconds = NULL,
                                   progress_log_format = c("text", "csv")) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  progress_log_format <- match.arg(progress_log_format)
  if (!is.null(heartbeat_seconds)) {
    if (!is.numeric(heartbeat_seconds) || length(heartbeat_seconds) != 1L ||
        is.na(heartbeat_seconds) || heartbeat_seconds <= 0) {
      stop("`heartbeat_seconds` must be a single positive numeric value or NULL.")
    }
  }

  required_cols <- c("first", "last")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop("Input data must contain columns: ", paste(missing_cols, collapse = ", "))
  }

  write_structured_log <- function(timestamp, event, search_term, rows, detail) {
    if (is.null(progress_log)) {
      return(invisible(NULL))
    }
    dir.create(dirname(progress_log), showWarnings = FALSE, recursive = TRUE)
    if (identical(progress_log_format, "text")) {
      detail_msg <- if (!is.null(detail) && nzchar(detail)) {
        paste0(" - ", detail)
      } else {
        ""
      }
      search_msg <- if (!is.na(search_term) && nzchar(search_term)) {
        paste0(" [", search_term, "]")
      } else {
        ""
      }
      rows_msg <- if (!is.na(rows)) {
        paste0(" (rows: ", rows, ")")
      } else {
        ""
      }
      cat(sprintf("[%s] %s%s%s%s\n", timestamp, event, search_msg, rows_msg, detail_msg),
        file = progress_log, append = TRUE)
    } else {
      new_row <- data.frame(
        timestamp = timestamp,
        event = event,
        search_term = if (is.na(search_term)) "" else search_term,
        rows = rows,
        detail = if (is.null(detail)) "" else detail,
        stringsAsFactors = FALSE
      )
      readr::write_csv(
        new_row,
        progress_log,
        append = file.exists(progress_log),
        col_names = !file.exists(progress_log)
      )
    }
  }

  dispatch_progress <- function(event,
                                 search_term = NA_character_,
                                 rows = NA_integer_,
                                 detail = NULL,
                                 index = NA_integer_) {
    timestamp <- Sys.time()
    timestamp_chr <- format(timestamp, "%Y-%m-%d %H:%M:%S")
    write_structured_log(timestamp_chr, event, search_term, rows, detail)

    if (is.function(progress_callback)) {
      update <- list(
        event = event,
        timestamp = timestamp,
        search_term = if (!is.na(search_term)) search_term else NULL,
        rows = if (!is.na(rows)) rows else NULL,
        detail = detail,
        index = if (!is.na(index)) index else NULL,
        total = if (exists("total_names", inherits = FALSE)) total_names else NULL
      )
      try(progress_callback(update), silent = TRUE)
    }
  }

  read_existing_accumulation <- function() {
    if (is.null(accumulate_path) || !file.exists(accumulate_path)) {
      return(NULL)
    }
    tryCatch(
      readr::read_csv(accumulate_path, show_col_types = FALSE),
      error = function(e) {
        dispatch_progress(
          event = "accumulation_error",
          detail = sprintf("Unable to read accumulation file '%s': %s", accumulate_path, e$message)
        )
        NULL
      }
    )
  }

  existing_accumulated <- NULL
  processed_terms <- character(0)
  if (isTRUE(resume)) {
    existing_accumulated <- read_existing_accumulation()
    if (!is.null(existing_accumulated) && "search_term" %in% names(existing_accumulated)) {
      processed_terms <- unique(existing_accumulated$search_term[!is.na(existing_accumulated$search_term)])
    }
  }

  if (!nrow(data)) {
    final_result <- if (!is.null(existing_accumulated)) existing_accumulated else dplyr::tibble()
    if (isTRUE(notify) && requireNamespace("beepr", quietly = TRUE)) {
      beepr::beep(2)
    }
    return(final_result)
  }

  first_names <- as.character(data$first)
  last_names <- as.character(data$last)
  total_names <- length(first_names)

  dispatch_progress(
    event = "start",
    detail = sprintf("Processing %d name(s)", total_names)
  )

  vc <- c(
    "Allergy & Immunology", "Anesthesiology", "Dermatology", "Emergency Medicine",
    "Family Medicine", "Internal Medicine", "Obstetrics & Gynecology", "Ophthalmology",
    "Orthopaedic Surgery", "Pediatrics", "Psychiatry & Neurology", "Radiology", "Surgery", "Urology"
  )
  bc <- c("Pathology", "Pediatrics", "Physical Medicine & Rehabilitation", "Plastic Surgery", "Preventive Medicine")
  credential_filter <- NULL
  if (!is.null(filter_credentials)) {
    credential_filter <- tolower(gsub("[^A-Za-z0-9]", "", filter_credentials))
  }

  pb <- NULL
  if (total_names > 0) {
    pb <- progress::progress_bar$new(total = total_names, clear = FALSE, show_after = 0)
  }

  heartbeat_enabled <- !is.null(heartbeat_seconds)
  last_heartbeat <- Sys.time()

  save_results <- function(result, file_prefix, directory) {
    if (is.null(directory) || is.na(directory) || !nrow(result)) {
      return(invisible(NULL))
    }
    dir.create(directory, showWarnings = FALSE, recursive = TRUE)
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_name <- file.path(directory, paste0(file_prefix, "_chunk_", timestamp, ".csv"))
    readr::write_csv(result, file_name)
    dispatch_progress(
      event = "chunk_saved",
      search_term = basename(file_name),
      rows = nrow(result),
      detail = "Chunk saved to disk"
    )
  }

  append_accumulated <- function(result) {
    if (is.null(accumulate_path) || is.na(accumulate_path) || !nrow(result)) {
      return(invisible(NULL))
    }
    dir.create(dirname(accumulate_path), showWarnings = FALSE, recursive = TRUE)
    append_mode <- file.exists(accumulate_path)
    tryCatch({
      readr::write_csv(result, accumulate_path, append = append_mode, col_names = !append_mode)
      dispatch_progress(
        event = "accumulated",
        rows = nrow(result),
        detail = sprintf("Appended to %s", accumulate_path)
      )
    }, error = function(e) {
      dispatch_progress(
        event = "accumulation_error",
        detail = sprintf("Unable to append to '%s': %s", accumulate_path, e$message)
      )
    })
  }

  extract_status <- function(msg) {
    code <- regmatches(msg, regexpr("\\b[0-9]{3}\\b", msg))
    if (length(code) && nzchar(code)) {
      return(paste("after", code))
    }
    msg_clean <- gsub("\\s+", " ", msg)
    paste("due to", substr(msg_clean, 1, 120))
  }

  max_attempts <- 3L
  base_delay <- 1

  search_npi <- function(first_name, last_name, index) {
    attempt <- 1L
    search_term <- trimws(paste(first_name, last_name))

    repeat {
      result <- tryCatch({
        npi_obj <- npi::npi_search(
          first_name = first_name,
          last_name = last_name,
          enumeration_type = enumeration_type,
          limit = limit,
          country_code = country_code
        )

        if (is.null(npi_obj)) {
          return(NULL)
        }

        flattened <- npi::npi_flatten(npi_obj, cols = c("basic", "taxonomies"))
        if (is.null(flattened) || !nrow(flattened)) {
          return(NULL)
        }

        filtered <- dplyr::filter(flattened, taxonomies_desc %in% vc | taxonomies_desc %in% bc)
        if (!nrow(filtered)) {
          return(NULL)
        }

        if (!"basic_credential" %in% names(filtered)) {
          filtered$basic_credential <- NA_character_
        }
        if (!"basic_first_name" %in% names(filtered)) {
          filtered$basic_first_name <- NA_character_
        }
        if (!"basic_last_name" %in% names(filtered)) {
          filtered$basic_last_name <- NA_character_
        }
        if (!"basic_middle_name" %in% names(filtered)) {
          filtered$basic_middle_name <- NA_character_
        }

        filtered <- dplyr::mutate(
          filtered,
          credential = basic_credential,
          credential_clean = tolower(gsub("[^A-Za-z0-9]", "", credential))
        )

        if (!is.null(credential_filter)) {
          filtered <- dplyr::filter(filtered, is.na(credential_clean) | credential_clean %in% credential_filter)
        }

        filtered <- dplyr::rename(
          filtered,
          first_name = basic_first_name,
          last_name = basic_last_name,
          middle_name = basic_middle_name
        )

        filtered <- dplyr::mutate(filtered, search_term = paste(first_name, last_name))
        filtered <- dplyr::select(filtered, -credential_clean, -basic_credential)
        filtered <- dplyr::distinct(filtered, npi, taxonomies_desc, .keep_all = TRUE)
        filtered <- dplyr::arrange(filtered, last_name, first_name)
        filtered
      }, error = function(e) {
        e
      })

      if (inherits(result, "error")) {
        reason <- extract_status(result$message)
        message(sprintf(
          "Attempt %d/%d for %s failed %s.",
          attempt,
          max_attempts,
          if (nzchar(search_term)) search_term else "NPI search",
          reason
        ))
        dispatch_progress(
          event = "retry",
          search_term = search_term,
          detail = sprintf("Attempt %d/%d %s", attempt, max_attempts, reason),
          index = index
        )

        if (attempt >= max_attempts) {
          message(sprintf(
            "Giving up on %s after %d attempt%s.",
            if (nzchar(search_term)) search_term else "NPI search",
            max_attempts,
            if (max_attempts == 1) "" else "s"
          ))
          dispatch_progress(
            event = "search_error",
            search_term = search_term,
            detail = result$message,
            index = index
          )
          return(NULL)
        }

        delay <- base_delay * 2^(attempt - 1)
        message(sprintf("Retrying %s in %.1f seconds...", search_term, delay))
        Sys.sleep(delay)
        attempt <- attempt + 1L
        next
      }

      return(result)
    }
  }

  out <- vector("list", length = total_names)
  for (i in seq_len(total_names)) {
    first_name <- first_names[[i]]
    last_name <- last_names[[i]]
    if (!is.null(pb)) {
      pb$tick()
    }
    if (heartbeat_enabled) {
      now <- Sys.time()
      if (difftime(now, last_heartbeat, units = "secs") >= heartbeat_seconds) {
        message("Still processing...")
        dispatch_progress(event = "heartbeat", detail = "Processing in progress")
        last_heartbeat <- now
      }
    }
    search_term <- trimws(paste(first_name, last_name))
    if (isTRUE(resume) && length(processed_terms) && search_term %in% processed_terms) {
      dispatch_progress(
        event = "skipped",
        search_term = search_term,
        detail = "Already present in accumulation",
        index = i
      )
      next
    }

    result <- search_npi(first_name, last_name, i)

    if (is.null(result) || !nrow(result)) {
      dispatch_progress(
        event = "no_results",
        search_term = search_term,
        detail = "Search returned no data",
        index = i
      )
      next
    }

    should_save <- is.numeric(save_chunk_size) && !is.na(save_chunk_size) && save_chunk_size > 0 && nrow(result) >= save_chunk_size
    if (should_save) {
      save_results(result, "results_of_search_and_process_npi", dest_dir)
    }

    append_accumulated(result)
    dispatch_progress(
      event = "result",
      search_term = search_term,
      rows = nrow(result),
      detail = "Retrieved rows",
      index = i
    )

    out[[i]] <- result
  }

  npi_data <- purrr::keep(out, function(x) is.data.frame(x) && nrow(x) > 0)
  if (!length(npi_data)) {
    final_result <- if (!is.null(existing_accumulated)) existing_accumulated else dplyr::tibble()
  } else {
    result <- dplyr::bind_rows(npi_data)
    if (!is.null(existing_accumulated)) {
      final_result <- dplyr::bind_rows(existing_accumulated, result)
      final_result <- dplyr::distinct(final_result)
    } else {
      final_result <- result
    }
  }

  dispatch_progress(
    event = "completed",
    rows = nrow(final_result),
    detail = "Finished processing all names"
  )

  if (isTRUE(notify) && requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  final_result
}
