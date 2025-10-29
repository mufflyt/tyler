#' Search NPI Database by Taxonomy
#'
#' This function searches the NPI Database for healthcare providers based on a taxonomy description.
#'
#' @param taxonomy_to_search A character vector containing the taxonomy description(s) to search for.
#' @param write_snapshot Logical. If `TRUE`, the retrieved data is saved as an `.rds`
#'   file for later reference. Defaults to `TRUE`.
#' @param snapshot_dir Directory where snapshot files should be written when
#'   `write_snapshot` is `TRUE`. Defaults to a session-specific folder inside
#'   [tempdir()] when not supplied.
#' @param notify Logical. If `TRUE`, play a notification sound when processing
#'   completes (requires the optional `beepr` package). Defaults to `TRUE`.
#' @return A data frame with filtered NPI data based on the specified taxonomy description.
#'
#' @examples
#' # Example usage with multiple taxonomy descriptions:
#' go_data <- search_by_taxonomy("Gynecologic Oncology")
#' fpmrs_data <- search_by_taxonomy("Female Pelvic Medicine and Reconstructive Surgery")
#' rei_data <- search_by_taxonomy("Reproductive Endocrinology")
#' mfm_data <- search_by_taxonomy("Maternal & Fetal Medicine")
#'
#' @importFrom npi npi_search npi_flatten
#' @importFrom dplyr bind_rows arrange filter select distinct mutate rename
#' @importFrom stringr str_remove_all str_to_lower str_detect str_extract str_trunc
#' @importFrom readr write_rds
#' @importFrom tibble tibble
#' @family npi
#' @export
search_by_taxonomy <- function(taxonomy_to_search,
                               write_snapshot = TRUE,
                               snapshot_dir = NULL,
                               notify = TRUE) {
  if (missing(taxonomy_to_search) || is.null(taxonomy_to_search)) {
    return(tibble::tibble())
  }

  taxonomy_to_search <- taxonomy_to_search[!is.na(taxonomy_to_search)]
  if (!length(taxonomy_to_search)) {
    return(tibble::tibble())
  }

  npi_data <- tibble::tibble()

  extract_status <- function(msg) {
    status <- stringr::str_extract(msg, "\\b[0-9]{3}\\b")
    if (!is.na(status)) {
      return(paste("after", status))
    }
    trunc_msg <- stringr::str_trunc(msg, 120)
    paste("due to", trunc_msg)
  }

  max_attempts <- 3L
  base_delay <- 1

  total_taxonomies <- length(taxonomy_to_search)
  for (index in seq_along(taxonomy_to_search)) {
    taxonomy <- taxonomy_to_search[index]
    message(sprintf("Searching taxonomy '%s' (%d of %d)...", taxonomy, index, total_taxonomies))
    attempt <- 1L
    repeat {
      result <- tryCatch({
        search_result <- npi::npi_search(
          taxonomy_description = taxonomy,
          country_code = "US",
          enumeration_type = "ind"
        )

        data_taxonomy <- NULL
        if (!is.null(search_result)) {
          data_taxonomy <- npi::npi_flatten(search_result)
        }

        if (!is.null(data_taxonomy) && nrow(data_taxonomy)) {
          data_taxonomy <- dplyr::distinct(data_taxonomy, npi, .keep_all = TRUE)
          data_taxonomy <- dplyr::mutate(
            data_taxonomy,
            credential = stringr::str_remove_all(basic_credential, "[[\\p{P}][\\p{S}]]"),
            credential_lower = stringr::str_to_lower(credential)
          )
          data_taxonomy <- dplyr::filter(
            data_taxonomy,
            is.na(credential_lower) | credential_lower %in% stringr::str_to_lower(c("MD", "DO"))
          )
          data_taxonomy <- dplyr::filter(data_taxonomy, addresses_country_name == "United States")
          data_taxonomy <- dplyr::filter(data_taxonomy, stringr::str_detect(taxonomies_desc, taxonomy))

          # Bug #13 fix: Validate required API columns exist before renaming
          required_cols <- c("basic_first_name", "basic_last_name", "basic_middle_name")
          missing_cols <- setdiff(required_cols, names(data_taxonomy))
          if (length(missing_cols) > 0) {
            stop(sprintf(
              "NPI API response missing required columns: %s. Available columns: %s",
              paste(missing_cols, collapse = ", "),
              paste(names(data_taxonomy), collapse = ", ")
            ))
          }

          data_taxonomy <- dplyr::rename(
            data_taxonomy,
            first_name = basic_first_name,
            last_name = basic_last_name,
            middle_name = basic_middle_name
          )
          data_taxonomy <- dplyr::mutate(data_taxonomy, search_term = taxonomy)
          data_taxonomy <- dplyr::arrange(data_taxonomy, last_name, first_name)
          data_taxonomy <- dplyr::distinct(data_taxonomy, npi, taxonomies_desc, .keep_all = TRUE)
          data_taxonomy <- dplyr::select(
            data_taxonomy,
            -credential_lower,
            -basic_last_updated, -basic_status, -basic_name_prefix, -basic_name_suffix,
            -basic_certification_date, -other_names_type, -other_names_code,
            -other_names_credential, -other_names_first_name, -other_names_last_name,
            -other_names_prefix, -other_names_suffix, -other_names_middle_name,
            -identifiers_code, -identifiers_desc, -identifiers_identifier,
            -identifiers_state, -identifiers_issuer, -taxonomies_code,
            -taxonomies_taxonomy_group, -taxonomies_state, -taxonomies_license,
            -addresses_country_code, -addresses_address_purpose, -addresses_address_type,
            -addresses_address_2, -addresses_fax_number, -endpoints_endpointType,
            -endpoints_endpointTypeDescription, -endpoints_endpoint,
            -endpoints_affiliation, -endpoints_useDescription,
            -endpoints_contentTypeDescription, -endpoints_country_code,
            -endpoints_country_name, -endpoints_address_type, -endpoints_address_1,
            -endpoints_city, -endpoints_state, -endpoints_postal_code,
            -endpoints_use, -endpoints_endpointDescription, -endpoints_affiliationName,
            -endpoints_contentType, -endpoints_contentOtherDescription,
            -endpoints_address_2, -endpoints_useOtherDescription
          )
        }

        data_taxonomy
      }, error = function(e) {
        list(error = e)
      })

      if (is.list(result) && !is.null(result$error)) {
        err <- result$error
        if (attempt >= max_attempts) {
          message(sprintf(
            "Attempt %d/%d for taxonomy '%s' failed %s. Giving up.",
            attempt,
            max_attempts,
            taxonomy,
            extract_status(err$message)
          ))
          break
        }

        delay <- base_delay * 2^(attempt - 1)
        message(sprintf(
          "Attempt %d/%d for taxonomy '%s' failed %s. Retrying in %.1f seconds...",
          attempt,
          max_attempts,
          taxonomy,
          extract_status(err$message),
          delay
        ))
        Sys.sleep(delay)
        attempt <- attempt + 1L
        next
      }

      if (!is.null(result) && nrow(result)) {
        npi_data <- dplyr::bind_rows(npi_data, result)
        message(sprintf("Retrieved %d record(s) for taxonomy '%s'.", nrow(result), taxonomy))
      } else {
        message(sprintf("No records returned for taxonomy '%s'.", taxonomy))
      }
      break
    }
  }

  if (isTRUE(write_snapshot)) {
    if (is.null(snapshot_dir)) {
      snapshot_dir <- tyler_tempdir("search_by_taxonomy", create = TRUE)
    } else if (!dir.exists(snapshot_dir)) {
      dir.create(snapshot_dir, showWarnings = FALSE, recursive = TRUE)
    }
    tryCatch({
      filename <- file.path(snapshot_dir, paste0("search_taxonomy_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".rds"))
      readr::write_rds(npi_data, filename)
    }, error = function(e) {
      message("Error saving data to file:\n", e$message)
    })
  }

  if (isTRUE(notify) && requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  npi_data
}
