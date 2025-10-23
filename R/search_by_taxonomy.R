#' Search NPI Database by Taxonomy
#'
#' This function searches the NPI Database for healthcare providers based on a taxonomy description.
#'
#' @param taxonomy_to_search A character vector containing the taxonomy description(s) to search for.
#' @return A data frame with filtered NPI data based on the specified taxonomy description.
#'
#' @examples
#' \dontrun{
#' # Example usage with multiple taxonomy descriptions:
#' go_data <- search_by_taxonomy("Gynecologic Oncology")
#' fpmrs_data <- search_by_taxonomy("Female Pelvic Medicine and Reconstructive Surgery")
#' rei_data <- search_by_taxonomy("Reproductive Endocrinology")
#' mfm_data <- search_by_taxonomy("Maternal & Fetal Medicine")
#' }
#'
#' @importFrom npi npi_search npi_flatten
#' @importFrom dplyr bind_rows arrange filter select distinct mutate rename
#' @importFrom stringr str_remove_all str_to_lower str_detect
#' @importFrom readr write_rds
#' @importFrom dplyr tibble
#' @family npi
#' @export
search_by_taxonomy <- function(taxonomy_to_search) {
  if (missing(taxonomy_to_search) || is.null(taxonomy_to_search)) {
    return(dplyr::tibble())
  }

  taxonomy_to_search <- taxonomy_to_search[!is.na(taxonomy_to_search)]
  if (!length(taxonomy_to_search)) {
    return(dplyr::tibble())
  }

  npi_data <- dplyr::tibble()

  for (taxonomy in taxonomy_to_search) {
    tryCatch({
      search_result <- npi::npi_search(
        taxonomy_description = taxonomy
      )

      if (is.null(search_result)) {
        next
      }

      data_taxonomy <- npi::npi_flatten(search_result)
      if (is.null(data_taxonomy) || !nrow(data_taxonomy)) {
        next
      }

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

      npi_data <- dplyr::bind_rows(npi_data, data_taxonomy)
    }, error = function(e) {
      message(sprintf("Error in search for %s:\n%s", taxonomy, e$message))
    })
  }

  tryCatch({
    dir.create("data", showWarnings = FALSE, recursive = TRUE)
    filename <- file.path("data", paste0("search_taxonomy_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".rds"))
    readr::write_rds(npi_data, filename)
  }, error = function(e) {
    message("Error saving data to file:\n", e$message)
  })

  if (requireNamespace("beepr", quietly = TRUE)) {
    if (requireNamespace("beepr", quietly = TRUE)) beepr::beep(2)
  }

  npi_data
}
