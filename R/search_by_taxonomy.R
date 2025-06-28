#' Search NPI Database by Taxonomy
#'
#' This function searches the NPI Database for healthcare providers based on a taxonomy description.
#'
#' @param taxonomy_to_search A character vector containing the taxonomy description(s) to search for.
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
#' @importFrom dplyr bind_rows arrange filter select distinct mutate
#' @importFrom stringr str_remove_all str_to_lower str_detect
#' @importFrom readr write_rds
#' @export
#' @seealso tyler
search_by_taxonomy <- function(taxonomy_to_search) {
  # Create an empty data frame to store search results
  data <- data.frame()
  cat("Starting search_by_taxonomy\n")

  # Loop over each taxonomy description
  for (taxonomy in taxonomy_to_search) {
    cat("Searching for taxonomy:", taxonomy, "\n")
    tryCatch({
      # Perform the search for the current taxonomy
      result <- npi::npi_search(
        taxonomy_description = taxonomy,
        country_code = "US",
        enumeration_type = "ind",
        limit = 1200
      )
      cat("Search completed for taxonomy:", taxonomy, "\n")

      if (!is.null(result)) {
        cat("Processing data for taxonomy:", taxonomy, "\n")
        # Process and filter the data for the current taxonomy
        data_taxonomy <- npi::npi_flatten(result)
        # Follow-up data transformations and filters
        data_taxonomy <- dplyr::distinct(data_taxonomy, "npi", .keep_all = TRUE)
        data_taxonomy <- dplyr::mutate(data_taxonomy, search_term = taxonomy)
        data_taxonomy <- dplyr::filter(data_taxonomy, addresses_country_name == "United States")
        data_taxonomy <- dplyr::mutate(data_taxonomy, basic_credential = stringr::str_remove_all(basic_credential, "[[\\p{P}][\\p{S}]]"))
        data_taxonomy <- dplyr::filter(data_taxonomy, stringr::str_to_lower(basic_credential) %in% stringr::str_to_lower(c("MD", "DO")))
        data_taxonomy <- dplyr::arrange(data_taxonomy, basic_last_name)
        data_taxonomy <- dplyr::filter(data_taxonomy, stringr::str_detect(taxonomies_desc, taxonomy))
        data_taxonomy <- dplyr::select(data_taxonomy, -c(basic_credential, basic_last_updated, basic_status, basic_name_prefix,
                                                         basic_name_suffix, basic_certification_date, other_names_type,
                                                         other_names_code, other_names_credential, other_names_first_name,
                                                         other_names_last_name, other_names_prefix, other_names_suffix,
                                                         other_names_middle_name, identifiers_code, identifiers_desc,
                                                         identifiers_identifier, identifiers_state, identifiers_issuer,
                                                         taxonomies_code, taxonomies_taxonomy_group, taxonomies_state,
                                                         taxonomies_license, addresses_country_code, addresses_country_name,
                                                         addresses_address_purpose, addresses_address_type, addresses_address_2,
                                                         addresses_fax_number, endpoints_endpointType, endpoints_endpointTypeDescription,
                                                         endpoints_endpoint, endpoints_affiliation, endpoints_useDescription,
                                                         endpoints_contentTypeDescription, endpoints_country_code, endpoints_country_name,
                                                         endpoints_address_type, endpoints_address_1, endpoints_city,
                                                         endpoints_state, endpoints_postal_code, endpoints_use, endpoints_endpointDescription,
                                                         endpoints_affiliationName, endpoints_contentType, endpoints_contentOtherDescription,
                                                         endpoints_address_2, endpoints_useOtherDescription))
        data_taxonomy <- dplyr::distinct(data_taxonomy, "npi", .keep_all = TRUE)
        data_taxonomy <- dplyr::distinct(data_taxonomy, basic_first_name, basic_last_name, basic_middle_name,
                                         basic_sole_proprietor, basic_gender, basic_enumeration_date, addresses_state, .keep_all = TRUE)
        data_taxonomy <- dplyr::mutate(data_taxonomy, full_name = paste(stringr::str_to_lower(basic_first_name), stringr::str_to_lower(basic_last_name)))

        # Append the data for the current taxonomy to the main data frame
        data <- dplyr::bind_rows(data, data_taxonomy)
        cat("Data appended for taxonomy:", taxonomy, "\n")
      } else {
        cat("No data found for taxonomy:", taxonomy, "\n")
      }
    }, error = function(e) {
      message(sprintf("Error in search for %s:\n%s", taxonomy, e$message))
    })
  }

  # Attempt to write the combined data frame to an RDS file
  tryCatch({
    filename <- paste("data/search_taxonomy", format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), ".rds", sep = "_")
    readr::write_rds(data, filename)
    cat("Data saved to file:", filename, "\n")
  }, error = function(e) {
    message("Error saving data to file:\n", e$message)
  })

  return(data)
}
