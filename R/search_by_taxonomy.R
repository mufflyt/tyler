#' Search NPI Database by Taxonomy
#'
#' This function searches the NPI Database for healthcare providers based on a taxonomy description.
#'
#' @param taxonomy_to_search A character vector containing the taxonomy description(s) to search for.
#' @return A data frame with filtered NPI data based on the specified taxonomy description.
#' @examples
#' # Example usage with multiple taxonomy descriptions:
#' data <- search_by_taxonomy(c("Gynecologic Oncology", "Female Pelvic Medicine and Reconstructive Surgery", "Reproductive Endocrinology", "Maternal & Fetal Medicine"))
#'
#' @import npi
#' @import dplyr
#' @import stringr
#' @export

search_by_taxonomy <- function(taxonomy_to_search) {
  # Create an empty list to store search results
  out <- list()

  # Loop over each taxonomy description
  for (taxonomy in taxonomy_to_search) {
    tryCatch({
      # Perform the search for the current taxonomy
      result <- npi::npi_search(
        taxonomy_description = taxonomy,
        country_code = "US",
        enumeration_type = "ind",
        limit = 1200
      )

      if (!is.null(result)) {
        out[[taxonomy]] <- result
      }
    }, error = function(e) {
      message(sprintf("Error in search for %s:\n%s", taxonomy, e$message))
    })
  }

  # Create a list of data frames for each taxonomy
  data_list <- lapply(names(out), function(taxonomy) {
    data <- npi::npi_flatten(out[[taxonomy]]) %>%
      dplyr::distinct(npi, .keep_all = TRUE) %>%
      dplyr::filter(addresses_country_name == "United States") %>%
      dplyr::mutate(basic_credential = stringr::str_remove_all(basic_credential, "[[\\p{P}][\\p{S}]]")) %>%
      dplyr::filter(stringr::str_to_lower(basic_credential) %in% stringr::str_to_lower(c("MD", "DO"))) %>%
      dplyr::arrange(basic_last_name) %>%
      dplyr::filter(stringr::str_detect(taxonomies_desc, taxonomy)) %>%
      dplyr::select(-basic_credential, -basic_last_updated, -basic_status, -basic_name_prefix, -basic_name_suffix, -basic_certification_date, -other_names_type, -other_names_code, -other_names_credential, -other_names_first_name, -other_names_last_name, -other_names_prefix, -other_names_suffix, -other_names_middle_name, -identifiers_code, -identifiers_desc, -identifiers_identifier, -identifiers_state, -identifiers_issuer, -taxonomies_code, -taxonomies_taxonomy_group, -taxonomies_desc, -taxonomies_state, -taxonomies_license, -addresses_country_code, -addresses_country_name, -addresses_address_purpose, -addresses_address_type, -addresses_address_2, -addresses_fax_number, -endpoints_endpointType, -endpoints_endpointTypeDescription, -endpoints_endpoint, -endpoints_affiliation, -endpoints_useDescription, -endpoints_contentTypeDescription, -endpoints_country_code, -endpoints_country_name, -endpoints_address_type, -endpoints_address_1, -endpoints_city, -endpoints_state, -endpoints_postal_code, -endpoints_use, -endpoints_endpointDescription, -endpoints_affiliationName, -endpoints_contentType, -endpoints_contentOtherDescription, -endpoints_address_2, -endpoints_useOtherDescription) %>%
      dplyr::distinct(npi, .keep_all = TRUE) %>%
      dplyr::distinct(basic_first_name, basic_last_name, basic_middle_name, basic_sole_proprietor, basic_gender, basic_enumeration_date, addresses_state, .keep_all = TRUE) %>%
      dplyr::mutate(full_name = paste(
        stringr::str_to_lower(basic_first_name),
        stringr::str_to_lower(basic_last_name)
      ))

    return(data)
  })

  # Write each data frame to a separate RDS file
  for (i in seq_along(data_list)) {
    filename <- paste(taxonomy_to_search[i], "data/search_taxonomy", format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), ".rds", sep = "_")
    readr::write_rds(data_list[[i]], filename)
  }

  return(data_list)
}


# search_by_taxonomy <- function(taxonomy_to_search) {
#   # Create empty list to store search results
#   out <- list()
#
#   # Loop over each taxonomy description and search for NPI numbers
#   for (i in 1:length(taxonomy_to_search)) {
#     tryCatch({
#       out[[i]] <- npi::npi_search(taxonomy_description = taxonomy_to_search[i],
#                                   country_code = "US",
#                                   enumeration_type = "ind",
#                                   #use_first_name_alias = FALSE,
#                                   limit = 1200)
#     }, error = function(e) {
#       message(sprintf("Error in search %s: %s\n%s", i, taxonomy_to_search[i], e$message))
#     })
#   }
#
#   # Combine search results into a single data frame
#   out <- do.call(rbind, out)
#
#   # Process and filter the data using the dynamic taxonomy description
#   data <- npi_flatten(out) %>%
#     dplyr::distinct(npi, .keep_all = TRUE) %>%
#     dplyr::filter(addresses_country_name == "United States") %>%
#     dplyr::mutate(basic_credential = stringr::str_remove_all(basic_credential, "[[\\p{P}][\\p{S}]]")) %>%
#     dplyr::filter(stringr::str_to_lower(basic_credential) %in% stringr::str_to_lower(c("MD", "DO"))) %>%
#     dplyr::arrange(basic_last_name) %>%
#     dplyr::filter(stringr::str_detect(taxonomies_desc, taxonomy_to_search)) %>% # Use dynamic taxonomy_to_search variable
#     dplyr::select(-basic_credential, -basic_last_updated, -basic_status, -basic_name_prefix, -basic_name_suffix, -basic_certification_date, -other_names_type, -other_names_code, -other_names_credential, -other_names_first_name, -other_names_last_name, -other_names_prefix, -other_names_suffix, -other_names_middle_name, -identifiers_code, -identifiers_desc, -identifiers_identifier, -identifiers_state, -identifiers_issuer, -taxonomies_code, -taxonomies_taxonomy_group, -taxonomies_desc, -taxonomies_state, -taxonomies_license, -addresses_country_code, -addresses_country_name, -addresses_address_purpose, -addresses_address_type, -addresses_address_2, -addresses_fax_number, -endpoints_endpointType, -endpoints_endpointTypeDescription, -endpoints_endpoint, -endpoints_affiliation, -endpoints_useDescription, -endpoints_contentTypeDescription, -endpoints_country_code, -endpoints_country_name, -endpoints_address_type, -endpoints_address_1, -endpoints_city, -endpoints_state, -endpoints_postal_code, -endpoints_use, -endpoints_endpointDescription, -endpoints_affiliationName, -endpoints_contentType, -endpoints_contentOtherDescription, -endpoints_address_2, -endpoints_useOtherDescription) %>%
#     dplyr::distinct(npi, .keep_all = TRUE) %>%
#     dplyr::distinct(basic_first_name, basic_last_name, basic_middle_name, basic_sole_proprietor, basic_gender, basic_enumeration_date, addresses_state, .keep_all = TRUE) %>%
#     dplyr::mutate(full_name = paste(
#       stringr::str_to_lower(basic_first_name),
#       stringr::str_to_lower(basic_last_name)
#     ))
#
#   readr::write_rds(data, paste(taxonomy_to_search, "data/search_taxonomy", format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), ".rds"))
#
#   return(data)
# }
#
# # Example usage of the function with dynamic taxonomy_to_search variable:
# # data <- search_taxonomy("Gynecologic Oncology")
# # , sep
