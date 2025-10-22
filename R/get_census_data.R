#' Get Census data of all state block groups
#'
#' This function retrieves Census data of all state block groups by looping
#' over the specified list of state FIPS codes.
#'
#' @param us_fips_list A vector of state FIPS codes which Census data is to be retrieved.
#' @param vintage The vintage year is Census data (default is 2022).
#'
#' @param api_key API key string. Defaults to the `CENSUS_API_KEY` environment
#'   variable.
#' @param quiet Should progress messages be suppressed? Default is `FALSE`.
#'
#' @return A dataframe containing Census data all state block groups.
#' @importFrom dplyr bind_rows
#' @importFrom censusapi getCensus
#' @importFrom purrr compact
#' @importFrom tibble tibble
#' @importFrom stringr str_pad
#' @family census
#' @export
#' @examples
#' \dontrun{
#' us_fips_list <- c("01", "02")
#' census_df <- get_census_data(us_fips_list)
#' }
get_census_data <- function(us_fips_list, vintage = 2022, api_key = Sys.getenv("CENSUS_API_KEY"), quiet = getOption("tyler.quiet", FALSE)) {

  if (!is.atomic(us_fips_list)) {
    stop("`us_fips_list` must be an atomic vector of state FIPS codes.")
  }

  us_fips_list <- as.character(us_fips_list)
  if (length(us_fips_list) == 0) {
    return(tibble::tibble())
  }

  if (anyNA(us_fips_list) || any(us_fips_list == "")) {
    stop("`us_fips_list` contains missing or empty values.")
  }

  if (!is.numeric(vintage) || length(vintage) != 1) {
    stop("`vintage` must be a single numeric value.")
  }

  if (!is.character(api_key) || length(api_key) != 1 || api_key == "") {
    stop("Census API key required via argument or CENSUS_API_KEY env var.")
  }

  us_fips_list <- stringr::str_pad(us_fips_list, width = 2, pad = "0")

  # Initialize an empty list to store state data
  state_data <- vector("list", length(us_fips_list))
  names(state_data) <- us_fips_list

  # Loop over states for block group data
  for (f in us_fips_list) {
    if (!quiet) {
      message("Processing FIPS: ", f)
    }

    stateget <- paste0("state:", f, "&in=county:*&in=tract:*")

    state_data[[f]] <- tryCatch(
      censusapi::getCensus(
        name = "acs/acs5",
        vintage = vintage,
        vars = c("NAME", paste0("B01001_0", c("01", 26, 33:49), "E")),
        region = "block group:*",
        regionin = stateget,
        key = api_key
      ),
      error = function(e) {
        if (!quiet) {
          message("Failed to retrieve data for FIPS ", f, ": ", conditionMessage(e))
        }
        NULL
      }
    )
  }

  acs_raw <- dplyr::bind_rows(purrr::compact(state_data))

  if (nrow(acs_raw) == 0) {
    return(tibble::tibble())
  }

  if (requireNamespace("beepr", quietly = TRUE) && !quiet) {
    beepr::beep(2)
  }

  acs_raw
}

# Usage example:
# us_fips_list <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12",
#                   "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
#                   "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
#                   "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
#                   "45", "46", "47", "48", "49", "50", "51", "53", "54", "55")
# all_census_data <- get_census_data(us_fips_list)
