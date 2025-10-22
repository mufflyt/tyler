#' Get Census data of all state block groups
#'
#' This function retrieves Census data of all state block groups by looping
#' over the specified list of state FIPS codes.
#'
#' @param us_fips_list A vector of state FIPS codes which Census data is to be retrieved.
#' @param vintage The vintage year is Census data (default is 2022).
#' @param api_key Census API key to use for requests. Defaults to the `CENSUS_API_KEY` environment variable.
#'
#' @return A dataframe containing Census data all state block groups.
#' @importFrom dplyr bind_rows tibble
#' @importFrom censusapi getCensus
#' @family census
#' @export
#' @examples
#' \dontrun{
#' us_fips_list <- c("01", "02")
#' census_df <- get_census_data(us_fips_list)
#' }
get_census_data <- function(us_fips_list, vintage = 2022, api_key = Sys.getenv("CENSUS_API_KEY")) {
  if (is.null(us_fips_list)) {
    stop("`us_fips_list` must be a character vector of FIPS codes.")
  }

  if (!length(us_fips_list)) {
    return(dplyr::tibble())
  }

  if (!is.character(us_fips_list)) {
    stop("`us_fips_list` must be a character vector of FIPS codes.")
  }

  if (!nzchar(api_key)) {
    stop("Census API key required via argument or CENSUS_API_KEY env var.")
  }

  state_data <- list()

  for (f in us_fips_list) {
    stateget <- paste0("state:", f, "&in=county:*&in=tract:*")
    res <- censusapi::getCensus(
      name = "acs/acs5",
      vintage = vintage,
      vars = c("NAME", paste0("B01001_0", c("01", 26, 33:49), "E")),
      region = "block group:*",
      regionin = stateget,
      key = api_key
    )

    if (!is.null(res) && nrow(res)) {
      state_data[[f]] <- res
    }
  }

  if (!length(state_data)) {
    return(dplyr::tibble())
  }

  acs_raw <- dplyr::bind_rows(state_data)

  if (requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  acs_raw
}
