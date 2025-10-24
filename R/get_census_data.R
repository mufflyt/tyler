#' Get Census data of all state block groups
#'
#' This function retrieves Census data of all state block groups by looping
#' over the specified list of state FIPS codes.
#'
#' @param us_fips_list A vector of state FIPS codes which Census data is to be retrieved.
#' @param vintage The vintage year of Census data (default is 2022).
#' @param api_key Census API key to use for requests. Defaults to the `CENSUS_API_KEY` environment variable.
#'
#' @return A tibble containing Census block group data for the requested
#'   states. The result includes padded 2020 FIPS columns (`statefp`,
#'   `countyfp`, `tractce`, `block_group`, `geoid`) and a `vintage` column
#'   indicating the Census vintage that supplied the estimates.
#' @importFrom dplyr bind_rows mutate rename
#' @importFrom censusapi getCensus
#' @importFrom tibble as_tibble tibble
#' @importFrom stringr str_pad
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
    return(tibble::tibble())
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
      vars = c("NAME", sprintf("B01001_%03dE", c(1, 2, 26:49))),
      region = "block group:*",
      regionin = stateget,
      key = api_key
    )

    if (!is.null(res) && nrow(res)) {
      res <- tibble::as_tibble(res, .name_repair = "minimal")

      if ("block group" %in% names(res)) {
        res <- dplyr::rename(res, block_group = `block group`)
      }

      if (!"block_group" %in% names(res)) {
        stop("Census response did not include a `block group` column.")
      }

      res <- dplyr::rename(res, name = NAME)

      res <- dplyr::mutate(
        res,
        statefp = stringr::str_pad(.data$state, 2, pad = "0"),
        countyfp = stringr::str_pad(.data$county, 3, pad = "0"),
        tractce = stringr::str_pad(.data$tract, 6, pad = "0"),
        block_group = stringr::str_pad(.data$block_group, 1, pad = "0"),
        geoid = paste0(.data$statefp, .data$countyfp, .data$tractce, .data$block_group),
        vintage = vintage
      )

      state_data[[f]] <- res
    }
  }

  if (!length(state_data)) {
    return(tibble::tibble())
  }

  acs_raw <- dplyr::bind_rows(state_data)

  if (requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  acs_raw
}
