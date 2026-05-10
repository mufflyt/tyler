#' Classify RUCA codes into Urban / Suburban / Rural
#'
#' @name mysterycall_classify_ruca
NULL


#' Classify RUCA codes into Urban, Suburban, or Rural
#'
#' The Rural-Urban Commuting Area (RUCA) classification system assigns numeric
#' codes to ZIP codes based on commuting patterns and population density. This
#' function maps those numeric codes to three categories used in healthcare
#' access research:
#'
#' | RUCA code | Category |
#' |---|---|
#' | 1-3  | Urban    |
#' | 4-6  | Suburban |
#' | 7-10 | Rural    |
#'
#' ZIP-to-RUCA crosswalk files are available from USDA ERS
#' (<https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes>).
#' Join your data to the crosswalk first, then pass the resulting `ruca_code`
#' column to this function.
#'
#' @param ruca_code Numeric vector of RUCA codes (typically integer or decimal,
#'   e.g. 1, 2, 3.1, 10.6). `NA` values are returned as `na_label`.
#' @param urban_max Numeric. RUCA codes `<= urban_max` are classified as Urban.
#'   Default `3`.
#' @param suburban_max Numeric. RUCA codes `> urban_max` and `<= suburban_max`
#'   are classified as Suburban. Default `6`.
#' @param labels Character vector of length 3 giving the labels for Urban,
#'   Suburban, and Rural categories in that order.
#'   Default `c("Urban", "Suburban", "Rural")`.
#' @param na_label Character scalar returned for `NA` inputs.
#'   Default `"Unknown"`.
#' @param as_factor Logical. When `TRUE` the result is returned as an ordered
#'   factor with levels `c(labels, na_label)`. Default `FALSE`.
#'
#' @return Character vector (or ordered factor) the same length as `ruca_code`.
#'
#' @family provider characteristics
#' @export
#'
#' @examples
#' codes <- c(1, 2, 4, 5, 7, 10, NA)
#' mysterycall_classify_ruca(codes)
#' # [1] "Urban"   "Urban"   "Suburban" "Suburban" "Rural"  "Rural"  "Unknown"
#'
#' mysterycall_classify_ruca(codes, as_factor = TRUE)
mysterycall_classify_ruca <- function(ruca_code,
                                       urban_max    = 3,
                                       suburban_max = 6,
                                       labels       = c("Urban", "Suburban", "Rural"),
                                       na_label     = "Unknown",
                                       as_factor    = FALSE) {

  if (!is.numeric(ruca_code)) {
    stop("`ruca_code` must be a numeric vector.", call. = FALSE)
  }
  if (!is.numeric(urban_max)    || length(urban_max)    != 1L) {
    stop("`urban_max` must be a single number.", call. = FALSE)
  }
  if (!is.numeric(suburban_max) || length(suburban_max) != 1L) {
    stop("`suburban_max` must be a single number.", call. = FALSE)
  }
  if (suburban_max <= urban_max) {
    stop("`suburban_max` must be greater than `urban_max`.", call. = FALSE)
  }
  if (!is.character(labels) || length(labels) != 3L) {
    stop("`labels` must be a character vector of length 3.", call. = FALSE)
  }
  if (!is.character(na_label) || length(na_label) != 1L) {
    stop("`na_label` must be a single character string.", call. = FALSE)
  }

  result <- ifelse(
    is.na(ruca_code), na_label,
    ifelse(ruca_code <= urban_max,    labels[[1L]],
    ifelse(ruca_code <= suburban_max, labels[[2L]],
                                      labels[[3L]]))
  )

  if (as_factor) {
    all_levels <- c(labels, na_label)
    result <- factor(result, levels = all_levels, ordered = TRUE)
  }

  result
}
