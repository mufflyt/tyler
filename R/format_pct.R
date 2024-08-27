#' Format a Numeric Value as a Percentage
#'
#' This function formats a numeric value as a percentage with a specified number of decimal places.
#'
#' @param x A numeric value or vector that you want to format as a percentage.
#' @param my_digits An integer specifying the number of decimal places to include in the formatted percentage. The default is 1.
#'
#' @return A character vector representing the formatted percentage(s) with the specified number of decimal places.
#'
#' @details The function converts a numeric value to a percentage format with the specified number of decimal places.
#' This is useful for consistent display of percentage values in reports or visualizations.
#'
#' @examples
#' # Example 1: Format a single numeric value
#' result <- format_pct(0.12345)
#' print(result)  # Output: "12.3%"
#'
#' # Example 2: Format a vector of numeric values with 2 decimal places
#' values <- c(0.12345, 0.6789, 0.54321)
#' formatted_values <- format_pct(values, my_digits = 2)
#' print(formatted_values)  # Output: "12.35%", "67.89%", "54.32%"
#'
#' # Example 3: Format a value with no decimal places
#' no_decimal <- format_pct(0.5, my_digits = 0)
#' print(no_decimal)  # Output: "50%"
#'
#' @export
format_pct <- function(x, my_digits = 1) {
  format(x, digits = my_digits, nsmall = my_digits)
}
