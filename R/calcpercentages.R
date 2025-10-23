#' Calculate the Percentage of the Most Common Value in a Categorical Variable
#'
#' This function calculates the percentage of the most common value in a specified categorical variable from a data frame.
#'
#' @param data_frame A data frame containing the categorical variable.
#' @param variable A character string representing the name of the categorical variable within `data_frame`.
#' @param verbose Logical; if TRUE, prints status messages during calculation.
#'
#' @return A data frame containing the most common value and its count, along with the percentage of the total count that it represents.
#'
#' @details The function converts the variable name to a character string, then counts
#'   the occurrences of each unique value in the specified column. It calculates the
#'   percentage each value represents of the total and returns the most common value
#'   with its count and percentage.
#'
#' @examples
#' # Example 1: Basic usage with a simple dataset
#' data_frame <- data.frame(category = c("A", "B", "A", "C", "A", "B", "B", "A"))
#' result <- calcpercentages(data_frame, "category")
#' print(result)
#'
#' # Example 2: Using a dataset with multiple most common values
#' df_tie <- data.frame(category = c("A", "B", "A", "B", "C", "C", "C", "A", "B"))
#' result <- calcpercentages(df_tie, "category")
#' print(result)
#'
#' # Example 3: Handling a dataset with missing values
#' df_na <- data.frame(category = c("A", NA, "A", "C", "A", "B", "B", NA))
#' result <- calcpercentages(df_na, "category")
#' print(result)
#'
#' @import dplyr
#' @importFrom rlang sym
#' @export
calcpercentages <- function(data_frame, variable, verbose = FALSE) {
  variable <- as.character(variable)  # Ensure the variable name is a string

  summary_df <- data_frame %>%
    dplyr::count(!!rlang::sym(variable), name = "n") %>%
    dplyr::mutate(percent = 100 * n / sum(n)) %>%
    dplyr::slice_max(n, n = 1)

  return(summary_df)
}
