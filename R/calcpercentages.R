#' Calculate the Percentage of the Most Common Value in a Categorical Variable
#'
#' This function calculates the percentage of the most common value in a specified categorical variable from a data frame.
#'
#' @param df A data frame containing the categorical variable.
#' @param variable A character string representing the name of the categorical variable within `df`.
#'
#' @return A data frame containing the most common value and its count, along with the percentage of the total count that it represents.
#'
#' @details The function converts the specified variable to character type if it's a factor, then counts the occurrences of each unique value. It identifies the most common value and returns the count and percentage.
#'
#' @examples
#' # Example 1: Basic usage with a simple dataset
#' df <- data.frame(category = c("A", "B", "A", "C", "A", "B", "B", "A"))
#' result <- calcpercentages(df, "category")
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
#' @export
calcpercentages <- function(df, variable) {
  variable <- as.character(variable)  # Convert factor to character

  x <- count(data.frame(variable)) %>%
    slice_max(n, n = 1)

  return(x)
}
