#' Calculate the Proportion of Each Level in a Categorical Variable
#'
#' This function calculates the proportion of each level in a specified categorical variable within a data frame.
#' It returns a data frame with the counts and percentages of each level.
#'
#' @param data A data frame containing the categorical variable.
#' @param variable_name The name of the categorical variable for which proportions are calculated, passed as an unquoted expression.
#'
#' @return A data frame with two columns: `n` (the count of each level) and `percent` (the percentage of the total count represented by each level).
#'
#' @details The function counts the occurrences of each unique value in the specified variable and calculates the percentage each value represents of the total count.
#' The percentages are rounded to two decimal places.
#'
#' @examples
#' # Example 1: Basic usage with a simple dataset
#' data <- data.frame(gender = c("Male", "Female", "Female", "Male", "Male", "Female"))
#' result <- calculate_proportion(data, gender)
#' print(result)
#'
#' # Example 2: Handling a dataset with missing values
#' df_na <- data.frame(gender = c("Male", NA, "Female", "Female", "Male", "Female", NA))
#' result <- calculate_proportion(df_na, gender)
#' print(result)
#'
#' # Example 3: Using a variable with multiple levels
#' df_multi <- data.frame(grade = c("A", "B", "A", "C", "B", "A", "C", "B"))
#' result <- calculate_proportion(df_multi, grade)
#' print(result)
#'
#' @import dplyr
#' @export
calculate_proportion <- function(data, variable_name) {
  tabyl_result <- data %>%
    count({{ variable_name }}, name = "n") %>%
    mutate(percent = n / sum(n) * 100)

  tabyl_result <- tabyl_result %>%
    mutate(across(where(is.numeric), round, 2))

  return(tabyl_result)
}
