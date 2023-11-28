#' Calculate Proportion and Generate Tabulation
#'
#' This function calculates the proportion of each category in a specified variable
#' and generates a tabulation of the counts and proportions.
#'
#' @param df A data frame containing the data.
#' @param variable_name The name of the categorical variable for which to calculate the proportion.
#' @return A list containing the calculated most common value, proportion variable, and the tabulation result.
#'
#'@import dplyr
#'@import janitor
#'
#' @examples
#' \dontrun{
#' library(tyler)
#' df <- data.frame(gender = c("Male", "Female", "Male", "Male", "Female"))
#' result <- calculate_proportion(df, "gender")
#' cat("Most Common Value: ", result$most_common, "\n")
#' cat("Proportion Variable: ", result$proportion_variable, "%\n")
#' print(result$tabyl_result)
#' }
#'
#' @export
calculate_proportion <- function(df, variable_name) {

  cat("Calculating proportions and generating tabulation...\n")

  tabyl_result <- df %>%
    dplyr::count(variable_name, name = "n") %>%
    dplyr::mutate(percent = n / sum(n))

  most_common <- get_most_common(tabyl_result, variable_name)
  proportion_variable <- calculate_proportion_variable(tabyl_result)

  cat("Calculation complete.\n")

  return(list(most_common = most_common, proportion_variable = proportion_variable, tabyl_result = tabyl_result))
}

#' Get the Most Common Value
#'
#' This function calculates and returns the most common value from the tabulation result.
#'
#' @param tabyl_result The tabulation result data frame.
#' @param variable_name The name of the categorical variable.
#' @return The most common value.
#'
#'
get_most_common <- function(tabyl_result, variable_name) {
  cat("Calculating the most common value...\n")
  most_common <- tabyl_result %>%
    dplyr::filter(percent == max(percent)) %>%
    dplyr::pull(variable_name)
  cat("Most common value calculated.\n")
  return(most_common)
}

#' Calculate the Proportion Variable
#'
#' This function calculates and returns the proportion variable, which is the maximum percentage in the tabulation result.
#'
#' @param tabyl_result The tabulation result data frame.
#' @return The proportion variable as a percentage.
#'
#'
calculate_proportion_variable <- function(tabyl_result) {
  cat("Calculating the proportion variable...\n")
  proportion_variable <- max(tabyl_result$percent)
  proportion_variable <- round(proportion_variable * 100, 1)
  cat("Proportion variable calculated.\n")
  return(proportion_variable)
}
