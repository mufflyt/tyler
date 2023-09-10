#' Calculate Proportion and Generate Tabulation
#'
#' This function calculates the proportion of each category in a specified variable
#' and generates a tabulation of the counts and proportions.
#'
#' @param df A data frame containing the data.
#' @param variable_name The name of the categorical variable for which to calculate the proportion.
#' @return A list containing the calculated proportion and the tabulation result.
#'
#' @examples
#' \dontrun{
#' library(tyler)
#' df <- data.frame(gender = c("Male", "Female", "Male", "Male", "Female"))
#' result <- calculate_proportion(df, gender)
#' # print(result$proportion)
#' # print(result$tabyl_result)
#' }
calculate_proportion <- function(df, variable_name) {
  tabyl_result <- df %>%
    dplyr::count({{ variable_name }}, name = "n") %>%
    dplyr::mutate(percent = n / sum(n))

  most_common <- tabyl_result %>%
    filter(percent == max(percent)) %>%
    dplyr::pull({{ variable_name }})

  proportion_variable <- max(tabyl_result$percent)
  proportion_variable <- round(proportion_variable * 100, 1)

  # return(list(proportion = paste0(proportion_variable, "%"), tabyl_result = tabyl_result))
  return(most_common)
}
