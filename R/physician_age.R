#' Calculate and Summarize Physician Age
#'
#' This function calculates the median age, as well as the 25th and 75th percentiles (Interquartile Range, IQR) of a specified age column in a data frame. It returns a sentence summarizing these statistics.
#'
#' @param df A data frame containing the age data.
#' @param age_column A character string representing the name of the column in `df` that contains the age data.
#'
#' @return A character string summarizing the median age and IQR of the specified age column in the dataset.
#'
#' @details The function calculates the median, 25th percentile (Q1), and 75th percentile (Q3) of the age data, rounding the results to two decimal places for the median and one decimal place for the percentiles. It then constructs a summary sentence describing these statistics.
#'
#' @examples
#' # Example 1: Basic usage with a small dataset
#' df <- data.frame(age = c(30, 40, 50, 60, 35, 45, 55, 65))
#' summary_sentence <- physician_age(df, "age")
#' print(summary_sentence)
#'
#' # Example 2: Handling missing data
#' df_with_na <- data.frame(age = c(30, 40, NA, 60, 35, NA, 55, 65))
#' summary_sentence <- physician_age(df_with_na, "age")
#' print(summary_sentence)
#'
#' # Example 3: Different age distribution
#' df_large <- data.frame(age = c(rep(30, 70), rep(40, 30), rep(50, 20), rep(60, 10)))
#' summary_sentence <- physician_age(df_large, "age")
#' print(summary_sentence)
#'
#' @import dplyr
#' @export
physician_age <- function(df, age_column) {
  # Calculate the median age
  median_age <- round(median(df[[age_column]], na.rm = TRUE), 2)

  # Calculate the 25th and 75th percentiles
  q25 <- quantile(df[[age_column]], probs = 0.25, na.rm = TRUE)
  q75 <- quantile(df[[age_column]], probs = 0.75, na.rm = TRUE)

  # Round the percentiles to one decimal place
  q25 <- round(q25, 1)
  q75 <- round(q75, 1)

  # Create the sentence
  sentence <- paste0(
    "The median age of the dataset was ", median_age,
    " (IQR 25th percentile ", q25, " to 75th percentile ", q75, ")."
  )

  return(sentence)
}
