#' Calculate and Summarize Physician Age
#'
#' This function calculates the median age, as well as the 25th and 75th percentiles (Interquartile Range, IQR) of a specified age column in a data frame. It returns a sentence summarizing these statistics.
#'
#' @param data A data frame containing the age data.
#' @param age_column A character string representing the name of the column in `data` that contains the age data.
#'
#' @return A character string summarizing the median age and IQR of the specified age column in the dataset.
#'
#' @details The function calculates the median, 25th percentile (Q1), and 75th percentile (Q3) of the age data, rounding the results to two decimal places for the median and one decimal place for the percentiles. It then constructs a summary sentence describing these statistics.
#'
#' @examples
#' # Example 1: Basic usage with a small dataset
#' data <- data.frame(age = c(30, 40, 50, 60, 35, 45, 55, 65))
#' summary_sentence <- mysterycall_physician_age(data, "age")
#' print(summary_sentence)
#'
#' # Example 2: Handling missing data
#' df_with_na <- data.frame(age = c(30, 40, NA, 60, 35, NA, 55, 65))
#' summary_sentence <- mysterycall_physician_age(df_with_na, "age")
#' print(summary_sentence)
#'
#' # Example 3: Different age distribution
#' df_large <- data.frame(age = c(rep(30, 70), rep(40, 30), rep(50, 20), rep(60, 10)))
#' summary_sentence <- mysterycall_physician_age(df_large, "age")
#' print(summary_sentence)
#'
#' @family summary
#' @export
mysterycall_physician_age <- function(data, age_column) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!age_column %in% names(data)) {
    stop(sprintf("Column '%s' not found in data.", age_column), call. = FALSE)
  }

  age_vals <- data[[age_column]][!is.na(data[[age_column]])]
  if (length(age_vals) < 2) {
    stop(sprintf("Column '%s' must have at least 2 non-missing values to compute age statistics.", age_column), call. = FALSE)
  }

  # Calculate the median age
  median_age <- round(median(age_vals), 2)

  # Calculate the 25th and 75th percentiles
  q25 <- quantile(age_vals, probs = 0.25)
  q75 <- quantile(age_vals, probs = 0.75)

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
