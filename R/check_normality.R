#' Check Normality and Summarize Data
#'
#' This function checks the normality of a specified variable in a dataframe using the Shapiro-Wilk test
#' and provides summary statistics (mean and standard deviation if normal, median and IQR if not normal).
#'
#' @param data A dataframe containing the data.
#' @param variable A string specifying the column name of the variable to be checked and summarized.
#'
#' @return A list containing the summary statistics (mean and standard deviation if normal, median and IQR if not normal).
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density labs stat_qq stat_qq_line
#' @importFrom dplyr %>%
#' @importFrom stats shapiro.test IQR median
#'
#' @examples
#' # Example usage with a dataframe 'df' and outcome variable 'business_days_until_appointment'
#' check_normality(df, "business_days_until_appointment")
check_normality <- function(data, variable) {
  # Start logging
  message("Starting normality check and summary calculation for variable: ", variable)

  # Extract the variable data and remove NA values
  data_var <- data[[variable]]
  message("Data extracted for variable: ", variable)

  # Check if the sample size is adequate for the Shapiro-Wilk test
  if (length(data_var) < 3) {
    stop("Sample size must be at least 3 for the Shapiro-Wilk test.")
  }

  # Check normality using Shapiro-Wilk test
  normality_test <- stats::shapiro.test(data_var)
  p_value <- normality_test$p.value
  message("Shapiro-Wilk normality test completed with p-value: ", p_value)

  # Log interpretation of the Shapiro-Wilk test result
  if (p_value > 0.05) {
    message("The p-value is greater than 0.05, indicating that the data is approximately normal.")
  } else {
    message("The p-value is less than or equal to 0.05, indicating that the data is not normally distributed.")
  }

  # Create Histogram with Density Plot
  hist_plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!sym(variable))) +
    ggplot2::geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", na.rm = TRUE) +
    ggplot2::geom_density(alpha = 0.2, fill = "#FF6666", na.rm = TRUE) +
    ggplot2::labs(title = paste("Histogram and Density Plot of", variable), x = variable)
  message("Histogram with Density Plot created.")

  # Create Q-Q Plot
  qq_plot <- ggplot2::ggplot(data, ggplot2::aes(sample = !!sym(variable))) +
    ggplot2::stat_qq(na.rm = TRUE) +
    ggplot2::stat_qq_line(na.rm = TRUE) +
    ggplot2::labs(title = paste("Q-Q Plot of", variable))
  message("Q-Q Plot created.")

  # Print plots
  print(hist_plot)
  print(qq_plot)

  # Determine summary statistic based on normality
  if (p_value > 0.05) {
    # Data is approximately normal
    mean_value <- mean(data_var, na.rm = TRUE)
    sd_value <- sd(data_var, na.rm = TRUE)
    result <- list(mean = mean_value, sd = sd_value)
    message("Data is approximately normal. Mean: ", mean_value, ", SD: ", sd_value)
  } else {
    # Data is not normal
    median_value <- stats::median(data_var, na.rm = TRUE)
    iqr_value <- stats::IQR(data_var, na.rm = TRUE)
    result <- list(median = median_value, iqr = iqr_value)
    message("Data is NOT normally distributed. Use non-parametric measures like median: ", median_value, ", IQR: ", iqr_value)
  }

  # Output results
  print(result)
  message("Summary calculation completed for variable: ", variable)

  return(result)
}

# Example usage with your dataframe and outcome variable
# check_normality(df, "business_days_until_appointment")
