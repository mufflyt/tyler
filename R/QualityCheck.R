#' Save Quality Check Table
#'
#' This function takes a data frame containing 'npi' and 'name' columns and creates a quality check table.
#' The table includes the count of observations for each 'npi' and 'name' combination where the count is greater than 2.
#' The resulting table is saved as a CSV file.
#'
#' @param df A data frame containing the columns 'npi' and 'name'.
#' @param filepath The path where the CSV file should be saved.
#' @return Prints a message to the console indicating that the CSV file has been saved successfully.
#' @importFrom dplyr group_by summarise arrange filter
#' @export

save_quality_check_table <- function(df, filepath) {
  # Group by 'npi' and 'name', calculate counts, filter where count > 2, and arrange by count descending
  filtered_data <- df %>%
    dplyr::group_by(npi, name) %>%
    dplyr::summarise(count = n(), .groups = 'drop') %>%
    dplyr::filter(count > 2) %>%
    dplyr::arrange(desc(count))

  # Save the filtered data to a CSV file
  write.csv(filtered_data, file = filepath, row.names = FALSE)

  # Print a message indicating successful file save
  cat("CSV file saved successfully!\n")

  # Return the filtered data for testing purposes
  return(filtered_data)
}
