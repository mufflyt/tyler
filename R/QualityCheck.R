#' Save Quality Check Table
#'
#' This function takes a data frame containing 'npi' and 'name' columns and creates a quality check table.
#' The table includes the count of observations for each 'npi' and 'name' combination where the count is greater than 2.
#' The resulting table is saved as a CSV file.
#'
#' @param df A data frame containing the columns 'npi' and 'name'.
#' @param filepath The path where the CSV file should be saved. Defaults to "obgyn/data/final_obgyn_results_of_Marcos_code/quality_check_table.csv".
#' @return Prints a message to the console indicating that the CSV file has been saved successfully.
#' @examples
#' df <- data.frame(npi = c(1, 2, 1, 2, 2), name = c("A", "B", "A", "B", "B"))
#' saveQualityCheckTable(df)
#' @export

qualitycheck <- function(df, filepath = "obgyn/data/final_obgyn_results_of_Marcos_code/quality_check_table.csv") {
  temp <- df %>%
    group_by(npi, name) %>% # Group the data by 'npi' and 'name'
    summarise(N = n()) %>% # Calculate the count of observations within each group and create a column named 'N'
    arrange(desc(N)) %>% # Arrange the data in descending order based on the 'N' column
    filter(N > 2) # Keep only the rows where the count 'N' is greater than 2

  # Save the original temp data frame to a CSV file
  write.csv(temp, file = filepath, row.names = FALSE)

  # If you want to inform the user that the file has been saved
  cat("CSV file saved successfully!")
}
