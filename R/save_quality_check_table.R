#' Save Quality Check Table
#'
#' This function takes a data frame containing 'npi' and 'name' columns and creates a quality check table.
#' The table includes the count of observations for each 'npi' and 'name' combination where the count is greater than 2.
#' The resulting table is saved as a CSV file.
#'
#' @param data A data frame containing the columns 'npi' and 'name'.
#' @param filepath The path where the CSV file should be saved.
#' @return The filtered data. A message is emitted indicating where the CSV was saved.
#' @details
#' The output table aggregates by `npi` and `name`, keeps combinations with more
#' than two records, sorts descending by frequency, and writes the result to CSV.
#' This helper is useful for flagging repeated provider entries that may require
#' manual review.
#' @importFrom dplyr group_by summarise arrange filter n desc
#' @seealso [run_mystery_caller_workflow()], [validate_dataframe()],
#'   [split_and_save()]
#' @family utilities
#' @export
#' @examples
#' \dontrun{
#' save_quality_check_table(my_data, "qc.csv")
#' }

save_quality_check_table <- function(data, filepath) {
  # Group by 'npi' and 'name', calculate counts, filter where count > 2, and arrange by count descending
  filtered_data <- data %>%
    dplyr::group_by(npi, name) %>%
    dplyr::summarise(count = n(), .groups = 'drop') %>%
    dplyr::filter(count > 2) %>%
    dplyr::arrange(desc(count))

  # Save the filtered data to a CSV file
  dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
  write.csv(filtered_data, file = filepath, row.names = FALSE)

  # Print a message indicating successful file save with context
  message(sprintf(
    "Saved quality-check table with %d row(s) to %s.",
    nrow(filtered_data),
    filepath
  ))

  # Return the filtered data for testing purposes
  return(filtered_data)
}
