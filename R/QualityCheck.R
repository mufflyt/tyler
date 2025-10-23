#' Save Quality Check Table
#'
#' This function takes a data frame containing `npi` and `name` columns and
#' creates a quality check table. The table includes the count of observations
#' for each `npi` and `name` combination where the count is greater than 2. The
#' resulting table is saved as a CSV file and returned invisibly for further
#' inspection.
#'
#' @param data A data frame containing the columns 'npi' and 'name'.
#' @param filepath The path where the CSV file should be saved.
#' @param verbose Logical flag controlling progress messages.
#' @return Invisibly returns the filtered tibble with an `"output_path"`
#'   attribute containing the saved CSV path.
#' @importFrom dplyr arrange desc filter group_by n summarise
#' @importFrom readr write_csv
#' @importFrom rlang .data
#' @family utilities
#' @export
#' @examples
#' \dontrun{
#' save_quality_check_table(my_data, "qc.csv")
#' }

save_quality_check_table <- function(data, filepath, verbose = TRUE) {
  assert_is_dataframe(data, "data")
  assert_has_columns(data, c("npi", "name"), "data")

  filtered_data <- data %>%
    dplyr::group_by(.data$npi, .data$name) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(.data$count > 2) %>%
    dplyr::arrange(dplyr::desc(.data$count))

  if (!dir.exists(dirname(filepath))) {
    dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
  }

  readr::write_csv(filtered_data, filepath)
  workflow_log(sprintf("Saved quality check table to %s", filepath), verbose = verbose)
  attr(filtered_data, "output_path") <- filepath

  invisible(filtered_data)
}
