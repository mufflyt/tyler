#' Remove Near-Zero Variance Variables from a Data Frame
#'
#' This function takes a data frame and returns a new data frame with near-zero variance variables removed.
#'
#' @param data_frame A data frame from which near-zero variance variables should be removed.
#' @param freqCut The ratio of the most common value to the second most common value. Defaults to 19.
#' @param uniqueCut The percentage of distinct values out of the number of total samples. Defaults to 10.
#'
#' @return A data frame with near-zero variance variables removed.
#'
#' @examples
#' \dontrun{
#' new_data <- remove_near_zero_var(data_frame)
#' }
#'
#'@importFrom caret nearZeroVar
#'@importFrom glue glue
#'@importFrom dplyr select
#'
#' @export
remove_near_zero_var <- function(data_frame, freqCut = 19, uniqueCut = 10) {
  # Log: Starting the function
  message("Starting the function to remove near-zero variance variables.")

  # Log: Checking if the data frame is empty
  if (nrow(data_frame) == 0 || ncol(data_frame) == 0) {
    message("The data frame is empty. Exiting function.")
    return(data_frame)
  }

  # Identify near-zero variance variables
  message("Identifying near-zero variance variables...")
  remove_cols <- caret::nearZeroVar(data_frame, names = TRUE, freqCut = freqCut, uniqueCut = uniqueCut)
  remove_cols <- sort(remove_cols)

  # Log: Number of near-zero variance variables found
  message(glue::glue("Found {length(remove_cols)} near-zero variance variables."))

  # Remove near-zero variance variables if any
  if (length(remove_cols) > 0) {
    # Log: Removing near-zero variance variables
    message("Removing near-zero variance variables...")
    data_frame <- data_frame %>% dplyr::select(-dplyr::all_of(remove_cols))
  } else {
    # Log: No near-zero variance variables to remove
    message("No near-zero variance variables to remove.")
  }

  # Log: Function completed
  message("Function completed.")

  return(data_frame)
}

# Example usage:
# new_data <- remove_near_zero_var(d)
