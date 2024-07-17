#' Remove Constant Variables from a Data Frame
#'
#' This function takes a data frame and returns a new data frame with constant variables removed.
#'
#' @param data_frame A data frame from which constant variables should be removed.
#'
#' @return A data frame with constant variables removed.
#'
#' @importFrom dplyr select where all_of
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' new_data <- remove_constant_vars(data_frame)
#' }
#'
#' @export
remove_constant_vars <- function(data_frame) {

  # Log: Starting the function
  message("Starting the function to remove constant variables.")

  # Log: Checking if the data frame is empty
  if (nrow(data_frame) == 0 || ncol(data_frame) == 0) {
    message("The data frame is empty. Exiting function.")
    return(data_frame)
  }

  # Identify constant variables
  message("Identifying constant variables...")
  const_vars <- dplyr::select(data_frame, dplyr::where(~ length(unique(.)) == 1)) %>% names()

  # Log: Number of constant variables found
  message(glue("Found {length(const_vars)} constant variables."))

  # Remove constant variables if any
  if (length(const_vars) > 0) {
    # Log: Removing constant variables
    message("Removing constant variables...")
    data_frame <- data_frame %>% dplyr::select(-dplyr::all_of(const_vars))
  } else {
    # Log: No constant variables to remove
    message("No constant variables to remove.")
  }

  # Log: Function completed
  message("Function completed.")

  return(data_frame)
}
