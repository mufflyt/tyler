#' Remove Constant Variables from a Data Frame
#'
#' This function takes a data frame and returns a new data frame with constant variables removed.
#'
#' @param data_frame A data frame from which constant variables should be removed.
#' @param verbose Logical; if TRUE, prints status messages while running. Default is FALSE.
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
remove_constant_vars <- function(data_frame, verbose = FALSE) {

  if (isTRUE(verbose)) {
    message("Starting the function to remove constant variables.")
  }

  if (nrow(data_frame) == 0 || ncol(data_frame) == 0) {
    if (isTRUE(verbose)) {
      message("The data frame is empty. Exiting function.")
    }
    return(data_frame)
  }

  if (isTRUE(verbose)) {
    message("Identifying constant variables...")
  }
  const_vars <- dplyr::select(data_frame, dplyr::where(~ length(unique(.)) == 1)) %>% names()

  if (isTRUE(verbose)) {
    message(glue("Found {length(const_vars)} constant variables."))
  }

  if (length(const_vars) > 0) {
    if (isTRUE(verbose)) {
      message("Removing constant variables...")
    }
    data_frame <- data_frame %>% dplyr::select(-dplyr::all_of(const_vars))
  } else {
    if (isTRUE(verbose)) {
      message("No constant variables to remove.")
    }
  }

  if (isTRUE(verbose)) {
    message("Function completed.")
  }

  return(data_frame)
}
