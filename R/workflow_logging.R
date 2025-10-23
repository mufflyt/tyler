#' Internal workflow logging helpers
#'
#' These utilities provide a single place to control how workflow-oriented
#' functions emit progress messages. They default to using the `cli` package
#' when available and gracefully fall back to base `message()` output.
#'
#' @keywords internal
workflow_log <- function(..., verbose = TRUE) {
  if (!isTRUE(verbose)) {
    return(invisible(NULL))
  }

  message_text <- paste0(..., collapse = "")
  if (!nzchar(message_text)) {
    return(invisible(NULL))
  }

  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_inform(message_text)
  } else {
    base::message(message_text)
  }

  invisible(NULL)
}
