#' Internal helpers for validating user inputs
#'
#' These utilities provide consistent, human-friendly validation
#' messages across the package. They are kept internal so that
#' exported functions can depend on them without bloating the public
#' API surface area.
#'
#' @keywords internal
validate_dataframe <- function(x, name = "data", allow_null = FALSE, allow_zero_rows = TRUE) {
  if (is.null(x)) {
    if (allow_null) {
      return(invisible(x))
    }
    stop(sprintf("`%s` must not be NULL.", name), call. = FALSE)
  }

  if (!inherits(x, "data.frame")) {
    stop(sprintf("`%s` must be a data frame; received an object of class %s.", name, paste(class(x), collapse = ", ")), call. = FALSE)
  }

  if (!allow_zero_rows && !nrow(x)) {
    stop(sprintf("`%s` must contain at least one row.", name), call. = FALSE)
  }

  invisible(x)
}

#' @keywords internal
validate_required_columns <- function(x, required, name = "data") {
  if (is.null(required) || !length(required)) {
    return(invisible(x))
  }

  missing_required <- setdiff(required, names(x))
  if (length(missing_required)) {
    stop(
      sprintf(
        "Required columns are missing from `%s`: %s",
        name,
        paste(sort(missing_required), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(x)
}
