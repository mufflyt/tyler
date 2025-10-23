#' Internal validation helpers for workflow functions
#'
#' These functions encapsulate common validation checks that previously lived
#' inline within the workflow helpers. Centralising the checks keeps individual
#' functions focused on their core responsibilities and makes the expected data
#' contracts easier to audit.
#'
#' @keywords internal
assert_is_dataframe <- function(x, arg_name) {
  if (!is.data.frame(x)) {
    stop(sprintf("`%s` must be a data frame.", arg_name), call. = FALSE)
  }
  invisible(x)
}

#' @keywords internal
assert_has_columns <- function(data, required, arg_name) {
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      sprintf(
        "`%s` is missing required columns: %s",
        arg_name,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(data)
}
