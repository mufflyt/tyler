#' Deprecated functions in tyler
#'
#' These helpers are retained for backward compatibility only. They emit
#' deprecation warnings and defer to the modern workflow helpers where
#' possible.
#'
#' @return
#' * `search_npi()` returns the tibble produced by
#'   [search_and_process_npi()].
#' * `test_and_process_isochrones()` and `process_and_save_isochrones()` do
#'   not return a value and instead stop with a deprecation error.
#'
#' @name tyler-deprecated
NULL

#' @rdname tyler-deprecated
#' @param input_data A data frame or CSV path containing `first` and `last`
#'   columns.
#' @param ... Additional arguments passed to [search_and_process_npi()].
#' @export
#' @deprecated `search_npi()` has been replaced by [search_and_process_npi()].
search_npi <- function(input_data, ...) {
  .Deprecated("search_and_process_npi", package = "tyler",
              msg = "search_npi() is deprecated. Use search_and_process_npi() with a data frame of first/last columns.")

  data <- if (is.data.frame(input_data)) {
    input_data
  } else if (is.character(input_data) && length(input_data) == 1) {
    readr::read_csv(input_data, show_col_types = FALSE)
  } else {
    stop("`input_data` must be a data frame or a file path to a CSV.")
  }

  required_cols <- c("first", "last")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop("Input data must contain columns: ", paste(missing_cols, collapse = ", "))
  }

  search_and_process_npi(data = data, ...)
}

#' @rdname tyler-deprecated
#' @param input_file Deprecated. Previously accepted a data frame with `lat`
#'   and `long` columns.
#' @export
#' @deprecated `test_and_process_isochrones()` has been replaced by
#'   [create_isochrones_for_dataframe()].
test_and_process_isochrones <- function(input_file, ...) {
  .Deprecated("create_isochrones_for_dataframe", package = "tyler",
              msg = "test_and_process_isochrones() is deprecated. Use create_isochrones_for_dataframe() with explicit error handling.")
  stop("test_and_process_isochrones() has been deprecated. Use create_isochrones_for_dataframe() for current workflows.")
}

#' @rdname tyler-deprecated
#' @param chunk_size Deprecated.
#' @export
#' @deprecated `process_and_save_isochrones()` has been replaced by
#'   [create_isochrones_for_dataframe()].
process_and_save_isochrones <- function(input_file, chunk_size = 25, ...) {
  .Deprecated("create_isochrones_for_dataframe", package = "tyler",
              msg = "process_and_save_isochrones() is deprecated. Use create_isochrones_for_dataframe() and downstream sf writers.")
  stop("process_and_save_isochrones() has been deprecated. Use create_isochrones_for_dataframe() for current workflows.")
}
