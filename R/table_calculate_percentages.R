#' Frequency counts and percentages for a categorical variable
#'
#' Counts each level of a categorical variable and computes its share of all
#' non-missing rows, returning all levels sorted from most to least common.
#'
#' @param data_frame A data frame containing the categorical variable.
#' @param variable A character string giving the column name of the categorical
#'   variable within `data_frame`.
#'
#' @return A data frame with one row per unique non-missing value of `variable`,
#'   sorted by descending count. Columns: the variable itself, `n` (count), and
#'   `percent` (percentage of total, as a numeric value).
#'
#' @examples
#' data_frame <- data.frame(category = c("A", "B", "A", "C", "A", "B", "B", "A"))
#' mysterycall_table_percentages(data_frame, "category")
#'
#' # Ties are all included, sorted alphabetically within the tied rank
#' df_tie <- data.frame(category = c("A", "B", "A", "B", "C", "C", "C", "A", "B"))
#' mysterycall_table_percentages(df_tie, "category")
#'
#' # NAs are excluded from counts and the denominator
#' df_na <- data.frame(category = c("A", NA, "A", "C", "A", "B", "B", NA))
#' mysterycall_table_percentages(df_na, "category")
#'
#' @importFrom rlang sym
#' @family table
#' @seealso [mysterycall_table_proportion()]
#' @export
mysterycall_table_percentages <- function(data_frame, variable) {
  variable <- as.character(variable)  # Ensure the variable name is a string

  summary_df <- data_frame %>%
    dplyr::count(!!rlang::sym(variable), name = "n") %>%
    dplyr::mutate(percent = 100 * n / sum(n)) %>%
    dplyr::arrange(dplyr::desc(n))

  return(summary_df)
}
