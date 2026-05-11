#' Most frequent level(s) of a categorical variable with percentage
#'
#' Counts each level of a categorical variable, computes its share of all
#' non-missing rows, and returns **only the level(s) with the highest count**.
#' When multiple levels tie for the top count, all tied levels are returned.
#' `NA` values are excluded from counts and the denominator.
#'
#' @param data_frame A data frame containing the categorical variable.
#' @param variable A character string giving the column name of the categorical
#'   variable within `data_frame`.
#'
#' @return A data frame with one row per level tied for the highest count.
#'   Columns: the variable itself, `n` (count), and `percent` (exact percentage
#'   of non-missing rows, not rounded).
#'
#' @examples
#' # "A" is most frequent: returns 1 row
#' data_frame <- data.frame(category = c("A", "B", "A", "C", "A", "B", "B", "A"))
#' mysterycall_table_percentages(data_frame, "category")
#'
#' # Three-way tie: all three returned
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
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::filter(n == max(n))

  return(summary_df)
}
