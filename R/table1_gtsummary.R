#' Publication-ready Table 1 via gtsummary
#'
#' @name mysterycall_table1_gtsummary
NULL

#' Generate a publication-ready Table 1 using gtsummary
#'
#' Wraps [gtsummary::tbl_summary()] with opinionated defaults suitable for
#' mystery-caller study demographics: no "Unknown" in missing-data reporting
#' (use the `missing` argument to change), bold labels, an overall column,
#' and optional p-values for stratified tables.
#'
#' @param data A data frame.
#' @param vars Character vector of variable names to include in the table.
#'   Must all be present in `data`.
#' @param strata_col Optional character scalar naming a stratification column
#'   (e.g. `"gender"`, `"insurance"`). When supplied, an overall column and
#'   p-values are added automatically.
#' @param label_list Optional named list mapping variable names to display
#'   labels, passed to `gtsummary::tbl_summary(label = ...)`.
#' @param missing One of `"no"` (default), `"ifany"`, or `"always"`. Passed
#'   directly to [gtsummary::tbl_summary()].
#' @param ... Additional arguments forwarded to [gtsummary::tbl_summary()].
#'
#' @return A `gtsummary` `tbl_summary` object. Print it directly or export
#'   with [gtsummary::as_flex_table()] / [gtsummary::as_gt()].
#'
#' @family manuscript
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   gender   = sample(c("Male","Female"), 100, replace = TRUE),
#'   setting  = sample(c("Academic","Private"), 100, replace = TRUE),
#'   age_cat  = sample(c("30-39","40-49","50-59"), 100, replace = TRUE),
#'   insurance = sample(c("BCBS","Medicaid"), 100, replace = TRUE)
#' )
#' tbl <- mysterycall_table1_gtsummary(
#'   df,
#'   vars       = c("gender", "setting", "age_cat"),
#'   strata_col = "insurance"
#' )
#' tbl
#' }
mysterycall_table1_gtsummary <- function(data,
                                          vars,
                                          strata_col  = NULL,
                                          label_list  = NULL,
                                          missing     = "no",
                                          ...) {
  if (!requireNamespace("gtsummary", quietly = TRUE)) {
    stop(
      "gtsummary is required. Install with install.packages('gtsummary').",
      call. = FALSE
    )
  }
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0L) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }
  if (!is.null(strata_col) && !strata_col %in% names(data)) {
    stop("`strata_col` not found in data.", call. = FALSE)
  }

  keep_cols <- unique(c(vars, strata_col))
  tbl <- gtsummary::tbl_summary(
    data    = data[, keep_cols, drop = FALSE],
    by      = strata_col,
    label   = label_list,
    missing = missing,
    ...
  )

  if (!is.null(strata_col)) {
    tbl <- gtsummary::add_overall(tbl)
    tbl <- gtsummary::add_p(tbl)
  }

  gtsummary::bold_labels(tbl)
}
