#' Data utilities for mystery-caller study management
#'
#' @name data_utils
NULL

#' Flag physicians called more than the allowed number of times
#'
#' Returns a subset of `data` containing only rows where the physician
#' identifier appears more than `max_calls` times, annotated with a call-count
#' column.
#'
#' @param data A data frame.
#' @param id_col Character scalar naming the physician-identifier column.
#' @param max_calls Integer. Maximum allowed calls per physician. Physicians
#'   appearing more than this many times are flagged. Default `2L`.
#'
#' @return A data frame (subset of `data`) with an additional `n_calls`
#'   column. An attribute `"n_flagged"` records the number of unique
#'   physicians flagged.
#'
#' @family data management
#' @export
#'
#' @examples
#' df <- data.frame(
#'   physician_id = c("A", "A", "A", "B", "B", "C"),
#'   call_date    = Sys.Date() + 0:5
#' )
#' mysterycall_check_duplicates(df, id_col = "physician_id", max_calls = 2L)
mysterycall_check_duplicates <- function(data, id_col, max_calls = 2L) {
  if (!is.data.frame(data))           stop("`data` must be a data frame.", call. = FALSE)
  if (!id_col %in% names(data))       stop("`id_col` not found in data.", call. = FALSE)
  if (!is.numeric(max_calls) || max_calls < 1L) {
    stop("`max_calls` must be a positive integer.", call. = FALSE)
  }

  tab         <- table(as.character(data[[id_col]]))
  flagged_ids <- names(tab[tab > max_calls])

  out        <- data[data[[id_col]] %in% flagged_ids, , drop = FALSE]
  out$n_calls <- as.integer(tab[as.character(out[[id_col]])])
  out <- out[order(-out$n_calls, as.character(out[[id_col]])), ]

  attr(out, "n_flagged") <- length(flagged_ids)
  attr(out, "max_calls") <- max_calls
  out
}


#' Draw a balanced stratified sample from a data frame
#'
#' Samples exactly `n_per_group` rows from each level of `group_col`. Groups
#' with fewer than `n_per_group` rows are returned in full.
#'
#' @param data A data frame.
#' @param group_col Character scalar naming the stratification column.
#' @param n_per_group Integer. Target sample size per group.
#' @param seed Optional integer random seed for reproducibility.
#'
#' @return A data frame with rows in original row-number order.
#'
#' @family data management
#' @export
#'
#' @examples
#' df <- data.frame(
#'   specialty = rep(c("Otolaryngology", "Urology", "Dermatology"), c(100, 80, 60)),
#'   x         = rnorm(240)
#' )
#' out <- mysterycall_stratified_sample(df, group_col = "specialty",
#'                                      n_per_group = 30L, seed = 42L)
#' table(out$specialty)
mysterycall_stratified_sample <- function(data, group_col, n_per_group, seed = NULL) {
  if (!is.data.frame(data))              stop("`data` must be a data frame.", call. = FALSE)
  if (!group_col %in% names(data))       stop("`group_col` not found in data.", call. = FALSE)
  if (!is.numeric(n_per_group) || length(n_per_group) != 1L || n_per_group < 1L) {
    stop("`n_per_group` must be a single positive integer.", call. = FALSE)
  }

  if (!is.null(seed)) set.seed(seed)

  groups <- split(seq_len(nrow(data)), data[[group_col]])
  idx <- unlist(lapply(groups, function(rows) {
    if (length(rows) <= n_per_group) rows else sample(rows, as.integer(n_per_group))
  }), use.names = FALSE)

  data[sort(idx), , drop = FALSE]
}


#' Standardise demographic variables for Table 1
#'
#' A convenience wrapper that applies age imputation, age categorisation, and
#' gender standardisation in a single call. Columns are added to (not
#' replacing) the input data frame. Only columns explicitly specified are
#' processed.
#'
#' @param data A data frame.
#' @param age_col Optional character scalar naming an existing numeric age
#'   column. When supplied, `age_category` is derived from it.
#' @param grad_year_col Optional character scalar naming a graduation-year
#'   column. Used to impute age when `age_col` is `NULL`.
#' @param gender_col Optional character scalar naming a gender/sex column.
#'   Values are recoded to `"Male"` (inputs `"m"` or `"male"`), `"Female"`
#'   (inputs `"f"` or `"female"`), or `"Unknown"` for any other value
#'   including `NA`. Matching is case-insensitive and whitespace-trimmed.
#'   This is a **binary classification**: non-binary or ambiguous values are
#'   silently bucketed into `"Unknown"`.
#' @param setting_col Optional character scalar naming a practice-setting
#'   column. Passed through as `setting_std`.
#' @param region_col Optional character scalar naming a region column. Passed
#'   through as `region_std`.
#' @param ref_year Integer reference year for age imputation. Default: current
#'   calendar year.
#'
#' @return `data` with zero or more additional standardised columns appended:
#'   `age_imputed`, `age_category`, `gender_std`, `setting_std`, `region_std`.
#'
#' @section Gender standardisation:
#'   The `gender_std` column is produced by a binary lookup applied
#'   case-insensitively after whitespace trimming:
#'   \tabular{ll}{
#'     **Input value** \tab **`gender_std`** \cr
#'     `"male"`, `"m"`, `"Male"`, `"M"` \tab `"Male"` \cr
#'     `"female"`, `"f"`, `"Female"`, `"F"` \tab `"Female"` \cr
#'     `NA` \tab `"Unknown"` \cr
#'     `""`, `"unknown"`, `"non-binary"`, any other string \tab `"Unknown"`
#'   }
#'   Non-binary values and any future Genderize.io API additions are all
#'   mapped to `"Unknown"` silently. Before calling this function, inspect
#'   `table(data[[gender_col]], useNA = "always")` to check for unexpected
#'   values that will be bucketed into `"Unknown"`.
#'
#' @seealso [mysterycall_genderize()] for the upstream API call that produces
#'   the raw `gender` column; [mysterycall_preflight_check()] to validate
#'   data quality before processing.
#'
#' @family data management
#' @export
#'
#' @examples
#' df <- data.frame(
#'   grad_year = c(1990, 2000, 2010),
#'   gender    = c("Female", "M", "male")
#' )
#' mysterycall_prepare_table1_vars(df, grad_year_col = "grad_year",
#'                                  gender_col = "gender", ref_year = 2026L)
mysterycall_prepare_table1_vars <- function(data,
                                             age_col       = NULL,
                                             grad_year_col = NULL,
                                             gender_col    = NULL,
                                             setting_col   = NULL,
                                             region_col    = NULL,
                                             ref_year      = as.integer(
                                               format(Sys.Date(), "%Y"))) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)

  out <- data

  # Age from existing column
  if (!is.null(age_col) && age_col %in% names(out)) {
    out$age_category <- mysterycall_age_category(out[[age_col]])
  }

  # Age from graduation year (only if no direct age column)
  if (is.null(age_col) && !is.null(grad_year_col) && grad_year_col %in% names(out)) {
    out$age_imputed  <- mysterycall_impute_age(out[[grad_year_col]], ref_year = ref_year)
    out$age_category <- mysterycall_age_category(out$age_imputed)
  }

  # Gender standardisation
  if (!is.null(gender_col) && gender_col %in% names(out)) {
    g <- tolower(trimws(as.character(out[[gender_col]])))
    out$gender_std <- ifelse(
      g %in% c("m", "male"),    "Male",
      ifelse(g %in% c("f", "female"), "Female", "Unknown")
    )
  }

  if (!is.null(setting_col) && setting_col %in% names(out)) {
    out$setting_std <- out[[setting_col]]
  }

  if (!is.null(region_col) && region_col %in% names(out)) {
    out$region_std <- out[[region_col]]
  }

  out
}
