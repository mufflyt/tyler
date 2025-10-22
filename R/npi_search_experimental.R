#' Search NPI Numbers for Given Names
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This experimental wrapper was created for manual debugging of API responses
#' from the NPPES registry. It relied on verbose console output and did not fit
#' the long-term interface of the package. The helper is now deprecated.
#'
#' @param input_data Ignored. The argument is retained for backwards
#'   compatibility.
#' @return A zero-row tibble.
#' @deprecated This debugging helper has been deprecated and will be removed in
#'   a future release.
#' @export
search_npi <- function(input_data) {
  lifecycle::deprecate_warn(
    when = "1.2.2",
    what = "tyler::search_npi()",
    details = "The debugging helper no longer performs registry lookups."
  )

  tibble::tibble()
}
