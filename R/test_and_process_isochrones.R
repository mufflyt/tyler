#' Test and Process Isochrones
#'
#' `r lifecycle::badge("deprecated")`
#'
#' The original implementation orchestrated interactive debugging sessions for
#' HERE isoline requests. It relied on hard-coded API keys, produced verbose
#' console output, and left behind temporary artifacts. The routine has been
#' retired in favour of reproducible workflows elsewhere in the package.
#'
#' @param input_file Ignored. Only kept so that existing scripts continue to
#'   call the function without failing.
#' @return `invisible()` with an empty tibble.
#' @deprecated This debugging helper has been deprecated and will be removed in
#'   a future release.
#' @export
test_and_process_isochrones <- function(input_file) {
  lifecycle::deprecate_warn(
    when = "1.2.2",
    what = "tyler::test_and_process_isochrones()",
    details = "The debugging helper no longer retrieves isolines."
  )

  invisible(tibble::tibble())
}

#' Process and Save Isochrones
#'
#' `r lifecycle::badge("deprecated")`
#'
#' The processing companion to [test_and_process_isochrones()] used to iterate
#' over HERE isoline responses and persist shapefiles for manual inspection.
#' The workflow has been superseded and is no longer part of the supported API.
#'
#' @param input_file Ignored.
#' @param chunk_size Ignored.
#' @return An empty tibble returned invisibly.
#' @seealso [test_and_process_isochrones()]
#' @deprecated This debugging helper has been deprecated and will be removed in
#'   a future release.
#' @export
process_and_save_isochrones <- function(input_file, chunk_size = 25) {
  lifecycle::deprecate_warn(
    when = "1.2.2",
    what = "tyler::process_and_save_isochrones()",
    details = "The debugging helper no longer retrieves isolines."
  )

  invisible(tibble::tibble())
}
