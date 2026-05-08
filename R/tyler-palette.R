#' Retrieve the standard label dictionary used in `tyler`
#'
#' @return A named list of canonical labels for common columns.
#' @export
tyler_standard_labels <- function() {
  list(
    npi = "National Provider Identifier",
    state = "State",
    city = "City",
    call_outcome = "Call Outcome",
    quality = "Quality Tier",
    call_time = "Call Duration (minutes)",
    hold_time = "Hold Duration (minutes)",
    eta = "Estimated Completion"
  )
}

#' Retrieve a standard color palette
#'
#' @param name Palette identifier. Supported values are `"primary"`,
#'   `"sequential"`, and `"diverging"`.
#'
#' @return A character vector of hex colors.
#' @export
tyler_standard_palette <- function(name = c("primary", "sequential", "diverging")) {
  name <- match.arg(name)
  palettes <- list(
    primary = c("#0B3C5D", "#328CC1", "#D9B310", "#1D2731"),
    sequential = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5"),
    diverging = c("#b30000", "#fdbf6f", "#1b7837")
  )
  palettes[[name]]
}
