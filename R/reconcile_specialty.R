#' Three-tier specialty reconciliation with audit columns
#'
#' @name mysterycall_reconcile_specialty
NULL

#' Reconcile specialty labels across primary and secondary sources
#'
#' Applies a three-tier decision rule:
#' \enumerate{
#'   \item **Tier 1 (high)**: primary column is non-missing and non-default —
#'     use it as-is.
#'   \item **Tier 2 (medium)**: primary is missing or the default label, but
#'     the secondary column is informative — adopt the secondary value.
#'   \item **Tier 3 (low)**: both sources are uninformative — set specialty to
#'     `default`.
#' }
#' Two audit columns are appended: one recording which source was used and one
#' recording the confidence tier.
#'
#' @param data A data frame.
#' @param primary_col Character scalar. Column containing the primary specialty
#'   label (e.g. from NPI registry).
#' @param secondary_col Optional character scalar. Column containing an
#'   alternative specialty label (e.g. from board certification data).
#' @param default Character scalar. The "uninformative" label that triggers
#'   tier-2 or tier-3 logic. Default `"General"`.
#' @param source_col Character scalar. Name of the new audit column recording
#'   which source was used. Default `"specialty_source"`.
#' @param confidence_col Character scalar. Name of the new audit column
#'   recording the tier (`"high"`, `"medium"`, or `"low"`).
#'   Default `"specialty_confidence"`.
#'
#' @return `data` with `primary_col` values possibly updated (tier-2 rows
#'   receive the secondary value) and two new audit columns appended.
#'
#' @family provider characteristics
#' @export
#'
#' @examples
#' df <- data.frame(
#'   specialty_npi   = c("Otolaryngology", "General", NA, "Rhinology"),
#'   specialty_abohns = c(NA, "Laryngology", "Rhinology", "General")
#' )
#' mysterycall_reconcile_specialty(
#'   df,
#'   primary_col   = "specialty_npi",
#'   secondary_col = "specialty_abohns"
#' )
mysterycall_reconcile_specialty <- function(data,
                                             primary_col,
                                             secondary_col   = NULL,
                                             default         = "General",
                                             source_col      = "specialty_source",
                                             confidence_col  = "specialty_confidence") {
  if (!is.data.frame(data))           stop("`data` must be a data frame.", call. = FALSE)
  if (!primary_col %in% names(data))  stop("`primary_col` not found in data.", call. = FALSE)

  out <- data
  out[[source_col]]     <- NA_character_
  out[[confidence_col]] <- NA_character_

  primary <- as.character(out[[primary_col]])

  # ── Tier 1: primary is informative ───────────────────────────────────────────
  t1 <- !is.na(primary) & nzchar(trimws(primary)) & primary != default
  out[[source_col]][t1]     <- primary_col
  out[[confidence_col]][t1] <- "high"

  # ── Tier 2: secondary fills the gap ──────────────────────────────────────────
  if (!is.null(secondary_col)) {
    if (!secondary_col %in% names(data)) {
      stop("`secondary_col` not found in data.", call. = FALSE)
    }
    secondary <- as.character(out[[secondary_col]])
    t2 <- !t1 & !is.na(secondary) & nzchar(trimws(secondary)) & secondary != default
    out[[primary_col]] <- ifelse(t2, secondary, as.character(out[[primary_col]]))
    out[[source_col]][t2]     <- secondary_col
    out[[confidence_col]][t2] <- "medium"
  }

  # ── Tier 3: fall back to default ─────────────────────────────────────────────
  t3 <- is.na(out[[source_col]])
  out[[source_col]][t3]     <- "default"
  out[[confidence_col]][t3] <- "low"
  primary_now <- as.character(out[[primary_col]])
  fill_default <- t3 & (is.na(primary_now) | !nzchar(trimws(primary_now)) | primary_now == default)
  out[[primary_col]][fill_default] <- default

  out
}
