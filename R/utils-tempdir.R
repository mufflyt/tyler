#' Internal helper for package-specific temporary directories
#'
#' Provides a stable location within [tempdir()] that can be reused
#' across functions which need to write ephemeral artefacts while
#' avoiding modifications to the user's working tree. Directories are
#' created on demand when `create = TRUE`.
#'
#' @param ... Optional path components appended to the base directory.
#' @param create Logical flag. When `TRUE`, ensure the directory exists.
#'
#' @return A character vector containing the resolved path.
#' @keywords internal
tyler_tempdir <- function(..., create = FALSE) {
  path <- file.path(tempdir(), "tyler", ...)
  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}
