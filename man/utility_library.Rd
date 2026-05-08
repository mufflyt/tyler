% Generated manually; keep in sync with R/utility_library.R
\name{utility_library}
\alias{utility_library}
\title{Utility helpers for tyler workflows}
\description{
Collection of helper functions that streamline dependency checking, file
management, logging, and quality reporting across \pkg{tyler} projects.

See the individual help pages for full documentation:
\itemize{
  \item \code{\link{tyler_check_dependencies}} — verify and optionally
  install package dependencies.
  \item \code{\link{tyler_resolve_path}} — normalize project-relative paths.
  \item \code{\link{tyler_check_data_completeness}} — compute column-level
  completeness metrics.
  \item \code{\link{tyler_export_with_backup}} — write outputs with
  timestamped backups.
  \item \code{\link{tyler_use_quiet_logging}} — toggle quiet-mode logging.
  \item \code{\link{tyler_log_info}} — emit a timestamped log message.
  \item \code{\link{tyler_standard_labels}} — canonical presentation labels.
  \item \code{\link{tyler_standard_palette}} — standard color palettes.
  \item \code{\link{tyler_quality_tier}} — map a completeness score to a
  quality tier.
}
}
\keyword{utilities}
