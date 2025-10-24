% Generated manually; keep in sync with R/utility_library.R
\name{utility_library}
\alias{tyler_check_dependencies}
\alias{tyler_resolve_path}
\alias{tyler_check_data_completeness}
\alias{tyler_export_with_backup}
\alias{tyler_use_quiet_logging}
\alias{tyler_log_info}
\alias{tyler_standard_labels}
\alias{tyler_standard_palette}
\alias{tyler_quality_tier}
\title{Utility helpers for tyler workflows}
\description{Collection of helper functions that streamline dependency
checking, file management, logging, and quality reporting across
`tyler` projects.}
\usage{
tyler_check_dependencies(packages, install = FALSE, repos = getOption("repos"), quietly = FALSE)

tyler_resolve_path(..., type = NULL, base_dir = getOption("tyler.base_dir", getwd()), create = FALSE)

tyler_check_data_completeness(data, required = NULL, id_cols = NULL, thresholds = c(high = 0.9, medium = 0.75))

tyler_export_with_backup(x, path, backup = TRUE, quiet = getOption("tyler.quiet", FALSE))

tyler_use_quiet_logging(quiet = TRUE)

tyler_log_info(message_text, quiet = getOption("tyler.quiet", FALSE))

tyler_standard_labels()

tyler_standard_palette(name = c("primary", "sequential", "diverging"))

tyler_quality_tier(score, thresholds = c(high = 0.9, medium = 0.75))
}
\arguments{
\item{packages}{Character vector of package names to verify.}

\item{install}{Logical; when \code{TRUE}, missing packages are installed via
\code{install.packages()}.}

\item{repos}{Repository vector forwarded to \code{install.packages()}.}

\item{quietly}{Logical flag suppressing console messages when \code{TRUE}.}

\item{...}{Additional path components appended when resolving project
paths.}

\item{type}{Optional alias used by \code{tyler_resolve_path()} for
common directories such as \code{"data"}, \code{"tables"}, or \code{"figures"}.}

\item{base_dir}{Base directory used when resolving paths. Defaults to
\code{getOption("tyler.base_dir", getwd())}.}

\item{create}{Logical flag. When \code{TRUE}, ensure the resolved path's
parent directory exists.}

\item{data}{Data frame assessed for completeness.}

\item{required}{Character vector listing mandatory column names.}

\item{id_cols}{Optional identifier columns used when assessing row
uniqueness.}

\item{thresholds}{Named numeric vector with \code{high} and \code{medium}
cut-offs for quality tier calculations.}

\item{x}{Object to be written to disk. Data frames are saved as CSV files
by default, \pkg{ggplot2} objects via \code{ggsave()}, and all other
objects with \code{saveRDS()}.}

\item{path}{Destination file path.}

\item{backup}{Logical flag indicating whether an existing file should be
backed up before overwriting.}

\item{quiet}{Logical flag controlling message verbosity.}

\item{message_text}{Character string to emit when quiet mode permits.}

\item{name}{Palette identifier supplied to \code{tyler_standard_palette()}.}

\item{score}{Numeric value between 0 and 1 used to derive a quality tier.}
}
\value{
Returns function-specific outputs:
\itemize{
  \item \code{tyler_check_dependencies()} returns a tibble summarising
  dependency status.
  \item \code{tyler_resolve_path()} and \code{tyler_export_with_backup()}
  return the resolved file path (invisibly).
  \item \code{tyler_check_data_completeness()} returns a list containing a
  per-column summary, overall quality tier, and average completeness score.
  \item \code{tyler_use_quiet_logging()} returns the previous quiet-mode
  value invisibly.
  \item \code{tyler_log_info()} returns \code{NULL} invisibly.
  \item \code{tyler_standard_labels()} returns a named list of canonical
  labels and \code{tyler_standard_palette()} returns a vector of hex
  colour codes.
  \item \code{tyler_quality_tier()} returns \code{"high"}, \code{"medium"},
  \code{"low"}, or \code{NA} when the score is missing.
}
}
\details{
These helpers centralise common project scaffolding concerns:
\describe{
  \item{Dependency checks}{\code{tyler_check_dependencies()} reports on
  installed packages and prints installation commands for missing
  dependencies.}
  \item{Path resolution}{\code{tyler_resolve_path()} normalises paths using
  standard directory aliases and creates directories on demand.}
  \item{Data completeness}{\code{tyler_check_data_completeness()} computes
  column-level completeness metrics and maps them to quality tiers.}
  \item{Safe exports}{\code{tyler_export_with_backup()} writes tables or
  figures while preserving existing outputs as timestamped backups.}
  \item{Quiet logging}{\code{tyler_use_quiet_logging()} and
  \code{tyler_log_info()} provide consistent, timestamped messages that
  respect quiet mode across helper functions.}
  \item{Presentation helpers}{\code{tyler_standard_labels()},
  \code{tyler_standard_palette()}, and \code{tyler_quality_tier()} supply
  standardised labels, colour palettes, and qualitative quality tiers for
  downstream reporting.}
}
}
