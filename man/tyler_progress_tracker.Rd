% Generated manually; keep in sync with R/tyler_progress_tracker.R
\name{tyler_progress_tracker}
\alias{tyler_progress_tracker}
\alias{tyler_progress_tracker}
\alias{tyler_progress_start}
\alias{tyler_progress_finish}
\alias{tyler_progress_fail}
\alias{tyler_progress_update}
\alias{tyler_progress_summary}
\title{Track multi-stage workflow progress}
\description{Helpers that record per-step status, emit periodic progress
updates, and surface quality tiers for long-running workflows.}
\usage{
tyler_progress_tracker(steps, update_every = 300, quiet = getOption("tyler.quiet", FALSE))

tyler_progress_start(tracker, step, note = NULL)

tyler_progress_finish(tracker, step, score = NULL, quality = NULL, note = NULL)

tyler_progress_fail(tracker, step, reason = NULL)

tyler_progress_update(tracker, force = FALSE)

tyler_progress_summary(tracker)
}
\arguments{
\item{steps}{Character vector naming each workflow stage.}

\item{update_every}{Number of seconds between automatic update messages.}

\item{quiet}{Logical flag that suppresses log output when \code{TRUE}.}

\item{tracker}{Object created by \code{tyler_progress_tracker()}.}

\item{step}{Step name to update.}

\item{note}{Optional free-text annotation stored alongside the step.}

\item{score}{Numeric score between 0 and 1 used to compute a quality tier.}

\item{quality}{Explicit quality tier that overrides \code{score} when provided.}

\item{reason}{Explanation recorded for failed steps.}

\item{force}{Logical flag forcing an immediate update regardless of elapsed time.}
}
\value{
\code{tyler_progress_tracker()} returns an object of class
\code{tyler_progress_tracker}. The other helpers return the tracker
(invisibly) for fluent chaining, except \code{tyler_progress_summary()},
which returns a tibble describing each step.}
\details{
The tracker records start and finish times, keeps a method-by-method
breakdown, and estimates completion times based on observed throughput.
Use \code{tyler_progress_update()} within long loops to emit
five-minute heartbeat messages, while \code{tyler_progress_finish()}
handles quality tier calculation using \code{tyler_quality_tier()}.
}
\examples{
tracker <- tyler_progress_tracker(c("Geocode", "Validate"), update_every = 10)
tyler_progress_start(tracker, "Geocode")
tyler_progress_finish(tracker, "Geocode", score = 0.92)
tyler_progress_summary(tracker)
}
