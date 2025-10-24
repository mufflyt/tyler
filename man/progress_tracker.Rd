% Generated manually; keep in sync with R/progress_tracker.R
\name{progress_tracker}
\alias{progress_tracker}
\alias{progress_tracker_start}
\alias{progress_tracker_finish}
\alias{progress_tracker_fail}
\alias{progress_tracker_update}
\alias{progress_tracker_summary}
\title{Track multi-stage workflow progress}
\description{Helpers that record per-step status, emit periodic progress
updates, and surface quality tiers for long-running workflows.}
\usage{
progress_tracker(steps, update_every = 300, quiet = getOption("tyler.quiet", FALSE))

progress_tracker_start(tracker, step, note = NULL)

progress_tracker_finish(tracker, step, score = NULL, quality = NULL, note = NULL)

progress_tracker_fail(tracker, step, reason = NULL)

progress_tracker_update(tracker, force = FALSE)

progress_tracker_summary(tracker)
}
\arguments{
\item{steps}{Character vector naming each workflow stage.}

\item{update_every}{Number of seconds between automatic update messages.}

\item{quiet}{Logical flag that suppresses log output when \code{TRUE}.}

\item{tracker}{Object created by \code{progress_tracker()}.}

\item{step}{Step name to update.}

\item{note}{Optional free-text annotation stored alongside the step.}

\item{score}{Numeric score between 0 and 1 used to compute a quality tier.}

\item{quality}{Explicit quality tier that overrides \code{score} when provided.}

\item{reason}{Explanation recorded for failed steps.}

\item{force}{Logical flag forcing an immediate update regardless of elapsed time.}
}
\value{
\code{progress_tracker()} returns an object of class
\code{tyler_progress_tracker}. The other helpers return the tracker
(invisibly) for fluent chaining, except \code{progress_tracker_summary()},
which returns a tibble describing each step.}
\details{
The tracker records start and finish times, keeps a method-by-method
breakdown, and estimates completion times based on observed throughput.
Use \code{progress_tracker_update()} within long loops to emit
five-minute heartbeat messages, while \code{progress_tracker_finish()}
handles quality tier calculation using \code{tyler_quality_tier()}.
}
\examples{
tracker <- progress_tracker(c("Geocode", "Validate"), update_every = 10)
progress_tracker_start(tracker, "Geocode")
progress_tracker_finish(tracker, "Geocode", score = 0.92)
progress_tracker_summary(tracker)
}
