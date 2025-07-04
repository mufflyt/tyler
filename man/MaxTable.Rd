% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max_table.R
\name{max_table}
\alias{max_table}
\title{Calculate the Maximum Value(s) and Corresponding Level(s) of a Factor Variable}
\usage{
max_table(InVec, mult = FALSE)
}
\arguments{
\item{InVec}{Input vector, expected to be a factor variable or convertible to a factor.}

\item{mult}{Logical value indicating whether to return multiple maximum values or just the first one. Default is FALSE.}
}
\value{
If \code{mult} is FALSE, returns the level corresponding to the maximum value of the factor variable.
If \code{mult} is TRUE, returns a character vector containing all the levels with the maximum value.
}
\description{
This function returns the level(s) corresponding to the maximum value(s) of a factor variable.
}
\examples{
vec <- factor(c("A", "B", "A", "C", "B", "B"))
max_table(vec) # Returns "A"
max_table(vec, mult = TRUE) # Returns c("A", "B")
}
