% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_isochrones_for_dataframe.R
\name{create_isochrones_for_dataframe}
\alias{create_isochrones_for_dataframe}
\title{Get isochrones for each point in a dataframe}
\usage{
create_isochrones_for_dataframe(
  input_file,
  breaks = c(1800, 3600, 7200, 10800)
)
}
\arguments{
\item{breaks}{A numeric vector specifying the breaks for categorizing drive times (default is c(1800, 3600, 7200, 10800)).  This allows for 30 minutes, 60 minutes, 120 minutes, and 180 minutes.}

\item{dataframe}{A dataframe containing the points for which isochrones are to be retrieved.}
}
\value{
A dataframe containing the isochrones data with added 'name' column.
Create Isochrones for Each Point in a Dataframe

This function retrieves isochrones for each point in a given dataframe by looping
over the rows and calling the create_isochrones function for each point.

A dataframe containing the isochrones data with added 'name' column.
}
\description{
This function retrieves isochrones for each point in a given dataframe by looping
over the rows and calling the create_isochrones function for each point.
}
