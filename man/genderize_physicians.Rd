% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genderize_physicians.R
\name{genderize_physicians}
\alias{genderize_physicians}
\title{Genderize Physicians Data}
\usage{
genderize_physicians(input_csv)
}
\arguments{
\item{input_csv}{The path to the input CSV file containing physician data.}
}
\value{
A data frame with genderized information joined to the original data.
}
\description{
This function reads a CSV file containing physician data, genderizes the first names,
and joins the gender information back to the original data. It then saves the
result to a new CSV file with a timestamp.
}
\examples{
\dontrun{
result <- genderize_physicians("sample.csv")
}

}
