% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/states_where_physicians_were_NOT_contacted.R
\name{states_where_physicians_were_NOT_contacted}
\alias{states_where_physicians_were_NOT_contacted}
\title{Summarize States Where Physicians Were NOT Contacted}
\usage{
states_where_physicians_were_NOT_contacted(filtered_data, all_states = NULL)
}
\arguments{
\item{filtered_data}{A data frame containing filtered data of contacted physicians.}

\item{all_states}{A character vector of all possible states including Washington, DC.
If not provided, a default set of states will be used.}
}
\value{
A character string summarizing the inclusion and exclusion of states.
}
\description{
This function summarizes the demographic details by identifying the states where physicians
were not successfully contacted and those that were included.
}
\examples{
# Example with provided all_states
filtered_data <- data.frame(state = c("California", "New York", "Texas"))
all_states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                 "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
                 "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
                 "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                 "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
                 "New Hampshire", "New Jersey", "New Mexico", "New York",
                 "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
                 "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                 "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                 "Washington", "West Virginia", "Wisconsin", "Wyoming",
                 "District of Columbia")
states_where_physicians_were_NOT_contacted(filtered_data, all_states)

# Example with default all_states
filtered_data <- data.frame(state = c("California", "New York", "Texas", "Nevada"))
states_where_physicians_were_NOT_contacted(filtered_data)

}
