# Summarize States Where Physicians Were NOT Contacted

This function summarizes the demographic details by identifying the
states where physicians were not successfully contacted and those that
were included. It also reports how many unique physicians were
successfully reached by filtering to rows with affirmative contact
indicators (e.g., "Yes" values in `contact_office` or
`included_in_study`).

## Usage

``` r
mysterycall_not_contacted_states(filtered_data, all_states = NULL)
```

## Arguments

- filtered_data:

  A data frame containing filtered data of contacted physicians.

- all_states:

  A character vector of all possible states including Washington, DC. If
  not provided, a default set of states will be used.

## Value

A data frame with one row per state and columns `state`, `status`
(`"included"` or `"excluded"`), and `unique_physicians` (the count of
unique contacted physicians, repeated on every row for convenience). The
summary sentence is attached as `attr(result, "summary_text")` for
human-readable output.

## Details

If `contact_office` and/or `included_in_study` exist, they are
interpreted as contact indicators and used to restrict the denominator
to successfully contacted rows. Accepted affirmative values include
logical `TRUE`, non-zero numerics, and character values such as `"yes"`,
`"y"`, `"true"`, and `"1"`.

The physician count is derived from the first available identifier among
`npi`, `name`, `physician_info`, and `physician_information`.

## See also

[`mysterycall_summarize_census()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_summarize_census.md),
[`mysterycall_get_clinician_data()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_get_clinician_data.md),
[`mysterycall_clean_phase1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_clean_phase1.md)

Other summary:
[`mysterycall_physician_age()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_physician_age.md)

## Examples

``` r
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
mysterycall_not_contacted_states(filtered_data, all_states)
#>                   state   status unique_physicians
#> 1               Alabama excluded                 3
#> 2                Alaska excluded                 3
#> 3               Arizona excluded                 3
#> 4              Arkansas excluded                 3
#> 5            California included                 3
#> 6              Colorado excluded                 3
#> 7           Connecticut excluded                 3
#> 8              Delaware excluded                 3
#> 9               Florida excluded                 3
#> 10              Georgia excluded                 3
#> 11               Hawaii excluded                 3
#> 12                Idaho excluded                 3
#> 13             Illinois excluded                 3
#> 14              Indiana excluded                 3
#> 15                 Iowa excluded                 3
#> 16               Kansas excluded                 3
#> 17             Kentucky excluded                 3
#> 18            Louisiana excluded                 3
#> 19                Maine excluded                 3
#> 20             Maryland excluded                 3
#> 21        Massachusetts excluded                 3
#> 22             Michigan excluded                 3
#> 23            Minnesota excluded                 3
#> 24          Mississippi excluded                 3
#> 25             Missouri excluded                 3
#> 26              Montana excluded                 3
#> 27             Nebraska excluded                 3
#> 28               Nevada excluded                 3
#> 29        New Hampshire excluded                 3
#> 30           New Jersey excluded                 3
#> 31           New Mexico excluded                 3
#> 32             New York included                 3
#> 33       North Carolina excluded                 3
#> 34         North Dakota excluded                 3
#> 35                 Ohio excluded                 3
#> 36             Oklahoma excluded                 3
#> 37               Oregon excluded                 3
#> 38         Pennsylvania excluded                 3
#> 39         Rhode Island excluded                 3
#> 40       South Carolina excluded                 3
#> 41         South Dakota excluded                 3
#> 42            Tennessee excluded                 3
#> 43                Texas included                 3
#> 44                 Utah excluded                 3
#> 45              Vermont excluded                 3
#> 46             Virginia excluded                 3
#> 47           Washington excluded                 3
#> 48        West Virginia excluded                 3
#> 49            Wisconsin excluded                 3
#> 50              Wyoming excluded                 3
#> 51 District of Columbia excluded                 3

# Example with default all_states
filtered_data <- data.frame(state = c("California", "New York", "Texas", "Nevada"))
mysterycall_not_contacted_states(filtered_data)
#>                   state   status unique_physicians
#> 1               Alabama excluded                 4
#> 2                Alaska excluded                 4
#> 3               Arizona excluded                 4
#> 4              Arkansas excluded                 4
#> 5            California included                 4
#> 6              Colorado excluded                 4
#> 7           Connecticut excluded                 4
#> 8              Delaware excluded                 4
#> 9               Florida excluded                 4
#> 10              Georgia excluded                 4
#> 11               Hawaii excluded                 4
#> 12                Idaho excluded                 4
#> 13             Illinois excluded                 4
#> 14              Indiana excluded                 4
#> 15                 Iowa excluded                 4
#> 16               Kansas excluded                 4
#> 17             Kentucky excluded                 4
#> 18            Louisiana excluded                 4
#> 19                Maine excluded                 4
#> 20             Maryland excluded                 4
#> 21        Massachusetts excluded                 4
#> 22             Michigan excluded                 4
#> 23            Minnesota excluded                 4
#> 24          Mississippi excluded                 4
#> 25             Missouri excluded                 4
#> 26              Montana excluded                 4
#> 27             Nebraska excluded                 4
#> 28               Nevada included                 4
#> 29        New Hampshire excluded                 4
#> 30           New Jersey excluded                 4
#> 31           New Mexico excluded                 4
#> 32             New York included                 4
#> 33       North Carolina excluded                 4
#> 34         North Dakota excluded                 4
#> 35                 Ohio excluded                 4
#> 36             Oklahoma excluded                 4
#> 37               Oregon excluded                 4
#> 38         Pennsylvania excluded                 4
#> 39         Rhode Island excluded                 4
#> 40       South Carolina excluded                 4
#> 41         South Dakota excluded                 4
#> 42            Tennessee excluded                 4
#> 43                Texas included                 4
#> 44                 Utah excluded                 4
#> 45              Vermont excluded                 4
#> 46             Virginia excluded                 4
#> 47           Washington excluded                 4
#> 48        West Virginia excluded                 4
#> 49            Wisconsin excluded                 4
#> 50              Wyoming excluded                 4
#> 51 District of Columbia excluded                 4
```
