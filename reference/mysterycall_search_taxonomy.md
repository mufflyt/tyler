# Search NPI Database by Taxonomy

This function searches the NPI Database for healthcare providers based
on a taxonomy description. The NPI API returns at most 1\\200 records
per query. To retrieve all providers for a large specialty, supply a
character vector of state abbreviations via the `states` argument; the
function will loop over each state and combine the results, effectively
bypassing the 1\\200-record cap.

## Usage

``` r
mysterycall_search_taxonomy(
  taxonomy_to_search,
  states = NULL,
  city = NULL,
  limit = 1200L,
  write_snapshot = TRUE,
  snapshot_dir = NULL,
  notify = TRUE
)
```

## Arguments

- taxonomy_to_search:

  A character vector containing the taxonomy description(s) to search
  for.

- states:

  Optional character vector of two-letter US state abbreviations (e.g.
  `c("CO", "CA", "NY")`). When supplied the search is repeated for each
  state so that all providers are captured even for large specialties
  that exceed the 1\\200-record per-query limit. Pass all 50 state
  abbreviations to perform a complete national search. Defaults to
  `NULL` (national search, capped at `limit` records).

- city:

  Optional city name passed to
  [`npi::npi_search()`](https://docs.ropensci.org/npi/reference/npi_search.html).

- limit:

  Maximum records to request per API call. Capped at 1\\200 by the NPI
  API. Defaults to `1200L`.

- write_snapshot:

  Logical. If `TRUE`, the retrieved data is saved as an `.rds` file for
  later reference. Defaults to `TRUE`.

- snapshot_dir:

  Directory where snapshot files should be written when `write_snapshot`
  is `TRUE`. Defaults to a session-specific folder inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) when not supplied.

- notify:

  Logical. If `TRUE`, play a notification sound when processing
  completes (requires the optional `beepr` package). Defaults to `TRUE`.

## Value

A data frame with filtered NPI data based on the specified taxonomy
description.

## See also

Other npi:
[`mysterycall_get_clinician_data()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_get_clinician_data.html),
[`scrape_physicians_data_with_tor()`](https://mufflyt.github.io/mysterycall/reference/scrape_physicians_data_with_tor.md),
[`mysterycall_search_and_process_npi()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_search_and_process_npi.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# National search (limited to 1200 records per taxonomy):
go_data <- mysterycall_search_taxonomy("Gynecologic Oncology")

# Full national search by looping over every state:
all_states <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
  "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
  "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
  "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"
)
fpmrs_data <- mysterycall_search_taxonomy(
  "Female Pelvic Medicine and Reconstructive Surgery",
  states = all_states
)
} # }
```
