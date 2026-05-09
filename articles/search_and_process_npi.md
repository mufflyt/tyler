# Search and Process NPI Numbers

## Overview

`search_and_process_npi()` is the name-based entry point for roster
creation. Use it when you already have clinician first and last names
from a patient directory, credentialing list, or manually curated study
roster and need to recover likely NPI matches.

The current exported interface expects a data frame passed through the
`data` argument. That data frame must contain:

- `first`
- `last`

### Minimal input

``` r

sample_names <- tibble::tibble(
  first = c("Jane", "Maria", "Alex"),
  last = c("Smith", "Garcia", "Johnson")
)

sample_names
#> # A tibble: 3 × 2
#>   first last   
#>   <chr> <chr>  
#> 1 Jane  Smith  
#> 2 Maria Garcia 
#> 3 Alex  Johnson
```

### Basic search

The simplest call looks like this:

``` r

results <- search_and_process_npi(
  data = sample_names,
  notify = FALSE
)
```

By default the function searches for U.S. individual NPIs and filters to
common physician credentials such as `MD` and `DO`.

### Customizing the search

For most projects you will want to set a few operational parameters
explicitly, especially if the roster is large.

``` r

results <- search_and_process_npi(
  data = sample_names,
  enumeration_type = "ind",
  limit = 10,
  country_code = "US",
  filter_credentials = c("MD", "DO"),
  notify = FALSE
)
```

### Tracking long runs

Large name-based searches can take time. The function supports progress
logs, chunked saves, and resumable accumulation files.

``` r

results <- search_and_process_npi(
  data = sample_names,
  limit = 10,
  dest_dir = "npi_chunks",
  accumulate_path = "npi_results.csv",
  resume = TRUE,
  progress_log = "npi_progress.csv",
  progress_log_format = "csv",
  heartbeat_seconds = 30,
  notify = FALSE
)
```

This is useful when the roster is large enough that you want durable
progress tracking or expect to resume after an interruption.

### What comes back

The returned object is a data frame of candidate NPI matches. Typical
columns include provider identity, credentials, location fields,
taxonomy descriptions, and a `search_term` column showing the input name
pair that produced the match.

``` r

dplyr::glimpse(results)

results |>
  dplyr::count(search_term, sort = TRUE)
```

### Cleaning the result set

Name-based matches are often one-to-many. A common first cleanup step is
to deduplicate on `npi` and then inspect specialty or geography fields.

``` r

results_unique <- results |>
  dplyr::distinct(npi, .keep_all = TRUE)

results_unique |>
  dplyr::count(taxonomies_desc, sort = TRUE)
```

If the next step is clinician enrichment, pass the cleaned result into
`retrieve_clinician_data()`.

``` r

clinician_data <- retrieve_clinician_data(results_unique)
```

### Practical guidance

- Start with a small roster to confirm that your filters are behaving as
  expected.
- Expect ambiguous matches for common names.
- Save progress when running large overnight jobs.
- Deduplicate and validate before downstream joins.

## Next step

If your project begins from a specialty rather than a name list, see the
taxonomy-search vignette for `search_by_taxonomy()`.
