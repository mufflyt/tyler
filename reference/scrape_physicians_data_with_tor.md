# Scrape Physicians' Data with Tor

Scrapes board-certification data for physicians within a specified ID
range from the ABOG API, routing all requests through a local Tor SOCKS
proxy to anonymise the source IP. IDs listed in `wrong_ids_path` are
skipped.

## Usage

``` r
scrape_physicians_data_with_tor(
  startID,
  endID,
  torPort,
  wrong_ids_path = NULL,
  timeout_sec = 30,
  delay_sec = 1,
  output_dir = tempdir(),
  output_format = c("csv", "parquet")
)
```

## Arguments

- startID:

  The starting diplomate ID for scraping.

- endID:

  The ending diplomate ID for scraping.

- torPort:

  The port number for the local Tor SOCKS proxy (typically 9150).

- wrong_ids_path:

  Optional path to a CSV or Parquet file containing an integer
  `WrongIDs` column listing IDs to skip.

- timeout_sec:

  Number of seconds before an individual HTTP request times out.
  Defaults to 30.

- delay_sec:

  Seconds to pause between successive requests. Defaults to 1.

- output_dir:

  Directory where result files are written. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). Pass `NULL` to
  skip all file I/O and only return the data.

- output_format:

  File format for saving results: `"csv"` (default) or `"parquet"`.

## Value

A data frame containing scraped physician records, invisibly. When
`output_dir` is not `NULL`, result files are also written there and
their paths reported via
[`message()`](https://rdrr.io/r/base/message.html).

## See also

Other npi:
[`mysterycall_get_clinician_data()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_get_clinician_data.html),
[`mysterycall_search_and_process_npi()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_search_and_process_npi.html),
[`mysterycall_search_taxonomy()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_search_taxonomy.html)

## Examples

``` r
if (FALSE) { # interactive()
# Requires a running Tor proxy and network access.
scrape_physicians_data_with_tor(startID = 9045999, endID = 9046000, torPort = 9150)
}
```
