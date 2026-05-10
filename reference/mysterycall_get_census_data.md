# Get Census data of all state block groups

This function retrieves Census data of all state block groups by looping
over the specified list of state FIPS codes.

## Usage

``` r
mysterycall_get_census_data(
  us_fips_list,
  vintage = 2022,
  api_key = Sys.getenv("CENSUS_API_KEY")
)
```

## Arguments

- us_fips_list:

  A vector of state FIPS codes which Census data is to be retrieved.

- vintage:

  The vintage year of Census data (default is 2022).

- api_key:

  Census API key to use for requests. Defaults to the `CENSUS_API_KEY`
  environment variable.

## Value

A tibble containing Census block group data for the requested states.
The result includes padded 2020 FIPS columns (`statefp`, `countyfp`,
`tractce`, `block_group`, `geoid`) and a `vintage` column indicating the
Census vintage that supplied the estimates.

## See also

Other census:
[`mysterycall_plot_census_age()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_census_age.md),
[`mysterycall_summarize_census()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_summarize_census.md)

## Examples

``` r
if (FALSE) { # interactive()
us_fips_list <- c("01", "02")
census_df <- mysterycall_get_census_data(us_fips_list)
}
```
