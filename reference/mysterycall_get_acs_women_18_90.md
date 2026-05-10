# Get ACS Female Population (Ages 18-90) by Census Tract

## Usage

``` r
mysterycall_get_acs_women_18_90(year = 2022, states = NULL, verbose = TRUE)

get_acs_women_18_90(...)
```

## Arguments

- year:

  Integer. ACS 5-year survey year (2009–2023). Default: `2022`.

- states:

  Character vector of two-letter state abbreviations, or `NULL` to
  download all states (slow, ~30 min). Default: `NULL`.

- verbose:

  Logical. Print progress messages. Default: `TRUE`.

## Value

Tibble with columns:

- GEOID:

  Census tract ID (11-digit string).

- women_18_90:

  Sum of female population aged 18-89.

- women_18_90_moe:

  Margin of error (90\\

Downloads ACS B01001 (Sex by Age) data for women aged 18-89 years.
Automatically sums across age groups and propagates margins of error
using the Census Bureau sum-of-squares formula: \\MOE\_{sum} =
\sqrt{\sum MOE_i^2}\\. Age groups included: 18-19, 20, 21, 22-24, 25-29,
30-34, 35-39, 40-44, 45-49, 50-54, 55-59, 60-61, 62-64, 65-66, 67-69,
70-74, 75-79, 80-84, 85-89. MOE propagation follows Census Bureau ACS
Handbook Appendix 3. All MOE values are at 90\\ For both sexes in one
call, use
[`mysterycall_get_acs_adults_18_90`](https://mufflyt.github.io/mysterycall/reference/mysterycall_get_acs_adults_18_90.md).
co_women \<- mysterycall_get_acs_women_18_90(year = 2022, states = "CO")
west_coast \<- mysterycall_get_acs_women_18_90(year = 2022, states =
c("CA", "OR", "WA")) Other census:
[`mysterycall_get_acs_adults_18_90()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_get_acs_adults_18_90.md),
[`mysterycall_get_census_data()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_get_census_data.md),
[`mysterycall_plot_census_age()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_census_age.md),
[`mysterycall_summarize_census()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_summarize_census.md)
census
