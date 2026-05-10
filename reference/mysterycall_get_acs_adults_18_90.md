# Get ACS Adult Population (Ages 18-90, Both Sexes) by Census Tract

## Usage

``` r
mysterycall_get_acs_adults_18_90(year = NULL, states = NULL, verbose = TRUE)

get_acs_adults_18_90(...)
```

## Arguments

- year:

  Integer. ACS 5-year survey year (2009–2023). Required — no default to
  ensure reproducibility.

- states:

  Character vector of two-letter state abbreviations, or `NULL` to
  download all states (slow, ~45 min). Default: `NULL`.

- verbose:

  Logical. Print progress messages. Default: `TRUE`.

## Value

Tibble with columns:

- GEOID:

  Census tract ID (11-digit string).

- men_18_90:

  Sum of male population aged 18-89.

- men_18_90_moe:

  Margin of error for males (90\\ women_18_90Sum of female population
  aged 18-89. women_18_90_moeMargin of error for females (90\\
  adults_18_90Total adult population (male + female).
  adults_18_90_moePropagated total margin of error (90\\ Downloads ACS
  B01001 (Sex by Age) data for adults aged 18-89 years, separated by
  sex. Returns male, female, and total adult populations with proper MOE
  propagation using the Census Bureau sum-of-squares formula:
  \\MOE\_{sum} = \sqrt{\sum MOE_i^2}\\. Age groups included: 18-19, 20,
  21, 22-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59, 60-61,
  62-64, 65-66, 67-69, 70-74, 75-79, 80-84, 85-89. MOE propagation
  follows Census Bureau ACS Handbook Appendix 3. All MOE values are at
  90\\ This function is more efficient than calling
  [`mysterycall_get_acs_women_18_90()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_get_acs_women_18_90.md)
  separately as it makes a single Census API call. co_adults \<-
  mysterycall_get_acs_adults_18_90(year = 2022, states = "CO")
  west_coast \<- mysterycall_get_acs_adults_18_90(year = 2022, states =
  c("CA", "OR", "WA")) Other census:
  [`mysterycall_get_acs_women_18_90()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_get_acs_women_18_90.md),
  [`mysterycall_get_census_data()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_get_census_data.md),
  [`mysterycall_plot_census_age()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_census_age.md),
  [`mysterycall_summarize_census()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_summarize_census.md)
  census
