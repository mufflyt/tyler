# Produce summary statistics from Census block group data

Produce summary statistics from Census block group data

## Usage

``` r
mysterycall_summarize_census(
  census_df,
  group_vars = "statefp",
  reproductive_age_vars = sprintf("B01001_%03dE", 30:38)
)
```

## Arguments

- census_df:

  A data frame produced by
  [`mysterycall_get_census_data()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_get_census_data.html)
  (or another table containing equivalent columns).

- group_vars:

  Character vector of column names used to group the summaries. Defaults
  to `"statefp"`. Supply `character(0)` to receive a single summary row
  for the entire dataset.

- reproductive_age_vars:

  Character vector of female age estimate columns that should be treated
  as reproductive-age (defaults to `B01001_030E:B01001_038E`,
  representing 15 to 44 years).

## Value

A tibble containing one row per grouping with population totals, female
share, male share, and the share of females of reproductive age.

## See also

Other census:
[`mysterycall_get_census_data()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_get_census_data.html),
[`mysterycall_plot_census_age()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_census_age.html)

## Examples

``` r
census_example <- tibble::tibble(
  statefp = c("01", "01", "02"),
  countyfp = c("001", "001", "013"),
  geoid = c("010010201001", "010010201002", "020130001001"),
  B01001_001E = c(1200, 1300, 900),
  B01001_002E = c(560, 610, 420),
  B01001_026E = c(640, 690, 480),
  B01001_030E = c(45, 47, 32),
  B01001_031E = c(40, 41, 28),
  B01001_032E = c(35, 36, 22),
  B01001_033E = c(30, 32, 20),
  B01001_034E = c(55, 58, 38),
  B01001_035E = c(60, 63, 40),
  B01001_036E = c(62, 64, 42),
  B01001_037E = c(58, 60, 39),
  B01001_038E = c(56, 57, 37)
)

mysterycall_summarize_census(census_example, group_vars = c("statefp"))
#> # A tibble: 2 × 9
#>   statefp block_group_count total_population female_population male_population
#>   <chr>               <int>            <dbl>             <dbl>           <dbl>
#> 1 01                      2             2500              1330            1170
#> 2 02                      1              900               480             420
#> # ℹ 4 more variables: reproductive_age_female <dbl>, female_share <dbl>,
#> #   male_share <dbl>, reproductive_age_female_share <dbl>
```
