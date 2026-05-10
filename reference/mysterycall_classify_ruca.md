# Classify RUCA codes into Urban / Suburban / Rural

The Rural-Urban Commuting Area (RUCA) classification system assigns
numeric codes to ZIP codes based on commuting patterns and population
density. This function maps those numeric codes to three categories used
in healthcare access research:

## Usage

``` r
mysterycall_classify_ruca(
  ruca_code,
  urban_max = 3,
  suburban_max = 6,
  labels = c("Urban", "Suburban", "Rural"),
  na_label = "Unknown",
  as_factor = FALSE
)
```

## Arguments

- ruca_code:

  Numeric vector of RUCA codes (typically integer or decimal, e.g. 1, 2,
  3.1, 10.6). `NA` values are returned as `na_label`.

- urban_max:

  Numeric. RUCA codes `<= urban_max` are classified as Urban. Default
  `3`.

- suburban_max:

  Numeric. RUCA codes `> urban_max` and `<= suburban_max` are classified
  as Suburban. Default `6`.

- labels:

  Character vector of length 3 giving the labels for Urban, Suburban,
  and Rural categories in that order. Default
  `c("Urban", "Suburban", "Rural")`.

- na_label:

  Character scalar returned for `NA` inputs. Default `"Unknown"`.

- as_factor:

  Logical. When `TRUE` the result is returned as an ordered factor with
  levels `c(labels, na_label)`. Default `FALSE`.

## Value

Character vector (or ordered factor) the same length as `ruca_code`.

## Details

|           |          |
|-----------|----------|
| RUCA code | Category |
| 1-3       | Urban    |
| 4-6       | Suburban |
| 7-10      | Rural    |

ZIP-to-RUCA crosswalk files are available from USDA ERS
(<https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes>).
Join your data to the crosswalk first, then pass the resulting
`ruca_code` column to this function.

## See also

Other provider characteristics:
[`mysterycall_academic_patterns()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_academic_patterns.md),
[`mysterycall_age_category()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_age_category.md),
[`mysterycall_assign_region()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_assign_region.md),
[`mysterycall_classify_medical_school()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_classify_medical_school.md),
[`mysterycall_classify_practice_setting()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_classify_practice_setting.md),
[`mysterycall_collapse_rare()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_collapse_rare.md),
[`mysterycall_extract_physician_name()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_extract_physician_name.md),
[`mysterycall_government_patterns()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_government_patterns.md),
[`mysterycall_impute_age()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_impute_age.md),
[`mysterycall_parse_certification_subspecialty()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_parse_certification_subspecialty.md),
[`mysterycall_recode_credentials()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_recode_credentials.md),
[`mysterycall_reconcile_specialty()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_reconcile_specialty.md),
[`mysterycall_reorder_by_freq()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_reorder_by_freq.md)

## Examples

``` r
codes <- c(1, 2, 4, 5, 7, 10, NA)
mysterycall_classify_ruca(codes)
#> [1] "Urban"    "Urban"    "Suburban" "Suburban" "Rural"    "Rural"    "Unknown" 
# [1] "Urban"   "Urban"   "Suburban" "Suburban" "Rural"  "Rural"  "Unknown"

mysterycall_classify_ruca(codes, as_factor = TRUE)
#> [1] Urban    Urban    Suburban Suburban Rural    Rural    Unknown 
#> Levels: Urban < Suburban < Rural < Unknown
```
