# Export Academic Indicator Summary

Creates a comprehensive summary report of all available academic
indicators, organized by evidence tier.

## Usage

``` r
mysterycall_get_academic_indicators_summary()

get_academic_indicators_summary(...)
```

## Value

Named list with elements `module_version`, `created_date`, `indicators`
(three-tier named list with confidence ranges and indicator scores),
`total_known_institutions`, `total_patterns`, and `usage_notes`.

## See also

[`mysterycall_classify_academic_affiliation`](https://mufflyt.github.io/mysterycall/reference/mysterycall_classify_academic_affiliation.md),
[`mysterycall_check_academic_name_patterns`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_academic_name_patterns.md)

Other classification:
[`mysterycall_check_academic_name_patterns()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_academic_name_patterns.md),
[`mysterycall_classify_academic_affiliation()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_classify_academic_affiliation.md)

## Examples

``` r
summary <- mysterycall_get_academic_indicators_summary()
summary$total_known_institutions
#> [1] 21
names(summary$indicators)
#> [1] "tier1_education_training" "tier2_research_clinical" 
#> [3] "tier3_name_patterns"     
```
