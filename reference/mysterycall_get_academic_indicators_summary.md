# Export Academic Indicator Summary

Creates a comprehensive summary report of all available academic
indicators, organized by evidence tier.

## Usage

``` r
mysterycall_get_academic_indicators_summary()
```

## Value

A named list with elements:

- `module_version`:

  Character. Package version string.

- `created_date`:

  Character. ISO date string (YYYY-MM-DD).

- `indicators`:

  Named list with three sublists: `tier1_education_training`,
  `tier2_research_clinical`, and `tier3_name_patterns`, each containing
  confidence ranges and indicator scores.

- `total_known_institutions`:

  Integer. Count of known academic institutions in the bundled lookup.

- `total_patterns`:

  Integer. Count of name-pattern rules.

- `usage_notes`:

  Character vector. Recommended usage guidelines.

## See also

[`mysterycall_classify_academic_affiliation()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_classify_academic_affiliation.md),
[`mysterycall_check_academic_name_patterns()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_academic_name_patterns.md)

Other provider characteristics:
[`mysterycall_academic_patterns()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_academic_patterns.md),
[`mysterycall_age_category()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_age_category.md),
[`mysterycall_assign_region()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_assign_region.md),
[`mysterycall_check_academic_name_patterns()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_academic_name_patterns.md),
[`mysterycall_classify_medical_school()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_classify_medical_school.md),
[`mysterycall_classify_practice_setting()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_classify_practice_setting.md),
[`mysterycall_classify_ruca()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_classify_ruca.md),
[`mysterycall_collapse_rare()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_collapse_rare.md),
[`mysterycall_extract_physician_name()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_extract_physician_name.md),
[`mysterycall_genderize()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_genderize.md),
[`mysterycall_government_patterns()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_government_patterns.md),
[`mysterycall_impute_age()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_impute_age.md),
[`mysterycall_most_common_gender()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_most_common_gender.md),
[`mysterycall_parse_certification_subspecialty()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_parse_certification_subspecialty.md),
[`mysterycall_physician_age()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_physician_age.md),
[`mysterycall_recode_credentials()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_recode_credentials.md),
[`mysterycall_reconcile_specialty()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_reconcile_specialty.md),
[`mysterycall_reorder_by_freq()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_reorder_by_freq.md)

## Examples

``` r
summary <- mysterycall_get_academic_indicators_summary()
summary$total_known_institutions
#> [1] 21
names(summary$indicators)
#> [1] "tier1_education_training" "tier2_research_clinical" 
#> [3] "tier3_name_patterns"     
```
