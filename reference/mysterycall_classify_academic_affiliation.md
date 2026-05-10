# Classify Academic vs. Non-Academic Practice Setting

Combines organization name patterns, optional hospital affiliation data,
and optional specialty-based adjustments to classify academic vs.
non-academic practice settings using a weighted scoring system.

## Usage

``` r
mysterycall_classify_academic_affiliation(
  org_name,
  hospital_affiliation = NULL,
  specialty = NULL
)

classify_academic_affiliation(...)
```

## Arguments

- org_name:

  Character vector of organization names to classify.

- hospital_affiliation:

  Character vector or NULL. Optional hospital affiliation names for
  additional pattern matching. Must be same length as `org_name` if
  provided. Default: `NULL`.

- specialty:

  Character vector or NULL. Optional physician specialties.
  Research-intensive specialties receive a 0.05 confidence boost. Must
  be same length as `org_name` if provided. Default: `NULL`.

## Value

Data frame with one row per input and columns:

- academic_classification:

  Character. "Academic" or "Non-Academic".

- confidence_score:

  Numeric. Confidence in the classification (0–1).

- matched_pattern:

  Character. Highest-scoring pattern match, or NA.

## See also

[`mysterycall_check_academic_name_patterns`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_academic_name_patterns.md),
[`mysterycall_get_academic_indicators_summary`](https://mufflyt.github.io/mysterycall/reference/mysterycall_get_academic_indicators_summary.md)

Other classification:
[`mysterycall_check_academic_name_patterns()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_academic_name_patterns.md),
[`mysterycall_get_academic_indicators_summary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_get_academic_indicators_summary.md)

## Examples

``` r
classify_academic_affiliation("University of Michigan Medical Center")
#> Warning: 'classify_academic_affiliation' is deprecated.
#> Use 'mysterycall_classify_academic_affiliation' instead.
#> See help("Deprecated")
#>   academic_classification confidence_score               matched_pattern
#> 1                Academic             0.99 KNOWN_ACADEMIC: UNIVERSITY OF
classify_academic_affiliation("Community Regional Hospital")
#> Warning: 'classify_academic_affiliation' is deprecated.
#> Use 'mysterycall_classify_academic_affiliation' instead.
#> See help("Deprecated")
#>   academic_classification confidence_score matched_pattern
#> 1            Non-Academic                0            <NA>
```
