# Check if Organization Name Suggests Academic Affiliation

Evaluates organization names against known academic institutions and
tiered pattern lists to determine academic affiliation probability.
Returns the highest confidence match found.

## Usage

``` r
mysterycall_check_academic_name_patterns(org_name, confidence_threshold = 0.85)
```

## Arguments

- org_name:

  Character vector of organization names to evaluate.

- confidence_threshold:

  Numeric. Minimum confidence level to classify as academic. Scores
  below this threshold are zeroed out. Default: `0.85`.

## Value

Data frame with one row per input name and columns:

- academic_indicator:

  Logical. TRUE if name matches an academic pattern at or above the
  confidence threshold.

- confidence_score:

  Numeric. Highest confidence score (0.0–0.99).

- matched_pattern:

  Character. Description of the matching pattern. NA if no match.

## See also

[`mysterycall_classify_academic_affiliation`](https://mufflyt.github.io/mysterycall/reference/mysterycall_classify_academic_affiliation.md)

Other classification:
[`mysterycall_classify_academic_affiliation()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_classify_academic_affiliation.md),
[`mysterycall_get_academic_indicators_summary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_get_academic_indicators_summary.md)

## Examples

``` r
check_academic_name_patterns(c("Johns Hopkins Hospital",
                               "Community Hospital",
                               "University of Michigan Medical Center"))
#> Warning: 'check_academic_name_patterns' is deprecated.
#> Use 'mysterycall_check_academic_name_patterns' instead.
#> See help("Deprecated")
#>   academic_indicator confidence_score               matched_pattern
#> 1               TRUE             0.99 KNOWN_ACADEMIC: JOHNS HOPKINS
#> 2              FALSE             0.00                          <NA>
#> 3               TRUE             0.99 KNOWN_ACADEMIC: UNIVERSITY OF
```
