# Generate a sample-size methods sentence

Calls
[`mysterycall_cochran_n()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_cochran_n.md)
and returns a ready-to-paste sentence for the methods section of a
mystery-caller manuscript.

## Usage

``` r
mysterycall_sample_size_text(N, margin_of_error = 0.05)
```

## Arguments

- N:

  Integer. Total population size.

- margin_of_error:

  Numeric in `(0, 1)`. Desired margin of error. Default `0.05` (+/-5%).

## Value

A single character string.

## See also

Other manuscript:
[`mysterycall_format_results_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_format_results_table.md),
[`mysterycall_methods_paragraph()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_methods_paragraph.md),
[`mysterycall_summarize_demographics()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_summarize_demographics.md),
[`mysterycall_table1_gtsummary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1_gtsummary.md)

## Examples

``` r
mysterycall_sample_size_text(369)
#> [1] "Using the Cochran formula for finite populations (N = 369, margin of error = 5%), a minimum sample size of 192 participants is required."
```
