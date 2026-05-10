# Format duration in human-readable form

Format duration in human-readable form

## Usage

``` r
mysterycall_format_duration(seconds)
```

## Arguments

- seconds:

  Duration in seconds

## Value

Formatted string (e.g., "2h 34m 15s")

## Examples

``` r
mysterycall_format_duration(45)
#> [1] "45.0s"
mysterycall_format_duration(125)
#> [1] "2m 5s"
mysterycall_format_duration(3700)
#> [1] "1h 1m 40s"
```
