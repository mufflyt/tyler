# Fail progress bar

Marks a progress bar as failed with an optional error message.

## Usage

``` r
mysterycall_progress_fail(pb, msg = NULL)
```

## Arguments

- pb:

  Progress bar object from
  [`mysterycall_progress_bar()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_bar.md).

- msg:

  Optional error message string displayed alongside the failure.

## Value

Invisible NULL.

## Examples

``` r
pb <- mysterycall_progress_bar("Processing", total = 10)
#> Starting: Processing (10 items)
mysterycall_progress_fail(pb, msg = "Geocoding API unreachable")
#>   ✗ Processing failed: Geocoding API unreachable
```
