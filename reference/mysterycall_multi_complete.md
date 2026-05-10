# Complete current step in multi-progress tracker

Complete current step in multi-progress tracker

## Usage

``` r
mysterycall_multi_complete(tracker, result = NULL)
```

## Arguments

- tracker:

  Multi-progress tracker object

- result:

  Optional result message

## Value

Invisible NULL

## Examples

``` r
# \donttest{
tracker <- mysterycall_multi_step(c("Geocode", "Validate"), 10)
mysterycall_multi_update(tracker, amount = 10)
mysterycall_multi_complete(tracker, result = "ok")
# }
```
