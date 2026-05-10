# Clear the isochrone memoization cache

The
[`mysterycall_create_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_create_isochrones.md)
function caches every result in memory for the duration of the R
session. Call this after processing a large batch to release that
memory.

## Usage

``` r
mysterycall_clear_isochrone_cache()
```

## Value

Invisibly `NULL`.

## Examples

``` r
mysterycall_clear_isochrone_cache()
```
