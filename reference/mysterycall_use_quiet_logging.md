# Toggle quiet logging for helper functions

Toggle quiet logging for helper functions

## Usage

``` r
mysterycall_use_quiet_logging(quiet = TRUE)
```

## Arguments

- quiet:

  Logical flag. When `TRUE`, suppress messages emitted by
  [`mysterycall_log_info()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_log_info.md)
  when using quiet-aware wrappers.

## Value

The previous quiet value (invisibly).

## Examples

``` r
old <- mysterycall_use_quiet_logging(TRUE)
# ... run operations silently ...
mysterycall_use_quiet_logging(old)
```
