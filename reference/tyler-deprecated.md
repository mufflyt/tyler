# Deprecated functions in mysterycall

These helpers are retained for backward compatibility only. They emit
deprecation warnings and defer to the modern workflow helpers where
possible.

## Usage

``` r
mysterycall_search_npi(input_data, ...)

mysterycall_test_isochrones(input_file, ...)

mysterycall_process_isochrones(input_file, chunk_size = 25, ...)
```

## Arguments

- input_data:

  A data frame or CSV path containing `first` and `last` columns.

- ...:

  Additional arguments passed to
  [`mysterycall_search_and_process_npi()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_search_and_process_npi.html).

- input_file:

  Deprecated. Previously accepted a data frame with `lat` and `long`
  columns.

- chunk_size:

  Deprecated.

## Value

- `mysterycall_search_npi()` returns the tibble produced by
  [`mysterycall_search_and_process_npi()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_search_and_process_npi.html).

- `mysterycall_test_isochrones()` and `mysterycall_process_isochrones()`
  do not return a value and instead stop with a deprecation error.
