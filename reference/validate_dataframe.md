# Internal helpers for validating user inputs

These utilities provide consistent, human-friendly validation messages
across the package. They are kept internal so that exported functions
can depend on them without bloating the public API surface area.

## Usage

``` r
validate_dataframe(
  x,
  name = "data",
  allow_null = FALSE,
  allow_zero_rows = TRUE
)
```

## Value

These helpers return the validated object (invisibly) so they can be
used inline while performing assertions.
