# Convert to Canonical Uppercase

Normalizes ASCII characters and converts the result to uppercase — the
standard format for USPS address comparison.

## Usage

``` r
mysterycall_caps(x)
```

## Arguments

- x:

  Character vector of strings to convert.

## Value

Character vector in uppercase with normalized ASCII and whitespace.

## See also

Other address-normalization:
[`mysterycall_ascii_norm()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_ascii_norm.md),
[`mysterycall_has_street_number()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_has_street_number.md),
[`mysterycall_is_po_box()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_is_po_box.md),
[`mysterycall_normalize_directionals()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_directionals.md),
[`mysterycall_normalize_state()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_state.md),
[`mysterycall_normalize_suffix()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_suffix.md),
[`mysterycall_normalize_units()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_units.md),
[`mysterycall_normalize_zip5()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_zip5.md),
[`mysterycall_strip_suite()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_strip_suite.md),
[`normalize_address_df()`](https://mufflyt.github.io/mysterycall/reference/normalize_address_df.md)

## Examples

``` r
caps("123 main street")
#> Warning: 'caps' is deprecated.
#> Use 'mysterycall_caps' instead.
#> See help("Deprecated")
#> [1] "123 MAIN STREET"
caps("456 oak ave #100")
#> Warning: 'caps' is deprecated.
#> Use 'mysterycall_caps' instead.
#> See help("Deprecated")
#> [1] "456 OAK AVE #100"
```
