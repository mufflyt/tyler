# Normalize Unit Designators

Standardizes unit designators (SUITE, APT, UNIT, FLOOR, ROOM) to USPS
abbreviations. Handles both inline units in address line 1 and separate
units in address line 2.

## Usage

``` r
mysterycall_normalize_units(addr1, addr2 = NA_character_)

normalize_units(...)
```

## Arguments

- addr1:

  Character scalar for the primary address line.

- addr2:

  Character scalar for the secondary address line (optional). Default is
  `NA_character_`.

## Value

A list with two elements:

- addr1:

  Normalized primary address line.

- addr2:

  Normalized secondary address line (or NA if not provided).

## See also

Other address-normalization:
[`mysterycall_ascii_norm()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_ascii_norm.md),
[`mysterycall_caps()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_caps.md),
[`mysterycall_has_street_number()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_has_street_number.md),
[`mysterycall_is_po_box()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_is_po_box.md),
[`mysterycall_normalize_directionals()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_directionals.md),
[`mysterycall_normalize_state()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_state.md),
[`mysterycall_normalize_suffix()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_suffix.md),
[`mysterycall_normalize_zip5()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_zip5.md),
[`mysterycall_strip_suite()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_strip_suite.md),
[`normalize_address_df()`](https://mufflyt.github.io/mysterycall/reference/normalize_address_df.md)

## Examples

``` r
normalize_units("123 Main St Suite 100", NA_character_)
#> Warning: 'normalize_units' is deprecated.
#> Use 'mysterycall_normalize_units' instead.
#> See help("Deprecated")
#> $addr1
#> [1] "123 MAIN ST STE 100"
#> 
#> $addr2
#> [1] NA
#> 
normalize_units("456 Oak Avenue", "Apartment 4B")
#> Warning: 'normalize_units' is deprecated.
#> Use 'mysterycall_normalize_units' instead.
#> See help("Deprecated")
#> $addr1
#> [1] "456 OAK AVENUE"
#> 
#> $addr2
#> [1] "APT 4B"
#> 
```
