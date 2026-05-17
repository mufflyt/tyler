# Run the built-in name-parser accuracy suite

Parses 13 curated edge-case physician name strings covering the DO
credential vs Vietnamese surname ambiguity, three-part comma formats,
hyphenated last names, name particles, and single-word names. Prints a
per-case report and a summary to the console.

## Usage

``` r
mysterycall_test_name_parser()
```

## Value

(invisibly) The validated tibble from
[`mysterycall_validate_parsed_names()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_validate_parsed_names.md).

## See also

[`mysterycall_parse_physician_name()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_parse_physician_name.md),
[`mysterycall_validate_parsed_names()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_validate_parsed_names.md)

Other name-parsing:
[`mysterycall_format_physician_name()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_format_physician_name.md),
[`mysterycall_parse_physician_name()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_parse_physician_name.md),
[`mysterycall_validate_parsed_names()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_validate_parsed_names.md)

## Examples

``` r
if (FALSE) { # interactive()
tbl <- mysterycall_test_name_parser()
sum(tbl$is_valid)       # number of valid parses
tbl$quality_issue       # per-case quality codes
}
```
