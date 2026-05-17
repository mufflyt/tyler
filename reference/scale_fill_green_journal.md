# Green Journal discrete fill scale

Convenience wrapper for
[`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
using the Okabe-Ito palette from
[`palette_green_journal()`](https://mufflyt.github.io/mysterycall/reference/palette_green_journal.md).

## Usage

``` r
scale_fill_green_journal(...)
```

## Arguments

- ...:

  Arguments passed to
  [`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html).

## Value

A ggplot2
[ggplot2::Scale](https://ggplot2.tidyverse.org/reference/Scale.html)
object.

## See also

[`scale_color_green_journal()`](https://mufflyt.github.io/mysterycall/reference/scale_color_green_journal.md)
for colour aesthetics;
[`palette_green_journal()`](https://mufflyt.github.io/mysterycall/reference/palette_green_journal.md)
for the underlying colour values.

Other green-journal-colors:
[`palette_green_journal()`](https://mufflyt.github.io/mysterycall/reference/palette_green_journal.md),
[`scale_color_green_journal()`](https://mufflyt.github.io/mysterycall/reference/scale_color_green_journal.md)

## Examples

``` r
library(ggplot2)
ggplot(mpg, aes(class, fill = drv)) +
  geom_bar() +
  scale_fill_green_journal()
```
