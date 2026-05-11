# Create a Formula for Poisson Model

This function creates a formula for a Poisson model based on the
provided data, response variable, and optional random effect.

## Usage

``` r
mysterycall_create_formula(data, response_var, random_effect = NULL)
```

## Arguments

- data:

  A dataframe containing the predictor and response variables.

- response_var:

  The name of the response variable in the dataframe.

- random_effect:

  Optional. The name of the random effect variable for the formula.

## Value

A formula object suitable for modeling in R.

## See also

Other modeling helpers:
[`mysterycall_check_normality()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_normality.md),
[`mysterycall_plot_emmeans()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_emmeans.md),
[`mysterycall_plot_interaction()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_interaction.md)

## Examples

``` r
# Example usage:
response_variable <- "days"
random_effect_term <- "name"  # Change this to the desired random effect variable
df3_filtered <- data.frame(days = c(5, 10, 15), age = c(30, 40, 50), name = c("A", "B", "C"))
formula <- mysterycall_create_formula(df3_filtered, response_variable, random_effect_term)
#> Creating formula with response variable: days
#> Predictor variables identified: age
#> Predictor variables after formatting: `age`
#> Initial formula string: days ~ `age`
#> Formula string with random effect: days ~ `age` + (1 | name )
#> Final formula object created: days ~ age + (1 | name)
formula
#> days ~ age + (1 | name)
#> <environment: 0x561a87cec558>
```
