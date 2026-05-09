# Generate a Summary Sentence for the Most Common Gender, Specialty, Training, and Academic Affiliation

This function calculates and returns a sentence that describes the most
common gender, specialty, training, and academic affiliation in the
provided dataset.

## Usage

``` r
mysterycall_most_common_gender(data)
```

## Arguments

- data:

  A data frame containing the columns `gender`, `specialty`,
  `Provider.Credential.Text`, and `academic_affiliation`.

## Value

A character string summarizing the most common gender, specialty,
training, and academic affiliation along with their respective
proportions.

## Details

The function filters out missing values in each column before
determining the most common value. It then calculates the proportion of
this most common value relative to the total non-missing values in that
column.

## See also

Other gender:
[`mysterycall_genderize()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_genderize.html)

## Examples

``` r
# Example 1: Basic usage with a small dataset
data <- data.frame(
  gender = c("Male", "Female", "Female", "Male", "Male"),
  specialty = c("Cardiology", "Cardiology", "Neurology", "Cardiology", "Neurology"),
  Provider.Credential.Text = c("MD", "MD", "DO", "MD", "DO"),
  academic_affiliation = c("Yes", "No", "Yes", "No", "Yes")
)
result <- mysterycall_most_common_gender(data)
print(result)
#> [1] "The most common gender in the dataset was male (60%). The most common specialty was Cardiology (60%). The most common training was MD (60%). The academic affiliation status most frequently occurring was yes (60%)."

# Example 2: Handling missing data
df_with_na <- data.frame(
  gender = c("Male", NA, "Female", "Male", "Male"),
  specialty = c("Cardiology", "Cardiology", "Neurology", NA, "Neurology"),
  Provider.Credential.Text = c("MD", "MD", "DO", "MD", "DO"),
  academic_affiliation = c("Yes", "No", "Yes", "No", NA)
)
result <- mysterycall_most_common_gender(df_with_na)
print(result)
#> [1] "The most common gender in the dataset was male (75%). The most common specialty was Cardiology (50%). The most common training was MD (60%). The academic affiliation status most frequently occurring was no (50%)."

# Example 3: Different proportions with a larger dataset
df_large <- data.frame(
  gender = c(rep("Male", 70), rep("Female", 30)),
  specialty = c(rep("Cardiology", 50), rep("Neurology", 30), rep("Orthopedics", 20)),
  Provider.Credential.Text = c(rep("MD", 60), rep("DO", 40)),
  academic_affiliation = c(rep("Yes", 40), rep("No", 60))
)
result <- mysterycall_most_common_gender(df_large)
print(result)
#> [1] "The most common gender in the dataset was male (70%). The most common specialty was Cardiology (50%). The most common training was MD (60%). The academic affiliation status most frequently occurring was no (60%)."
```
