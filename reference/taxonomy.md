# Taxonomy Codes for Obstetricians and Gynecologists

This dataset contains taxonomy codes for Obstetricians and Gynecologists
among other healthcare providers.

## Usage

``` r
taxonomy
```

## Format

A data frame with three columns:

- Code:

  NUCC (National Uniform Claim Committee) taxonomy code.

- Classification:

  Provider classification (e.g., Allopathic & Osteopathic Physicians).

- Specialization:

  Provider specialization within the classification.

## Source

<https://www.nucc.org/images/stories/PDF/taxonomy_23_0.pdf>

## Value

A tibble mapping NUCC taxonomy codes to descriptive provider types
relevant to obstetrics and gynecology research.

## See also

Other datasets:
[`ACOG_Districts`](https://mufflyt.github.io/mysterycall/reference/ACOG_Districts.md),
[`acgme`](https://mufflyt.github.io/mysterycall/reference/acgme.md),
[`cityStateToLatLong`](https://mufflyt.github.io/mysterycall/reference/cityStateToLatLong.md),
[`fips`](https://mufflyt.github.io/mysterycall/reference/fips.md)

## Examples

``` r
# \donttest{
# Load the taxonomy dataset
data(taxonomy)

# Explore the dataset
print(taxonomy)
#> # A tibble: 862 × 3
#>    Code       Classification             Specialization                    
#>    <chr>      <chr>                      <chr>                             
#>  1 101Y00000X Counselor                  NA                                
#>  2 101YA0400X Counselor                  Addiction (Substance Use Disorder)
#>  3 101YM0800X Counselor                  Mental Health                     
#>  4 101YP1600X Counselor                  Pastoral                          
#>  5 101YP2500X Counselor                  Professional                      
#>  6 101YS0200X Counselor                  School                            
#>  7 102L00000X Psychoanalyst              NA                                
#>  8 102X00000X Poetry Therapist           NA                                
#>  9 103G00000X Clinical Neuropsychologist NA                                
#> 10 103GC0700X Clinical Neuropsychologist Clinical                          
#> # ℹ 852 more rows
# }
```
