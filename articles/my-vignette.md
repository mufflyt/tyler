# Searching the NPI Database Starting with Taxonomy Codes

## Overview

The
[`mysterycall::taxonomy`](https://mufflyt.github.io/mysterycall/reference/taxonomy.md)
data and `mysterycall::search_by_taxonomy` function in the R package
offers a convenient and efficient way to query the NPI Database for
healthcare providers based on their taxonomy descriptions. This vignette
provides a comprehensive guide on how to effectively utilize this
function, explores its various capabilities, and offers illustrative use
cases.

### Installation

Before you can harness the power of the `search_by_taxonomy` function,
it is essential to ensure that you have the `tyler` package installed.
You can effortlessly install it using the following command:

## Example Usage

### Understanding Taxonomy Descriptions

Taxonomy descriptions, derived from the National Physician Taxonomy
Codes and the NPPES (National Plan and Provider Enumeration System)
database, are fundamental components of the United States healthcare
system. They play a pivotal role in the identification and
categorization of healthcare providers for various purposes, including
billing, insurance, and regulatory compliance.

In particular, the
[`mysterycall::taxonomy`](https://mufflyt.github.io/mysterycall/reference/taxonomy.md)
data frame contains NUCC taxonomy codes utilized in NPPES data files.
Each Taxonomy Code comprises a unique ten-character identifier that aids
in the identification of healthcare provider types and their areas of
expertise. Notably, the OBGYN taxonomy codes are sourced from Version
23.1 dated July 1, 2023.

Taxonomy codes can be obtained from the National Uniform Claim Committee
(NUCC) website here. You can employ these codes to pinpoint specific
taxonomy descriptions for your search. For instance, if you are
interested in finding taxonomy codes that include the string `"GYN"` you
can use this code to facilitate your search in the search_by_taxonomy
function.

``` r

obgyn_taxonomy <- mysterycall::taxonomy %>% 
  dplyr::filter(str_detect(`Classification`, fixed("GYN", ignore_case = TRUE))) %>% 
  dplyr::select(Code, Specialization)
```

``` r
Code       Specialization                                   
   <chr>      <chr>                                            
 1 207V00000X NA                                               
 2 207VC0300X Complex Family Planning                          
 3 207VC0200X Critical Care Medicine                           
 4 207VF0040X Female Pelvic Medicine and Reconstructive Surgery
 5 207VX0201X Gynecologic Oncology                             
 6 207VG0400X Gynecology                                       
 7 207VH0002X Hospice and Palliative Medicine                  
 8 207VM0101X Maternal & Fetal Medicine                        
 9 207VB0002X Obesity Medicine                                 
10 207VX0000X Obstetrics                                       
11 207VE0102X Reproductive Endocrinology 
```

### Search by Taxonomy Description

The `search_by_taxonomy` function excels at searching the NPI Database
for healthcare providers based on taxonomy descriptions. This
functionality proves invaluable when verifying external data regarding
subspecialist provider counts and filling in gaps for providers who may
not be board-certified but are actively practicing (board-eligible).
This data can be seamlessly integrated with other databases, enhancing
its utility. For internal use, you can refer to
`"Exploratory/Workforce/subspecialists_only"`. One significant advantage
is that all search results include a National Provider Identifier (NPI).

### Example Usage

In this illustrative example, we employ the `search_by_taxonomy`
function to identify healthcare providers specializing in “Hospice and
Palliative Medicine” based on taxonomy descriptions. The resulting
output is a dataframe containing information about physicians with
either an MD or DO qualification, practicing in the United States as
individuals, and self-identifying with a taxonomy of “Hospice and
Palliative Medicine.”

``` r

# Search for providers based on taxonomy descriptions
taxonomy_descriptions <- c("Hospice and Palliative Medicine")

data <- search_by_taxonomy(taxonomy_to_search = taxonomy_descriptions)
```

``` r

# Note: basic_credential was removed in the select() statement above
# So we'll count by basic_gender and state instead
dplyr::count(data, basic_gender, addresses_state)
```

You can easily summarize the resulting data frame in a user-friendly
format.

``` r

# Summarize the resulting data frame by state
data %>%
  dplyr::count(addresses_state)
```

We will often need to merge the rows from the `search_taxonomy` data and
the `get_clinicians` data. These are the steps needed to make the
structure and names similar to the `get_clinicians` data.

    all_taxonomy_search_data <- data %>% 
      distinct(npi, .keep_all = TRUE) %>%
      
      # Keep only the OBGYN subspecialist taxonomy descriptions.  
      filter(taxonomies_desc %in% c("Obstetrics & Gynecology, Female Pelvic Medicine and Reconstructive Surgery", "Obstetrics & Gynecology, Gynecologic Oncology", "Obstetrics & Gynecology, Maternal & Fetal Medicine", "Obstetrics & Gynecology, Reproductive Endocrinology")) %>%
      
      # Extract the first five of the zip code.  
      mutate(addresses_postal_code = str_sub(addresses_postal_code,1 ,5)) %>%
      mutate(basic_enumeration_date = ymd(basic_enumeration_date)) %>%
      
      # Pull the year out of the enumeration full data.  
      mutate(basic_enumeration_date_year = year(basic_enumeration_date), .after = ifelse("basic_enumeration_date" %in% names(.), "basic_enumeration_date", last_col())) %>%
      mutate(basic_middle_name = str_sub(basic_middle_name,1 ,1)) %>%
      mutate(across(c(basic_first_name, basic_last_name, basic_middle_name), .fns = ~str_remove_all(., "[[\\p{P}][\\p{S}]]"))) %>%
      
      # Get data ready to add these taxonomy rows to the `search_and_process_npi`/GOBA data set.
      rename(NPI = npi, first_name = basic_first_name, last_name = basic_last_name, middle_name = basic_middle_name, GenderPhysicianCompare = basic_gender, sub1 = taxonomies_desc, city = addresses_city, state = addresses_state, name.x = full_name, `Zip CodePhysicianCompare` = addresses_postal_code) %>%
      mutate(GenderPhysicianCompare = recode(GenderPhysicianCompare, "F" = "Female", "M" = "Male", type_convert = TRUE)) %>%
      
      # Show the subspecialty from goba.  
      mutate(sub1 = recode(sub1, "Obstetrics & Gynecology, Female Pelvic Medicine and Reconstructive Surgery" = "FPM", "Obstetrics & Gynecology, Gynecologic Oncology" = "ONC", "Obstetrics & Gynecology, Maternal & Fetal Medicine" = "MFM", "Obstetrics & Gynecology, Reproductive Endocrinology" = "REI", type_convert = TRUE))

### Function Details

#### Parameters

`taxonomy_to_search`: A character vector that should contain the desired
taxonomy description(s) to be used as search criteria.

#### Output

The function returns a data frame that has been filtered to include NPI
data matching the specified taxonomy description(s).

## Conclusion

The `search_by_taxonomy` function stands as a wrapper for exploring the
NPI Database through taxonomy descriptions. It empowers users to
identify healthcare providers with precise specializations, rendering it
a resource for healthcare-related research and in-depth analysis.

## Features and bugs

If you have ideas for other features that would make name handling
easier, or find a bug, the best approach is to either report it or add
it!
