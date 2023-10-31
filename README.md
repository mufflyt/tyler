
# tyler

<!-- badges: start -->
<!-- badges: end -->

The goal of the 'tyler' package provides a collection of functions designed to facilitate mystery caller studies, often used in evaluating patient access to healthcare. It includes tools for searching and processing National Provider Identifier (NPI) numbers based on names and analyzing demographic data associated with these NPIs. The package simplifies the handling of NPI data and the creation of informative tables for analysis and reporting.


## Installation

You can install the development version of tyler from [GitHub](https://github.com/) with:
``` r
# install.packages("devtools")
devtools::install_github("mufflyt/tyler")
```

# Data: Workforce
### Data: `tyler::acgme`
Dataframe of the OBGYN residency programs scraped from https://apps.acgme.org/ads/Public/Programs/Search.  Name, city, state, accreditation date, program director name, website, rotations, and affiliated hospitals are included.  
'tyler::acgme' - This is a dataframe of every OBGYN residency from the ACGME web site.  This data can be used to map the obgyn residencies, etc.  
```r
obgyn_residencies <- tyler::acgme

# A tibble: 318 × 142
   program_name       address zip   city  state sponsoring_instituti…¹ sponsoring_instituti…² phone original_accreditati…³
   <chr>              <chr>   <chr> <chr> <chr> <chr>                  <chr>                  <chr> <chr>                 
 1 University of Ala… "Unive… 35249 Birm… Alab… 010498                 University of Alabama… (205… September 01, 1949    
 2 USA Health Program "Unive… 36604 Mobi… Alab… 010406                 USA Health             (251… August 01, 1960       
 3 University of Ari… "Banne… 85006 Phoe… Ariz… 038179                 University of Arizona… (602… May 07, 1951   
```

### Data: 'tyler::ACOG_Districts' 
This is a dataframe of state names, ACOG Districts, US Census Bureau Subregions, and state abbreviations. 
```r
acog_districts <- tyler::ACOG_Districts
tyler::ACOG_Districts

# A tibble: 52 × 4
   State                ACOG_District Subregion     State_Abbreviations
   <chr>                <chr>         <chr>         <chr>              
 1 Alabama              District VII  District VII  AL                 
 2 Alaska               District VIII District VIII AK                 
 3 Arizona              District VIII District VIII AZ                 
 4 Arkansas             District VII  District VII  AR                 
 5 California           District IX   District IX   CA                 
 6 Colorado             District VIII District VIII CO                 
 7 Connecticut          District I    District I    CT                 
 8 Delaware             District IV   District IV   DE                 
 9 District of Columbia District IV   District IV   DC                 
10 Florida              District XII  District XII  FL 
```

### Data: 'tyler::taxonomy' 
This is a dataframe of NUCC taxonomy codes used in the NPPES data files.  Of note the OBGYN taxonomy codes are: Version 23.1 from 7/1/2023.  https://nucc.org/index.php/code-sets-mainmenu-41/provider-taxonomy-mainmenu-40/csv-mainmenu-57.  For example: 
```r
obgyn_taxonomy <- tyler::taxonomy %>% filter(str_detect(`Classification`, fixed("GYN", ignore_case = TRUE))) %>% select (Code, Specialization)

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
### Data: 'tyler::physicians' 
Internal package dataframe with the names and coordinates for subspecialists in OBGYN.  The source file `Physicians.rds` is found at `tyler/inst/extdata`
``` r
library(tyler)
tyler::physicians
```

# Making Maps
### `tyler::create_basemap`
This is a nice leaflet map with all the features you want for an interactive html map.  We can use it for dot maps.  

