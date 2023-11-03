
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

# MAKING MAPS
### `tyler::geocode_unique_addresses`
Takes a csv file of addresses and prints out the lat and long as separate columns.  
```r
output_data <- 
    geocode_unique_addresses(file_path = "/Users/tylermuffly/Dropbox (Personal)/Tannous/data/address_for_geocoding.csv", 
    google_maps_api_key = "????", 
    output_file_path = "/Users/tylermuffly/Dropbox (Personal)/Tannous/data/geocoded_unique_addresses.csv")
```

### `tyler::create_basemap`
This is a nice leaflet map with all the features you want for an interactive html map.  We can use it for dot maps.  
```r
# Create a base map with a custom title
my_map <- create_base_map("TITLE")

# Display the map and add circle markers
my_map <- my_map %>%
  leaflet::addCircleMarkers(lng = ~longitude,
                           lat = ~latitude,
                           data = data_points,
                           popup = ~popup_text,
                           radius = ~radius,
                           color = ~color,
                           fill = TRUE,
                           stroke = FALSE,
                           fillOpacity = 0.8)
```
<img src="https://github.com/mufflyt/tyler/assets/44621942/87a04a9d-7ddd-46b6-8917-947530983088" width="50%">

### `tyler::create_isochrones`
A function that interfaces with HERE API to gather the geometry for the isochrones.  Does not need to be used on its own.  Used INTERNALLY only.  

### `tyler::create_isochrones_for_dataframe`
A function that iterates the `tyler::create_isochrones` over an entire dataframe.  The only input is a dataframe and the breaks for the number of minutes for each isochrones.  
```r
isochrones_data <- create_isochrones_for_dataframe(gyn_onc, breaks = c(0, 30, 60, 120, 180))
```

### `tyler::map_by_acog_districts.R`
Leaflet dot map of physicians on colored ACOG Districts.  Loops through each ACOG district to generate hex maps individually.  
```r
#Use case:
generate_acog_districts_sf("inst/extdata/ACOG_Districts.csv")
generate_acog_districts_sf()

all_map <-
  tyler::generate_maps(
    physician_file = "inst/extdata/Physicians.rds",
    acog_districts_file = "inst/extdata/ACOG_Districts.csv",
    trait_map = "all",
    honey_map = "all",
    grid_size = c(0.2, 0.2),
    specific_district = "District V"
  ))
```
<img src="https://github.com/mufflyt/tyler/assets/44621942/2511d71c-f5c3-48be-ac5f-f439a67bf89a" width="50%">
<img src="https://github.com/mufflyt/tyler/assets/44621942/58553c2b-f7c7-4f86-be35-c650e54dd2c3" width="50%">

### `tyler::create_individual_isochrone_plots.R`
Function to create individual plots and shapefiles for specified drive times.  
```r
# Usage example:
# List of unique drive times for which you want to create plots and shapefiles
drive_times <- unique(isochrones$drive_time)
create_individual_isochrone_plots(isochrones, drive_times)
```
<img src="https://github.com/mufflyt/tyler/assets/44621942/2daffc4f-e5d7-4f35-9b0e-054b979cdd7f" width="25%">

# DEMOGRAPHICS
```r
 #       *"B01001_026E  Estimate _Total _Female                      \n",
 #       "B01001_027E  Estimate_Total_Female_Under 5 years       \n",
 #       "B01001_028E  Estimate_Total_Female_5 to 9 years        \n",
 #       "B01001_029E  Estimate_Total_Female_10 to 14 years      \n",
 #       "B01001_030E  Estimate_Total_Female_15 to 17 years      \n",
 #       "B01001_031E  Estimate_Total_Female_18 and 19 years     \n",
 #       "B01001_032E  Estimate_Total_Female_20 years            \n",
 #       *"B01001_033E  Estimate_Total_Female_21 years            \n",
 #       *"B01001_034E  Estimate_Total_Female_22 to 24 years      \n",
 #       *"B01001_035E  Estimate_Total_Female_25 to 29 years      \n",
 #       *"B01001_036E  Estimate_Total_Female_30 to 34 years      \n",
 #       *"B01001_037E  Estimate_Total_Female_35 to 39 years      \n",
 #       *"B01001_038E  Estimate_Total_Female_40 to 44 years      \n",
 #       *"B01001_039E  Estimate_Total_Female_45 to 49 years      \n",
 #       *"B01001_040E  Estimate_Total_Female_50 to 54 years      \n",
 #       *"B01001_041E  Estimate_Total_Female_55 to 59 years      \n",
 #       *"B01001_042E  Estimate_Total_Female_60 and 61 years     \n",
 #       *"B01001_043E  Estimate_Total_Female_62 to 64 years      \n",
 #       *"B01001_044E  Estimate_Total_Female_65 and 66 years     \n",
 #       *"B01001_045E  Estimate_Total_Female_67 to 69 years      \n",
 #       *"B01001_046E  Estimate_Total_Female_70 to 74 years      \n",
 #       *"B01001_047E  Estimate_Total_Female_75 to 79 years      \n",
 #       *"B01001_048E  Estimate_Total_Female_80 to 84 years      \n",
 #       *"B01001_049E  Estimate_Total_Female_85 years and over   \n",
```

### `tyler::get_census_data`
This function retrieves Census data using `censusapi` for all states' block groups by looping over the specified list of state FIPS codes.  This only brings back data on females from "B01001_01, 26, 33:49E".

```r
all_census_data <- get_census_data(us_fips_list, "your_censusapi_key_here", vintage=2019)
```

# NPI Search
Search first names, last names, only individuals `enumeration_type = "ind"`, and only physicians `("MD", "DO")` in the United States from the [NPPES]([https://github.com/](https://npiregistry.cms.hhs.gov/search)).  

```r
search_and_process_npi <- function(input_file,
                                   enumeration_type = "ind",
                                   limit = 5L,
                                   country_code = "US",
                                   filter_credentials = c("MD", "DO"))

input_file <- "/Users/tylermuffly/Dropbox (Personal)/Nomogram/nomogram/data/nppes_search/Lo_R_Author.csv"
output_result <- search_and_process_npi(input_file)
```

