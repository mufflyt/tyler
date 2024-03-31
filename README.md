
# tyler
# Warning!!! This project is under active development, wait for the release of version 2.0.0 if you want to use it in production. Please help to improve this package by submitting issues and pull requests.

<!-- badges: start -->
<img src="https://github.com/mufflyt/tyler/assets/44621942/3c4faeb4-7fe5-42e8-b2bf-7832588c6f57" width="20%">
<!-- badges: end -->

The goal of the 'tyler' package provides a collection of functions designed to facilitate mystery caller studies, often used in evaluating patient access to healthcare. It includes tools for searching and processing National Provider Identifier (NPI) numbers based on names and analyzing demographic data associated with these NPIs. The package simplifies the handling of NPI data and the creation of informative tables for analysis and reporting. The second goal is to assist with workforce distribution research for OBGYNs.  

## Installation

You can install the development version of tyler from [GitHub](https://github.com/) with:
``` r
# install.packages("devtools")
devtools::install_github("mufflyt/tyler")
```

See the package vignette for a fuller introduction and suggestions on how to use the `tyler()` function efficiently.

vignette(topic = "????", package = "tyler")


### Add in hospital information data from the AHA scraper!!!!

# Workflow 👨‍🦲
1) Gather all the physician data that is needed:
     * Search by subspecialty taxonomy: `tyler::taxonomy` and `tyler::search_by_taxonomy` 👨‍🦲
     * Search by physician name in `goba`: `tyler::search_and_process_npi` 👨‍🦲
     * Merge these two physician data sources together.  See the code at: `exploratory/Workforce/subspecialists_only`  👨‍🦲
     * Add in the physician age from healthgrades.com: ??????
     * Get Physician Compare physician demographics: `tyler::retrieve_clinician_data`  👨‍🦲
     * Complete the gender for all physicians: `tyler::genderize_physicians` 
     * Check with secondary sources...
       
3) By state name determine the ACOG District.
      * dplyr::left_join with `tyler::ACOG_Districts`
5) Geocode the addresses to latitude and longitude for mapping. 👨‍🦲
      * `tyler::geocode_unique_addresses`
6) Get the US Census Bureau data associated with the block groups:
   * `tyler::get_census_data` 👨‍🦲
7) Create the isochrones based on drive times: 
   * `tyler::create_isochrones` 👨‍🦲
   * `tyler::create_isochrones_for_dataframe` 👨‍🦲
   * All this is heavily borrowed from "https://github.com/khnews/2021-delta-appalachia-stroke-access"
8) Create overlap maps of isochrones and block groups
   * `tyler::calculate_intersection_overlap_and_save`- THIS NEEDS WORK
   * `tyler::create_block_group_overlap_map`
   * 

# Data: Workforce
### Data: `tyler::acgme`
The ACGME (Accreditation Council for Graduate Medical Education) is a prominent organization in the United States responsible for accrediting and overseeing graduate medical education programs, including residency and fellowship programs. Residency programs are a crucial component of medical education in the United States. They provide specialized training to medical school graduates, allowing them to become competent and independent physicians in a specific medical specialty. Dataframe of the OBGYN residency programs scraped from https://apps.acgme.org/ads/Public/Programs/Search.  Name, city, state, accreditation date, program director name, website, rotations, and affiliated hospitals are included.  
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
The American College of Obstetricians and Gynecologists (ACOG) is a professional organization representing obstetricians and gynecologists in the United States. ACOG divides its membership into various geographical regions known as "ACOG Districts."  Because there are single-state ACOG Districts (e.g., California, Texas, Florida) we also need to use US Census Bureau subdivisions.  Subdivisions are important for census and statistical purposes because they help organize and categorize population data at the local level. This is a dataframe of state names, ACOG Districts, US Census Bureau Subregions, and state abbreviations. 
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

### Data: 'tyler::physicians' 
Internal package dataframe with the names and coordinates for subspecialists in OBGYN.  The source file `Physicians.rds` is found at `tyler/inst/extdata`
``` r
tyler::physicians

# A tibble: 4,659 × 5
          NPI name                        subspecialty                                        lat   long
        <dbl> <chr>                       <chr>                                             <dbl>  <dbl>
 1 1922051358 Katherine Boyd              Female Pelvic Medicine and Reconstructive Surgery  42.6  -82.9
 2 1750344388 Thomas Byrne                Maternal-Fetal Medicine                            35.2 -102. 
 3 1548520133 Bobby Garcia                Female Pelvic Medicine and Reconstructive Surgery  40.8  -73.9
```

### Data: 'tyler::taxonomy' 
Physician Taxonomy Codes and the NPPES (National Plan and Provider Enumeration System) database are both essential components of the healthcare system in the United States. They play a crucial role in identifying and categorizing healthcare providers for various purposes, including billing, insurance, and regulatory compliance. This is a dataframe of NUCC taxonomy codes used in the NPPES data files.  Each Taxonomy Code consists of a unique ten-character identifier that helps identify the type of healthcare provider and their area of expertise. Of note the OBGYN taxonomy codes are: Version 23.1 from 7/1/2023.  https://nucc.org/index.php/code-sets-mainmenu-41/provider-taxonomy-mainmenu-40/csv-mainmenu-57.  For example: 
```r
obgyn_taxonomy <- tyler::taxonomy %>% filter(str_detect(`Classification`, fixed("GYN", ignore_case = TRUE))) %>% select (Code, Specialization)

   Code       Specialization                                   
   <chr>      <chr>                                            
 1 207V00000X Obstetrics & Gynecology                                               
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

### Searching for Data: `tyler::search_by_taxonomy`
This function searches the NPI Database for healthcare providers based on a taxonomy description.  The `search_by_taxonomy` function is a wrapper on the `npi::npi_search` accessing the registry's Version 2.1 API.  Many thanks to the author and maintainers of the `npi` package for their amazing work.This helps confirm outside data about subspecialist provider counts and fill in the gaps for providers who are not board-certified but are practicing (board-eligible).  This data can be matched to other databases.  Please see `Exploratory/workforce/subspecialists_only` for more code on how to do this.  The nice thing is that all these search results will come with an NPI.  
```
# This will allow us to get subspecialty names and NPI numbers
go_data <- search_by_taxonomy("Gynecologic Oncology")
fpmrs_data <- search_by_taxonomy("Female Pelvic Medicine and Reconstructive Surgery")
rei_data <- search_by_taxonomy("Reproductive Endocrinology")
mfm_data <- search_by_taxonomy("Maternal & Fetal Medicine")

# Merge all data frames into one
      all_taxonomy_search_data <- bind_rows(
        go_data,
        fpmrs_data,
        rei_data,
        mfm_data) %>%
        dplyr::distinct(npi, .keep_all = TRUE)

      dim(all_taxonomy_search_data)
      glimpse(all_taxonomy_search_data)

# 1200 records requested
# Requesting records 0-200...
# Requesting records 200-400...
```

### Searching for Data: `tyler::search_and_process_npi`
National Provider Identifier Search: Search first names, last names, only individuals `enumeration_type = "ind"`, and only physicians `("MD", "DO")` in the United States from the [NPPES]([https://github.com/](https://npiregistry.cms.hhs.gov/search)).  NPI numbers provide a standardized way to identify and track healthcare providers, including physicians, across the United States. Government agencies, such as the Centers for Medicare & Medicaid Services (CMS), use NPI-based data to plan and allocate healthcare resources, including provider reimbursements, medical services, and workforce distribution.

```r
search_and_process_npi <- function(input_file,
                                   enumeration_type = "ind",
                                   limit = 5L,
                                   country_code = "US",
                                   filter_credentials = c("MD", "DO"))

input_file <- "/Users/tylermuffly/Dropbox (Personal)/Nomogram/nomogram/data/nppes_search/Lo_R_Author.csv"
output_result <- search_and_process_npi(input_file)
```

### Searching for Data: `tyler::validate_and_remove_invalid_npi`
This cleans the NPI numbers before it goes into `tyler::retrieve_clinician_data` because if one incorrect NPI number is inserted then it screws up the entire search.  Saves each find as csv file.  
```r
input_csv_path <- "~/Dropbox (Personal)/workforce/subspecialists_only.csv"  # Replace with the path to your CSV file
valid_df <- validate_and_remove_invalid_npi(input_csv_path)

Search result saved as: data/search_results_1053601807_20231119192903.csv                                
Search result saved as: data/search_results_1528351640_20231119192911.csv                                
✖ No results for npi = 1063703494
No results for NPI: 1063703494 
```

### Searching for Data: `tyler::retrieve_clinician_data`
Physician Compare has sunset as of December 1, 2020 and has been replaced by: https://www.medicare.gov/care-compare/?redirect=true&providerType=Physician.  The entire data set is at https://data.cms.gov/provider-data/dataset/mj5m-pzi6.  The very very cool library called `provider` was super helpful with accessing this.  
```r
# Call the retrieve_clinician_data function with an NPI value
input_csv_path <- ("~/Dropbox (Personal)/workforce/subspecialists_only.csv")
clinician_data <- tyler::retrieve_clinician_data(input_csv_path)

✖ No results for npi = 1093151441
NULL
# A tibble: 3 × 17
  npi     pac   enid  first last  gender school grad_year specialty facility_name pac_org members_org address_org city_org
  <chr>   <chr> <chr> <chr> <chr> <fct>  <chr>      <int> <chr>     <chr>         <chr>         <int> <chr>       <chr>   
1 119406… 3476… I202… JACL… DENE… Female NEW Y…      2013 OBSTETRI… SPECTRUM HEA… 458756…        1551 25 MICHIGA… GRAND R…
2 119406… 3476… I202… JACL… DENE… Female NEW Y…      2013 OBSTETRI… SPECTRUM HEA… 458756…        1551 4444 KALAM… KENTWOOD
3 119406… 3476… I202… JACL… DENE… Female NEW Y…      2013 OBSTETRI… SPECTRUM HEA… 458756…        1551 4069 LAKE … GRAND R…
# ℹ 3 more variables: state_org <ord>, zip_org <chr>, phone_org <chr>
```

### Searching for Data: `tyler::genderize_physicians`
This is a wrapper around the `gender` package to help fill in the gender of physician names.  It requires a csv with a column called `first_name`.  A lot of gender data was found via Physician Compare in the past.  
```r
tyler::genderize_physicians <- function(input_csv) 
```

### Searching for Data: `tyler::geocode_unique_addresses`
Takes a csv file of addresses and prints out the lat and long as separate columns.  You will need a google_maps_api_key.  Geocoding is the process of converting human-readable addresses or place names into geographic coordinates (latitude and longitude) that can be used to locate places on a map. The Google Geocoding API is a service provided by Google that allows developers to perform geocoding and reverse geocoding, which is the process of converting coordinates back into human-readable addresses. 
```r
output_data <- 
    tyler::geocode_unique_addresses(file_path = "/Users/tylermuffly/Dropbox (Personal)/Tannous/data/address_for_geocoding.csv", 
    google_maps_api_key = "????", 
    output_file_path = "/Users/tylermuffly/Dropbox (Personal)/Tannous/data/geocoded_unique_addresses.csv")
```

# GET ISOCHRONES
The methods for calculating patient travel distance to hospitals varies widely. See this paper and reference it in the paper: [isprs-archives-XLVIII-4-W7-2023-53-2023.pdf](https://github.com/mufflyt/tyler/files/13433577/isprs-archives-XLVIII-4-W7-2023-53-2023.pdf)
Most of the difference in travel distance between methods is associated with the distance measure (driving vs. straight-line) with little difference due to geocoding methods per AHRQ.  [MS2021-02-HospDist-DataSuppl.xlsx](https://github.com/mufflyt/tyler/files/13433393/MS2021-02-HospDist-DataSuppl.xlsx)
* AHRQ uses the patient location as the geographic centroid of the patient's zip code.  https://hcup-us.ahrq.gov/reports/methods/MS2021-02-Distance-to-Hospital.jsp
* AHRQ: To measure the distance between the patient location and the point of care (hospital or emergency department), the shortest or "straight-line" distance (i.e., the geodetic or great circle distance) is commonly used because it can be readily calculated (e.g., through statistical software programs such as SAS®).
* AHRQ: An alternative distance metric that has been used is the driving distance or driving times that can be obtained from various mapping software such as Google Maps, 16,17,18 MapQuest,19 OpenStreetMaps,20 and ArcGIS Network Analyst.21
* AHRQ: Uses `ggmap` to geocode the addresses of hospitals.
* AHRQ: There is an equation to change straight-line distance to drive time:
```r
`Di=αBi+Ciβ+ LiΥ + εi`
  Where:

- i indexes patients
- Di : driving distance
- Bi : baseline distance
- Ci : vector of census division dummy variables
- Li : vector of urban/rural location dummy variables
- α : coefficient for baseline distance
- β : vector of coefficients for census division dummy variables
- Υ : vector of coefficients for urban/rural location dummy variables
- εi : mean-zero error term
```
* March of Dimes Maternity Care Deserts uses drive time.  [MaternityCareDesertsReport-TechnicalNotes.pdf](https://github.com/mufflyt/tyler/files/13433413/MaternityCareDesertsReport-TechnicalNotes.pdf)

* ESRI methodology:  https://doc.arcgis.com/en/arcgis-online/analyze/create-drive-time-areas.htm  Limitations: "You must be granted the network analysis privilege to use Create Drive-Time Areas.", "Travel times cannot exceed 9 hours (540 minutes) when walking or 5 hours (300 minutes) for all other travel times.", "Travel distances cannot exceed 27 miles (43.45 kilometers) when walking or 300 miles (482.8 kilometers) for all other travel distances."
* Veteran's Administration: They use drive time.  https://www.federalregister.gov/documents/2020/07/15/2020-14341/update-to-access-standards-drive-time-calculations
* ![Screenshot 2023-11-21 at 9 17 29 PM](https://github.com/mufflyt/tyler/assets/44621942/316ab22f-2348-4a07-97c0-0c835356b168)
  
* Department of Transportation: https://www.transportation.gov/priorities/equity/justice40/etc-explorer

* Potential reference:  Lidsky ME, Sun Z, Nussbaum DP, Adam MA, Speicher PJ, Blazer DG. Going the extra mile: improved survival for pancreatic cancer patients traveling to high-volume centers. Annals of Surgery. 2017;266(2):333–8.
* Bliss RL, Katz JN, Wright EA, Losina E. Estimating proximity to care: are straight line and zipcode centroid distances acceptable measures? Medical Care. 2012;50(1):99–106.


### `tyler::create_isochrones`
A function that interfaces with HERE API to gather the geometry for the isochrones.  Does not need to be used on its own.  Used INTERNALLY only.  We use the HERE API to calculate optimal routes and directions for various modes of transportation, including driving, walking, cycling, and public transit. It provides detailed turn-by-turn instructions, estimated travel times, and route alternatives.  This is simpler than using an OSRM server running the AWS cloud, and the cost is minimal.  

### `tyler::create_isochrones_for_dataframe`
A function that iterates the `tyler::create_isochrones` over an entire dataframe.  The only input is a dataframe and the breaks for the number of minutes for each drive-time isochrone.  Drive time isochrones take into account road networks, traffic conditions, and other factors that influence actual travel time. Geodesic distances, on the other hand, represent straight-line distances "as the crow flies" and do not consider road networks. For real-world navigation or route planning, drive time isochrones provide more accurate estimates of travel time.  While drive time isochrones have these advantages, geodesic distances are still valuable in scenarios where the focus is solely on measuring straight-line distances or when road network information is not available or necessary. 
```r
isochrones_data <- tyler::create_isochrones_for_dataframe(gyn_onc, breaks = c(0, 30, 60, 120, 180))
```
### `tyler::create_individual_isochrone_plots.R`
Function to create individual plots and shapefiles for specified drive times.  It generates individual plots for each drive time, providing a visual representation of the accessible areas on a map. The function shapefiles, which are geospatial data files used for storing geographic information, including the boundaries of the reachable areas.
```r
# Usage example:
# List of unique drive times for which you want to create plots and shapefiles
drive_times <- unique(isochrones$drive_time)
tyler::create_individual_isochrone_plots(isochrones, drive_times)
```
#### 30-minute isochrones
<img src="https://github.com/mufflyt/tyler/assets/44621942/2daffc4f-e5d7-4f35-9b0e-054b979cdd7f" width="25%">

#### 60-minute isochrones
<img src="https://github.com/mufflyt/tyler/assets/44621942/3643c555-628b-409c-bbfd-718f7b5c9663" width="25%">

#### 120-minute isochrones
<img src="https://github.com/mufflyt/tyler/assets/44621942/8ad18c72-5467-419b-92c1-4b863192a711" width="25%">

#### 180-minute isochrones
<img src="https://github.com/mufflyt/tyler/assets/44621942/49000172-e535-41c9-bdff-d1b262334195" width="25%">

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
![Screenshot 2023-11-25 at 12 47 18 PM](https://github.com/mufflyt/tyler/assets/44621942/e20b6b4e-89a5-4cd0-a0b4-df7bea4a8c40)
Source: (https://www2.census.gov/about/training-workshops/2020/2020-09-09-ced-presentation.pdf)

This function retrieves Census data using `censusapi` for all states' block groups by looping over the specified list of state FIPS codes.  This only brings back data on females from "B01001_01, 26, 33:49E".  FIPS codes, or Federal Information Processing Standards codes, are a standardized set of codes used to uniquely identify geographic areas in the United States. These codes are assigned to various administrative and geographical entities, such as states, counties, cities, and more. We used block groups for the analysis.  In the United States Census Bureau's geographic hierarchy, a "block group" is a smaller and more detailed geographic unit used for collecting and reporting demographic and statistical data. Block groups are subdivisions of census tracts and are typically designed to contain between 600 and 3,000 people, although this can vary depending on the population density of the area. Block groups are used as the primary units for collecting detailed demographic and socioeconomic data during the decennial census and the American Community Survey (ACS). Census enumerators visit households within each block group to collect information on population, housing, employment, income, education, and more. In a densely populated urban area, a block group might represent a city block or a small neighborhood within a larger city. For example, a block group could cover a few city blocks in downtown Manhattan, New York City.

```r
all_census_data <- tyler::get_census_data(us_fips_list, "your_censusapi_key_here", vintage=2019)
```

### `tyler::create_block_group_overlap_map`
Function Parameters:
* bg_data: A SpatialPolygonsDataFrame representing block group data.
* isochrones_data: A SpatialPolygonsDataFrame representing isochrone data.
* output_html: File path for exporting the map as an HTML file.
* output_png: File path for exporting the map as a PNG image.
```r
# Define output file paths for HTML and PNG
output_html <- "figures/overlap_bg_map.html"
output_png <- "figures/overlap_bg_map.png"

# Create and export the map
tyler::create_block_group_overlap_map(block_groups, isochrones_joined_map, output_html, output_png)

# Call the create_block_group_overlap_map function with your data
tyler::create_block_group_overlap_map(
  bg_data = your_block_group_data,
  isochrones_data = your_isochrones_data,
  output_html = "your_output_html_file.html",
  output_png = "your_output_png_file.png"
)
```
<img src="https://github.com/mufflyt/tyler/assets/44621942/713a0397-30fc-4a5a-972b-5feb691a1922" width="75%">

# `tyler:hrr`
This function loads the hospital referral region shapefile and optionally removes Hawaii and Alaska.  Then it creates a ggplot map of all HRR regions: 

```r
tyler::hrr_generate_maps <- function(physician_sf, trait_map = "all", honey_map = "all", breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200), map_title = "OBGYN Residency\n Faculty Subspecialists") 
```
# MAKING MAPS
### `tyler::create_basemap`
This is a nice leaflet map with all the features you want for an interactive html map.  We can use it for dot maps.  Leaflet provides a user-friendly and intuitive interface for interacting with maps. It supports features like zooming, panning, and click events, making it easy for users to explore and interact with geographic data.
```r
# Create a base map with a custom title
my_map <- tyler::create_base_map("TITLE")

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

### `create_and_save_physician_dot_map.R`
Leaflet dot map of physicians on colored ACOG Districts.  We introduce some jitter for people who work at the same location.  Dot maps allow viewers to identify patterns and trends in the data distribution. 
```r
tyler::create_and_save_physician_dot_map(physician_data = gyn_onc, jitter_range = 0.05, color_palette = "magma", popup_var = "name")
```
<img src="https://github.com/mufflyt/tyler/assets/44621942/43227656-cd1c-4242-a2db-cdf1ebce20e8" width="50%">

<img src="https://github.com/mufflyt/tyler/assets/44621942/2511d71c-f5c3-48be-ac5f-f439a67bf89a" width="50%">

### `tyler::honeycomb_generate_maps_by_acog_districts.R`
Loops through each ACOG district to generate hex maps individually.  Hexagon grids provide a uniform and regular tessellation of geographic space. Unlike traditional square grids or irregular polygons, hexagons have consistent shapes and sizes. This uniformity makes it easier to analyze and interpret the distribution of data.
```r
#Use case:
tyler::generate_acog_districts_sf("inst/extdata/ACOG_Districts.csv")
tyler::generate_acog_districts_sf()

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
<img src="https://github.com/mufflyt/tyler/assets/44621942/58553c2b-f7c7-4f86-be35-c650e54dd2c3" width="50%">



# Installing Selenium with R
I am running Firefox version 117.0.  This is compatible with geckodriver 0.33.0 (https://github.com/mozilla/geckodriver/releases).  

```r
# Specify the geckodriver version
geckodriver_version <- "0.33.0"


# Start the RSelenium driver
rD <- RSelenium::rsDriver(
  browser = 'firefox',
  geckover = geckodriver_version,
  port = 4456L,
  verbose = TRUE)
```

# Read in the National Downloadable File (DAC)
I utilized Postico and a postgres SQL database because the files are huge and I don't have enough RAM to hold the data in memory.  2022_National_Downloadable_File.csv and 2021_National_Downloadable_File.csv both had errors with importing the data.  So I had to read in the data to exploratory and clean the text using Work with Text Data >> Clean up Tex. 
 We got most of the data from CMS but the 2015 Physician Compare Downloadable File is available at https://data.nber.org/compare/physician/2015/1.    
```
library(dbplyr)
library(dplyr)
library(DBI)  # This is required for connecting to the database
library(RPostgres)  # If using PostgreSQL
library(tidyr)
library(stringr)
library(tidyverse)

# Define your database connection details
db_host <- "localhost"
db_port <- 5433  # Default PostgreSQL port
db_name <- "template1"
db_user <- "postgres"
db_password <- "fatbastard"

# Create a database connection
db_connection <- dbConnect(
  RPostgres::Postgres(),
  dbname = db_name,
  host = db_host,
  port = db_port,
  user = db_user,
  password = db_password
)

####################
read_and_clean_tables <- function(db_connection, years) {
  # Initialize an empty list to store the data frames
  cleaned_tables <- list()
  
  # Loop through the specified years
  for (year in years) {
    # Extract the year from the table name
    year_num <- as.numeric(str_extract(year, "\\d+"))
    
    # Create the table name
    table_name <- paste0(year)
    
    # Read the table from the database
    table_data <- dplyr::tbl(db_connection, table_name)
    
    # Perform cleaning operations
    cleaned_data <- table_data %>%
      filter(`Primary Specialty` %in% c("OBSTETRICS/GYNECOLOGY", "GYNECOLOGICAL ONCOLOGY")) %>%
      mutate(`Zip Code` = str_sub(`Zip Code`, 1, 5)) %>%
      distinct(NPI, .keep_all = TRUE) %>%
      mutate(Year = as.character(year_num))
    
    # Ensure NPI is of integer type
    cleaned_data$NPI <- as.integer(dplyr::pull(cleaned_data, NPI))
    
    # Store the cleaned data frame in the list
    cleaned_tables[[year]] <- cleaned_data
  }
  
  # Return a named list of cleaned data frames
  names(cleaned_tables) <- years
  return(cleaned_tables)
}

# Usage example: Read and clean tables for multiple years
years_to_process <- c("2016", "2017", "2018", "2019", "2020") #, "2021", "2022")
cleaned_tables <- read_and_clean_tables(db_connection, years_to_process)

#########################################
full_join_cleaned_tables <- function(cleaned_tables) {
  # Initialize the result data frame with the first cleaned table
  result_data <- cleaned_tables[[1]]
  
  # Loop through the remaining cleaned tables and perform full joins
  for (i in 2:length(cleaned_tables)) {
    year <- names(cleaned_tables)[i]
    
    # Perform a full join with a suffix of the year
    result_data <- full_join(result_data, cleaned_tables[[i]], by = "NPI", suffix = c("", paste0(".", year)))
  }
  
  # Return the result data frame
  return(result_data)
}

# Usage example: Read and clean tables for multiple years
years_to_process <- c("2016", "2017", "2018", "2019", "2020", "2021", "2022")
cleaned_tables <- read_and_clean_tables(db_connection, years_to_process)

# Perform a full join with suffixes
result_full_join <- full_join_cleaned_tables(cleaned_tables)

# Print the resulting data frame
print(result_full_join)

collect_result_full_join <- collect(result_full_join)

readr::write_rds(collect_result_full_join, "Desktop/collect_result_full_join.rds")

dbDisconnect(db_connection)
```


# Citation
If you use this package, I would appreciate a citation.
```
citation("tyler")
```

# Code of conduct
Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.
