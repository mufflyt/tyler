
# tyler

<!-- badges: start -->
<img src="https://github.com/mufflyt/tyler/assets/44621942/3c4faeb4-7fe5-42e8-b2bf-7832588c6f57" width="15%" align="right">

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/mufflyt/tyler/branch/master/graph/badge.svg)](https://app.codecov.io/gh/mufflyt/tyler?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/tyler)](https://CRAN.R-project.org/package=tyler)
[![R-CMD-check](https://github.com/mufflyt/tyler/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mufflyt/tyler/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**tyler** provides a toolkit for mystery caller and audit studies that evaluate
patient access to healthcare. It handles the full workflow: finding providers
in the NPI registry, validating and geocoding their addresses, generating
drive-time isochrones, overlaying Census demographics, and producing publication-ready
tables and maps.

## Installation

```r
# install.packages("devtools")
devtools::install_github("mufflyt/tyler")
```

The package loads quickly with a small footprint. Geospatial and modelling
packages are optional and installed only when you need them:

```r
install.packages(c("hereR", "sf", "lwgeom"))   # drive-time isochrones
install.packages("leaflet")                      # interactive maps
install.packages("ggmap")                        # Google Maps geocoding
install.packages(c("ggspatial", "rnaturalearth")) # HRR hex maps
install.packages("lme4")                         # mixed-effects models
install.packages("censusapi")                    # Census block-group data
```

## Quick start

A typical mystery caller study moves through four stages. Here is a minimal
end-to-end example for gynecologic oncology:

```r
library(tyler)
library(dplyr)

# ── 1. Build a provider roster ────────────────────────────────────────────────

# Search by taxonomy across all 50 states (bypasses the 1,200-record API cap)
all_states <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN",
  "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV",
  "NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
  "TX","UT","VT","VA","WA","WV","WI","WY"
)

gyn_onc <- search_by_taxonomy("Gynecologic Oncology", states = all_states)

# Validate NPI numbers before downstream lookups
gyn_onc_valid <- validate_and_remove_invalid_npi(gyn_onc)

# Enrich with CMS Physician Compare demographics
gyn_onc_enriched <- get_clinician_data(gyn_onc_valid)

# ── 2. Geocode ────────────────────────────────────────────────────────────────

# Requires a Google Maps API key in GOOGLE_API_KEY env var
geocoded <- geocode_unique_addresses(
  gyn_onc_enriched,
  google_maps_api_key = Sys.getenv("GOOGLE_API_KEY")
)

# ── 3. Drive-time isochrones ──────────────────────────────────────────────────

# Requires a HERE API key in HERE_API_KEY env var
isochrones <- create_isochrones_for_dataframe(
  geocoded,
  breaks = c(1800, 3600, 7200, 10800)   # 30 / 60 / 120 / 180 min
)

# Free the in-memory isochrone cache after a large batch
tyler_clear_isochrone_cache()

# ── 4. Map ────────────────────────────────────────────────────────────────────

map_create_physician_dot(geocoded, popup_var = "name")
```

## Core functions

| Stage | Function | Description |
|---|---|---|
| **Find providers** | `search_by_taxonomy()` | NPI registry search by taxonomy; loops over states to bypass the 1,200-record cap |
| | `search_and_process_npi()` | NPI registry search by first/last name |
| | `validate_and_remove_invalid_npi()` | Remove invalid NPI numbers before enrichment |
| | `get_clinician_data()` | Pull demographics from CMS Physician Compare |
| | `genderize_physicians()` | Estimate physician gender via the Genderize.io API |
| **Geocode** | `geocode_unique_addresses()` | Convert addresses to lat/lon via Google Maps |
| **Isochrones** | `create_isochrones_for_dataframe()` | Drive-time polygons for every row using the HERE API |
| | `tyler_clear_isochrone_cache()` | Release the in-session memoization cache |
| **Census** | `get_census_data()` | ACS block-group demographics by state FIPS |
| | `calculate_intersection_overlap_and_save()` | Overlap area between isochrones and block groups |
| **Maps** | `map_create_physician_dot()` | Interactive Leaflet dot map coloured by ACOG district |
| | `map_create_block_group_overlap()` | Block-group overlap map exported to HTML + PNG |
| | `hrr_generate_maps()` | Hexagon density map by Hospital Referral Region |
| **Tables** | `table_generate_overall()` | Table 1 summary (via `arsenal`) |
| | `table_calculate_percentages()` | Column-percentage tables |

## Built-in datasets

| Dataset | Description |
|---|---|
| `tyler::taxonomy` | NUCC taxonomy codes (v23.1) for all OBGYN subspecialties |
| `tyler::ACOG_Districts` | State → ACOG district + Census subregion crosswalk |
| `tyler::acgme` | All 318 ACGME-accredited OBGYN residency programs |
| `tyler::physicians` | Sample roster of 4,659 OBGYN subspecialists with coordinates |
| `tyler::fips` | State FIPS codes and abbreviations |
| `tyler::cityStateToLatLong` | City/state → lat/lon lookup table |

```r
# Example: find all OBGYN taxonomy codes
library(tyler)
library(dplyr)
library(stringr)

tyler::taxonomy |>
  filter(str_detect(Classification, fixed("GYN", ignore_case = TRUE))) |>
  select(Code, Specialization)
#> # A tibble: 11 × 2
#>    Code       Specialization
#>    <chr>      <chr>
#>  1 207V00000X Obstetrics & Gynecology
#>  2 207VF0040X Female Pelvic Medicine and Reconstructive Surgery
#>  3 207VX0201X Gynecologic Oncology
#>  4 207VM0101X Maternal & Fetal Medicine
#>  5 207VE0102X Reproductive Endocrinology
#>  ...
```

## Learn more

Full documentation, function reference, and worked vignettes live at
**<https://mufflyt.github.io/tyler/>**:

- [Create Isochrones](https://mufflyt.github.io/tyler/articles/create_isochrones.html)
- [Get Census Data](https://mufflyt.github.io/tyler/articles/get_census_data.html)
- [Geocoding](https://mufflyt.github.io/tyler/articles/geocode.html)
- [Search & Process NPI](https://mufflyt.github.io/tyler/articles/search_and_process_npi.html)
- [Validate & Remove Invalid NPI](https://mufflyt.github.io/tyler/articles/validate_and_remove_invalid_npi.html)
- [Aggregating Provider Data](https://mufflyt.github.io/tyler/articles/aggregating_provider_data.html)

## Citing tyler

```r
citation("tyler")
```

> Muffly, T. (2026). *tyler: Common Functions for Mystery Caller or Audit
> Studies Evaluating Patient Access to Care* (R package version 1.2.2).
> <https://github.com/mufflyt/tyler>

## Code of conduct

Please note that this project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating you agree
to abide by its terms.
