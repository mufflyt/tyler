v1.3.0  ·  R package

End-to-end toolkit for mystery caller and audit studies evaluating
patient access to healthcare — from NPI roster building to drive-time
isochrones, Census demographics, and publication-ready maps and tables.

[Function
reference](https://mufflyt.github.io/mysterycall/reference/index.md)
[Vignettes](https://mufflyt.github.io/mysterycall/articles/index.md)
[GitHub](https://github.com/mufflyt/mysterycall)

\# Install from GitHub  
pak::pkg_install("mufflyt/mysterycall")  
  
\# Optional geospatial/modelling packages (loaded on demand)  
install.packages(c("hereR", "sf", "leaflet", "censusapi", "lme4"))

Four-stage workflow

1

#### Build roster

Search the NPI registry by taxonomy across all 50 states, bypass the
1,200-record API cap, and enrich with CMS demographics.

2

#### Geocode

Convert provider addresses to lat/lon via the Google Maps API,
deduplicating so each unique address is only looked up once.

3

#### Drive-time isochrones

Generate drive-time polygons (30 / 60 / 120 / 180 min) using the
drive-time routing service, with built-in memoization for large batches.

4

#### Analyse & report

Overlay Census block-group demographics, compute overlap areas, and
produce Leaflet maps and `arsenal` summary tables.

Key functions

🔍

#### Provider search

`mysterycall_search_taxonomy()`  
`mysterycall_search_and_process_npi()`  
`mysterycall_validate_npi()`  
`mysterycall_get_clinician_data()`

📍

#### Geocoding & isochrones

`mysterycall_geocode()`  
`mysterycall_isochrones_for_df()`  
`mysterycall_create_isochrones()`  
[`mysterycall_clear_isochrone_cache()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_clear_isochrone_cache.md)

🗺️

#### Mapping

`mysterycall_map_physicians()`  
`mysterycall_map_block_group()`  
`mysterycall_map_acog_districts()`  
`mysterycall_hrr_maps()`

📊

#### Census & tables

`mysterycall_get_census_data()`  
`mysterycall_calculate_overlap()`  
`mysterycall_table_overall()`  
`mysterycall_table_percentages()`

Built-in datasets

| Dataset | Description | Rows |
|----|----|----|
| `taxonomy` | NUCC taxonomy codes (v23.1) for OBGYN subspecialties | ~900 |
| `ACOG_Districts` | State → ACOG district + Census subregion crosswalk | 51 |
| `acgme` | All 318 ACGME-accredited OBGYN residency programs | 318 |
| `physicians` | Sample roster of OBGYN subspecialists with coordinates | 4,659 |
| `fips` | State FIPS codes and abbreviations | 51 |
| `acog_presidents` | Historical ACOG presidents | — |
| `census_summaries` | Pre-computed Census block-group demographics | — |

Learn more

| Vignette | Topic |
|----|----|
| [Create Isochrones](https://mufflyt.github.io/mysterycall/articles/create_isochrones.md) | Drive-time polygons from geocoded addresses |
| [Geocoding](https://mufflyt.github.io/mysterycall/articles/geocode.md) | Address → lat/lon with Google Maps |
| [Get Census Data](https://mufflyt.github.io/mysterycall/articles/get_census_data.md) | ACS block-group demographics |
| [Search & Process NPI](https://mufflyt.github.io/mysterycall/articles/search_and_process_npi.md) | Name-based provider lookup |
| [Aggregating Provider Data](https://mufflyt.github.io/mysterycall/articles/aggregating_provider_data.md) | Combining taxonomy + NPI data |
| [News & Changelog](https://mufflyt.github.io/mysterycall/articles/imotive-news.md) | Release notes |

Citation

``` r

citation("mysterycall")
```

> Muffly, T. (2026). *mysterycall: Mystery Caller Study Tools for
> Healthcare Access Research* (R package version 1.3.0).
> <https://github.com/mufflyt/mysterycall>
