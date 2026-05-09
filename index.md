---
pagetitle: "mysterycall — Mystery Caller Study Tools"
---

```{=html}
<!-- Hero -->
<div class="mc-hero">
  <h1>mysterycall</h1>
  <div class="mc-version">v1.3.0 &nbsp;·&nbsp; R package</div>
  <p class="lead">
    End-to-end toolkit for mystery caller and audit studies evaluating
    patient access to healthcare — from NPI roster building to drive-time
    isochrones, Census demographics, and publication-ready maps and tables.
  </p>
  <a class="mc-btn mc-btn-primary"  href="reference/index.html">Function reference</a>
  <a class="mc-btn mc-btn-secondary" href="articles/index.html">Vignettes</a>
  <a class="mc-btn mc-btn-secondary" href="https://github.com/mufflyt/mysterycall">GitHub</a>
</div>
```

```{=html}
<div class="mc-install">
<span class="mc-comment"># Install from GitHub</span><br>
pak::pkg_install("mufflyt/mysterycall")<br><br>
<span class="mc-comment"># Optional geospatial/modelling packages (loaded on demand)</span><br>
install.packages(c("hereR", "sf", "leaflet", "censusapi", "lme4"))
</div>
```

```{=html}
<p class="mc-section-title">Four-stage workflow</p>

<div class="mc-steps">

  <div class="mc-step">
    <div class="mc-step-num">1</div>
    <h4>Build roster</h4>
    <p>Search the NPI registry by taxonomy across all 50 states, bypass the
       1,200-record API cap, and enrich with CMS demographics.</p>
  </div>

  <div class="mc-step">
    <div class="mc-step-num">2</div>
    <h4>Geocode</h4>
    <p>Convert provider addresses to lat/lon via the Google Maps API,
       deduplicating so each unique address is only looked up once.</p>
  </div>

  <div class="mc-step">
    <div class="mc-step-num">3</div>
    <h4>Drive-time isochrones</h4>
    <p>Generate drive-time polygons (30 / 60 / 120 / 180 min) using the
       HERE API, with built-in memoization for large batches.</p>
  </div>

  <div class="mc-step">
    <div class="mc-step-num">4</div>
    <h4>Analyse &amp; report</h4>
    <p>Overlay Census block-group demographics, compute overlap areas,
       and produce Leaflet maps and <code>arsenal</code> summary tables.</p>
  </div>

</div>
```

```{=html}
<p class="mc-section-title">Key functions</p>

<div class="mc-cards">

  <div class="mc-card">
    <div class="mc-card-icon">🔍</div>
    <h4>Provider search</h4>
    <p><code>mysterycall_search_taxonomy()</code><br>
       <code>mysterycall_search_and_process_npi()</code><br>
       <code>mysterycall_validate_npi()</code><br>
       <code>mysterycall_get_clinician_data()</code></p>
  </div>

  <div class="mc-card">
    <div class="mc-card-icon">📍</div>
    <h4>Geocoding &amp; isochrones</h4>
    <p><code>mysterycall_geocode()</code><br>
       <code>mysterycall_isochrones_for_df()</code><br>
       <code>mysterycall_create_isochrones()</code><br>
       <code>mysterycall_clear_isochrone_cache()</code></p>
  </div>

  <div class="mc-card">
    <div class="mc-card-icon">🗺️</div>
    <h4>Mapping</h4>
    <p><code>mysterycall_map_physicians()</code><br>
       <code>mysterycall_map_block_group()</code><br>
       <code>mysterycall_map_acog_districts()</code><br>
       <code>mysterycall_hrr_maps()</code></p>
  </div>

  <div class="mc-card">
    <div class="mc-card-icon">📊</div>
    <h4>Census &amp; tables</h4>
    <p><code>mysterycall_get_census_data()</code><br>
       <code>mysterycall_calculate_overlap()</code><br>
       <code>mysterycall_table_overall()</code><br>
       <code>mysterycall_table_percentages()</code></p>
  </div>

</div>
```

```{=html}
<p class="mc-section-title">Built-in datasets</p>

<table class="table mc-datasets">
<thead><tr>
  <th>Dataset</th><th>Description</th><th>Rows</th>
</tr></thead>
<tbody>
<tr><td><code>taxonomy</code></td>
    <td>NUCC taxonomy codes (v23.1) for OBGYN subspecialties</td><td>~900</td></tr>
<tr><td><code>ACOG_Districts</code></td>
    <td>State → ACOG district + Census subregion crosswalk</td><td>51</td></tr>
<tr><td><code>acgme</code></td>
    <td>All 318 ACGME-accredited OBGYN residency programs</td><td>318</td></tr>
<tr><td><code>physicians</code></td>
    <td>Sample roster of OBGYN subspecialists with coordinates</td><td>4,659</td></tr>
<tr><td><code>fips</code></td>
    <td>State FIPS codes and abbreviations</td><td>51</td></tr>
<tr><td><code>acog_presidents</code></td>
    <td>Historical ACOG presidents</td><td>—</td></tr>
<tr><td><code>census_summaries</code></td>
    <td>Pre-computed Census block-group demographics</td><td>—</td></tr>
</tbody>
</table>
```

```{=html}
<p class="mc-section-title">Learn more</p>
```

| Vignette | Topic |
|---|---|
| [Create Isochrones](articles/create_isochrones.html) | Drive-time polygons with the HERE API |
| [Geocoding](articles/geocode.html) | Address → lat/lon with Google Maps |
| [Get Census Data](articles/get_census_data.html) | ACS block-group demographics |
| [Search & Process NPI](articles/search_and_process_npi.html) | Name-based provider lookup |
| [Aggregating Provider Data](articles/aggregating_provider_data.html) | Combining taxonomy + NPI data |
| [News & Changelog](articles/imotive-news.html) | Release notes |

```{=html}
<p class="mc-section-title">Citation</p>
```

```r
citation("mysterycall")
```

> Muffly, T. (2026). *mysterycall: Mystery Caller Study Tools for Healthcare
> Access Research* (R package version 1.3.0).
> <https://github.com/mufflyt/mysterycall>
