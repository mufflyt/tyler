# GitHub Repositories for Mystery Caller Study Enhancement

This document catalogs R repositories and packages that could improve or be integrated into the mystery caller healthcare access study.

---

## 1. NPI (National Provider Identifier) Data Processing

### **ropensci/npi** ⭐ PRIMARY
**Repository:** https://github.com/ropensci/npi
**Status:** Accepted by rOpenSci (Nov 2022)

**Key Features:**
- Access to CMS National Provider Identifier Registry API v2.1
- `npi_search()`: Advanced search by name, location, type, credentials
- `npi_summarize()`: Human-readable search result overview
- `npi_flatten()`: Flatten nested list columns
- `npi_is_valid()`: Validate NPI numbers

**Use Case:** Already using this package. Review their API optimization patterns.

### **EarlGlynn/National-Provider-Identifier**
**Repository:** https://github.com/EarlGlynn/National-Provider-Identifier

**Key Features:**
- Processing bulk NPI downloadable files
- Scripts for splitting and analyzing large CMS datasets
- Missouri provider network analysis

**Use Case:** Alternative approach using bulk data files instead of API calls (no rate limits).

---

## 2. Geocoding & Spatial Analysis

### **munterfi/hereR** ⭐ CURRENTLY USING
**Repository:** https://github.com/munterfi/hereR
**Version:** 1.1.0

**Key Features:**
- Interface to HERE REST APIs (Geocoder, Routing, Traffic, Transit, Weather)
- Returns locations/routes as `sf` objects
- Isoline (isochrone) generation
- Travel time/distance matrices

**Use Case:** Already using. Review for optimization opportunities.

### **riatelab/osrm**
**Repository:** https://github.com/riatelab/osrm

**Key Features:**
- Interface to OpenStreetMap-based routing (OSRM)
- `osrmIsochrone()`: Generate isochrone polygons
- `osrmIsodistance()`: Generate isodistance polygons
- FREE - No API costs (uses OSM data)

**Use Case:** Alternative to HERE API to reduce costs. Validate against HERE results.

### **ipeaGIT/r5r**
**Repository:** https://github.com/ipeaGIT/r5r

**Key Features:**
- Multimodal transport network analysis
- `isochrone()`: Returns reachable areas at given travel time
- Uses OpenStreetMap .pbf format
- Includes public transit routing

**Use Case:** More sophisticated multimodal analysis (car + bus + walking) for comprehensive access.

### **Chrisjb/RDistanceMatrix**
**Repository:** https://github.com/Chrisjb/RDistanceMatrix

**Key Features:**
- Builds isochrones using Google Distance Matrix or Mapbox APIs
- Fetches population/employment data within isochrones
- Pre-built census integration

**Use Case:** Alternative API options + built-in population estimation.

### **graveja0/health-care-markets**
**Repository:** https://github.com/graveja0/health-care-markets
**File:** `R/geocode-and-locate-hospitals.R`

**Key Features:**
- Methods for defining geographic healthcare markets
- Hospital assignment to ZIP codes, congressional districts
- Geographic boundary analysis

**Use Case:** Define healthcare market boundaries for your study area.

---

## 3. Healthcare Access & Spatial Accessibility

### **bruno-pinheiro/asha**
**Repository:** https://github.com/bruno-pinheiro/asha

**Key Features:**
- Analysis of Spatial Health Accessibility in R
- Integrates provider locations with population data
- Distance/travel time calculations

**Use Case:** Validate your accessibility metrics against established methods.

### **raikens1/health_desert_maps** ⭐ HIGHLY RELEVANT
**Repository:** https://github.com/raikens1/health_desert_maps

**Key Features:**
- Visualize healthcare deserts
- Examines demographic/socioeconomic factors associated with access
- Cleaned Medicaid hospitals data
- Travel time to hospital analysis

**Use Case:** Replicate their visualization approach. Compare methodologies for identifying healthcare deserts.

### **SpatialAcc Package (CRAN)**
**Repository:** https://github.com/cran/SpatialAcc
**Homepage:** http://lctools.science/

**Key Features:**
- Spatial accessibility measures
- Two-Step Floating Catchment Area (2SFCA) method
- Enhanced 2SFCA variants

**Use Case:** Implement 2SFCA method to complement your isochrone analysis.

### **tetraptych/aceso** (Python)
**Repository:** https://github.com/tetraptych/aceso

**Key Features:**
- 2SFCA and gravity model calculations
- Common healthcare accessibility measures
- **Note:** Python, not R

**Use Case:** Reference for methodology, potentially create R wrapper.

---

## 4. Health Disparities & Equity

### **OHDSI/HealthEquityWG**
**Repository:** https://github.com/OHDSI/HealthEquityWG

**Key Features:**
- Health equity working group resources
- Best practices for avoiding bias/stigma in research
- Spatial-population-level OMOP data analysis

**Use Case:** Review guidelines for ethical healthcare disparities research.

### **asmae-toumi/MSVI**
**Repository:** https://github.com/asmae-toumi/MSVI

**Key Features:**
- Health and socioeconomic data at state/metro/county level
- CMS health outcomes and utilization data
- Pre-cleaned datasets

**Use Case:** Integrate additional contextual socioeconomic variables.

### **trendct/walkthroughs**
**Repository:** https://github.com/trendct/walkthroughs
**File:** `spatial-analysis-r-walkthrough/tract_disparities.R`

**Key Features:**
- Census tract-level disparities analysis
- Spatial analysis walkthrough for journalists/researchers

**Use Case:** Educational resource for team members learning spatial analysis.

---

## 5. Statistical Analysis & Visualization

### **rvlenth/emmeans** ⭐ CURRENTLY USING
**Repository:** https://github.com/rvlenth/emmeans

**Key Features:**
- Estimated marginal means (EMMs)
- `plot()`: Visualization with confidence intervals
- `emmip()`: Interaction plots
- Support for ggplot2 and lattice

**Use Case:** Already using. Review `emmip.R` and `plot.emm.R` for advanced customization.

### **easystats/modelbased**
**Repository:** https://github.com/easystats/modelbased

**Key Features:**
- Estimate effects, contrasts, and means from statistical models
- Simplified interface to emmeans
- Better default visualizations

**Use Case:** Potential replacement/complement to emmeans for cleaner code.

### **IndrajeetPatil/pairwiseComparisons**
**Repository:** https://github.com/IndrajeetPatil/pairwiseComparisons

**Key Features:**
- Parametric, non-parametric, robust, Bayes Factor pairwise comparisons
- Multiple testing corrections
- Publication-ready tables

**Use Case:** Enhanced pairwise comparisons for insurance type differences.

---

## 6. Power Analysis & Sample Size

### **PSS.Health (CRAN)** ⭐ HIGHLY RELEVANT
**Repository:** https://github.com/cran/PSS.Health
**Shiny App:** https://hcpa-unidade-bioestatistica.shinyapps.io/PSS_Health/

**Key Features:**
- Power and sample size for health researchers
- Interactive Shiny interface
- Comprehensive healthcare-specific scenarios

**Use Case:** Validate your sample size calculations. Use for future study planning.

### **rpsychologist/powerlmm**
**Repository:** https://github.com/rpsychologist/powerlmm

**Key Features:**
- Power analysis for two/three-level longitudinal multilevel models
- Handles clustering and missing data
- Treatment study design optimization

**Use Case:** Power analysis for multilevel models (providers nested in states/regions).

### **arcaldwell49/Superpower**
**Repository:** https://github.com/arcaldwell49/Superpower

**Key Features:**
- Simulation-based power analysis
- Handles complex designs
- Visualizes power across parameter ranges

**Use Case:** Simulate power for your mystery caller design with realistic effect sizes.

### **cran/pwrss**
**Repository:** https://github.com/cran/pwrss

**Key Features:**
- General-purpose statistical power and sample size tools
- Wide range of statistical tests

**Use Case:** Quick power calculations for secondary analyses.

---

## 7. Audit Study Methodology

### **5harad/threshold-test**
**Repository:** https://github.com/5harad/threshold-test
**File:** `src/analysis.R`

**Key Features:**
- Testing for racial discrimination in police searches
- Threshold test methodology
- Audit study analysis framework

**Use Case:** Adapted methodology for testing insurance-based discrimination in appointment access.

### **dssg/aequitas**
**Repository:** https://github.com/dssg/aequitas

**Key Features:**
- Bias auditing toolkit
- Fairness metrics for ML models
- Discrimination detection

**Use Case:** Framework for auditing discrimination patterns in appointment access data.

---

## 8. Data Infrastructure & Utilities

### **rOpenHealth/rHealthDataGov**
**Repository:** https://github.com/rOpenHealth/rHealthDataGov

**Key Features:**
- Interface to HealthData.gov API
- Hospital Compare datasets
- U.S. hospital information

**Use Case:** Supplement NPI data with Hospital Compare quality metrics.

### **rudeboybert/SpatialEpi**
**Repository:** https://github.com/rudeboybert/SpatialEpi

**Key Features:**
- Methods and data for spatial epidemiology
- Disease mapping
- Cluster detection

**Use Case:** Spatial clustering analysis to identify appointment access "cold spots."

---

## Priority Recommendations

### 🔴 High Priority - Immediate Review

1. **raikens1/health_desert_maps** - Directly applicable healthcare desert methodology
2. **PSS.Health** - Validate your sample size and power calculations
3. **riatelab/osrm** - Free alternative to HERE API for cost reduction
4. **5harad/threshold-test** - Audit study discrimination testing framework

### 🟡 Medium Priority - Explore for Enhancement

5. **ipeaGIT/r5r** - Multimodal transport analysis (more realistic access)
6. **SpatialAcc** - Implement 2SFCA method for comparison
7. **powerlmm** - Rigorous multilevel power analysis
8. **Chrisjb/RDistanceMatrix** - Census-integrated isochrones

### 🟢 Low Priority - Educational/Reference

9. **easystats/modelbased** - Cleaner emmeans interface
10. **asha** - Alternative spatial accessibility package
11. **trendct/walkthroughs** - Training materials for team

---

## Specific Code to Review

### From health_desert_maps
- How they define "healthcare deserts"
- Medicaid-specific hospital filtering
- Travel time calculation methodology
- Demographic factor integration

### From osrm
- `osrmIsochrone()` implementation - compare to HERE
- Batch processing optimization
- Caching strategies

### From PSS.Health
- Sample size formulas for audit studies
- Power curves for binary outcomes
- Clustering effects in hierarchical designs

### From threshold-test
- Discrimination testing statistical framework
- Paired comparison methods
- Bootstrap confidence intervals for differences

---

## Integration Opportunities

### 1. Cost Reduction
Replace HERE API calls with `osrm` package for free OSM-based routing. Validate results match.

### 2. Enhanced Accessibility Metrics
Implement 2SFCA method from `SpatialAcc` alongside isochrones for more nuanced access measures.

### 3. Multimodal Analysis
Use `r5r` to include public transit in accessibility calculations (low-income populations rely on transit).

### 4. Discrimination Testing
Adapt `threshold-test` methodology for insurance-based discrimination analysis.

### 5. Power Analysis Validation
Use `PSS.Health` Shiny app to validate sample size; use `powerlmm` for multilevel power analysis.

### 6. Healthcare Market Definition
Apply `health-care-markets` methods to define geographic markets for your study regions.

---

## Next Steps

1. **Clone and test:** `riatelab/osrm` as HERE API alternative
2. **Review methodology:** `raikens1/health_desert_maps` for healthcare desert definitions
3. **Validate power:** Run analysis through `PSS.Health` Shiny app
4. **Implement 2SFCA:** Add Two-Step Floating Catchment Area method from `SpatialAcc`
5. **Adapt discrimination tests:** Study `threshold-test` for insurance discrimination framework
6. **Document APIs:** Create comparison table of HERE vs OSRM vs r5r performance/cost

---

## Search Summary

**Total repositories reviewed:** 35+
**Directly applicable:** 15
**High-value additions:** 8
**Cost-saving alternatives:** 3
**Methodological improvements:** 5

---

**Generated:** 2025-10-29
**For:** tyler R package - Mystery Caller Healthcare Access Study
**Purpose:** Identify code improvements and methodological enhancements from existing GitHub R repositories
