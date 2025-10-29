# Tyler Package: Real Data Testing Action Plan

## Executive Summary

The tyler package contains **10 critical assumptions** that must be validated with real data before production use. This document outlines which assumptions to test first and what data you need.

---

## PHASE 1: CRITICAL ASSUMPTIONS (Test Before ANY Analysis)

### 1. NPI Validation Rate
**WHY:** NPIs are your sample definition. A validation issue affects everything downstream.

**TEST:**
```r
# Load your physician database
npi_numbers <- your_database$npi_column

# Quick audit
cat("Total NPIs:", length(npi_numbers), "\n")
cat("10-digit NPIs:", sum(nchar(as.character(npi_numbers)) == 10), "\n")
cat("Unique:", length(unique(npi_numbers)), "\n")

# Validate with package
validated <- validate_and_remove_invalid_npi(data.frame(npi = npi_numbers))
cat("NPIs passing validation:", nrow(validated), "\n")
cat("Validation rate:", round(nrow(validated) / length(npi_numbers) * 100, 1), "%\n")
```

**DECISION POINT:** 
- If validation rate < 70%, investigate why. May need to adjust validate_and_remove_invalid_npi.R logic
- Document validation rate in your methods

---

### 2. Gender Prediction Accuracy
**WHY:** Gender-based analyses require high accuracy. Need to know if genderize.io probability threshold needs adjustment.

**TEST:**
```r
# Genderize a sample of 500-1000 physicians
sample_physicians <- your_database %>% slice_sample(n = 1000)
result <- genderize_physicians(
  input_csv = "sample_physicians.csv",
  output_dir = tempdir()
)

# Analyze probability distribution
library(dplyr)
result %>%
  summarise(
    n_total = n(),
    n_matched = sum(!is.na(gender)),
    match_rate = round(n_matched / n_total * 100, 1),
    prob_min = min(probability, na.rm = TRUE),
    prob_max = max(probability, na.rm = TRUE),
    prob_median = median(probability, na.rm = TRUE),
    prob_low = sum(probability < 0.6, na.rm = TRUE),
    prob_weak_rate = round(sum(probability < 0.6, na.rm = TRUE) / n_matched * 100, 1)
  )

# DECISION: Is match_rate > 90%? Is prob_median > 0.85?
```

**VALIDATION (if available):**
- If you have ground truth gender (clinic records, self-report):
  - Compare predictions vs. ground truth
  - Calculate accuracy, sensitivity, specificity by probability threshold
  - Find optimal threshold (aim for 95% accuracy)

**DECISION POINT:**
- If match rate < 70%, review first names - may have unusual distribution
- If prob_median < 0.75, consider filtering out low-confidence predictions

---

### 3. Address Geocoding Success Rate
**WHY:** Geographic completeness affects your entire analysis. Need real success rates by region.

**TEST:**
```r
# Sample 200 addresses across states
sample_addresses <- your_database %>%
  slice_sample(n = 200) %>%
  select(address, state)

# Register Google Maps API
library(ggmap)
register_google(key = "YOUR_API_KEY")

# Geocode with error tracking
geocoded <- geocode_unique_addresses(
  file_path = "sample_addresses.csv",
  google_maps_api_key = "YOUR_API_KEY",
  failed_output_path = "failed_addresses.csv"
)

# Analyze results
cat("Success rate:", round(
  sum(!is.na(geocoded$latitude)) / nrow(geocoded) * 100, 1
), "%\n")

# By state
geocoded %>%
  group_by(state) %>%
  summarise(
    n = n(),
    success = sum(!is.na(latitude)),
    success_rate = round(success / n * 100, 1)
  )
```

**DECISION POINT:**
- If overall success < 90%, investigate failures - may need address standardization
- If state-level variation > 20%, may need stratified analysis
- Document success rates in methods

---

### 4. Specialty/Credential Filtering
**WHY:** This defines your analytic sample. Need to audit false negatives.

**TEST:**
```r
# Query NPI API for "Obstetrics & Gynecology"
all_results <- search_by_taxonomy(
  "Obstetrics & Gynecology",
  write_snapshot = TRUE
)

# Analyze credentials
all_results %>%
  group_by(credential) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))

# What % have MD/DO?
cat("MD/DO rate:", round(
  sum(all_results$credential %in% c("MD", "DO"), na.rm = TRUE) /
  nrow(all_results) * 100, 1
), "%\n")

# Manual audit: sample 50 results, verify they're real providers
```

**DECISION POINT:**
- If MD/DO rate < 85%, may need to expand credential filter (PA-C, APRN, etc.)
- If you find missing specialties (MFM, GO, REI), update hardcoded lists

---

## PHASE 2: HIGH-PRIORITY ASSUMPTIONS (Test Before Main Analysis)

### 5. Drive Time Break Appropriateness

**TEST:**
```r
# Generate isochrones for 10 providers across regions
providers_sample <- your_database %>%
  slice_sample(n = 10)

isochrones <- create_isochrones_for_dataframe(
  input_file = "providers_sample.csv",
  breaks = c(1800, 3600, 7200, 10800)  # 30, 60, 120, 180 min
)

# For each provider, check population coverage
# (requires census block group data)
# What % of population within each break?

# DECISION: Are all 4 breaks needed? Or could you use 30/60/90?
```

---

### 6. Census Data Vintage Selection

**TEST:**
```r
# Request both ACS 2020 and ACS 2022 for your study area
census_2020 <- get_census_data(
  us_fips_list = c("06", "08", "41"),  # CA, CO, OR
  vintage = 2020
)

census_2022 <- get_census_data(
  us_fips_list = c("06", "08", "41"),
  vintage = 2022
)

# Compare estimates
census_2020 %>%
  summarise(
    n_block_groups = n(),
    total_pop_2020 = sum(B01001_001E, na.rm = TRUE),
    female_pct_2020 = round(
      sum(B01001_026E, na.rm = TRUE) /
      sum(B01001_001E, na.rm = TRUE) * 100, 2
    )
  )

census_2022 %>%
  summarise(
    n_block_groups = n(),
    total_pop_2022 = sum(B01001_001E, na.rm = TRUE),
    female_pct_2022 = round(
      sum(B01001_026E, na.rm = TRUE) /
      sum(B01001_001E, na.rm = TRUE) * 100, 2
    )
  )

# DECISION: Which vintage is more appropriate for your provider data year?
```

---

## PHASE 3: MEDIUM-PRIORITY ASSUMPTIONS (If Time Permits)

### 7. API Rate Limit Stress Test
- Run search_by_taxonomy() for 50+ specialties
- Log retry attempts per specialty
- Determine if 3 attempts sufficient

### 8. Overlap Distribution Normality
- Generate isochrones for 50+ providers
- Test overlap% distribution with Shapiro-Wilk
- Use check_normality() to confirm non-parametric approach

---

## ESTIMATED TIME COMMITMENT

| Phase | Task | Estimated Hours | Data Needed |
|-------|------|-----------------|-------------|
| 1a | NPI validation | 2 | 10,000 NPIs |
| 1b | Gender accuracy | 4 | 1,000 physicians |
| 1c | Geocoding success | 6 | 200 addresses |
| 1d | Credential audit | 2 | 100 NPI results |
| 2a | Drive time analysis | 4 | 10 providers |
| 2b | Census comparison | 3 | 3 states data |
| **Total Phase 1+2** | **21 hours** | |

---

## DELIVERABLES

After completing Phase 1, document:
1. **Data Quality Report**
   - NPI validation rate: [X]%
   - Gender match rate: [X]%, median probability: [X]
   - Address geocoding rate: [X]%
   - Specialty filter: [X]% MD/DO

2. **Methods Section Updates**
   - Include validation rates
   - Document any adjustments to thresholds/filters
   - Note regional variation if relevant

3. **Supplementary Tables**
   - NPI validation results by source
   - Gender accuracy by probability threshold
   - Geocoding success by state/urbanicity
   - Credential distribution in final sample

---

## RED FLAGS TO WATCH FOR

| Assumption | Red Flag | Action |
|-----------|----------|--------|
| NPI validation | < 70% valid | Audit format, may need preprocessing |
| Gender accuracy | < 85% match rate | Investigate name distribution, may need cleanup |
| Geocoding | < 85% success | Address standardization needed |
| Credentials | < 80% MD/DO | May need to expand inclusion criteria |
| Census vintage | > 5% difference | Investigate which vintage is correct |

---

## NEXT STEPS

1. **This Week:** Identify which real data you have access to
2. **Next Week:** Run Phase 1 audit tests (NPI, gender, geocoding, credentials)
3. **Before Analysis:** Complete Phase 2 tests (drive time, census)
4. **Methods Section:** Document all validation results

Questions? See ASSUMPTIONS_REQUIRING_REAL_DATA_TESTING.md for detailed analysis.
