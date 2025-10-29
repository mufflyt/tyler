# Tyler Package: Critical Assumptions Requiring Real Data Testing

This document identifies all assumptions embedded in the tyler package that should be validated with real-world data before production deployment.

---

## 1. DATA QUALITY ASSUMPTIONS

### 1.1 NPI Number Format (CRITICAL)
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/validate_and_remove_invalid_npi.R` (Line 47)
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/search_by_taxonomy.R` (Line 63-67)

**Assumption:** NPI numbers are always exactly 10 digits
```r
valid_format <- nchar(npi_df$npi) == 10 & !grepl("\\D", npi_df$npi)
```

**What Could Go Wrong:**
- NPIs from legacy systems might be 9 digits with leading zero padding
- International provider identifiers might have different lengths
- NPIs from partnerships (11-12 digits) would be silently rejected

**Real Data Tests:**
- Load 10,000 NPIs from your existing database
  - Check distribution of nchar() - should be mostly 10, but verify 0% < 9 or > 10
  - Are there any leading zeros being stripped by CSV import?
  - Test with real NPI validation API: What % of "10 digit" values actually pass the Luhn checksum?

**Current Test Coverage:**
- ✓ Basic format tests exist (test-validate_and_remove_invalid_npi.R lines 10-22)
- ✗ Missing: Test what % of "valid format" NPIs actually pass npi::npi_is_valid()
- ✗ Missing: Test handling of NPIs with dashes, spaces, or other formatting

---

### 1.2 Phone Number Format Assumptions
**Files:**
- No explicit validation found in codebase
- Could be in clean_phase_1_results.R or clean_phase_2_results.R (need deeper investigation)

**What Needs Testing:**
- Are phone numbers assumed to be 10 digits (US-only)?
- What happens with extensions (x1234)?
- International numbers from your dataset?
- Fax numbers mixed with phone numbers?

---

### 1.3 Address Geocodability Assumptions
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/geocode.R` (Lines 38-148)

**Assumption:** All addresses in the "address" column can be geocoded with Google Maps API
```r
unique_add <- dplyr::distinct(data, address)
total_unique <- nrow(unique_add)
```

**What Could Go Wrong:**
- Only 3 retry attempts (Line 80-85) - is this enough?
- Google Maps API has hard 2.5 second queries per second limit
- Rural/frontier addresses have lower match rates
- Addresses with typos fail silently (no tracking of failure reasons by address)

**Real Data Tests:**
- Sample 1,000 addresses from your physician data
  - What % geocode successfully on first attempt?
  - What % fail after 3 retries with exponential backoff (1, 2, 4 seconds)?
  - Group results by state/urbanicity - is geocoding success rate uniform?
  - Test with intentionally malformed addresses (missing ZIP, city only, etc.)

**Current Test Coverage:**
- ✗ No existing tests for geocode.R function
- ✗ No real-world success rate benchmarks

---

### 1.4 Census Block Group Stability
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/get_census_data.R` (Lines 44-87)
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/calculate_intersection_overlap_and_save.R` (Lines 67-140)

**Assumption:** Census block groups are stable across 2010 vs 2020 Census periods
- Function requires matching vintage years (2010 vs 2020) with mandatory crosswalk
```r
if (!identical(provider_year, acs_year)) {
  if (!identical(sort(c(provider_year, acs_year)), c(2010, 2020))) {
    stop("Provider data_year and ACS vintage differ...")
  }
}
```

**What Could Go Wrong:**
- Block groups changed between Census periods - some split, some merged
- Your crosswalk function might not correctly map all affected GEOIDs
- 2020 ACS data uses 2020 geometries but your provider data is from 2019

**Real Data Tests:**
- Take 100 providers with 2010 vintage Census data
  - Apply crosswalk to 2020 geometries
  - Verify all GEOIDs resolve (no NAs introduced)
  - Calculate area overlap before/after - should be ~100% (allows 5% tolerance)
  - Compare provider address to original 2010 GEOID vs crosswalked 2020 GEOID - is provider still in the same or adjacent block group?

**Current Test Coverage:**
- ✓ Some validation in calculate_intersection_overlap_and_save.R (Lines 78-140)
- ✗ Missing: End-to-end test with real 2010→2020 crosswalks

---

## 2. API RATE LIMITING ASSUMPTIONS

### 2.1 NPI Search Retry Logic
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/search_by_taxonomy.R` (Lines 53-54, 138-149)
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/search_and_process_npi.R` (Lines 266-267, 375-379)

**Hardcoded Values:**
```r
max_attempts <- 3L
base_delay <- 1
delay <- base_delay * 2^(attempt - 1)  # Exponential backoff: 1, 2, 4 seconds
```

**Questions to Answer with Real Data:**
- Is 3 attempts optimal or too conservative?
- Does the NPI API have different rate limits for batch vs single searches?
- What's the actual 99th percentile latency? (delay should exceed this)
- Are 1, 2, 4 second delays sufficient for rate limit recovery?

**Real Data Tests:**
- Run search_by_taxonomy() for ALL 20 major OB/GYN subspecialties
  - Log: number of attempts per taxonomy
  - Calculate: success rate with 3 attempts
  - Simulate: would 2 or 4 attempts change outcomes? (post-hoc analysis)
  - Check API response headers for rate-limit info (if available)

**Current Test Coverage:**
- ✓ Retry logic tested in test-search_by_taxonomy.R (basic)
- ✗ Missing: Rate-limiting stress test (100+ rapid searches)
- ✗ Missing: Latency analysis of NPI API responses

---

### 2.2 Genderize.io API Batching
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/genderize_physicians.R` (Lines 104-186)

**Hardcoded Values:**
```r
batch_size = 10  # Line 104
max_retries <- 3  # Line 134
wait_time <- retry_count * 2  # Exponential backoff: 2, 4, 6 seconds
```

**Questions to Answer:**
- Is batch size 10 optimal? (API allows up to 1,000 per request)
- Free tier has daily limits - how many physicians can you genderize per day?
- What % of first names have missing probability values? (no training data in genderize.io)

**Real Data Tests:**
- Genderize 10,000 unique first names from your physician dataset
  - Track: daily API quota consumption
  - Measure: what % of names return probability < 0.6 (weak prediction)?
  - Analyze: are weak predictions concentrated in certain ethnic groups/regions?
  - Benchmark: compare genderize.io predictions against ground truth (if available)

**Current Test Coverage:**
- ✓ Batching tested (test-genderize_physicians.R lines 110-133)
- ✓ Error handling tested
- ✗ Missing: Real probability distribution analysis
- ✗ Missing: Demographic bias assessment

---

## 3. STATISTICAL ASSUMPTIONS

### 3.1 Geographic Overlap Distribution
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/calculate_intersection_overlap_and_save.R` (Lines 210-223)
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/check_normality.R` (Lines 36-93)

**Assumption:** Overlap percentages follow a normal distribution
```r
non_missing_overlap <- stats::na.omit(block_groups_proj$overlap)
summary_bg <- summary(non_missing_overlap)
median <- round(stats::quantile(non_missing_overlap, probs = 0.5), 4) * 100
p75 <- round(stats::quantile(non_missing_overlap, probs = 0.75), 4) * 100
```

**The Problem:** The code calculates quantiles but doesn't validate the normality assumption!
- Isochrones are typically bimodal (either 0% or 50%+ overlap)
- Reporting just mean/SD could be misleading if data is skewed

**Real Data Tests:**
- Calculate isochrones for 50 real provider locations
  - Run intersections for each drive time (30, 60, 120, 180 min)
  - For each drive time, test normality using Shapiro-Wilk (p > 0.05?)
  - Plot histogram/density of overlap % - is it roughly bell-shaped?
  - Calculate skewness/kurtosis - should be close to 0/3 if normal
  - Compare median vs mean - large divergence suggests non-normal distribution

**Current Test Coverage:**
- ✓ check_normality.R exists (test-calculate_intersection_overlap_and_save.R)
- ✗ Missing: Actual application in calculate_intersection_overlap_and_save.R

---

### 3.2 Service Area Completeness
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/summarize_census_data.R` (Lines 47-134)

**Assumption:** All providers (isochrones) have defined service areas
```r
isochrones_filtered <- isochrones_joined[isochrones_joined$drive_time == drive_time_minutes, , drop = FALSE]
if (nrow(isochrones_filtered) == 0) {
  stop("Error: no isochrones found for the requested drive time.")
}
```

**What Could Go Wrong:**
- Some providers might lack coordinates (missing lat/long)
- Some locations might fail HERE API calls (5-10% failure rate possible)
- Rural locations have larger isochrones - do you have enough data to analyze?

**Real Data Tests:**
- Load provider dataset, create isochrones
  - Count: how many providers lack coordinates?
  - Count: how many fail isochrone generation? (compare input vs output row counts)
  - Analyze: are failures correlated with state, urban/rural classification, provider type?

---

## 4. GENDER PREDICTION ASSUMPTIONS

### 4.1 Probability Threshold for Gender Assignment
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/genderize_physicians.R` (Lines 169-182)

**Current Behavior:** ALL gender predictions returned, regardless of probability
```r
gender = vapply(entries, function(x) {
  if (is.null(x$gender)) NA_character_ else x$gender
}, character(1), USE.NAMES = FALSE),
probability = vapply(entries, function(x) {
  if (is.null(x$probability)) NA_real_ else as.numeric(x$probability)
}, numeric(1), USE.NAMES = FALSE),
```

**Missing Logic:** There's NO filtering on probability threshold!
- Genderize.io returns probability (0-1) for each prediction
- A probability of 0.51 is barely better than random guessing
- No code currently filters out low-confidence predictions

**Real Data Tests:**
- Genderize 1,000 physicians from your dataset
  - Get ground truth gender (from clinic records, if available)
  - Calculate accuracy for different probability thresholds:
    - All predictions (current behavior)
    - Only predictions where probability ≥ 0.75
    - Only predictions where probability ≥ 0.90
  - Report: sensitivity, specificity, positive predictive value by threshold
  - Benchmark: what probability threshold gives you 95% accuracy?

**Current Test Coverage:**
- ✓ Basic genderization tested
- ✗ Missing: NO tests for prediction accuracy/reliability
- ✗ Missing: Probability threshold optimization

---

### 4.2 First Name Uniqueness Assumption
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/genderize_physicians.R` (Lines 52-60)

**Assumption:** First name gender is consistent across physicians
- "Lynn", "Morgan", "Casey" are unisex in English-speaking countries
- Same name might be masculine in one culture, feminine in another

**Real Data Tests:**
- From your 10,000 genderized physicians:
  - Identify first names that appear > 5 times
  - For names with multiple predictions:
    - What % have conflicting gender assignments?
    - Examples: "Morgan" (60% male, 40% female)?
  - For these ambiguous names, check:
    - Is there a pattern by specialty? (e.g., more female OB/GYN, more male Urology)
    - Are predictions from genderize.io consistent, or does it vary by request batch?

---

## 5. TAXONOMY/CREDENTIAL ASSUMPTIONS

### 5.1 Hardcoded Specialty Lists
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/search_and_process_npi.R` (Lines 200-205)
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/search_by_taxonomy.R` (Lines 78-84)

**Hardcoded Credentials:**
```r
vc <- c(
  "Allergy & Immunology", "Anesthesiology", "Dermatology", "Emergency Medicine",
  "Family Medicine", "Internal Medicine", "Obstetrics & Gynecology", "Ophthalmology",
  "Orthopaedic Surgery", "Pediatrics", "Psychiatry & Neurology", "Radiology", "Surgery", "Urology"
)
bc <- c("Pathology", "Pediatrics", "Physical Medicine & Rehabilitation", "Plastic Surgery", "Preventive Medicine")

credential_lower %in% stringr::str_to_lower(c("MD", "DO"))  # search_by_taxonomy.R line 83
```

**Questions:**
- Is this list complete for OB/GYN search?
  - Missing "Maternal & Fetal Medicine"? (Yes - it's in examples but not in vc/bc)
  - Missing "Gynecologic Oncology"? (mentioned in examples but not in lists)
  - Missing "Reproductive Endocrinology"? (mentioned in examples but not in lists)
  - Missing "Female Pelvic Medicine"?

- Why are both MD and DO required as separate credentials?
  - What about DPM, PA-C, APRN, CNM who might practice in OB/GYN?

- Are there new credentials issued after this code was written?
  - Board-certified specialists from other countries?

**Real Data Tests:**
- Query NPI database for all entries matching your target specialties
  - Count: how many have credentials NOT in {MD, DO}?
  - List: what are the other credentials? (PA-C, APRN, NP, etc.)
  - Decision: Should you include these? What % of your target population do they represent?

**Current Test Coverage:**
- ✓ Basic taxonomy filtering tested
- ✗ Missing: Audit of what's excluded (false negatives)

---

### 5.2 Verified Credentials
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/search_by_taxonomy.R` (Lines 78-84)

**Current Logic:** Keeps records if credential is MD/DO OR credential is missing
```r
data_taxonomy <- dplyr::filter(
  data_taxonomy,
  is.na(credential_lower) | credential_lower %in% stringr::str_to_lower(c("MD", "DO"))
)
```

**Implication:** Missing credentials are treated the same as MD/DO (included)
- This might inflate your dataset with uncredentialed individuals
- NPI database includes organizational entries and non-providers

**Real Data Tests:**
- Compare output of search_by_taxonomy() against manual audit
  - Sample 100 results
  - Check: what % actually have MD/DO credentials?
  - Check: what % are missing credentials?
  - Check: are missing credentials from real providers or data entry errors?

---

## 6. DRIVE TIME ASSUMPTIONS

### 6.1 Default Drive Time Breaks
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/create_isochrones_for_dataframe.R` (Lines 27)
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/create_isochrones.R` (Lines 34)

**Hardcoded Default:**
```r
breaks = c(1800, 3600, 7200, 10800)  # 30, 60, 120, 180 minutes
posix_time = as.POSIXct("2023-10-20 08:00:00", format = "%Y-%m-%d %H:%M:%S")  # FIXED TIME!
routing_mode = "fast"
traffic = TRUE
```

**Problems:**
1. Fixed departure time (2023-10-20 08:00 AM) is hardcoded
   - Real access times vary by day/time
   - Isochrone for 8 AM rush hour ≠ 2 AM middle of night
   - Your data might be collected at different times

2. 180-minute cutoff might be too short/long
   - For rural OB/GYNs, patients might drive 3+ hours
   - For urban clinics, most patients within 30 minutes
   - Are these breaks appropriate for all regions?

3. "fast" routing mode + traffic=TRUE
   - Some providers might route "shortest" (less traffic-dependent)
   - Is "balanced" optimize mode better for real-world access?

**Real Data Tests:**
- For 10 providers in different regions (urban, rural, mixed):
  - Generate isochrones for multiple times of day (8 AM, 12 PM, 5 PM, 10 PM)
  - Compare: how much do isochrones vary by time of day?
  - Calculate: what % of your patient population falls within each break?
    - If 99% within 60 min, do you need 120/180 min breaks?
    - If 30% beyond 180 min, should you increase?

- Regional analysis:
  - Urban vs rural: do the same breaks work?
  - Interstate vs intrastate: do highway routing patterns change?

**Current Test Coverage:**
- ✗ No tests for isochrone generation
- ✗ No tests for temporal variation

---

## 7. CENSUS DATA ASSUMPTIONS

### 7.1 Vintage Year Consistency
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/get_census_data.R` (Line 51)
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/calculate_intersection_overlap_and_save.R` (Lines 75-140)

**Hardcoded Default:**
```r
vintage = 2022  # ACS 2022 (latest when written)
```

**Problems:**
- ACS 2022 has lower data quality than ACS 2020 (less margin of error)
- 2020 Census might be more appropriate for some analyses
- What if your provider data is from 2019? (Would require 2010 Census crosswalk)

**Real Data Tests:**
- For your cohort:
  - What year is provider data from?
  - Request both ACS 2020 and ACS 2022 for same block groups
  - Compare: how much do estimates differ? (% change in population, female %)
  - Check: which vintage has better data quality (lower margin of error)?

**Current Test Coverage:**
- ✓ Vintage specified in get_census_data()
- ✗ Missing: Guidance on which vintage to use

---

### 7.2 Age Group Definitions
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/summarize_census_data.R` (Lines 49, 88, 173-179)

**Hardcoded Reproductive Age Definition:**
```r
reproductive_age_vars = sprintf("B01001_%03dE", 30:38)  # Default: ages 15-44
# B01001_030E = 15-17
# B01001_031E = 18-19
# B01001_032E = 20
# B01001_033E = 21
# B01001_034E = 22-24
# B01001_035E = 25-29
# B01001_036E = 30-34
# B01001_037E = 35-39
# B01001_038E = 40-44
```

**Question:** Is 15-44 the right definition for OB/GYN?
- Medical literature uses 15-49
- Your study might need 18-44 (exclude minors)
- Your study might need 20-45 (exclude very young)

**Real Data Tests:**
- Compare population estimates under different age definitions:
  - 15-44 (current default)
  - 18-49 (medical convention)
  - 20-45 (alternative)
- For your block groups: what % population change between definitions?
- For your provider isochrones: would this change coverage conclusions?

**Current Test Coverage:**
- ✓ Age variables customizable via reproductive_age_vars parameter
- ✓ Examples shown
- ✗ Missing: Guidance on best practice for OB/GYN

---

## 8. DATA PROCESSING ASSUMPTIONS

### 8.1 Credential Cleaning
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/search_by_taxonomy.R` (Lines 76-80)

**Credential Cleaning Logic:**
```r
credential = stringr::str_remove_all(basic_credential, "[[\\p{P}][\\p{S}]]")
credential_lower = stringr::str_to_lower(credential)
```

**What Gets Removed:**
- Punctuation: `.`, `,`, `-`, `'`
- Symbols: `©`, `™`, etc.

**Examples of transformation:**
- "M.D." → "MD" ✓
- "D.O." → "DO" ✓
- "MD, FACOG" → "MD FACOG" (keeps, but won't match filter)
- "M.D., Ph.D." → "MD PhD" (won't match filter - needs both in c("MD", "DO"))

**Real Data Tests:**
- Audit NPI credentials in your raw data:
  - What's the distribution of credential formats?
  - How many match exactly "MD" or "DO"?
  - How many don't match but should be included? ("M.D.", "FACOG", "APRN", etc.)

---

### 8.2 Country Filtering
**Files:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/search_by_taxonomy.R` (Lines 85)
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/search_and_process_npi.R` (Lines 279)

**Assumption:** All providers are in "United States"
```r
data_taxonomy <- dplyr::filter(data_taxonomy, addresses_country_name == "United States")
```

**Reality:**
- Some U.S. clinicians practice in Canada, Mexico, or internationally
- Telehealth providers might have addresses outside U.S.
- Army/Navy/VA providers might be stationed abroad

**Impact:** Unknown, depends on your research question

**Real Data Tests:**
- How many NPI results are non-U.S.?
- Are these real providers you want to include?

---

## 9. SUMMARY: TESTING PRIORITIZATION

### CRITICAL (Test Immediately)
1. **NPI Validation** - affects sample composition
2. **Gender Prediction Accuracy** - potential demographic bias
3. **Address Geocoding Success Rate** - geographic completeness
4. **Specialty/Credential Filtering** - sample definition

### HIGH (Test Before Analysis)
5. **Drive Time Breaks** - geographic access definition
6. **Census Vintage** - population estimates
7. **API Rate Limits** - workflow reliability

### MEDIUM (Test if Possible)
8. **Overlap Distribution Normality** - statistical reporting
9. **Census Block Group Crosswalks** - geographic accuracy

### LOW (Document/Accept)
10. **Hardcoded Specialties** - inherent study limitation

---

## 10. RECOMMENDED TEST DATASETS

### Minimum Viable Dataset for Testing
- **500 NPI numbers** (known to be valid or invalid)
- **200 addresses** (known geocodable vs not)
- **100 provider locations** (for isochrone testing)
- **50 first names** (for genderize accuracy assessment)
- **5 states** worth of Census block group data

### Ideal Dataset for Validation
- **10,000 NPI numbers** from your actual database
- **1,000 unique addresses** with known ground truth
- **100 real provider locations** across regions
- **5,000 physician first names** with ground truth gender (if available)
- **Full Census data** for all states in study

---

## APPENDIX: Files Referenced

**Core Processing Functions:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/search_by_taxonomy.R` - NPI taxonomy search + credential filtering
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/search_and_process_npi.R` - NPI search with retry logic
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/validate_and_remove_invalid_npi.R` - NPI validation
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/genderize_physicians.R` - Gender prediction via API
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/geocode.R` - Address geocoding via Google Maps API
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/create_isochrones_for_dataframe.R` - Drive time isochrones
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/create_isochrones.R` - Isochrone generation core
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/get_census_data.R` - Census data retrieval
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/summarize_census_data.R` - Census demographic summaries
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/calculate_intersection_overlap_and_save.R` - Geographic overlap calculation

**Validation Functions:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/utils-validation.R` - General validation utilities
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/geospatial_validators.R` - Spatial data validation
- `/Users/tylermuffly/Dropbox (Personal)/tyler/R/check_normality.R` - Normality testing

**Existing Tests:**
- `/Users/tylermuffly/Dropbox (Personal)/tyler/tests/testthat/test-validate_and_remove_invalid_npi.R`
- `/Users/tylermuffly/Dropbox (Personal)/tyler/tests/testthat/test-genderize_physicians.R`
- `/Users/tylermuffly/Dropbox (Personal)/tyler/tests/testthat/test-search_by_taxonomy.R`
- `/Users/tylermuffly/Dropbox (Personal)/tyler/tests/testthat/test-geocode.R`
- `/Users/tylermuffly/Dropbox (Personal)/tyler/tests/testthat/test-get_census_data.R`
- `/Users/tylermuffly/Dropbox (Personal)/tyler/tests/testthat/test-calculate_intersection_overlap_and_save.R`

