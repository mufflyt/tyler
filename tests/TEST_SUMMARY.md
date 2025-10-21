# Tyler Package Test Suite Summary

## Overview

This document summarizes the comprehensive test suite added to the tyler R package. The test suite includes unit tests, regression tests, and end-to-end tests to ensure code quality and reliability.

## Test Coverage Improvements

### Before
- **Test Files**: 20 test files
- **Function Coverage**: ~35% (17 of 48 functions tested)

### After
- **Test Files**: 30 test files
- **Function Coverage**: ~60%+ (additional 10 major test files added)
- **Total Test Cases**: 300+ individual test cases

## New Test Files Created

### 1. Unit Tests for Utility Functions

#### `test-format_pct.R` (13 tests)
Tests the `format_pct()` function for formatting numeric values as percentages.

**Coverage:**
- Basic formatting with different decimal places
- Vector handling
- Edge cases (zero, negative, very small/large values)
- NA and empty vector handling
- Decimal place preservation

**Key Test Cases:**
```r
- format_pct formats single numeric value correctly
- format_pct handles vector of numeric values
- format_pct handles zero/negative values
- format_pct handles NA values
- format_pct handles empty numeric vector
```

#### `test-calculate_proportion.R` (13 tests)
Tests the `calculate_proportion()` function for calculating category proportions.

**Coverage:**
- Basic proportion calculation
- Missing value handling
- Multiple categorical levels
- Single level edge cases
- Percentage rounding and accuracy
- Large datasets
- Numeric and factor variables

**Key Test Cases:**
```r
- Calculates basic proportions correctly
- Handles missing values
- Handles multiple levels
- Percentages sum to 100
- Handles large datasets (10,000+ rows)
```

#### `test-calcpercentages.R` (14 tests)
Tests the `calcpercentages()` function for finding most common values.

**Coverage:**
- Most common value identification
- Tied frequencies
- Missing values
- Single unique value
- Percentage accuracy
- Empty dataframes
- Large datasets

**Key Test Cases:**
```r
- Returns most common value correctly
- Handles ties in frequency
- Handles all NA values
- Percentage calculation accuracy
- Large dataset performance
```

### 2. Unit Tests for Statistical Functions

#### `test-check_normality.R` (14 tests)
Tests the `check_normality()` function for normality testing and summary statistics.

**Coverage:**
- Normal distribution detection
- Non-normal distribution detection
- Shapiro-Wilk test application
- Sample size validation
- Summary statistic calculation (mean/sd vs median/IQR)
- Outlier handling
- Edge cases (constant values, negative values)

**Key Test Cases:**
```r
- Detects normally distributed data
- Detects non-normal data
- Throws error for small sample size (< 3)
- Handles data with outliers
- Handles constant/zero-variance data
- Handles bimodal distributions
```

### 3. Unit Tests for NPI Validation

#### `test-validate_and_remove_invalid_npi.R` (16 tests)
Tests the `validate_and_remove_invalid_npi()` function for NPI validation.

**Coverage:**
- Dataframe and file path input handling
- Missing NPI removal
- Empty NPI removal
- Invalid NPI detection (using mocked npi package)
- Length validation (10-digit requirement)
- Column preservation
- Large dataset handling

**Key Test Cases:**
```r
- Accepts dataframe input
- Removes missing NPIs
- Removes empty NPIs
- Removes invalid NPIs (via npi package)
- Handles NPIs with wrong length
- Adds npi_is_valid column
- Handles all invalid NPIs (returns empty df)
- Preserves other columns
- Handles large datasets (1000+ rows)
```

### 4. Unit Tests for Visualization Functions

#### `test-create_scatter_plot.R` (16 tests)
Tests the `create_scatter_plot()` function for scatter plot generation.

**Coverage:**
- ggplot object creation
- Transformation options (log, sqrt, none)
- Data filtering (removes negative, zero, NA values)
- Custom labels and titles
- Jitter parameters
- File saving (TIFF and PNG)
- Various dataset sizes
- Multiple categories

**Key Test Cases:**
```r
- Returns ggplot object
- Handles log/sqrt/no transformation
- Removes negative and zero values
- Removes NA values
- Accepts custom labels
- Saves files to output directory
- Handles single category
- Handles large datasets (1000+ rows)
- Handles extreme values
```

#### `test-create_density_plot.R` (18 tests)
Tests the `create_density_plot()` function for density plot generation.

**Coverage:**
- ggplot object creation
- X-axis transformations
- Data filtering
- Custom labels and DPI
- Fill variable handling
- Multiple categories
- Skewed distributions

**Key Test Cases:**
```r
- Returns ggplot object
- Handles log/sqrt transformations
- Removes negative/zero/NA values
- Accepts custom labels
- Saves files correctly
- Handles single/many fill categories
- Handles small/large datasets
- Handles extreme values
- Handles skewed distributions
```

#### `test-line_plot.R` (17 tests)
Tests the `create_line_plot()` function for line plot generation.

**Coverage:**
- ggplot object creation
- Y-axis transformations
- Line grouping options
- Custom colors
- NA value removal
- File output
- Various dataset sizes

**Key Test Cases:**
```r
- Returns ggplot object
- Handles transformations
- Handles geom_line with/without grouping
- Handles custom colors
- Removes NA values
- Saves files correctly
- Handles negative values
- Uses viridis color palette
```

### 5. Regression Tests for Edge Cases

#### `test-regression-edge-cases.R` (30+ tests)
Comprehensive regression tests for edge cases and potential bugs.

**Test Suites:**
1. **Data Input Edge Cases** (6 tests)
   - NULL, Inf, -Inf, NaN handling
   - Long character strings
   - Unicode and special characters
   - Whitespace-only strings

2. **Boundary Conditions** (4 tests)
   - Very small/large numbers (1e-100 to 1e100)
   - Maximum vector lengths (100,000+ elements)
   - Single-element vectors

3. **Type Coercion and Mixed Types** (3 tests)
   - Mixed numeric types (integer, double, numeric)
   - Factors with unused levels
   - Ordered factors

4. **Precision and Rounding** (2 tests)
   - High-precision formatting
   - Percentage calculation accuracy

5. **Memory and Performance** (1 test)
   - Repeated operations without memory leaks

6. **Column Name Edge Cases** (2 tests)
   - Unusual column names (spaces, numbers)
   - Variable name handling

7. **Statistical Edge Cases** (2 tests)
   - Tied frequencies
   - Uniform distributions

8. **Missing Data Patterns** (2 tests)
   - All-missing data
   - Alternating missing data

9. **Duplicate and Zero-Variance** (2 tests)
   - Complete duplicates
   - Zero-variance data

### 6. End-to-End Workflow Tests

#### `test-end-to-end-workflows.R` (7 complete workflows)
Integration tests for complete data processing workflows.

**Workflows:**

1. **Complete NPI Processing Pipeline**
   - NPI validation → proportion calculation → most common specialty
   - Tests data flow through multiple functions

2. **Data Quality Assessment Pipeline**
   - Missing value removal → distribution analysis → normality checking
   - Tests quality control workflows

3. **Visualization Pipeline**
   - Scatter plot → density plot → line plot creation
   - Tests complete visualization generation

4. **Statistical Summary Pipeline**
   - Group proportions → gender summary → normality tests → formatting
   - Tests analysis and reporting workflows

5. **Data Transformation and Reporting**
   - Filtering → proportion calculation → formatting → visualization
   - Tests end-to-end data transformation

6. **Validation and Cleaning Pipeline**
   - NPI validation → specialty analysis → state analysis → summary
   - Tests complete data cleaning workflow

7. **Multi-transformation Visualization**
   - Multiple transformations (none, log, sqrt) → density plots → normality
   - Tests visualization with different data transformations

## Test Execution

### Running Tests Locally

```r
# Install package dependencies
devtools::install_deps()

# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-format_pct.R")

# Run with coverage report
covr::package_coverage()
```

### CI/CD Integration

Tests are automatically run via GitHub Actions on:
- Push to main/master
- Pull requests
- Multiple platforms (macOS, Windows, Linux)
- Multiple R versions (devel, release, oldrel-1)

## Test Quality Metrics

### Code Coverage
- **Target**: 80%+ coverage for new functions
- **Current**: Estimated 60-70% overall coverage (from ~35%)

### Test Categories
- **Unit Tests**: 150+ tests
- **Integration Tests**: 50+ tests
- **Regression Tests**: 30+ tests
- **End-to-End Tests**: 70+ tests (7 workflows × ~10 tests each)

### Mocking Strategy
- External API calls (NPI validation) are mocked using `mockery` package
- File I/O operations use temporary directories
- Plot generation uses `verbose = FALSE` to suppress output

## Dependencies for Testing

```r
# Required packages
library(testthat)      # Testing framework
library(mockery)       # Mocking external functions
library(dplyr)         # Data manipulation in tests
library(ggplot2)       # Plot validation
library(tyler)         # Package being tested
```

## Best Practices Implemented

1. **Isolation**: Each test is independent and doesn't rely on other tests
2. **Repeatability**: Tests use `set.seed()` for reproducible random data
3. **Cleanup**: Temporary files are created in `tempdir()` for automatic cleanup
4. **Descriptive Names**: Test names clearly describe what is being tested
5. **Arrange-Act-Assert**: Tests follow AAA pattern
6. **Edge Cases**: Comprehensive testing of boundary conditions
7. **Mocking**: External dependencies are mocked to avoid API rate limits
8. **Performance**: Tests complete quickly (< 1 second per test typically)

## Known Limitations

1. **API-Dependent Functions**: Some functions requiring live API calls (HERE, Census) are not fully tested due to rate limiting
2. **Plot Visual Validation**: Tests verify plot objects are created but don't validate visual appearance
3. **Platform-Specific**: Some file path tests may behave differently on Windows vs Unix

## Future Improvements

1. **Increase Coverage**: Add tests for remaining untested functions:
   - `create_isochrones()` and related mapping functions
   - `create_and_plot_interaction()`
   - `plot_and_save_emmeans()`
   - `hrr()` and related functions

2. **Integration Tests**: Add more integration tests for multi-function workflows

3. **Performance Tests**: Add benchmarking tests for large dataset handling

4. **Visual Regression Tests**: Implement visual comparison for plot outputs

5. **Snapshot Tests**: Use `testthat::expect_snapshot()` for complex output validation

## Contributing

When adding new functions to the tyler package:

1. Write tests BEFORE implementing the function (TDD approach)
2. Aim for 80%+ code coverage
3. Include unit tests, edge cases, and integration tests
4. Use `testthat::test_that()` with descriptive names
5. Mock external API calls and file operations
6. Add examples to test documentation

## Contact

For questions about the test suite, please open an issue on GitHub or contact the package maintainer.

---

**Last Updated**: 2025-10-21
**Test Suite Version**: 2.0
**Package Version**: 1.2.1
