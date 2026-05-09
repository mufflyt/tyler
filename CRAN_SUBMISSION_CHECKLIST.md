# CRAN Submission Checklist for tyler

This checklist is tailored to the current repository and is intended to
make CRAN submission reproducible.

## 1) Preflight metadata checks

Confirm `DESCRIPTION` fields are final for release (especially
`Version`, `Date`, `Title`, `Description`, `URL`, `BugReports`).

Ensure `NEWS.md` has a release section matching the target version/date.

Verify maintainer email is monitored for CRAN correspondence.

## 2) Keep the source tarball CRAN-clean

Top-level analysis/ops markdown files should not ship in the CRAN
tarball.

Verify `.Rbuildignore` excludes internal operational markdown files.

Re-run source build and inspect tarball contents.

## 3) Run checks in a clean environment

Run locally (or CI with R installed):

``` bash
R CMD build .
R CMD check --as-cran tyler_*.tar.gz
```

Recommended additional checks:

``` r

# in R
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("rcmdcheck", quietly = TRUE)) install.packages("rcmdcheck")

devtools::check(cran = TRUE)
rcmdcheck::rcmdcheck(args = c("--as-cran"), error_on = "warning")
```

## 4) Reverse dependency and runtime-sensitive validation

Because this package has API-facing and geospatial workflows:

Validate core paths without optional packages installed.

Validate messaging when optional dependencies are missing.

Run representative end-to-end tests with real API keys outside CRAN.

## 5) Submission artifacts

Generate/update `cran-comments.md` before upload.

Include platform/R-version check summary and NOTEs rationale.

Submit with `devtools::release()` or CRAN web form.

Template for `cran-comments.md`:

``` md
## Test environments
- local macOS, R x.y.z
- local Ubuntu, R x.y.z
- win-builder (devel/release/oldrelease)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
No reverse dependencies.

## Notes
(Explain any remaining NOTE clearly and why it is acceptable.)
```

## 6) After acceptance

Tag release in git.

Update pkgdown site.

Announce release notes.
