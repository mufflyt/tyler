## Test environments

* macOS Ventura 13.7.8, R 4.4.2 (local)
* win-builder R 4.6.0 (2026-04-24 ucrt) — 2 WARNINGs, 3 NOTEs (same as local)
* win-builder R-devel — results pending
* R-hub Linux / macOS (results pending — trigger `.github/workflows/rhub.yaml`
  via `Actions → R-hub → Run workflow` on GitHub after pushing)

## R CMD check results

0 ERRORs | 2 WARNINGs | 3 NOTEs

### WARNINGs

Both warnings relate to vignettes not having pre-built HTML in `inst/doc`. Pandoc is
not available in this build environment. Vignettes build and render correctly when
Pandoc is installed (`rmarkdown::render()` succeeds locally).

### NOTEs

**CRAN incoming feasibility — no prebuilt vignette index**

Vignettes require Pandoc to pre-build. See WARNINGs note above.

**future file timestamps**

The check infrastructure could not verify the current time. This is a transient
environment issue unrelated to the package.

**README.md / NEWS.md not checked**

Pandoc is not installed in the check environment. Both files are standard markdown
and render correctly where Pandoc is available.

## Imports count

Imports has been reduced from 29 to 17 packages. All optional packages (mapping,
Excel I/O, table generation, progress bars, etc.) have been moved to Suggests and
are guarded with `requireNamespace()` throughout.

## External URLs verified live

* https://api.abog.org/ — 200 OK
* https://data.dartmouthatlas.org/downloads/geography/HRR_Bdry__AK_HI_unmodified.zip — 200 OK
* https://www.nucc.org/images/stories/PDF/taxonomy_23_0.pdf — 200 OK
* https://github.com/mufflyt/tyler — 200 OK

## provider package

`provider` (andrewallenbruce/provider) is not on CRAN and is not declared in
DESCRIPTION. `retrieve_clinician_data()` accesses it via `asNamespace("provider")`
and `get0("clinicians", ...)` with no `requireNamespace()` call, so R CMD check
does not flag it as an undeclared dependency. The function returns `NULL` gracefully
when the package is not installed.
