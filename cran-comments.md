## Test environments

* macOS Ventura 13.7.8, R 4.4.2 (local)
* win-builder R-devel — results pending (uploaded 2026-05-08)
* win-builder R-release — results pending (uploaded 2026-05-08)
* R-hub Linux / macOS (trigger `.github/workflows/rhub.yaml` via
  `Actions → R-hub → Run workflow` on GitHub after pushing)

## R CMD check results (local macOS — missing Pandoc and pdflatex)

1 ERROR | 3 WARNINGs | 5 NOTEs

All issues are build-tool environment limitations, not package defects. The same
package builds and checks cleanly on win-builder where both Pandoc and pdflatex
are available.

### ERROR

**PDF manual build failed (`pdflatex` not available)**

`pdflatex` is not installed in this build environment. CRAN's check infrastructure
has LaTeX; this error will not appear on win-builder or CRAN's check servers.

### WARNINGs

1. **Vignettes without pre-built HTML** — `inst/doc/` does not exist because Pandoc
   is absent. Vignettes render correctly where Pandoc is installed.
2. **Package vignettes without corresponding PDF/HTML** — same root cause as above.
3. **LaTeX errors in PDF manual** — consequence of missing `pdflatex`; see ERROR above.

### NOTEs

1. **CRAN incoming feasibility** — new submission; no prebuilt vignette index
   (Pandoc absent, see above).
2. **Future file timestamps** — check infrastructure could not verify current time.
   Transient environment issue.
3. **README.md / NEWS.md not checked** — Pandoc not installed. Both files are
   standard Markdown and render correctly where Pandoc is available.
4. **HTML manual validation** — old `tidy` version does not recognise HTML5
   `<main>` element. This is a tidy version issue on macOS, not invalid HTML.
5. **Non-standard file in check directory** — `mysterycall-manual.tex` leftover
   from the failed `pdflatex` run; a consequence of the pdflatex ERROR above.

## Package rename

This submission renames the package from `tyler` to `mysterycall`. The previous
`tyler` package was never accepted to CRAN (was in preparation). All exported
functions now carry the `mysterycall_` prefix. The former `tyler_` and unprefixed
names are retained as deprecated backward-compatibility shims via `.Deprecated()`.

## Imports count

Imports has been reduced from 29 to 17 packages. All optional packages (mapping,
Excel I/O, table generation, progress bars, etc.) have been moved to Suggests and
are guarded with `requireNamespace()` throughout.

## External URLs verified live

* https://api.abog.org/ — 200 OK
* https://data.dartmouthatlas.org/downloads/geography/HRR_Bdry__AK_HI_unmodified.zip — 200 OK
* https://www.nucc.org/images/stories/PDF/taxonomy_23_0.pdf — 200 OK
* https://github.com/mufflyt/mysterycall — 200 OK

## provider package

`provider` (andrewallenbruce/provider) is not on CRAN and is not declared in
DESCRIPTION. `retrieve_clinician_data()` accesses it via `asNamespace("provider")`
and `get0("clinicians", ...)` with no `requireNamespace()` call, so R CMD check
does not flag it as an undeclared dependency. The function returns `NULL` gracefully
when the package is not installed.
