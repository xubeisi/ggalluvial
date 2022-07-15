## Test environments

* local OS X install, R 4.0.0 (via `devtools::check()`)
* Rhub (via `rhub::check_for_cran()`)
* win-builder (devel, current, and previous; via `devtools::check_win_*()`)

In response to a previous failed submission, **vdiffr** tests are now skipped on CRAN and when not installed, using `testthat::skip_*()`.

### R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

One comment flagged that examples for one reference page (`stat_alluvium`) took > 5s or > 10s to run. I have trimmed the total number of examples, but each showcases functionality that has previously caused users difficulty.

### Rhub

There were no WARNINGs.

On some platforms, one NOTE flagged the example runtime issue above (with a 5s threshold). On one platform (Fedora Linux, R-devel, clang, gfortran), several NOTEs flagged additional example runtime issues.

On one platform (Ubuntu Linux 16.04 LTS, R-release, GCC), one ERROR was due to **vdiffr** being suggested but not available, and two NOTEs flagged possibly invalid URLs, both of which have been verified.

### WinBuilder

There were no ERRORs. Each run produced the WARNING "checking CRAN incoming feasibility" of unknown cause as well as a NOTE about the runtime issue above. On some runs, one NOTE flagged several last names in the DESCRIPTION as possibly misspelled words. (These have been checked.)

## Downstream dependencies

There are three dependencies on CRAN:

- **easyalluvial**
- **sigminer**
- **immunarch**

There are also four Bioconductor dependencies:

- **CrossICC**
- **projectR**
- **MutationalPatterns**
- **scRepertoire**

`revdepcheck::revdep_check()` on both CRAN and GitHub source code produced no ERRORs, WARNINGs, or NOTEs.
