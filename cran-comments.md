## R CMD check results

0 errors | 0 warnings | 2 notes

### Note 1: Package suggested but not available for checking: 'oce'

`oce` is listed in `Suggests` and is used only in `auto_tidal_correct()`, which
is wrapped in `\dontrun{}`. The package is available on CRAN but is not
available on all check platforms. The dependency is genuinely optional and the
package functions correctly without it.

### Note 2: unable to verify current time

Standard infrastructure note on some check servers; not related to package code.

## Test environments

- Local macOS (R 4.4.x), `devtools::check(args = c("--as-cran"))`
- win-builder (R-release and R-devel)

## Submission notes

This is version 1.5.0, a new submission.
