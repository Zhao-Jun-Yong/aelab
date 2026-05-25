## Resubmission — v1.1.3 (2nd attempt)

This fixes the example error flagged in the CRAN incoming pre-test.

### Fix for example error

`calculate_regression()` example previously hardcoded the reference datetime as
`as.POSIXct("2023-05-04 09:16:15", tz = "Asia/Taipei")`. On CRAN's Debian/Windows
servers (local timezone UTC), the `n2o` dataset's `date_time` column (stored with
`tzone = ""`) was parsed 8 hours off by `lubridate::ymd_hms()`, so the time-window
filter returned 0 rows and `lm()` received all-NA input. The reference time is now
derived from `n2o_converted$real_datetime[1]`, making the example timezone-independent.

### Changes since v1.1.2

* `sig_labels()` (new): runs Kruskal-Wallis + Dunn post-hoc tests per facet
  level and returns a `geom_text`-ready data frame with compact letter display
  (CLD) annotations. Facets where KW p ≥ `alpha` are dropped silently.
* `ks_test()` (fix): group names containing hyphens no longer corrupt CLD
  output. Names are sanitised to dots before `dunnTest`/`cldList` and restored
  in the output labels and comparison strings.

### Notes on `\dontrun` in examples

Plot function examples (`plot_point`, `plot_line`, `plot_box`, `plot_bar`,
and related scale/palette functions) are wrapped in `\dontrun{}` because they
set `family = "Century Gothic"` in the ggplot2 theme. This system font is not
available in the PostScript font database on check servers, which causes a hard
error (not merely a warning) during example rendering. The examples are
syntactically correct and functional on systems where the font is installed.

### R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: "unable to verify current time" — network access restricted in check
  environment; not a package issue.
