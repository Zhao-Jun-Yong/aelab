# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Development Commands

```r
# Install dependencies
devtools::install_deps()

# Regenerate documentation (NAMESPACE and man/)
devtools::document()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-ghg_flux.R")

# Full R CMD check (CRAN-style)
devtools::check()

# Build vignette
devtools::build_vignettes()

# Load package for interactive development
devtools::load_all()
```

## Package Architecture

**aelab** is an aquatic ecology lab data processing package with two main workflows:

### GHG Flux Workflow (`R/ghg_flux.R`)
Calculates greenhouse gas flux from chamber measurements:
1. `tidy_ghg_analyzer()` - Import raw data from LI-COR or LGR gas analyzers
2. `convert_time()` - Align analyzer timestamps with real-world time
3. `calculate_regression()` - Compute concentration change slopes via linear regression
4. `calculate_ghg_flux()` - Apply flux formula: F = (S×V×c)/(R×T×A)
5. `convert_ghg_unit()` - Convert between GHG units

### DO Metabolism Workflow (`R/hobo_do.R`)
Calculates dissolved oxygen metabolism (GPP, ER, NEM):
1. `process_hobo()` - Import Onset HOBO U26 logger CSV data
2. `process_weather()` / `process_info()` - Load supporting metadata
3. `combine_hobo()` / `combine_weather()` - Merge datasets
4. `calculate_do()` - Compute GPP, ecosystem respiration, and net ecosystem metabolism
5. `plot_hobo()` - Visualize DO concentration time series

## Key Conventions

- **Timezone**: Default is `Asia/Taipei` for datetime operations
- **Roxygen2**: All exports use `@export`, imports use `@importFrom pkg func`
- **Test fixtures**: Located in `tests/testthat/ref/`
- **Example data**: Raw files in `inst/extdata/`, processed datasets in `data/`
- **CRAN compliance**: Package is published on CRAN; avoid brand names in DESCRIPTION

## Dependencies

Core: tibble, lubridate, dplyr, readxl, openxlsx, ggplot2, readr, tidyr, stringr
