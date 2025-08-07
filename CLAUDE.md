# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R package for analyzing blood sugar data from xDrip, created by a health-conscious 30-something male. The project combines continuous glucose monitoring (CGM) data with capillary glucose measurements and sleep data to analyze glucose patterns.

## Common Development Commands

### Package Development
```r
# Test the package
devtools::test()

# Build documentation from roxygen comments
devtools::document()

# Build vignettes (refresh .rmd content)
devtools::build_vignettes()

# Build package website with pkgdown
pkgdown::build_site()
```

### Report Generation
The main analysis is in `Inst/article/report.qmd` which uses Quarto to generate HTML and Word documents:
```bash
# Render the main report (from the Inst/article/ directory)
quarto render report.qmd
```

## Architecture

### Data Pipeline Architecture
1. **Raw Data Sources**:
   - xDrip SQLite databases in `Inst/extdata/Xdrip/` (zipped exports)
   - Sleep time data in Excel format
   - Factor recoding tables and unreliable interval definitions

2. **Data Processing Flow**:
   - `R/load_all_Rdata.r`: Utility to load multiple .Rdata files
   - `Inst/article/functions/wrangling.r`: Main data processing from SQLite
   - `Inst/article/backend/`: Pre-computed expensive operations saved as .Rdata
   - Core package function: `read_xdrip_data()` (documented in `R/wrangling.R`)

3. **Analysis & Reporting**:
   - `Inst/article/report.qmd`: Master report file
   - `Inst/article/child*.qmd`: Modular report sections
   - `Inst/article/functions/load_stuff.r`: Setup and library loading
   - `Vignettes/Report.Rmd`: Package vignette

### Key Data Transformations
- Converts xDrip timestamp format (milliseconds) to R datetime
- Transforms glucose values from mg/dL to mmol/L (division by 18.016)
- Filters unreliable measurement periods based on `unreliable_interval.xlsx`
- Joins CGM data with capillary measurements and sleep data using fuzzy matching
- Creates interpolation functions for continuous glucose estimates

### File Organization
- `R/`: Package functions (main: `read_xdrip_data()`)
- `Inst/extdata/`: Read-only data files (SQLite exports, Excel sheets)
- `Inst/article/`: Main analysis pipeline and reporting
- `Inst/iter*.R`: Development iterations/scratch files
- `Tests/testthat/`: Unit tests
- `man/`: Generated documentation (via roxygen2)

## Testing Strategy
Unit tests in `Tests/testthat/test-read_xdrip_data.r` verify:
- Correct data frame structure and column types
- Expected columns: DAY, TIME, UDT_CGMS, BG_LEVEL, REMARK, datetim, glucose, remark_factor, delta_time_remark
- Date/time parsing and numeric conversions

## File Directory

### Package Structure Files
- **DESCRIPTION**: Package metadata defining dependencies, author, and basic info
- **NAMESPACE**: Auto-generated roxygen2 file for exported functions (currently minimal)
- **LICENSE.md**: MIT license text for the package
- **README.md**: Basic project description and folder structure overview
- **Martysbloodsugar.Rproj**: RStudio project configuration file
- **_pkgdown.yml**: Configuration for generating package website with pkgdown
- **CLAUDE.md**: This guidance file for Claude Code development assistance

### Core Package Functions
- **R/load_all_Rdata.r**: Utility function to bulk load .Rdata files from a directory
- **man/read_xdrip_data.Rd**: Auto-generated documentation for main data processing function

### Development Iterations
- **Inst/iter1.R**: Early commented-out iteration of xDrip CSV data processing (deprecated)
- **Inst/iter2_plotting.r**: Plotting script that loads pre-processed data and creates glucose visualizations
- **Inst/iter3_sqlite.r**: SQLite-based data processing connecting to xDrip database exports
- **Inst/wrangling.R**: Main xDrip data processing function with roxygen2 documentation (package function)

### Analysis & Reporting
- **Inst/article/report.qmd**: Master Quarto report combining all analysis sections
- **Inst/article/child1_preamble.qmd**: Introduction section describing CGM technology and study setup
- **Inst/article/child2.qmd**: Calibration analysis comparing sensor readings with fingertip measurements  
- **Inst/article/child3.qmd**: Statistical modeling section with spline-based glucose prediction models
- **Inst/article/functions/load_stuff.r**: Library loading and knitr setup for report generation
- **Inst/article/functions/wrangling.r**: SQLite data processing pipeline for newer database exports

### Computational Backend
- **Inst/article/backend/estim_pi.r**: Monte Carlo simulation for π estimation (computational demo)
- **Inst/article/backend/sensor_resid_corr.r**: Sensor residual correlation analysis requiring pre-processed data
- **Inst/acf_spline.r**: Autocorrelation function simulation with spline models
- **Inst/sensor_resid_corr.r**: Duplicate sensor analysis script (same functionality as backend version)

### Testing & Documentation
- **Tests/testthat/test-read_xdrip_data.r**: Unit tests validating data structure and column types
- **Vignettes/Report.Rmd**: R Markdown vignette providing project overview and output
- **docs/pkgdown.yml**: Build metadata for generated website

## Important Notes
- The package uses `here::here()` for robust path handling
- Heavy computational work is pre-computed and saved in `backend/` as .Rdata files
- Reports are bilingual (English/Hungarian comments in some files)
- Uses SQLite for efficient handling of large xDrip datasets
- Website deployment via GitHub Pages with pkgdown
- Contains duplicate/deprecated files showing development evolution
- Inconsistent file naming conventions (.r vs .R) across the project