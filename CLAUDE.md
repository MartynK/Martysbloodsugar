# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R package for analyzing blood sugar data from xDrip, created by a health-conscious 30-something male. 
The project combines continuous glucose monitoring (CGM) data with capillary glucose measurements and sleep 
data to analyze glucose patterns.

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
- File naming conventions standardized to .R (previously inconsistent .r vs .R)

## Known Issues & Development Notes

### Content Structure Issues
- **Article separation needed**: Current report mixes methodological details with practical findings
  - Need to separate into: 1) Technical/methodological article, 2) Practical findings article
  - Spline analysis with food effects (child3.qmd:361-445) should be backbone of practical section
- **Missing conclusions section** in main report - needs summary of key findings and takeaways
- **GitHub Pages deployment broken**: Images not displaying when moved online

### Data Quality Issues  
- **Hungarian data entries**: CSV files contain mixed Hungarian/English food/activity labels
  - Examples: "tökfőzelék" (pumpkin stew), "ebéd" (lunch), "gyúrás" (workout)
  - Need translation/standardization for international readability
- **Deprecated iteration files**: iter1.R contains commented-out CSV processing code
  
### File Organization
- **Duplicate files identified**: 
  - `Inst/sensor_resid_corr.R` duplicates `Inst/article/backend/sensor_resid_corr.R`
  - Should consolidate or clearly document purpose differences

## R Style Guide Options

### General policies 

-  Readability trumps performance and even functionality. (Tasks are usually quick and handle small objects.)
-  Quick operations should be kept within a file; if a task is long (2+ minutes), then it merits its own R script with a save() or save.image() at the end.
-  The train-of-thought within a project should be kept via naming the scripts (eg. numbering prefixes like 01_intro.r 02_desc_stats.r etc.) or related files (child1.qmd, child2.qmd...)
-  Length of a single file should be kept <500 lines (ideally 400 lines). Use save.image() and load() or source() appropriately.
-  After a long script, the end state or the relevant object should be saved under the data/ folder as an .Rdata or a .rda object denting the file name (eg. end_state_iter8_mixedmod_stuff.RData)
-  Each script should start with a comment briefly explaining what the script does. Then Library calls and helper function source()-ing as appropriate.
-  Be afraid of state change. If a state change occurs, try to give the changed object a different name. Try to identify common needs for objects (eg. data wrangling wise) at the beginning tand try to construct objects which are then used several times throughout the processes. Also don't skimp on simple 'throwaway' objects if some modification is required for a single task and would most likely not be needed elsewhere.
-  Prefer the long format for data; be aware that input data from the database itself may be in the wide format. In those cases validate the transformations.
  


### Pipe Operators

-  ✅ Use magrittr pipes %>% over native pipes |> 
-  Chain operations with pipes when >3 steps or when aesthetically better >=2
-  Break long pipe chains at logical points

### Iteration & Functional Programming

-  ✅ Use for loops over purrr/map functions especially if <3000 iterations are expected
-  Use vectorized operations when possible
-  Aim for pre-allocating vectors/lists in loops

### Naming Conventions - Variables

-  ✅ Use snake_case for all variables 
-  Use descriptive variable names (>3 characters)
-  Avoid abbreviations unless well-known

### Naming Conventions - Objects

-  ✅ Prefix data frames / tibbles with dat_
-  ✅ Use chunk-based naming: dat_chunkname_locf if using an object in a single chunk only
-  Prefix models with mod_ or model_
-  Prefix plots with fig_ or plot_
-  Prefix functions with fun_ or no prefix
-  Use lst_ for lists, vec_ for vectors


### Naming Conventions - Functions

-  Use verb_noun pattern for function names
-  Use snake_case for function names, capitalize if function is Vectorized
-  Start with action verbs (get_, create_, calculate_)
-  End with data type if appropriate (_df, _list, _plot)

### Code Organization

-  ✅ Descriptive chunk names in R Markdown, aim for uniqueness (you do this well)
-  Use # for major sections, ## for subsections, ### to keep things organized within a subsection
-  Load all libraries at top of script
-  Define constants/parameters at top after libraries, in ALL_CAPS
-  Use consistent indentation (2 spaces vs 4 spaces)

### Assignment & Operators

-  Use <- for assignment (R standard)
-  Space around operators: x + y not x+y
-  No space before comma, space after: c(1, 2, 3)
-  Always elaborate for if statements eg. if (cond == TRUE) {...

### Line Length & Formatting

-  Maximum 80 characters per line
-  Break long function calls across lines
-  Align parameters in multi-line function calls
-  Use trailing commas in multi-line lists

### Comments & Documentation

-  Use # for inline comments with space after
-  Use #' for roxygen2 documentation
-  Write comments explaining "why" and "what" too. Use copious amounts of comments.
-  Use TODO/FIXME/NOTE for code annotations
-  Document all function parameters and returns

### Error Handling & Defensive Programming

-  Always check for NULL/missing data before operations
-  Use stop() for critical errors, warning() for non-critical, and message() for good-to-know info
-  Validate function inputs at start of function
-  Use meaningful error messages
-  Use try()/tryCatch() for operations that might fail, especially if nested within a loop

## Key considerations

- The main goal is to produce 'Reports' from input data.
- 'Reports' mainly consist of text, figures and tables, in a .qmd ecology.
- I prefer a structure where figures and tables are named and referenced in the Report.
- Reports are generated using Quarto from inst/report/report.qmd

### When to Refactor Large Files
  - Break files when they exceed ~400-500 lines
  - Split at logical section boundaries (e.g., after Primary Endpoint,
  before ROM analyses)
  - Each child document should end with save.image(file =
  here::here("inst", "report", "state_after_childX.RData"))
  - Next child document should start with load(here::here("inst", "report",
   "state_after_childX.RData"))

## R packages for Ubuntu

Ubuntu Compiled R Package Libraries for Cross-Environment Compatibility:
To enable full statistical analysis capabilities including mixed-effects
modeling and advanced plotting, Ubuntu-compiled R packages are stored at
@/mnt/c/Users/mrkma/OneDrive/DKM/Stats_R/R/_Libraries/_Ubuntu_packages/
and accessed via .libPaths() configuration. This directory contains over
200 compiled R packages including critical dependencies that require
system-level compilation (nloptr, lme4, effects, emmeans, zoo,
RcppArmadillo) which cannot be easily installed in restricted environments
 due to cmake and system library requirements. The Ubuntu packages are
fully compatible across similar Linux environments and can be activated by
 prepending the library path: .libPaths(c('/mnt/c/Users/mrkma/OneDrive/DKM
/Stats_R/R/_Libraries/_Ubuntu_packages', .libPaths())) before loading
packages. This approach enables complete statistical workflows including
lme4::lmer() mixed-effects models, emmeans::emmeans() contrasts,
zoo::na.locf() last-observation-carried-forward imputation, and
effects::predictorEffects() visualization without requiring admin
privileges or system-level package compilation. The compiled libraries
maintain full functionality across different computational environments
while preserving reproducibility and ensuring consistent statistical
analysis capabilities between development scripts (inst/iter1.r,
inst/iter2.r) and production report generation workflows.

## Development Workflow

### Essential Setup
Every analysis script should assume this setup first:
r
source(here::here("inst", "function", "load_stuff.r"))


This single command:
- Loads common packages (dplyr, ggplot2, lubridate, nlme, splines, etc.)
- Sources all functions from R/ directory
- Loads preprocessed data from data/meteostat_data.Rdata
- Loads backend models and results from inst/function/backend/


