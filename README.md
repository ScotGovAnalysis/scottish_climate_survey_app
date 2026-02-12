# Scottish Climate Survey Dashboard

A Shiny application for analyzing weighted survey data with grouping and statistical comparisons.

## Project Structure

```
R/
  ├── app.R                    # Main application entry point
  ├── global.R                 # Global settings and library loading
  ├── ui.R                     # User interface definition
  ├── server.R                 # Server logic
  ├── functions/               # Modular function files
  │   ├── utils.R              # General utility functions
  │   ├── data_io.R            # Data reading and validation
  │   ├── factors.R            # Factor creation and manipulation
  │   ├── ui_helpers.R         # UI generation helpers
  │   ├── analysis.R           # Survey data analysis functions
  │   ├── plotting.R           # Plotting and visualization
  │   ├── statistics.R         # Statistical tests
  │   └── grouped_analysis.R   # Multi-select question analysis
├── tests/                   # Unit tests
│   ├── testthat.R           # Test runner
│   └── testthat/            # Individual test files
│       ├── test-utils.R
│       ├── test-factors.R
│       └── ...
└── inst/
     └── tidymodels_test_lasso.R
     └── tidymodels_test_lasso_smote_cluser.R
└── src/
└── README.md                # This file
```

## Installation

1. Install required packages:

```r
install.packages(c(
  "shiny", "readxl", "dplyr", "tidyr", "stringr", "forcats",
  "ggplot2", "DT", "scales", "survey", "tibble", "testthat"
))
```

2. Optional: Install RColorBrewer for enhanced color palettes:

```r
install.packages("RColorBrewer")
```

## Running the Application

```r
# From the project root directory
shiny::runApp()
```

Or simply run `app.R` in RStudio.

## Running Tests

```r
# Run all tests
testthat::test_dir("tests/testthat")

# Or use devtools
devtools::test()
```

## Features

### 1. Plots - Raw
- Weighted proportion plots for single questions
- Primary and optional secondary grouping variables
- Customizable Likert color palettes
- Exclude specific response levels (Don't know, Prefer not to say, etc.)
- Download plots as JPG and tables as CSV

### 2. Plots - Grouped
- Analysis of multi-select questions (@ prefixed variables)
- Heatmap visualization of selection rates by group
- Significance testing with Rao-Scott chi-squared tests
- Significance markers (★) for notable differences

### 3. Statistics
- Chi-squared tests for overall differences across groups
- Pairwise Wald comparisons with Bonferroni correction
- Custom grouping of response levels
- Custom grouping of demographic groups

## Data Format

The application expects an Excel workbook (.xlsx) with three sheets:

### Data
Contains the raw survey responses with one row per respondent.

### Variable Labels
Two columns:
- `Variable`: Variable name (must match Data columns)
- `Label`: Human-readable label

### Value Labels
Three columns:
- `Variable`: Variable name
- `Value`: Coded value (e.g., "1", "2")
- `Label`: Human-readable label for the value

## Function Organization

Functions are organized by purpose for easy testing and maintenance:

- **utils.R**: General utilities (null coalescing, filename sanitization)
- **data_io.R**: Reading and validating Excel workbooks
- **factors.R**: Creating factors with value labels, collapsing levels
- **ui_helpers.R**: Building UI elements (dropdowns, headers)
- **analysis.R**: Preparing data for analysis, creating survey designs
- **plotting.R**: Creating plots and tables
- **statistics.R**: Statistical tests (chi-squared, pairwise comparisons)
- **grouped_analysis.R**: Multi-select question analysis and visualization

## Testing

Unit tests are provided for core functions. Tests are organized by function file:

- `test-utils.R`: Tests for utility functions
- `test-factors.R`: Tests for factor manipulation
- Add more test files as needed for comprehensive coverage

To add new tests:

1. Create a test file in `tests/testthat/` named `test-[function_file].R`
2. Follow the naming convention: `test_that("description", { ... })`
3. Run tests with `testthat::test_dir("tests/testthat")`

## Development Notes

- Survey designs use the `survey` package for weighted analysis
- Negative codes in numeric variables are treated as missing
- The app supports files up to 100 MB
- Lonely PSUs are adjusted automatically in survey designs

## Other non-app files
The folder inst/ contains analysis scripts. Lasso model prediction of qclimate1 and heirarchical clustering of predictors. The folder src/ contains other analysis scripts
