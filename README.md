# extrar

## Overview

The `extrar` package focuses on streamlining the generation, formatting, and export of complex grouped survey data analysis. It builds around robust pipelines for iteratively processing group variables and generating styled Excel presentation outputs. It has two main components:

1. **Analysis Workflow Pipeline (`run_group_analysis_pipeline`)**: Integrates directly with survey designs and `analysistools` to iteratively apply standard analysis across multiple group variables. It automatically generates and merges distinct baseline UUIDs, creating a comprehensive and combined horizontal dataset.
2. **Formatted Excel Exporting (`format_my_xlsx_variable_x_group`)**: Function to reshape and pivot the complex survey analysis outputs into a presentation-ready Excel document featuring dynamic percentage formatting, rich table stylings, automated conditional color scaling, and optional logic for grouping/inserting blank rows.

> **Note:** `extrar` builds on top of the [`presentresults`](https://github.com/impact-initiatives/presentresults) package for label creation and results table formatting.

## Installation

You can install the development version from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("iAthman83/extraR")
```

## Usage

### Step 1: Prepare the List of Analysis (LOA)

Create or load your LOA file. It must contain these columns:

| Column | Description |
|---|---|
| `analysis_type` | Type of analysis (e.g. `prop_select_one`, `mean`) |
| `analysis_var` | The variable to analyse |
| `group_var` | Leave blank — populated automatically by the pipeline |
| `level` | Confidence level, e.g. `0.95` |

**Note on Metadata:** You can include optional metadata columns such as `sector`, `indicator`, and `sub_indicator` directly in your LOA file. These extra columns will be automatically joined to the results when you pass their names via the `extra_columns` argument in Step 2. Look at the examples below for more information.

```r
library(readxl)
loa <- read_excel("path/to/loa.xlsx")

# Define the grouping variables (Overall must come first)
grouping_variables <- c("Overall", "gender", "female_hoh")
```

### Step 2: Run the Analysis Pipeline

`run_group_analysis_pipeline()` loops over every grouping variable and produces a single combined wide dataset.

```r
library(extrar)

group_analysis <- run_group_analysis_pipeline(
  dataset          = my_dataset,
  loa              = loa,
  group_variables  = grouping_variables,
  tool_survey      = tool_survey,
  tool_choices     = tool_choices,
  weight_column    = "weights",
  strata_column    = "sample_location",
  extra_columns    = c("sector", "indicator", "sub_indicator")  # optional
)

# Access the combined results
group_analysis$combined_results
```

### Step 3: Export a Formatted Excel Output

`format_my_xlsx_variable_x_group()` takes the combined results and writes a styled Excel file.

- If `insert_empty_rows = TRUE`, a blank separator row is inserted after every block of related questions.
- `empty_rows_col` controls which column defines the grouping for blank row insertion. It defaults to `"analysis_var"` (renamed to `"question"` in the output), since all rows for a given question share the same value.

```r
format_my_xlsx_variable_x_group(
  table_group_x_variable = group_analysis$combined_results,
  file_path              = "output/analysis_output.xlsx",
  insert_empty_rows      = TRUE,
  empty_rows_col         = "analysis_var",  # groups rows by question block
  overwrite              = TRUE
)
```

## Examples

Below are examples of how the LOA translates into the final formatted Excel output.

### 1. Basic Analysis (Without Extra Columns)

When running the pipeline with standard analysis types and no extra metadata columns:

<p align="center">
  <img src="man/figures/loa_basic.png" width="45%" alt="LOA Without Extra Columns" />
  &nbsp;&nbsp;&nbsp;
  <img src="man/figures/output_basic.png" width="45%" alt="Formatted Output Without Extra Columns" />
</p>
<p align="center"><em>Left: Basic LOA input. Right: Resulting formatted Excel output.</em></p>

### 2. Advanced Analysis (With Extra Columns)

When passing metadata columns (like `sector` and `indicator`) via the `extra_columns` argument, they are neatly preserved and arranged in the final output block:

<p align="center">
  <img src="man/figures/loa_extra.png" width="45%" alt="LOA With Extra Columns" />
  &nbsp;&nbsp;&nbsp;
  <img src="man/figures/output_extra.png" width="45%" alt="Formatted Output With Extra Columns" />
</p>
<p align="center"><em>Left: LOA with sector/indicator columns. Right: Output featuring those extra columns mapped correctly.</em></p>
