# extrar

## Overview

The `extrar` package focuses on streamlining the generation, formatting, and export of complex grouped survey data analysis. It builds around robust pipelines for iteratively processing group variables and generating styled Excel presentation outputs. It has two main components:

1. **Analysis Workflow Pipeline (`run_group_analysis_pipeline`)**: Integrates directly with survey designs and `analysistools` to iteratively apply standard analysis across multiple group variables. It automatically generates and merges distinct baseline UUIDs, creating a comprehensive and combined horizontal dataset.
2. **Formatted Excel Exporting (`format_my_xlsx_variable_x_group` & `my_create_table_variable_x_group`)**: Functions to reshape and pivot the complex survey analysis outputs into a presentation-ready Excel document featuring dynamic percentage formatting, rich table stylings, automated conditional color scaling, and optional logic for grouping/inserting blank rows.

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

You can also include optional metadata columns such as `sector`, `indicator`, and `sub_indicator` — these can be passed through to the output via `extra_columns`.

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
