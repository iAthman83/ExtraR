# extrar

## Overview

The `extrar` package provides powerful tools for standardizing dataset structures and preparing "other" text responses for recoding. It also takes the headache out of complex survey data analysis by automatically processing multiple grouping variables at once and seamlessly mapping metadata (like sectors and indicators) straight from your List of Analysis (LOA) into the final output.

It has three main components:

1. **Analysis Workflow Pipeline (`run_group_analysis_pipeline`)**: Integrates directly with survey designs and `presentresults` to iteratively apply standard analysis across multiple group variables. It automatically generates output from all the analysis types listed in your LOA file.
2. **Formatted Excel Exporting (`format_my_xlsx_variable_x_group`)**: Function to reshape and pivot the complex survey analysis outputs into a presentation-ready Excel document.
3. **Data Prep & Log Generation**: A suite of tools built to parse standard data streams (`read_raw_data`, `read_loop_data`) by dynamically applying correct typecasting using your Kobo tool, and extracting, merging, and exporting "other" text responses into a structured Excel recode file (`save_other_responses`).

> **Note:** `extrar` package builds on top of the following packages: [`cleaningtools`](https://github.com/impact-initiatives/cleaningtools), [`presentresults`](https://github.com/impact-initiatives/presentresults)

## Installation

You can install the development version from GitHub with:
```r
# install.packages("devtools")
devtools::install_github("iAthman83/extraR")
```

## Data Preparation & Cleaning

`extrar` includes built-in functions designed to help you quickly pull raw data and configure your recode sheets before entering the main analysis pipeline. 

### Safely Read and Standardize Data (`read_raw_data` & `read_loop_data`)
Instead of manually mutating every single column, these tools read your data while using your original `kobo_survey` object to auto-detect integer, decimal, date, and datetime columns, fixing them instantly upon import:
* **`read_raw_data`**: Standardizes the main dataset, ensures UUID columns are aligned, and optionally adds extra dates or times missing from the standard format.
* **`read_loop_data`**: Pulls in roster sheets and generates a robust composite UUID (`[row_number]_[parent_uuid]`) to prevent primary key merging issues in loops.

### Handling "Other" Responses (`save_other_responses`)
Preparing text responses for cleaning log translations can be extremely tedious. The "other" responses suite connects all "other" text fields from both your main data and loop datasets into one centralized recode sheet:
```r
# 1. Map out which 'text' questions correspond to 'other' inputs 
other_labels <- get_other_labels(kobo_survey)

# 2. Match those specific choices with the dropdown elements from your Kobo tool 
other_db <- get_other_db(kobo_survey, kobo_choices, other_labels)

# 3. Pull all actual raw responses, merge them, and optionally attach extra metadata columns
other_responses <- prepare_other_responses(
  raw_data, 
  other_db, 
  kobo_choices, 
  raw_loops = list(loop_data1, loop_data2), # Include loop data where necessary
  extra_columns = c("enumerator_id", "governorate")
)

# 4. Generate a heavily formatted Excel workbook complete with column tracking, colors, and dynamic dropdown logic validations
save_other_responses(other_responses, save_location = "output/", other_db = other_db)
```

## Data Analysis

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
