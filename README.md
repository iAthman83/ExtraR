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
