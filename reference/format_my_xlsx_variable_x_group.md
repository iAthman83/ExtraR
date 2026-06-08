# Format XLSX Variable by Group

This function formats an existing data table and writes it to an Excel
workbook with a variable-by-group layout, applying specific styles such
as percentage formatting, background colors, and conditional color
scales. It references all styles and row-insertion steps internally,
producing a complete formatted file in one call.

## Usage

``` r
format_my_xlsx_variable_x_group(
  table_group_x_variable,
  file_path = NULL,
  table_name = "variable_x_group_table",
  value_columns = c("stat", "stat_low", "stat_upp"),
  total_columns = NULL,
  readme_sheet_name = "readme",
  table_sheet_name = "variable_x_group_table",
  overwrite = FALSE,
  insert_empty_rows = FALSE,
  empty_rows_col = "analysis_var"
)
```

## Arguments

- table_group_x_variable:

  A data frame or list containing the data to be formatted.

- file_path:

  Character string for the output Excel file path. Defaults to NULL
  (returns workbook).

- table_name:

  Character string indicating the name of the table if the input is a
  list.

- value_columns:

  Character vector matching value column headers.

- total_columns:

  Character vector matching total column headers.

- readme_sheet_name:

  Name of the readme sheet to be created.

- table_sheet_name:

  Name of the tabular data sheet to be created.

- overwrite:

  Logical indicating whether to overwrite existing Excel files.

- insert_empty_rows:

  Logical. If TRUE, empty rows will be inserted based on the
  `empty_rows_col`.

- empty_rows_col:

  Character string. The column name used for grouping when inserting
  empty rows.

## Value

An `openxlsx` workbook object if `file_path` is NULL. Otherwise, it
writes the workbook to `file_path`.
