# Create Table Variable x Group

function to add sector and indicator to the table for output

## Usage

``` r
my_create_table_variable_x_group(
  results_table,
  analysis_key = "analysis_key",
  value_columns = c("stat", "stat_low", "stat_upp"),
  extra_columns = NULL,
  list_for_excel = FALSE
)
```

## Arguments

- results_table:

  the results

- analysis_key:

  the key

- value_columns:

  columns

- extra_columns:

  optional columns

- list_for_excel:

  internal use

## Value

A pivotted dataframe
