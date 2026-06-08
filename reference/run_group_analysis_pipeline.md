# Run Full Group Analysis Pipeline

This function iterates over a list of group variables, performs the
standard analysis for each group, adds labels, and pivots the results.
For the first group in the list (usually 'Overall'), it assigns a unique
UUID to each result row. For all subsequent group variables, it matches
the results back to the original UUIDs using `analysis_var`,
`analysis_type`, and `analysis_var_value`. This avoids the need to
repeatedly read and write to an Excel file during the loop, producing
one massive dataset spanning all groups.

## Usage

``` r
run_group_analysis_pipeline(
  dataset,
  loa,
  group_variables = c("Overall"),
  tool_survey,
  tool_choices,
  weight_column = "weights",
  strata_column = "sample_location",
  value_columns = c("stat", "n", "n_total"),
  extra_columns = NULL
)
```

## Arguments

- dataset:

  The survey dataset.

- loa:

  List of Analysis (LOA) as a data frame. This is typically loaded from
  an Excel file containing the full list of variables to analyse, their
  analysis types, and metadata columns. Common optional metadata columns
  that can be passed via `extra_columns` include `sector`, `indicator`,
  and `subset_indicator`.

- group_variables:

  A character vector of group variables to iterate over. Defaults to
  c("Overall").

- tool_survey:

  The Kobo tool survey sheet.

- tool_choices:

  The Kobo tool choices sheet.

- weight_column:

  The column name for survey weights. Defaults to "weights".

- strata_column:

  The column name for survey strata. Defaults to "sample_location".

- value_columns:

  Columns to extract from results. Defaults to c("stat", "n",
  "n_total").

- extra_columns:

  Extra LOA metadata columns to carry through to the output (e.g.
  `c("sector", "indicator", "subset_indicator")`). Defaults to NULL.

## Value

A list containing `combined_results` (the merged dataset spanning all
group variables) and `uuid_table` (the baseline UUID mappings).
