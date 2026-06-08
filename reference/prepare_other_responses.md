# Prepare Other Responses Dataframe

Combines other responses from all sheets/loops into a single dataframe
ready for use with
[`save_other_responses()`](https://iathman83.github.io/ExtraR/reference/save_other_responses.md).
The function dynamically includes extra columns (e.g., enumerator,
sample_area) if provided.

## Usage

``` r
prepare_other_responses(
  raw_data,
  other_db,
  kobo_choices,
  raw_loops = NULL,
  extra_columns = NULL,
  uuid_column = "_uuid"
)
```

## Arguments

- raw_data:

  Main raw dataset dataframe (always required).

- other_db:

  Dataframe from
  [`get_other_db()`](https://iathman83.github.io/ExtraR/reference/get_other_db.md)
  with question metadata.

- kobo_choices:

  Dataframe of Kobo choices sheet.

- raw_loops:

  Optional list of loop/roster dataframes (default is NULL). Pass as a
  named or unnamed list, e.g.,
  `list(household_roster = df1, children = df2)` or `list(df1, df2)`.

- extra_columns:

  Optional character vector of additional column names to include from
  the raw data (e.g., `c("enumerator", "sample_area")`). Default is
  NULL.

- uuid_column:

  Name of the uuid column in raw_data (default is "\_uuid").

## Value

A dataframe formatted for
[`save_other_responses()`](https://iathman83.github.io/ExtraR/reference/save_other_responses.md).
