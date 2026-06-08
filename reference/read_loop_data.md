# Read Loop/Roster Dataset

Reads a loop or roster dataset from an Excel file, converts date and
datetime columns, generates a composite UUID (row number + parent UUID),
and converts integer/decimal columns to numeric.

## Usage

``` r
read_loop_data(
  filename,
  kobo_survey,
  sheet,
  loop_name,
  uuid_column = "_submission__uuid",
  submission_time_column = "_submission__submission_time"
)
```

## Arguments

- filename:

  Path to the Excel file containing the loop data.

- kobo_survey:

  A dataframe containing the Kobo survey sheet (used for identifying
  numeric columns).

- sheet:

  Sheet number or name to read from the Excel file.

- loop_name:

  A string label for this loop (e.g., "household_roster", "children").
  This is stored in a `df_name` column.

- uuid_column:

  Name of the parent UUID column in the loop data (default is
  "\_submission\_\_uuid").

- submission_time_column:

  Name of the submission time column in the loop data (default is
  "\_submission\_\_submission_time").

## Value

A dataframe with a composite UUID, dates converted, and numeric columns
cast to numeric.
