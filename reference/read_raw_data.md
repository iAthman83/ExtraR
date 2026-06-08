# Read Raw Main Dataset

Reads the main raw dataset from an Excel file, converts date and
datetime columns based on the Kobo survey definition, renames key
identifier columns, and converts integer/decimal columns to numeric.

## Usage

``` r
read_raw_data(
  filename,
  kobo_survey,
  extra_date_cols = NULL,
  extra_datetime_cols = NULL,
  sheet = 1
)
```

## Arguments

- filename:

  Path to the Excel file containing the raw data.

- kobo_survey:

  A dataframe containing the Kobo survey sheet (used for identifying
  date and numeric columns).

- extra_date_cols:

  Optional character vector of additional date column names to convert
  (default is NULL). These are appended to the auto-detected "today"
  type columns from the survey.

- extra_datetime_cols:

  Optional character vector of additional datetime column names to
  convert (default is NULL). These are appended to the standard
  `c("start_time", "end_time", "_submission_time")`.

- sheet:

  Sheet number or name to read from the Excel file (default is 1).

## Value

A dataframe with dates converted, identifiers renamed, and numeric
columns cast to numeric.
