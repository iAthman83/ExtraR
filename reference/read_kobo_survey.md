# Read Kobo Survey Sheet

Reads and processes the survey sheet from a Kobo XLS form. Column names
are lowercased, rows without a name are removed, and `q_type` and
`list_name` columns are extracted from the `type` column.

## Usage

``` r
read_kobo_survey(filepath, sheet_name = "survey")
```

## Arguments

- filepath:

  Path to the Kobo XLS/XLSX tool file.

- sheet_name:

  Name of the survey sheet (default is "survey").

## Value

A dataframe containing the processed survey sheet with added `q_type`
and `list_name` columns.
