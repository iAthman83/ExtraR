# Read Kobo Choices Sheet.

Reads and processes the choices sheet from a Kobo XLS form. Column names
are lowercased, rows without a `list_name` are removed, and duplicate
entries are dropped.

## Usage

``` r
read_kobo_choices(filepath, sheet_name = "choices")
```

## Arguments

- filepath:

  Path to the Kobo XLS/XLSX tool file.

- sheet_name:

  Name of the choices sheet (default is "choices").

## Value

A dataframe containing the processed choices sheet with columns
`list_name`, `name`, and `label`.
