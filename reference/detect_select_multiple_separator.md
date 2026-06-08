# Detect Select Multiple Separator Dynamically

Scans the columns of the active clean_data/raw_data dataset to determine
whether the separator for select_multiple choice columns is a dot (`.`)
or a slash (`/`).

## Usage

``` r
detect_select_multiple_separator(ref_name, clean_data = NULL)
```

## Arguments

- ref_name:

  The name of the select_multiple question.

- clean_data:

  The active dataset. If `NULL`, looks for `clean_data` or `raw_data` in
  the environment.

## Value

A character string, either `"."` or `"/"`.
