# Add Recoding of select_one Other Response to Cleaning Log

Logs the recoding of a `select_one` question's "other" response. Looks
up the choice name from the selected label in the choices sheet, logs
the removal of the text response, and updates the parent selection.

## Usage

``` r
add_to_cleaning_log_other_recode_one(x)
```

## Arguments

- x:

  A list or data frame row representing the "other" response record.

## Value

NULL. Updates the global variable `cleaning_log_other` in-place.
