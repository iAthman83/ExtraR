# Add Recoding of select_multiple Other Response to Cleaning Log

Logs the recoding of a `select_multiple` question's "other" response.
Looks up the choice name(s) for the selected labels, removes the "other"
selection, adds the new selections, and updates the global cleaning log.

## Usage

``` r
add_to_cleaning_log_other_recode_multiple(x)
```

## Arguments

- x:

  A list or data frame row representing the "other" response record.

## Value

NULL. Updates the global variable `cleaning_log_other` in-place.
