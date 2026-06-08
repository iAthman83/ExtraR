# Add Recoding of Other Response to Cleaning Log

Routes a recoding action to the appropriate handler based on whether the
referenced question is a `select_one` or `select_multiple`.

## Usage

``` r
add_to_cleaning_log_other_recode(x)
```

## Arguments

- x:

  A list or data frame row representing the "other" response record.

## Value

NULL. Updates the global variable `cleaning_log_other` in-place.
