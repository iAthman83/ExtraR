# Add Removal of Other Response to Cleaning Log

Logs the removal of an "other" response string and handles the clean-up
of its associated question value. For single-choice questions, it sets
the main selection to `NA`. For multiple-choice questions, it removes
the other choice from the choice string and sets the corresponding
binary option column to `"0"`.

## Usage

``` r
add_to_cleaning_log_other_remove(x)
```

## Arguments

- x:

  A list or data frame row representing the "other" response record.
  Must contain `uuid`, `name` (other question name), `ref_name` (main
  question name), `ref_type` (main question type), and either
  `response_ar` or `response_en`.

## Value

NULL. Updates the global variable `cleaning_log_other` in-place.
