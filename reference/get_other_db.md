# Get Other DB

Processes the 'other_labels' alongside the survey inputs to map out the
available choices for recoding.

## Usage

``` r
get_other_db(kobo_survey, kobo_choices, other_labels)
```

## Arguments

- kobo_survey:

  A dataframe representing the Kobo survey.

- kobo_choices:

  A dataframe representing the Kobo choices.

- other_labels:

  A dataframe retrieved from `get_other_labels`.

## Value

A dataframe representing the mapping required for other responses
database.
