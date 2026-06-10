# Apply Kobo Labels

Renames the analysis output options columns using a Kobo choices
dictionary before a user can save the analysis.

## Usage

``` r
apply_kobo_labels(dataset, column_name, kobo_choices)
```

## Arguments

- dataset:

  The dataset to modify

- column_name:

  The name of the column to relabel

- kobo_choices:

  The choices dictionary from Kobo

## Value

The updated dataset
