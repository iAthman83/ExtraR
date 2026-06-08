# Get Choice Name from Choice Label

Looks up the name for a given choice label within a specific list in the
Kobo choices sheet. Automatically looks up the choices sheet in the
environment if not explicitly provided.

## Usage

``` r
get_name_from_label(list_name, label, kobo_choices = NULL)
```

## Arguments

- list_name:

  The `list_name` value to filter by.

- label:

  The choice `label` value to look up.

- kobo_choices:

  Optional dataframe containing the Kobo choices sheet. If `NULL`, looks
  for `kobo_choices` in the environment.

## Value

The name string for the matching choice, or `NA` if not found.
