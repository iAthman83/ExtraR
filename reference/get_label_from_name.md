# Get Choice Label from Choice Name

Looks up the label for a given choice name within a specific list in the
Kobo choices sheet.

Looks up the label for a given choice name within a specific list in the
Kobo choices sheet. Automatically looks up the choices sheet in the
environment if not explicitly provided.

## Usage

``` r
get_label_from_name(list.name, name, kobo_choices = NULL)

get_label_from_name(list.name, name, kobo_choices = NULL)
```

## Arguments

- list.name:

  The `list_name` value to filter by.

- name:

  The choice `name` value to look up.

- kobo_choices:

  Optional dataframe containing the Kobo choices sheet. If `NULL`, looks
  for `kobo_choices` in the environment.

## Value

The label string for the matching choice.

The label string for the matching choice, or `NA` if not found.
