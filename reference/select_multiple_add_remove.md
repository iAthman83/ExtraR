# Generate Cleaning Log entries for select_multiple Modification

Computes the necessary cleaning log entries when choices are added
and/or removed from a `select_multiple` question. Automatically updates
both the concatenated value column and the individual binary option
columns (e.g. `question/choice`).

## Usage

``` r
select_multiple_add_remove(
  el,
  to_remove,
  to_add = c(),
  exclusive_options = c("none", "none_of_the_above", "dont_know", "dk", "pnd",
    "no_other_choices")
)
```

## Arguments

- el:

  A list containing `uuid`, `ref_name` (main question name), and
  `change_type`.

- to_remove:

  A character vector of choice names to remove.

- to_add:

  A character vector of choice names to add.

- exclusive_options:

  A character vector of choice names that are exclusive (e.g., "none",
  "dont_know"). If selected, all other choices will be deselected.

## Value

A data frame containing cleaning log entries with columns `uuid`,
`question`, `old_value`, `new_value`, and `change_type`.
