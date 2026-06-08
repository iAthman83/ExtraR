# Remove a Choice from a Space-Separated Select Multiple String

Vector-safe utility to remove a specific choice from a space-separated
string of choices. Automatically handles empty strings and missing
values.

## Usage

``` r
remove_choice(concat_value, choice)
```

## Arguments

- concat_value:

  A character vector containing space-separated choices.

- choice:

  A character vector of the choice(s) to remove.

## Value

A character vector with the choice removed.
