# Add a Choice to a Space-Separated Select Multiple String

Vector-safe utility to add a specific choice to a space-separated string
of choices. Automatically handles duplicate entries, empty strings, and
missing values.

## Usage

``` r
add_choice(concat_value, choice)
```

## Arguments

- concat_value:

  A character vector containing space-separated choices.

- choice:

  A character vector of the choice(s) to add.

## Value

A character vector with the choice added.
