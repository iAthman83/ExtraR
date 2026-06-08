# Check Kobo Survey Constraints

Scans the relevant column of a Kobo survey sheet for all `selected()`
constraint expressions and validates that each referenced answer exists
in the choices sheet. Returns a character vector of invalid constraints,
if any.

## Usage

``` r
check_constraints(questions, choices)
```

## Arguments

- questions:

  A dataframe containing the Kobo survey sheet.

- choices:

  A dataframe containing the Kobo choices sheet.

## Value

A character vector of invalid constraint expressions, or `NULL` if all
constraints are valid.
