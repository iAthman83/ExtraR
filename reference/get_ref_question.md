# Extract Referenced Question Name from Relevance Expression

Parses a Kobo relevance expression string to extract the referenced
question name enclosed in curly braces (e.g., `${question_name}`).

Parses a Kobo relevance expression string to extract the referenced
question name enclosed in curly braces (e.g., `${question_name}`). This
function is fully vectorized.

## Usage

``` r
get_ref_question(x)

get_ref_question(x)
```

## Arguments

- x:

  A character vector containing Kobo relevance expressions.

## Value

The extracted question name, or `NA` if not found.

A character vector of the extracted question names, or `NA` if not
found.
