# Check if Answer Exists in Choices List

Validates a single constraint expression of the form
`selected(${question_name}, 'answer')` by checking whether the answer
value exists in the corresponding choices list.

## Usage

``` r
check_answer_in_list(questions, choices, constraint)
```

## Arguments

- questions:

  A dataframe containing the Kobo survey sheet.

- choices:

  A dataframe containing the Kobo choices sheet.

- constraint:

  A string containing a single constraint expression.

## Value

`TRUE` if the answer is valid or the constraint is not a selected()
type, `FALSE` otherwise.
