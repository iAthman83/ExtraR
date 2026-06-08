# Get Column Value by UUID

Retrieves a value from a specific column for a given UUID. If the UUID
contains an underscore (indicating a loop entry), it searches the loop
dataset; otherwise it searches the main dataset.

Retrieves a value from a specific column for a given UUID. If the UUID
contains an underscore (indicating a loop/roster entry), it searches the
loop dataset; otherwise it searches the main dataset. This function is
fully vectorized, handles missing/empty values gracefully, and
automatically looks up datasets in the environment if not explicitly
provided.

## Usage

``` r
get_value_from_uuid(
  uuid,
  column,
  raw_data = NULL,
  raw_roster = NULL,
  raw_loop = NULL
)

get_value_from_uuid(
  uuid,
  column,
  raw_data = NULL,
  raw_roster = NULL,
  raw_loop = NULL
)
```

## Arguments

- uuid:

  A character vector of UUIDs to look up.

- column:

  The column name (scalar character) to retrieve the value from.

- raw_data:

  The main raw dataset dataframe. If `NULL`, looks for `raw_data` in the
  environment.

- raw_roster:

  Optional roster dataset dataframe. If `NULL`, looks for `raw_roster`
  or `raw_loop` in the environment.

- raw_loop:

  Optional loop dataset dataframe. Synonym for `raw_roster`.

## Value

The value from the specified column for the matching UUID.

A vector of values from the specified column for the matching UUIDs.
