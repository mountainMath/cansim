# Normalize retrieved data table values to appropriate scales

Facilitates working with Statistics Canada data table values retrieved
using the package by setting all units to counts/dollars instead of
millions, etc. If "replacement_value" is not set, it will replace the
`VALUE` field with normalized values and drop the `scale` column.
Otherwise it will keep the scale columns and create a new column named
replacement_value with the normalized value. It will attempt to parse
the `REF_DATE` field and create an R date variable. This is currently
experimental.

## Usage

``` r
normalize_cansim_values(
  data,
  replacement_value = "val_norm",
  normalize_percent = TRUE,
  default_month = "01",
  default_day = "01",
  factors = TRUE,
  strip_classification_code = FALSE,
  cansimTableNumber = NULL,
  internal = FALSE
)
```

## Arguments

- data:

  A retrieved data table as returned from
  [`get_cansim()`](https://mountainmath.github.io/cansim/reference/get_cansim.md)
  or `get_cansim_ndm()`

- replacement_value:

  (Optional) the name of the column the manipulated value should be
  returned in. Defaults to "val_norm"

- normalize_percent:

  (Optional) When `TRUE` (the default) normalizes percentages by
  changing them to rates

- default_month:

  The default month that should be used when creating Date objects for
  annual data (default set to "01")

- default_day:

  The default day of the month that should be used when creating Date
  objects for monthly data (default set to "01")

- factors:

  (Optional) Logical value indicating if dimensions should be converted
  to factors. (Default set to `TRUE`).

- strip_classification_code:

  Logical value indicating if classification code should be stripped
  from names. (Default set to `FALSE`, if `factors=TRUE` this is
  overridden and set to `TRUE`).

- cansimTableNumber:

  (Optional) Only needed when operating on results of SQLite
  connections.

- internal:

  (Optional) Flag to indicate that this function is called internally.

## Value

Returns a tibble with with adjusted values.

## Examples

``` r
if (FALSE) { # \dontrun{
cansim_table <- get_cansim("34-10-0013")
normalize_cansim_values(cansim_table)
} # }
```
