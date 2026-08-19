# Use metadata to extract categories for column of specific level

For tables with data with hierarchical categories, metadata containing
hierarchy level descriptions is used to extract categories at a
specified level of hierarchy only.

## Usage

``` r
categories_for_level(
  data,
  column_name,
  level = NA,
  strict = FALSE,
  remove_duplicates = TRUE
)
```

## Arguments

- data:

  data table object as returned from
  [`get_cansim()`](https://mountainmath.github.io/cansim/reference/get_cansim.md)

- column_name:

  the quoted name of the column to extract categories from

- level:

  the hierarchy level depth to which to extract categories, where 0 is
  top category

- strict:

  (default `FALSE`) when `TRUE` will only extract that specific
  hierarchy level

- remove_duplicates:

  (default `TRUE`) When set to `TRUE` higher level grouping categories
  already captured by lower level hierarchy data will be removed

## Value

A vector of categories

## Examples

``` r
if (FALSE) { # \dontrun{
data <- get_cansim("16-10-0117")
categories_for_level(data,"North American Industry Classification System (NAICS)",level=2)
} # }
```
