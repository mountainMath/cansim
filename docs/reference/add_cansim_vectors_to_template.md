# Retrieve series info for given table id and coordinates

Retrieves vector information for given table and coordinates. This can
be used to query data by vectors, it only returns vector information on
coordinates which are present in the data table, so it gives an
effective way to filter coordinates. Vector information is not available
for census data tables.

## Usage

``` r
add_cansim_vectors_to_template(template, refresh = FALSE)
```

## Arguments

- template:

  A (possibly filtered) cansim table template as returned by
  \`get_cansim_table_template\`

- refresh:

  Refresh the data from the Statistics Canada API

## Value

a tibble containing the table template with added vector information

## Examples

``` r
if (FALSE) { # \dontrun{
template <- get_cansim_table_template("34-10-0013")
template |>
  filter(Geography=="Canada") |>
  add_cansim_vectors_to_template()
} # }
```
