# Disconnect from a cansim connection

Closes the database connection behind a table retrieved with
`get_cansim_connection(..., format="sqlite")`. Parquet and feather
connections hold no connection to close and are left alone, so code that
does not know which format it was handed can close it either way.

## Usage

``` r
disconnect_cansim_connection(connection)
```

## Arguments

- connection:

  A connection to a cansim table as returned by `get_cansim_connection`

## Value

\`NULL\`

## Examples

``` r
if (FALSE) { # \dontrun{
con <- get_cansim_connection("34-10-0013", format="sqlite")
disconnect_cansim_connection(con)
} # }
```
