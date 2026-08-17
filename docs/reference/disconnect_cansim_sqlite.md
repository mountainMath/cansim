# Disconnect from a cansim database connection (deprecated)

This method is deprecated and will be removed in a future version,
please use \`disconnect_cansim_connection()\` instead.

## Usage

``` r
disconnect_cansim_sqlite(connection)
```

## Arguments

- connection:

  connection to database

## Value

\`NULL“

## Examples

``` r
if (FALSE) { # \dontrun{
con <- get_cansim_connection("34-10-0013", format="sqlite")
disconnect_cansim_connection(con)
} # }
```
