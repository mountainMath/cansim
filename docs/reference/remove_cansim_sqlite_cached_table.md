# Remove cached cansim SQLite database (deprecated)

This method is deprecated and will be removed in a future version,
please use \`remove_cansim_cached_tables(..., format="sqlite")\`
instead.

## Usage

``` r
remove_cansim_sqlite_cached_table(
  cansimTableNumber,
  language = NULL,
  cache_path = Sys.getenv("CANSIM_CACHE_PATH")
)
```

## Arguments

- cansimTableNumber:

  Number of the table to be removed

- language:

  Language for which to remove the cached data, named as in
  [`get_cansim()`](https://mountainmath.github.io/cansim/reference/get_cansim.md).
  If unspecified (\`NULL\`) tables for all languages will be removed

- cache_path:

  Optional, default value is \`Sys.getenv('CANSIM_CACHE_PATH')\`

## Value

\`NULL“

## Examples

``` r
if (FALSE) { # \dontrun{
con <- get_cansim_connection("34-10-0013", format="sqlite")
disconnect_cansim_sqlite(con)
remove_cansim_cached_tables("34-10-0013", format="sqlite")
} # }
```
