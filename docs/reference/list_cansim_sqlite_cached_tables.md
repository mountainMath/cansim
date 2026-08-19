# List cached cansim SQLite database (deprecated)

This method is deprecated and will be removed in a future version,
please use \`list_cansim_cached_tables()\` instead.

## Usage

``` r
list_cansim_sqlite_cached_tables(
  cache_path = Sys.getenv("CANSIM_CACHE_PATH"),
  refresh = FALSE
)
```

## Arguments

- cache_path:

  Optional, default value is \`Sys.getenv('CANSIM_CACHE_PATH')\`.

- refresh:

  Optional, refresh the last updated date of cached cansim tables

## Value

A tibble with the list of all tables that are currently cached at the
given cache path.

## Examples

``` r
if (FALSE) { # \dontrun{
list_cansim_cached_tables()
} # }
```
