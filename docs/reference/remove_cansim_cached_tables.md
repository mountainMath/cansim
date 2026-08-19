# Remove cached cansim SQLite and parquet database

Remove cached cansim SQLite and parquet database

## Usage

``` r
remove_cansim_cached_tables(
  cansimTableNumber,
  format = c("parquet", "feather", "sqlite"),
  language = NULL,
  cache_path = Sys.getenv("CANSIM_CACHE_PATH")
)
```

## Arguments

- cansimTableNumber:

  Vector of the table(s) to be removed, or a (filtered) table as
  returned by \`list_cansim_cached_tables\` with the list of tables to
  be removed.

- format:

  Format of cache to remove, possible values are \`"parquet"\`,
  \`"feather"\` or \`"sqlite"\` or a subset of these (the default is all
  of these)

- language:

  Language for which to remove the cached data, named as in
  [`get_cansim()`](https://mountainmath.github.io/cansim/reference/get_cansim.md).
  If unspecified (\`NULL\`) tables for all languages will be removed.

- cache_path:

  Optional, default value is \`Sys.getenv('CANSIM_CACHE_PATH')\`

## Value

\`NULL“

## Examples

``` r
if (FALSE) { # \dontrun{
con <- get_cansim_connection("34-10-0013", format="parquet")
remove_cansim_cached_tables("34-10-0013", format="parquet")
} # }
```
