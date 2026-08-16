# Retrieve a Statistics Canada data table using NDM catalogue number as SQLite database connection (deprecated)

This method is deprecated and will be removed in a future version,
please use \`get_cansim_connection(..., format="sqlite")\` instead.
Retrieves a data table using an NDM catalogue number as an SQLite table.
Retrieved table data is cached permanently if a cache path is supplied
or for duration of the current R session. The function will check
against the latest release data for the table and emit a warning message
if the cached table is out of date.

## Usage

``` r
get_cansim_sqlite(
  cansimTableNumber,
  language = "english",
  refresh = FALSE,
  auto_refresh = FALSE,
  timeout = 1000,
  cache_path = Sys.getenv("CANSIM_CACHE_PATH")
)
```

## Arguments

- cansimTableNumber:

  the NDM table number to load

- language:

  `"english"` (the default) or `"french"`. Short forms such as `"en"`,
  `"eng"`, `"fr"` or `"fra"` are accepted, as are the French names
  `"anglais"` and `"francais"`; case and accents are ignored

- refresh:

  (Optional) When set to `TRUE`, forces a reload of data table (default
  is `FALSE`)

- auto_refresh:

  (Optional) When set to `TRUE`, it will reload of data table if a new
  version is available (default is `FALSE`)

- timeout:

  (Optional) Timeout in seconds for downloading cansim table to work
  around scenarios where StatCan servers drop the network connection.

- cache_path:

  (Optional) Path to where to cache the table permanently. By default,
  the data is cached in the path specified by
  \`Sys.getenv('CANSIM_CACHE_PATH')\`, if this is set. Otherwise it will
  use \`tempdir()\`.

## Value

A database connection to a local SQLite database with the StatCan Table
data.

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- get_cansim_connection("34-10-0013", format="sqlite")

# Work with the data connection
glimpse(con)

disconnect_cansim_sqlite(con)
} # }
```
