# Retrieve a Statistics Canada data table using NDM catalogue number as parquet, feather, or sqlite database connection

Retrieves a data table using an NDM catalogue number as parquet,
feather, or SQLite database connection. Retrieved table data is cached
permanently if a cache path is supplied or for duration of the current R
session. If the table is cached the function will check if a newer
version is available and emit a warning message if the cached table is
out of date.

## Usage

``` r
get_cansim_connection(
  cansimTableNumber,
  language = "english",
  format = "parquet",
  partitioning = c(),
  refresh = FALSE,
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

- format:

  (Optional) The format of the data table to retrieve. Either
  `"parquet"`, `"feather"`, or `sqlite` (default is `"parquet"`).

- partitioning:

  (Optional) Partition columns to use for parquet or feather formats.

- refresh:

  (Optional) Valid options are `FALSE` (the default), `TRUE`, and
  `"auto"`. When set to `TRUE`, forces a reload of data table, when set
  to `"auto"` it will refresh the table by downloading the newest
  version from StatCan if the table is out of date. If set to `FALSE`
  and the table is out of date a warning will be emitted to alert the
  user that the data is outdated.

- timeout:

  (Optional) Timeout in seconds for downloading cansim table to work
  around scenarios where StatCan servers drop the network connection.

- cache_path:

  (Optional) Path to where to cache the table permanently. By default,
  the data is cached in the path specified by
  \`Sys.getenv('CANSIM_CACHE_PATH')\`, if this is set. Otherwise it will
  use \`tempdir()\`.

## Value

A database connection to a local parquet, feather, or sqlite database
with the StatCan Table data. The data frames after calling \`collect()\`
or \`collect_and_normalize()\` are identical up to possibly different
row order.

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- get_cansim_connection("34-10-0013")

# Work with the data connection
glimpse(con)

} # }
```
