# Repartitions a cached cansim table to a new partitioning scheme

Repartitions and already downloaded and cached parquet or feather
dataset

## Usage

``` r
cansim_repartition_cached_table(
  cansimTableNumber,
  new_partitioning = c(),
  language = "english",
  format = "parquet",
  cache_path = Sys.getenv("CANSIM_CACHE_PATH")
)
```

## Arguments

- cansimTableNumber:

  the NDM table number to load

- new_partitioning:

  (Optional) Partition columns to use for parquet or feather formats.

- language:

  `"english"` (the default) or `"french"`. Short forms such as `"en"`,
  `"eng"`, `"fr"` or `"fra"` are accepted, as are the French names
  `"anglais"` and `"francais"`; case and accents are ignored

- format:

  (Optional) The format of the data table to retrieve. Either
  `"parquet"`, `"feather"`, or `sqlite` (default is `"parquet"`).

- cache_path:

  (Optional) Path to where to cache the table permanently. By default,
  the data is cached in the path specified by
  \`Sys.getenv("CANSIM_CACHE_PATH")\`, if this is set. Otherwise it will
  use \`tempdir()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
cansim_repartition_cached_table("34-10-0013",new_partitioning=c("GeoUID"))

} # }
```
