# Set persistent cansim cache location

Cansim provides session caching for retrieved data. This function will
create a persistent cache across sessions for data accessed via
\`get_cansim_connection\` and caches data in a database across
sessions..

## Usage

``` r
set_cansim_cache_path(cache_path, overwrite = FALSE, install = FALSE)
```

## Arguments

- cache_path:

  a local directory to use for saving cached data

- overwrite:

  Option to overwrite any existing cache path already stored locally.

- install:

  Option to install permanently for use across sessions.

## Examples

``` r
if (FALSE) { # \dontrun{
set_cansim_cache_path("~/cansim_cache")

# This will set the cache path permanently until overwritten again
set_cansim_cache_path("~/cancensus_cache", install = TRUE)
} # }
```
