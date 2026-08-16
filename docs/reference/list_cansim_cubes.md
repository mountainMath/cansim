# Get overview list for all Statistics Canada data cubes

Generates an overview table containing metadata of available Statistics
Canada data cubes.

## Usage

``` r
list_cansim_cubes(lite = FALSE, refresh = FALSE, quiet = FALSE)
```

## Arguments

- lite:

  Get the version without cube dimensions and comments for faster
  retrieval, default is `FALSE`.

- refresh:

  Default is `FALSE`, repeated calls during the same session will hit
  the cached data.

- quiet:

  Optional, suppress messages To refresh the code list during a running
  R session set to `TRUE`

## Value

A tibble with available Statistics Canada data cubes, including NDM
table number, cube title, start and end dates, achieve status, subject
and survey codes, frequency codes and a list of cube dimensions.

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
list_cansim_cubes()
} # }
```
