# Search through Statistics Canada data cubes

Searches through Statistics Canada data cubes using a search term.

## Usage

``` r
search_cansim_cubes(search_term, refresh = FALSE)
```

## Arguments

- search_term:

  User-supplied search term used to find Statistics Canada data cubes
  with matching titles, table numbers, subject and survey codes.

- refresh:

  Default is `FALSE`. The underlying cube list is cached for the
  duration of the R sessions and will regenerate the cube list if set to
  `TRUE`

## Value

A tibble with available Statistics Canada data cubes, listing title,
Statistics Canada data cube catalogue number, deprecated CANSIM table
number, survey and subject.

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
search_cansim_cubes("Labour force")
} # }
```
