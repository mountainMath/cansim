# Retrieve series info for given table id and coordinates

Retrieves series information by coordinates

## Usage

``` r
get_cansim_series_info_cube_coord(
  cansimTableNumber,
  coordinates,
  timeout = 1000,
  refresh = FALSE
)
```

## Arguments

- cansimTableNumber:

  A new or old CANSIM/NDM table number, coordinates are specific to a
  single table

- coordinates:

  A vector of coordinates

- timeout:

  (Optional) Number of seconds StatCan is allowed to go without sending
  data before the call is abandoned. This does not limit how long the
  call may take overall, a response that keeps arriving is left alone.

- refresh:

  Refresh the data from the Statistics Canada API

## Value

a tibble containing the series information for the given coordinates

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_series_info_cube_coord("34-10-0013", c("1.1.1.1.1.1", "2.1.1.1.1.1"))
#> # A tibble: 0 × 0
# }
```
