# Retrieve data for series that changed, by table and coordinate

Retrieve the data points Statistics Canada changed for the given
coordinates of a table. Coordinates among the ones asked about that did
not change contribute no rows, and if none of them changed the result is
an empty table rather than an error. The StatCan API can only process
300 coordinates at a time, if more than 300 coordinates are specified
the function will batch the requests to the API.

## Usage

``` r
get_cansim_changed_series_data_for_coordinates(
  cansimTableNumber,
  coordinates,
  language = "english",
  timeout = 200,
  factors = TRUE,
  default_month = "07",
  default_day = "01"
)
```

## Arguments

- cansimTableNumber:

  The table number the coordinates belong to

- coordinates:

  The coordinates to retrieve changed data for

- language:

  `"english"` (the default) or `"french"`. Short forms such as `"en"`,
  `"eng"`, `"fr"` or `"fra"` are accepted, as are the French names
  `"anglais"` and `"francais"`; case and accents are ignored

- timeout:

  (Optional) Number of seconds StatCan is allowed to go without sending
  data before the download is abandoned, to work around scenarios where
  StatCan servers drop the network connection. This does not limit how
  long a download may take overall, a transfer that keeps delivering
  data is left alone. StatCan prepares a whole response before sending
  any of it, which for large requests can take the better part of a
  minute, so values much below the default of 200 risk cutting off
  legitimate requests.

- factors:

  (Optional) Logical value indicating if dimensions should be converted
  to factors. (Default set to `TRUE`).

- default_month:

  The default month that should be used when creating Date objects for
  annual data (default set to "07")

- default_day:

  The default day of the month that should be used when creating Date
  objects for monthly data (default set to "01")

## Value

A tibble with the changed data for the specified coordinates

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
get_cansim_changed_series_data_for_coordinates("34-10-0013","1.1")
} # }
```
