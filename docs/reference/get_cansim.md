# Retrieve a Statistics Canada data table using NDM catalogue number

Retrieves a data table using an NDM catalogue number as a tidy data
frame. Retrieved table data is cached for the duration of the current R
session only by default.

## Usage

``` r
get_cansim(
  cansimTableNumber,
  language = "english",
  refresh = FALSE,
  timeout = 200,
  factors = TRUE,
  default_month = "07",
  default_day = "01"
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
  objects for monthly data (default set to "01") Set to higher values
  for large tables and slow network connection. (Default is `200`).

## Value

A tibble with StatCan Table data and added `Date` column with inferred
date objects and added `val_norm` column with normalized value from the
`VALUE` column.

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
get_cansim("34-10-0013")
} # }
```
