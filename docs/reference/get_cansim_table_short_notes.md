# Retrieve Statistics Canada data table short notes

Returns table notes given an NDM table number in English or French.
Retrieved table information data is cached for the duration of the R
session only.

## Usage

``` r
get_cansim_table_short_notes(
  cansimTableNumber,
  language = "english",
  refresh = FALSE,
  timeout = 200
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

## Value

A tibble with the StatCan Notes for the table

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_table_short_notes("34-10-0013")
#> # A tibble: 3 × 2
#>   `Note ID` Note                                                                
#>   <chr>     <chr>                                                               
#> 1 1         "The methodology used in the current release differs from that used…
#> 2 2         "Changes occurred in census metropolitan area geographical boundari…
#> 3 3         "Changes occurred in census metropolitan area geographical boundari…
# }
```
