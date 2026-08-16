# Retrieve Statistics Canada data table notes and column categories

Returns table notes given an NDM table number in English or French.
Retrieved table information data is cached for the duration of the R
session only.

## Usage

``` r
get_cansim_table_notes(
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

  (Optional) Timeout in seconds for downloading cansim table to work
  around scenarios where StatCan servers drop the network connection.

## Value

A tibble with table notes.

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_table_notes("34-10-0013")
#> # A tibble: 22 × 4
#>    `Note ID` Note                                 `Dimension name` `Member Name`
#>    <chr>     <chr>                                <chr>            <chr>        
#>  1 1         "The methodology used in the curren… NA               NA           
#>  2 2         "Changes occurred in census metropo… Geography        Québec, Queb…
#>  3 2         "Changes occurred in census metropo… Geography        Saguenay, Qu…
#>  4 2         "Changes occurred in census metropo… Geography        Sherbrooke, …
#>  5 2         "Changes occurred in census metropo… Geography        Trois-Rivièr…
#>  6 2         "Changes occurred in census metropo… Geography        Guelph, Onta…
#>  7 2         "Changes occurred in census metropo… Geography        Ottawa-Gatin…
#>  8 2         "Changes occurred in census metropo… Geography        Gatineau part
#>  9 2         "Changes occurred in census metropo… Geography        Kelowna, Bri…
#> 10 2         "Changes occurred in census metropo… Geography        Abbotsford-M…
#> # ℹ 12 more rows
# }
```
