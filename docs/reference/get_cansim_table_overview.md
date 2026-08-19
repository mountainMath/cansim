# Retrieve Statistics Canada data table overview text

Prints table overview information as console output. In order to display
table overview information, the selected CANSIM table must be loaded
entirely to display overview information. Overview information is
printed in console an in English or French, as specified.

## Usage

``` r
get_cansim_table_overview(
  cansimTableNumber,
  language = "english",
  refresh = FALSE
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

## Value

none

Nothing is printed if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_table_overview("34-10-0013")
#> Residential property values
#> CANSIM Table 34-10-0013
#> Start Reference Period: 2005-01-01, End Reference Period: 2015-01-01, Frequency: 12
#> 
#> Column Geography (50)
#> Canada, Newfoundland and Labrador, Prince Edward Island, Nova Scotia, New Brunswick, Quebec, Ontario, Manitoba, Saskatchewan, Alberta, ...
#> 
#> Column Type of property (1)
#> Residential
# }
```
