# Retrieve a Statistics Canada data table URL given a table number

Retrieve URL of a table from the API given a table number. Offers a more
stable approach than manually guessing the URL of the table.

## Usage

``` r
get_cansim_table_url(cansimTableNumber, language = "english")
```

## Arguments

- cansimTableNumber:

  the NDM table number to load

- language:

  `"english"` (the default) or `"french"`. Short forms such as `"en"`,
  `"eng"`, `"fr"` or `"fra"` are accepted, as are the French names
  `"anglais"` and `"francais"`; case and accents are ignored

## Value

String object containing URL for specified table number

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_table_url("34-10-0013")
#> [1] "https://www150.statcan.gc.ca/n1/tbl/csv/34100013-eng.zip"
get_cansim_table_url("34-10-0013", language = "fr")
#> [1] "https://www150.statcan.gc.ca/n1/tbl/csv/34100013-fra.zip"
# }
```
