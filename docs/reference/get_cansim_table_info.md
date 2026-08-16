# Retrieve Statistics Canada data table information

Returns table information given an NDM table catalogue number in English
or French. Retrieved table information data is cached for the duration
of the R session only.

## Usage

``` r
get_cansim_table_info(
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

A tibble with the table overview information

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_table_info("34-10-0013")
#> # A tibble: 1 × 7
#>   `Cube Title`               `Product Id` `CANSIM Id` `Archive Status` Frequency
#>   <chr>                      <chr>        <chr>       <chr>            <chr>    
#> 1 Residential property valu… 34-10-0013   026-0018    CURRENT - a cub… 12       
#> # ℹ 2 more variables: `Start Reference Period` <chr>,
#> #   `End Reference Period` <chr>
# }
```
