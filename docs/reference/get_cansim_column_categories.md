# Retrieve Statistics Canada data table categories for a specific column

Returns table column details given an NDM table number in English or
French. Retrieved table information data is cached for the duration of
the R session only.

## Usage

``` r
get_cansim_column_categories(
  cansimTableNumber,
  column,
  language = "english",
  refresh = FALSE,
  timeout = 200
)
```

## Arguments

- cansimTableNumber:

  the NDM table number to load

- column:

  the specified column for which to retrieve category information for

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

A tibble with detailed information on StatCan table categories for the
specified field

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_column_categories("34-10-0013", "Geography")
#> # A tibble: 50 × 7
#>    `Dimension ID` `Dimension name` `Member ID` `Member Name`  `Parent Member ID`
#>    <chr>          <chr>            <chr>       <chr>          <chr>             
#>  1 1              Geography        1           Canada         NA                
#>  2 1              Geography        2           Newfoundland … 1                 
#>  3 1              Geography        3           Prince Edward… 1                 
#>  4 1              Geography        4           Nova Scotia    1                 
#>  5 1              Geography        5           New Brunswick  1                 
#>  6 1              Geography        6           Quebec         1                 
#>  7 1              Geography        7           Ontario        1                 
#>  8 1              Geography        8           Manitoba       1                 
#>  9 1              Geography        9           Saskatchewan   1                 
#> 10 1              Geography        10          Alberta        1                 
#> # ℹ 40 more rows
#> # ℹ 2 more variables: Terminated <chr>, Hierarchy <chr>
# }
```
