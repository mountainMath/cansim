# Get NDM code sets

Useful to get a list of surveys or subjects and used internally

## Usage

``` r
get_cansim_code_set(
  code_set = c("scalar", "frequency", "symbol", "status", "uom", "survey", "subject",
    "wdsResponseStatus"),
  refresh = FALSE
)
```

## Arguments

- code_set:

  the code set to retrieve.

- refresh:

  Default is `FALSE`, repeated calls during the same session will hit
  the cached data. To refresh the code list during a running R session
  set to `TRUE`

## Value

A tibble with english and french labels for the given code set

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_code_set("survey")
#> # A tibble: 900 × 3
#>    surveyCode surveyEn                                                  surveyFr
#>    <chr>      <chr>                                                     <chr>   
#>  1 1105       Business Register                                         Registr…
#>  2 1141       Average Fair Market Value/Purchase Price for New Homes i… Juste v…
#>  3 1209       Survey of Environmental Goods and Services                Enquête…
#>  4 1301       Gross Domestic Product by Industry - National (Monthly)   Produit…
#>  5 1302       Gross Domestic Product by Industry - Annual               Produit…
#>  6 1303       Gross Domestic Product by Industry - Provincial and Terr… Produit…
#>  7 1401       Supply, Use and Input-Output Tables                       Tableau…
#>  8 1402       Productivity Measures and Related Variables - National a… Mesures…
#>  9 1529       Capital Invested Abroad by Canadian Enterprises           Capitau…
#> 10 1530       Capital Invested in secondary foreign companies by Canad… Capitau…
#> # ℹ 890 more rows
# }
```
