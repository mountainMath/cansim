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
#>  1 7530       Natural Resources Canada (Glaciers)                       Ressour…
#>  2 7531       Environment Canada - Temperature and Precipitation        Environ…
#>  3 7538       Employment and Social Development Canada, Homeless Shelt… Emploi …
#>  4 8009       Business Activity, Expenditure and Output Survey          Enquête…
#>  5 8011       General Social Survey Historical Database                 Base de…
#>  6 8012       Census of Agriculture: Environmental Geography Aggregati… Recense…
#>  7 8013       Longitudinal Employment Analysis Program                  Program…
#>  8 8014       Respondent Selection Study for the General Social Survey  Étude s…
#>  9 1105       Business Register                                         Registr…
#> 10 1141       Average Fair Market Value/Purchase Price for New Homes i… Juste v…
#> # ℹ 890 more rows
# }
```
