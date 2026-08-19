# Major economic indicator release schedule

Returns every release date of major economic indicators since March 14,
2012. This also includes scheduled future releases.

## Usage

``` r
get_cansim_key_release_schedule()
```

## Value

a tibble with data, and details for major economic indicator release

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_key_release_schedule()
#> # A tibble: 2,881 × 5
#>    date       type    title                                    description url  
#>    <date>     <chr>   <chr>                                    <chr>       <chr>
#>  1 2012-03-16 meeting Canada's international transactions in … "January 2… /dai…
#>  2 2012-03-16 meeting Monthly Survey of Manufacturing          "January 2… /dai…
#>  3 2012-03-19 meeting Wholesale trade                          "January 2… /dai…
#>  4 2012-03-20 meeting Travel between Canada and other countri… ""          /dai…
#>  5 2012-03-22 meeting Retail trade                             "January 2… /dai…
#>  6 2012-03-23 meeting Consumer Price Index                     "February … /dai…
#>  7 2012-03-29 meeting Industrial product and raw materials pr… "February … /dai…
#>  8 2012-03-29 meeting National tourism indicators              "Fourth qu… /dai…
#>  9 2012-03-30 meeting Gross domestic product by industry       "January 2… /dai…
#> 10 2012-03-30 meeting Payroll employment, earnings and hours,… "January 2… /dai…
#> # ℹ 2,871 more rows
# }
```
