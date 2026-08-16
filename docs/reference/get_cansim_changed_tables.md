# Retrieve a list of modified tables since a given date

Retrieve a list of tables that have been modified or updated since the
specified date.

## Usage

``` r
get_cansim_changed_tables(start_date, end_date = NULL)
```

## Arguments

- start_date:

  Starting date in `YYYY-MM-DD` format to look for changes that changed
  on or after that date

- end_date:

  Optional end date in `YYYY-MM-DD` format to look for changes that
  changed on or before that date, default is same as start date

## Value

A tibble with Statistics Canada data table product ids and their release
times

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_changed_tables("2018-08-01")
#> # A tibble: 8 × 2
#>   productId releaseTime     
#>       <int> <chr>           
#> 1  23100251 2018-08-01T08:35
#> 2  33100036 2018-08-01T08:30
#> 3  10100139 2018-08-01T08:30
#> 4  10100125 2018-08-01T08:30
#> 5  10100107 2018-08-01T08:30
#> 6  33100005 2018-08-01T08:30
#> 7  33100033 2018-08-01T08:30
#> 8  33100084 2018-08-01T08:30
# }
```
