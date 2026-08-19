# Get the latest release data for a StatCan table, if available

This can be used to check when a table has last been updated.

## Usage

``` r
get_cansim_table_last_release_date(cansimTableNumber)
```

## Arguments

- cansimTableNumber:

  the NDM table number

## Value

A datetime object if a release data is available, NULL otherwise.

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_table_last_release_date("34-10-0013")
#> [1] "2018-05-09 12:30:00 UTC"
# }
```
