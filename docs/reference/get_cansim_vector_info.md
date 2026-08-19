# Retrieve metadata for specified Statistics Canada data vectors

Allows for the retrieval of metadata for Statistics Canada data vectors

## Usage

``` r
get_cansim_vector_info(vectors)
```

## Arguments

- vectors:

  a vector of cansim vectors

## Value

A tibble with metadata for selected vectors

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_vector_info("v41690973")
#> # A tibble: 1 × 10
#>   DECIMALS VECTOR    table      COORDINATE title_en title_fr   UOM frequencyCode
#>      <int> <chr>     <chr>      <chr>      <chr>    <chr>    <int>         <int>
#> 1        1 v41690973 18-10-0004 2.2        Canada;… Canada;…    17             6
#> # ℹ 2 more variables: SCALAR_ID <int>, title <chr>
# }
```
