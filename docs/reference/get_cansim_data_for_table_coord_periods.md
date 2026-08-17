# Retrieve data for specified Statistics Canada data product for last N periods for specific coordinates

Allows for the retrieval of data for a Statistics Canada data table with
specific table and coordinates. This allows partial targeted download of
tables and can be effectively combined with the
`get_cansim_table_template` function to help pinpoint data series of
interest. The StatCan API can only process 300 coordinates at a time, if
more than 300 coordinates are specified the function will batch the
requests to the API.

## Usage

``` r
get_cansim_data_for_table_coord_periods(
  tableCoordinates,
  periods = NULL,
  language = "english",
  refresh = FALSE,
  timeout = 200,
  factors = TRUE,
  default_month = "07",
  default_day = "01"
)
```

## Arguments

- tableCoordinates:

  Either a list with vectors of coordinates by table number, or a
  (filtered) data frame as returned by `get_cansim_table_template`.

- periods:

  Optional numeric value for number of latest periods to retrieve data
  for, default is `NULL` in which case data for all periods is
  downloaded. Alternatively this can be specified by coordinate if
  tableCoordinates is a data frame, this argument will be ignored if
  that data frame as a "periods" column.

- language:

  `"english"` (the default) or `"french"`. Short forms such as `"en"`,
  `"eng"`, `"fr"` or `"fra"` are accepted, as are the French names
  `"anglais"` and `"francais"`; case and accents are ignored

- refresh:

  (Optional) When set to `TRUE`, forces a reload of data table (default
  is `FALSE`)

- timeout:

  (Optional) Number of seconds StatCan is allowed to go without sending
  data before the download is abandoned, to work around scenarios where
  StatCan servers drop the network connection. This does not limit how
  long a download may take overall, a transfer that keeps delivering
  data is left alone. StatCan prepares a whole response before sending
  any of it, which for large requests can take the better part of a
  minute, so values much below the default of 200 risk cutting off
  legitimate requests.

- factors:

  (Optional) Logical value indicating if dimensions should be converted
  to factors. (Default set to `TRUE`).

- default_month:

  The default month that should be used when creating Date objects for
  annual data (default set to "07")

- default_day:

  The default day of the month that should be used when creating Date
  objects for monthly data (default set to "01")

## Value

A tibble with data matching specified coordinate and period input
arguments

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_data_for_table_coord_periods(list("35-10-0003"=c("1.1","1.12")),periods=3)
#> Accessing CANSIM NDM coordinates from Statistics Canada
#> # A tibble: 6 × 17
#>   REF_DATE   Date       GEO     REF_DATE_2 Custodial and commun…¹ VALUE val_norm
#>   <chr>      <date>     <fct>   <chr>      <fct>                  <dbl>    <dbl>
#> 1 2021-01-01 2021-01-01 Newfou… 2022-01-01 Total actual-in count    2.1      2.1
#> 2 2022-01-01 2022-01-01 Newfou… 2023-01-01 Total actual-in count    1.8      1.8
#> 3 2023-01-01 2023-01-01 Newfou… 2024-01-01 Total actual-in count   NA       NA  
#> 4 2021-01-01 2021-01-01 Newfou… 2022-01-01 Probation rate per 10…  29.1     29.1
#> 5 2022-01-01 2022-01-01 Newfou… 2023-01-01 Probation rate per 10…  19.4     19.4
#> 6 2023-01-01 2023-01-01 Newfou… 2024-01-01 Probation rate per 10…  16.7     16.7
#> # ℹ abbreviated name: ¹​`Custodial and community supervision`
#> # ℹ 10 more variables: UOM <chr>, UOM_ID <chr>, SCALAR_ID <int>, VECTOR <chr>,
#> #   cansimTableNumber <chr>, COORDINATE <chr>, SYMBOL <int>, releaseTime <chr>,
#> #   frequencyCode <int>, DECIMALS <int>
# }
```
