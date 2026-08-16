# Retrieve data for specified Statistics Canada data vector(s) for last N periods

Allows for the retrieval of data for specified vector series for the N
most-recently released periods. Accessing data by vector allows for
targeted extraction of time series. Discovering vectors of interest can
be achieved using the StatCan table web interface or using
`get_cansim_table_template` function to help pinpoint data series of
interest, and then chaining the `add_cansim_vectors_to_template`
function to add cansim vector information to the template data. The
StatCan API can only process 300 coordinates at a time, if more than 300
coordinates are specified the function will batch the requests to the
API.

## Usage

``` r
get_cansim_vector_for_latest_periods(
  vectors,
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

- vectors:

  The list of vectors to retrieve

- periods:

  Numeric value for number of latest periods to retrieve data for, but
  default all data is retrieved.

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

A tibble with data for specified vector(s) for the last N periods

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_vector_for_latest_periods("v41690973",10)
#> Accessing CANSIM NDM vectors from Statistics Canada
#> # A tibble: 10 × 16
#>    REF_DATE  Date       GEO   Products and product…¹ VALUE val_norm UOM   UOM_ID
#>    <chr>     <date>     <fct> <fct>                  <dbl>    <dbl> <chr> <chr> 
#>  1 2025-09-… 2025-09-01 Cana… All-items               165.     165. 2002… 17    
#>  2 2025-10-… 2025-10-01 Cana… All-items               165.     165. 2002… 17    
#>  3 2025-11-… 2025-11-01 Cana… All-items               165.     165. 2002… 17    
#>  4 2025-12-… 2025-12-01 Cana… All-items               165      165  2002… 17    
#>  5 2026-01-… 2026-01-01 Cana… All-items               165      165  2002… 17    
#>  6 2026-02-… 2026-02-01 Cana… All-items               166.     166. 2002… 17    
#>  7 2026-03-… 2026-03-01 Cana… All-items               167.     167. 2002… 17    
#>  8 2026-04-… 2026-04-01 Cana… All-items               168      168  2002… 17    
#>  9 2026-05-… 2026-05-01 Cana… All-items               170.     170. 2002… 17    
#> 10 2026-06-… 2026-06-01 Cana… All-items               169      169  2002… 17    
#> # ℹ abbreviated name: ¹​`Products and product groups`
#> # ℹ 8 more variables: SCALAR_ID <int>, VECTOR <chr>, cansimTableNumber <chr>,
#> #   COORDINATE <chr>, SYMBOL <int>, releaseTime <chr>, frequencyCode <int>,
#> #   DECIMALS <int>
# }
```
