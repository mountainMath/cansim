# Retrieve data for a Statistics Canada data vector released within a given time frame

Allows for the retrieval of data for specified vector series for a given
time window. Accessing data by vector allows for targeted extraction of
time series. Discovering vectors of interest can be achieved using the
StatCan table web interface or using `get_cansim_table_template`
function to help pinpoint data series of interest, and then chaining the
`add_cansim_vectors_to_template` function to add cansim vector
information to the template data. The StatCan API can only process 300
coordinates at a time, if more than 300 coordinates are specified the
function will batch the requests to the API.

## Usage

``` r
get_cansim_vector(
  vectors,
  start_time = as.Date("1800-01-01"),
  end_time = Sys.time(),
  use_ref_date = TRUE,
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

- start_time:

  Starting date in `YYYY-MM-DD` format, applies to `REF_DATE` or
  `releaseTime`, depending on `use_ref_date` parameter

- end_time:

  Set an optional end time filter in `YYYY-MM-DD` format (defaults to
  current system time)

- use_ref_date:

  Optional, `TRUE` by default. When set to `TRUE`, uses `REF_DATE` of
  vector data to filter, otherwise it uses StatisticsCanada's
  `releaseDate` value for filtering the specified vectors.

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

A tibble with data for vectors released between start and end time

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_vector("v41690973","2015-01-01")
#> Accessing CANSIM NDM vectors from Statistics Canada
#> # A tibble: 139 × 16
#>    REF_DATE  Date       GEO   Products and product…¹ VALUE val_norm UOM   UOM_ID
#>    <chr>     <date>     <fct> <fct>                  <dbl>    <dbl> <chr> <chr> 
#>  1 2015-01-… 2015-01-01 Cana… All-items               124.     124. 2002… 17    
#>  2 2015-02-… 2015-02-01 Cana… All-items               125.     125. 2002… 17    
#>  3 2015-03-… 2015-03-01 Cana… All-items               126.     126. 2002… 17    
#>  4 2015-04-… 2015-04-01 Cana… All-items               126.     126. 2002… 17    
#>  5 2015-05-… 2015-05-01 Cana… All-items               127.     127. 2002… 17    
#>  6 2015-06-… 2015-06-01 Cana… All-items               127.     127. 2002… 17    
#>  7 2015-07-… 2015-07-01 Cana… All-items               127.     127. 2002… 17    
#>  8 2015-08-… 2015-08-01 Cana… All-items               127.     127. 2002… 17    
#>  9 2015-09-… 2015-09-01 Cana… All-items               127.     127. 2002… 17    
#> 10 2015-10-… 2015-10-01 Cana… All-items               127.     127. 2002… 17    
#> # ℹ 129 more rows
#> # ℹ abbreviated name: ¹​`Products and product groups`
#> # ℹ 8 more variables: SCALAR_ID <int>, VECTOR <chr>, cansimTableNumber <chr>,
#> #   COORDINATE <chr>, SYMBOL <int>, releaseTime <chr>, frequencyCode <int>,
#> #   DECIMALS <int>
# }
```
