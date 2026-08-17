# Retrieving individual Statistics Canada vectors

### Retrieving individual vectors

Many of the time-series data available from Statistics Canada have
individual vector codes. These vector codes follow a naming format of a
lower-case “v” and an identifying numbers. Time-series tables will often
bundle many series together, resulting in large and sometimes unwieldy
files. Many users of Canadian statistical data, who are often concerned
with specific time series such as the CPI or international arrivals,
will typically know the exact series they need. For this reason, the
`cansim` package also provides two functions to make it easier to
retrieve individual vectors:
[`get_cansim_vector()`](https://mountainmath.github.io/cansim/reference/get_cansim_vector.md)
and
[`get_cansim_vector_for_latest_periods()`](https://mountainmath.github.io/cansim/reference/get_cansim_vector_for_latest_periods.md).

### get_cansim_vector()

Running `search_cansim_cubes("consumer price index")` shows 32 tables as
results. However, if you are tracking the Canadian Consumer Price Index
(CPI) over time, you might already know the Statistics Canada vector
code the seasonally-unadjusted all-items CPI value: *v41690973*. To
retrieve just this data series on its own without all of the additional
data available in related tables, we can use the
[`get_cansim_vector()`](https://mountainmath.github.io/cansim/reference/get_cansim_vector.md)
function with the vector code and the date onwards from which we want to
get vector results for.

``` r

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
```

The call to `get_cansim_vector` takes three inputs: a string code (or
codes) for `vectors`, a `start_time` in YYYY-MM-DD format, and an
optional value for `end_time`, also in YYYY-MM-DD format. By default,
the `start_time` and `end_time` vectors uses Statistics Canada’s
reference periods (“REF_DATE”) for selecting the date range of the data
for retrieved vectors. There are a few optional input parameters for
this function. If `end_time` is not provided, the call will use the
current date as the default series end time. If the optional parameter
`use_ref_date` is set to `FALSE`, then vector retrieval will instead
filter on the release date of the vector itself.

Vectors can be coerced into a list object in order to retrieve multiple
series at the same time. For example, provincial seasonally-unadjusted
CPI values have their own vector codes. The vector code for British
Columbia all-items CPI is *v41692462*.

The below code retrieves monthly Canadian and BC CPI values for the
period January 2015 to December 2017 only. Monthly data series are
always dated to the first day of the month.

``` r

vectors <- c("v41690973","v41692462")

get_cansim_vector(vectors, "2017-01-01")
#> Accessing CANSIM NDM vectors from Statistics Canada
#> # A tibble: 230 × 16
#>    REF_DATE  Date       GEO   Products and product…¹ VALUE val_norm UOM   UOM_ID
#>    <chr>     <date>     <fct> <fct>                  <dbl>    <dbl> <chr> <chr> 
#>  1 2017-01-… 2017-01-01 Cana… All-items               130.     130. 2002… 17    
#>  2 2017-02-… 2017-02-01 Cana… All-items               130.     130. 2002… 17    
#>  3 2017-03-… 2017-03-01 Cana… All-items               130.     130. 2002… 17    
#>  4 2017-04-… 2017-04-01 Cana… All-items               130.     130. 2002… 17    
#>  5 2017-05-… 2017-05-01 Cana… All-items               130.     130. 2002… 17    
#>  6 2017-06-… 2017-06-01 Cana… All-items               130.     130. 2002… 17    
#>  7 2017-07-… 2017-07-01 Cana… All-items               130.     130. 2002… 17    
#>  8 2017-08-… 2017-08-01 Cana… All-items               130.     130. 2002… 17    
#>  9 2017-09-… 2017-09-01 Cana… All-items               131.     131. 2002… 17    
#> 10 2017-10-… 2017-10-01 Cana… All-items               131.     131. 2002… 17    
#> # ℹ 220 more rows
#> # ℹ abbreviated name: ¹​`Products and product groups`
#> # ℹ 8 more variables: SCALAR_ID <int>, VECTOR <chr>, cansimTableNumber <chr>,
#> #   COORDINATE <chr>, SYMBOL <int>, releaseTime <chr>, frequencyCode <int>,
#> #   DECIMALS <int>
```

### get_cansim_vectors_for_latest_periods()

Some vectors extend backwards for a significant number of periods that
may not be of interest. `get_cansim_vectors_for_lates_periods()` is a
wrapper around `get_cansim_vectors` that takes a `periods` input instead
of arguments for `start_time` and `end_time`, and provides data for the
selected vector(s) for the last `n` periods for which data is available,
irrespective of dates.

``` r

get_cansim_vector_for_latest_periods("v41690973", periods = 60)
#> Accessing CANSIM NDM vectors from Statistics Canada
#> # A tibble: 60 × 16
#>    REF_DATE  Date       GEO   Products and product…¹ VALUE val_norm UOM   UOM_ID
#>    <chr>     <date>     <fct> <fct>                  <dbl>    <dbl> <chr> <chr> 
#>  1 2021-08-… 2021-08-01 Cana… All-items               143.     143. 2002… 17    
#>  2 2021-09-… 2021-09-01 Cana… All-items               143.     143. 2002… 17    
#>  3 2021-10-… 2021-10-01 Cana… All-items               144.     144. 2002… 17    
#>  4 2021-11-… 2021-11-01 Cana… All-items               144.     144. 2002… 17    
#>  5 2021-12-… 2021-12-01 Cana… All-items               144      144  2002… 17    
#>  6 2022-01-… 2022-01-01 Cana… All-items               145.     145. 2002… 17    
#>  7 2022-02-… 2022-02-01 Cana… All-items               147.     147. 2002… 17    
#>  8 2022-03-… 2022-03-01 Cana… All-items               149.     149. 2002… 17    
#>  9 2022-04-… 2022-04-01 Cana… All-items               150.     150. 2002… 17    
#> 10 2022-05-… 2022-05-01 Cana… All-items               152.     152. 2002… 17    
#> # ℹ 50 more rows
#> # ℹ abbreviated name: ¹​`Products and product groups`
#> # ℹ 8 more variables: SCALAR_ID <int>, VECTOR <chr>, cansimTableNumber <chr>,
#> #   COORDINATE <chr>, SYMBOL <int>, releaseTime <chr>, frequencyCode <int>,
#> #   DECIMALS <int>
```

### Naming vector series

In these examples, we have used *v41690973* for Canada and *v41692462*
for BC. This can be hard to remember and can get annoying to work with.
Both vector retrieval functions in the `cansim` package allow for named
vector extraction. This works by providing a user-determined string
directly into a `get_*` call. This may be useful when working with table
code and vector codes that do not have any information in their name and
become easy to lose track of.

### Normalizing data

Data retrieved as vectors also gains the additional `val_norm` column
with normalized values.

### Putting it all together

This quick example uses a list with two named vectors and a starting
date as an input value, converts values (“normalizes”) on the fly, and
prepares a simple `ggplot2` graphic.

``` r


vectors <- c("Canadian CPI"="v41690973",
             "BC CPI"="v41692462")

data <- get_cansim_vector(vectors, "2010-01-01")
#> Accessing CANSIM NDM vectors from Statistics Canada

library(ggplot2)
ggplot(data,aes(x=Date,y=val_norm,color=label)) +
  geom_line() +
  labs(title="Consumer Price Index, January 2010 to September 2018",
       subtitle = "Seasonally-unadjusted, all-items (2002 = 100)",
       caption=paste0("CANSIM vectors ",paste0(vectors,collapse = ", ")),x="",y="",color="")
```

![Vignette example plot, CPI time
series](retrieving_cansim_vectors_files/figure-html/unnamed-chunk-4-1.png)
To access metadata for vectors we can use the `get_cansim_vector_info`
call

``` r

get_cansim_vector_info(vectors)
#> # A tibble: 2 × 10
#>   DECIMALS VECTOR    table      COORDINATE title_en title_fr   UOM frequencyCode
#>      <int> <chr>     <chr>      <chr>      <chr>    <chr>    <int>         <int>
#> 1        1 v41690973 18-10-0004 2.2        Canada;… Canada;…    17             6
#> 2        1 v41692462 18-10-0004 26.2       British… Colombi…    17             6
#> # ℹ 2 more variables: SCALAR_ID <int>, title <chr>
```
