# Getting started with the cansim package

### About

The `cansim` package provides R bindings to [Statistics Canada’s main
socioeconomic time series
database](https://www150.statcan.gc.ca/n1/en/type/data), previously
known as (and frequently referred to in this package, and elsewhere, as)
CANSIM. Data can be accessed by table number, vector or both table
number and coordinate. The package accepts both old and new (NDM) CANSIM
table catalogue numbers.

### Installing cansim

The `cansim` package is available on CRAN and can be installed directly
using the default package installation process:

``` r

install.packages("cansim")
```

Alternatively, the latest development version of the package can be
downloaded from [Github](https://github.com/mountainMath/cansim) using
the [devtools](https://cran.r-project.org/package=devtools) or
[remotes](https://cran.r-project.org/package=remotes) packages.

``` r

# install.packages("remotes")
remotes::install_github("mountainmath/cansim")

library(cansim)
```

### Usage

If you know the data table catalogue number you are interested in, use
`get_cansim` to download the entire table.

``` r

data <- get_cansim("14-10-0293")
#> Accessing CANSIM NDM product 14-10-0293 from Statistics Canada
#> Parsing data
head(data)
#> # A tibble: 6 × 24
#>   REF_DATE Date       GEO    DGUID      GeoUID Labour force charact…¹ Statistics
#>   <chr>    <date>     <fct>  <chr>      <chr>  <fct>                  <fct>     
#> 1 2001-03  2001-03-01 Canada 2016A0000… 11124  Population             Estimate  
#> 2 2001-03  2001-03-01 Canada 2016A0000… 11124  Labour force           Estimate  
#> 3 2001-03  2001-03-01 Canada 2016A0000… 11124  Labour force           Standard …
#> 4 2001-03  2001-03-01 Canada 2016A0000… 11124  Labour force           Standard …
#> 5 2001-03  2001-03-01 Canada 2016A0000… 11124  Employment             Estimate  
#> 6 2001-03  2001-03-01 Canada 2016A0000… 11124  Employment             Standard …
#> # ℹ abbreviated name: ¹​`Labour force characteristics`
#> # ℹ 17 more variables: VALUE <dbl>, val_norm <dbl>, UOM <chr>, UOM_ID <chr>,
#> #   SCALAR_FACTOR <chr>, SCALAR_ID <chr>, VECTOR <chr>, COORDINATE <chr>,
#> #   STATUS <chr>, SYMBOL <chr>, TERMINATED <chr>, DECIMALS <chr>,
#> #   `Hierarchy for GEO` <chr>,
#> #   `Classification Code for Labour force characteristics` <chr>,
#> #   `Hierarchy for Labour force characteristics` <chr>, …
```

By default, the data tables retrieved by the package comes in the
original format provided by Statistics Canada and is enriched by several
added columns and transformations.

- An additional `Date` column is added that tries to intelligently infer
  a Date object from the `REF_DATE` column.
- An additional `val_norm` column is added, that applies the appropriate
  scaling factor to the `VALUE` column. So if data is coded as
  “thousands of dollars”, a value of `2.4` in the `VALUE` column is
  converted to a value of `2400` in the `val_norm` column. Similarly, a
  percentage of `12.2` in the `VALUE` column is converted to a value of
  `0.122` in the `val_norm` column.
- Categorical variables are converted to factors and, if necessarily,
  de-duplicated by appending the name of the “parent” category in
  parenthesis. This ensures that column variables are unique and that
  they retain their original ordering.

Taking a look at an overview of the data within a table is a common
first step. This is implemented in the package with the
`get_cansim_table_overview(table_number)` function.

``` r

get_cansim_table_overview("14-10-0293")
#> Reading CANSIM NDM product 14-10-0293 information from cache.
#> Labour force characteristics by economic region, three-month moving average, unadjusted for seasonality, last 5 months, inactive
#> CANSIM Table 14-10-0293
#> Start Reference Period: 2001-03-01, End Reference Period: 2020-12-01, Frequency: Monthly
#> 
#> Column Geography (76)
#> Newfoundland and Labrador, Prince Edward Island, Nova Scotia, New Brunswick, Quebec, Ontario, Manitoba, Saskatchewan, Alberta, British Columbia, ...
#> 
#> Column Labour force characteristics (10)
#> Labour force, Not in labour force, Employment, Unemployment, Full-time employment, Part-time employment, Population, Unemployment rate, Participation rate, Employment rate
#> 
#> Column Statistics (3)
#> Estimate, Standard error of estimate, Standard error of year-over-year change
```

When a table number is unknown, you can browse the available tables or
search by survey name, keyword or title.

``` r

search_cansim_cubes("housing price indexes")
#> Retrieving cube information from StatCan servers...
#> Warning: StatCan returned table titles or dimension names containing non-breaking spaces
#> or control characters. These render as an ordinary space or as nothing at all,
#> so the names cannot be typed or copy-pasted, the package has replaced them with
#> regular spaces. Repaired 120 names, for example "… end of the fiscal year
#> ending closest to December<U+00A0>31". Nothing on your end causes this and
#> nothing on your end can fix it, the characters are in the data StatCan
#> publishes. This warning will disappear on its own once StatCan stops sending
#> them, which is tracked at https://github.com/mountainMath/cansim/issues/169.
#> Set options(cansim.suppress_repair_warnings=TRUE) to silence this.
#> # A tibble: 2 × 20
#>   cansim_table_number cubeTitleEn   cubeTitleFr productId cansimId cubeStartDate
#>   <chr>               <chr>         <chr>       <chr>     <chr>    <date>       
#> 1 18-10-0073          New housing … Indices de… 18100073  327-0005 1981-01-01   
#> 2 18-10-0095          New housing … Indices de… 18100095  327-0029 1981-01-01   
#> # ℹ 14 more variables: cubeEndDate <date>, releaseTime <dttm>, archived <lgl>,
#> #   subjectCode <chr>, surveyCode <chr>, frequencyCode <chr>,
#> #   corrections <chr>, issueDate <date>, dimensionNameEn <chr>,
#> #   dimensionNameFr <chr>, surveyEn <chr>, surveyFr <chr>, subjectEn <chr>,
#> #   subjectFr <chr>
```

Individual series in Statistics Canada data tables can also be accessed
by using individual numbered vectors. This is especially useful when
building reports using specific indicators. For convenience, the
`cansim` package allows users to specify named vectors, where the
`label` field will be added to the returned data frame containing the
specified name for each vector.

``` r

get_cansim_vector(c("Metro Van Apartment Construction Price Index"="v44176267",
                    "Metro Van CPI"="v41692930"),
                  start_time = "2015-05-01",
                  end_time="2015-08-01") |>
  dplyr::select(Date,GEO,label,VALUE,val_norm)
#> Accessing CANSIM NDM vectors from Statistics Canada
#> # A tibble: 5 × 5
#>   Date       GEO                         label                    VALUE val_norm
#>   <date>     <fct>                       <chr>                    <dbl>    <dbl>
#> 1 2015-05-01 Vancouver, British Columbia Metro Van CPI             122.     122.
#> 2 2015-06-01 Vancouver, British Columbia Metro Van CPI             122.     122.
#> 3 2015-07-01 Vancouver, British Columbia Metro Van CPI             122.     122.
#> 4 2015-08-01 Vancouver, British Columbia Metro Van CPI             123.     123.
#> 5 2015-07-01 Vancouver, British Columbia Metro Van Apartment Con…  153      153
```

Larger tables, or tables that update infrequently can be cached in
database form for faster access and better performance. The
`get_cansim_connection` function facilitates this, it works mostly
identitcal to the `get_cansim` function, but returns a database
connection to a local database with the StatCan Table data. Calling
`collect_and_normalize`, after possibly filtering data, adds metadata
and loads data into memory in a form that is identical to the data
retrieved by `get_cansim`.

``` r

data <- get_cansim_connection("14-10-0293") |>
  collect_and_normalize()
#> Reading CANSIM NDM product 14-10-0293 from parquet.
head(data)
#> # A tibble: 6 × 24
#>   REF_DATE Date       GEO    DGUID      GeoUID Labour force charact…¹ Statistics
#>   <chr>    <date>     <fct>  <chr>      <chr>  <fct>                  <fct>     
#> 1 2001-03  2001-03-01 Canada 2016A0000… 11124  Population             Estimate  
#> 2 2001-03  2001-03-01 Canada 2016A0000… 11124  Labour force           Estimate  
#> 3 2001-03  2001-03-01 Canada 2016A0000… 11124  Labour force           Standard …
#> 4 2001-03  2001-03-01 Canada 2016A0000… 11124  Labour force           Standard …
#> 5 2001-03  2001-03-01 Canada 2016A0000… 11124  Employment             Estimate  
#> 6 2001-03  2001-03-01 Canada 2016A0000… 11124  Employment             Standard …
#> # ℹ abbreviated name: ¹​`Labour force characteristics`
#> # ℹ 17 more variables: VALUE <dbl>, val_norm <dbl>, UOM <chr>, UOM_ID <chr>,
#> #   SCALAR_FACTOR <chr>, SCALAR_ID <chr>, VECTOR <chr>, COORDINATE <chr>,
#> #   STATUS <chr>, SYMBOL <chr>, TERMINATED <chr>, DECIMALS <chr>,
#> #   `Hierarchy for GEO` <chr>,
#> #   `Classification Code for Labour force characteristics` <chr>,
#> #   `Hierarchy for Labour force characteristics` <chr>, …
```

For more information refer to the *Working with large tables* vignette.

### License

The code in this package is licensed under the MIT license. The bundled
table metadata in Sysdata.R, as well as all Statistics Canada data
retrieved using this package is made available under the Statistics
Canada Open Licence Agreement, a copy of which is included in the R
folder. The Statistics Canada Open Licence Agreement requires that:

    Subject to this agreement, Statistics Canada grants you a worldwide, royalty-free, non-exclusive licence to:

      - use, reproduce, publish, freely distribute, or sell the Information;
      - use, reproduce, publish, freely distribute, or sell Value-added Products; and,
      - sublicence any or all such rights, under terms consistent with this agreement.

    In doing any of the above, you shall:

      - reproduce the Information accurately;
      - not use the Information in a way that suggests that Statistics Canada endorses you or your use of the Information;
      - not misrepresent the Information or its source;
      - use the Information in a manner that does not breach or infringe any applicable laws;
      - not merge or link the Information with any other databases for the purpose of attempting to identify an individual person, business or organization; and
      - not present the Information in such a manner that gives the appearance that you may have received, or had access to, information held by Statistics Canada about any identifiable individual person, business or organization.

### Attribution

Subject to the Statistics Canada Open Licence Agreement, licensed
products using Statistics Canada data should employ the following
acknowledgement of source:

    Acknowledgment of Source

    (a) You shall include and maintain the following notice on all licensed rights of the Information:

      - Source: Statistics Canada, name of product, reference date. Reproduced and distributed on an "as is" basis with the permission of Statistics Canada.

    (b) Where any Information is contained within a Value-added Product, you shall include on such Value-added Product the following notice:

      - Adapted from Statistics Canada, name of product, reference date. This does not constitute an endorsement by Statistics Canada of this product.
