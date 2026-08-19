# Retrieve table metadata from Statistics Canada API

Retrieves table metadata given an input table number or vector of table
numbers using either the new or old table number format. Patience is
suggested as the Statistics Canada API can be very slow. The
\`list_cansim_tables()\` function can be used as an alternative to
retrieve a (cached) list of CANSIM tables with (more limited) metadata.

## Usage

``` r
get_cansim_cube_metadata(cansimTableNumber, type = "overview", refresh = FALSE)
```

## Arguments

- cansimTableNumber:

  A new or old CANSIM/NDM table number or a vector of table numbers

- type:

  Which type of metadata to get, options are "overview", "members",
  "notes", or "corrections".

- refresh:

  Refresh the data from the Statistics Canada API

## Value

a tibble containing the table metadata. When several table numbers are
given, the metadata for all tables is retrieved in a single API call and
the results are stacked. Types other than "overview" carry no table
identifier of their own, for those a \`cansimTableNumber\` column is
added to identify the table.

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_cube_metadata("34-10-0013")
#> # A tibble: 1 × 17
#>   responseStatusCode productId  cansimId cubeTitleEn   cubeTitleFr cubeStartDate
#>   <chr>              <chr>      <chr>    <chr>         <chr>       <chr>        
#> 1 0                  34-10-0013 026-0018 Residential … Valeurs de… 2005-01-01   
#> # ℹ 11 more variables: cubeEndDate <chr>, frequencyCode <chr>,
#> #   nbSeriesCube <chr>, nbDatapointsCube <chr>, releaseTime <dttm>,
#> #   archiveStatusCode <chr>, archiveStatusEn <chr>, archiveStatusFr <chr>,
#> #   subjectCode <chr>, surveyCode <chr>, issueDate <chr>
# }
```
