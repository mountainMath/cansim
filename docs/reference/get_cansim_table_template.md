# Retrieve table template from Statistics Canada API

A table template consists of the dimensions and members and coordinates
of a table that can be used to explore and filter table data before
downloading subsets of the table. To add vector Ids to (a possibly
filtered) template the \`add_cansim_vectors_to_template\` function can
be used.

## Usage

``` r
get_cansim_table_template(
  cansimTableNumber,
  language = "english",
  refresh = FALSE
)
```

## Arguments

- cansimTableNumber:

  A new or old CANSIM/NDM table number or a vector of table numbers

- language:

  `"english"` (the default) or `"french"`. Short forms such as `"en"`,
  `"eng"`, `"fr"` or `"fra"` are accepted, as are the French names
  `"anglais"` and `"francais"`; case and accents are ignored

- refresh:

  Refresh the data from the Statistics Canada API

## Value

a tibble containing the table template, with a \`cansimTableNumber\`
column identifying the table. When several table numbers are given, the
templates are stacked and columns for dimensions that only appear in
some of the tables are filled with \`NA\` for the other tables.

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
# \donttest{
get_cansim_table_template("34-10-0013")
#> # A tibble: 50 × 4
#>    cansimTableNumber COORDINATE Geography                 `Type of property`
#>    <chr>             <chr>      <chr>                     <chr>             
#>  1 34-10-0013        1.1        Canada                    Residential       
#>  2 34-10-0013        2.1        Newfoundland and Labrador Residential       
#>  3 34-10-0013        3.1        Prince Edward Island      Residential       
#>  4 34-10-0013        4.1        Nova Scotia               Residential       
#>  5 34-10-0013        5.1        New Brunswick             Residential       
#>  6 34-10-0013        6.1        Quebec                    Residential       
#>  7 34-10-0013        7.1        Ontario                   Residential       
#>  8 34-10-0013        8.1        Manitoba                  Residential       
#>  9 34-10-0013        9.1        Saskatchewan              Residential       
#> 10 34-10-0013        10.1       Alberta                   Residential       
#> # ℹ 40 more rows
# }
```
