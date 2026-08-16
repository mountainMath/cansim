# Search through Statistics Canada data tables (deprecated)

This method is deprecated, please use \`search_cansim_cubes\` instead.
Searches through Statistics Canada data tables using a search term. A
new table is generated if it already does not exist or if refresh option
is set to `TRUE`. Search-terms are case insensitive, but will accept
regular expressions for more advanced searching. The search function can
search either through table titles or through table descriptions,
depending on the whether or not `search_description` is set to `TRUE` or
not. If `refresh = TRUE`, the table will be updated and regenerated
using Statistics Canada's latest data. This can take some time since
this process involves scraping through several hundred web pages to
gather the required metadata. If option `cache_path` is set it will look
for and store the overview table in that directory.

## Usage

``` r
search_cansim_tables(search_term, search_fields = "both", refresh = FALSE)
```

## Arguments

- search_term:

  User-supplied search term used to find Statistics Canada data tables
  with matching titles

- search_fields:

  By default, this function will search through table titles and
  keywords. Setting this parameter to "title" will only search through
  the title, setting it to "keyword" will only search through keywords

- refresh:

  Default is `FALSE`, and will regenerate the table if set to `TRUE`

## Value

A tibble with available Statistics Canada data tables, listing title,
Statistics Canada data table catalogue number, deprecated CANSIM table
number, description and geography that match the search term.

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
search_cansim_tables("Labour force")
} # }
```
