# View CANSIM table or vector information in browser

Opens CANSIM table or vector on Statistics Canada's website using
default browser. This may be useful for getting further info on CANSIM
table and survey methods.

## Usage

``` r
view_cansim_webpage(cansimTableNumber = NULL)
```

## Arguments

- cansimTableNumber:

  CANSIM or NDM table number or cansim vectors with "v" prefix. If no
  number is provided, the vector search page on the Statistic Canada
  website will be opened.

## Value

none

## Examples

``` r
if (FALSE) { # \dontrun{
view_cansim_webpage("34-10-0013")
} # }
```
