# Translate deprecated CANSIM table number into new NDM-format table catalogue number

Returns NDM table catalogue equivalent given a standard old-format
CANSIM table number

## Usage

``` r
cansim_old_to_new(oldCansimTableNumber)
```

## Arguments

- oldCansimTableNumber:

  deprecated style CANSIM table number (e.g. "427-0001")

## Value

A character string with the new-format NDM table number

## Examples

``` r
cansim_old_to_new("026-0018")
#> [1] "34-10-0013"
```
