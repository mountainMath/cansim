# Add provincial abbreviations as factor

Add provincial abbreviations as factor

## Usage

``` r
add_provincial_abbreviations(data)
```

## Arguments

- data:

  A tibble as returned by `get_cansim` with provincial level data

## Value

The input tibble with additional factor GEO.abb that contains
language-specific provincial abbreviations

## Examples

``` r
if (FALSE) { # \dontrun{
df <- get_cansim("17-10-0005")
df <- add_provincial_abbreviations(df)
} # }
```
