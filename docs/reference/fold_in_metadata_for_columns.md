# Fold in metadata and for selected columns

Fold in metadata and for selected columns

## Usage

``` r
fold_in_metadata_for_columns(data, data_path, column_names)
```

## Arguments

- data:

  A tibble with StatCan table data as e.g. returned by `get_cansim`.

- data_path:

  base path to save parsed metadata

- column_names:

  the names of the columns

## Value

A tibble including the metadata information
