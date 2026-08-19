# convert csv to arrow

convert csv to arrow

## Usage

``` r
csv2arrow(
  csv_file,
  arrow_file,
  format = "parquet",
  col_names,
  value_column = "VALUE",
  partitioning = c(),
  na = c(NA, "..", "", "...", "F"),
  repair_columns = c(),
  text_encoding = "UTF-8",
  delim = ","
)
```

## Arguments

- csv_file:

  input csv path

- arrow_file:

  output arrow database path

- format:

  format of arrow file, "parquet" or "feather" (default parquet)

- col_names:

  column names of the csv file

- value_column:

  name of the value column with numeric data

- partitioning:

  optional partition columns

- na:

  na character strings

- repair_columns:

  columns whose values should be repaired of non-breaking spaces and
  control characters before writing, usually the dimension columns

- text_encoding:

  encoding of csv file (default UTF-8)

- delim:

  (Optional) csv deliminator, default is ","

## Value

A database connection
