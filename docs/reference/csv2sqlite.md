# convert csv to sqlite adapted from https://rdrr.io/github/coolbutuseless/csv2sqlite/src/R/csv2sqlite.R

convert csv to sqlite adapted from
https://rdrr.io/github/coolbutuseless/csv2sqlite/src/R/csv2sqlite.R

## Usage

``` r
csv2sqlite(
  csv_file,
  sqlite_file,
  table_name,
  transform = NULL,
  chunk_size = 5e+06,
  append = FALSE,
  col_types = NULL,
  na = c(NA, "..", "", "...", "F"),
  text_encoding = "UTF-8",
  delim = ",",
  ...
)
```

## Arguments

- csv_file:

  input csv path

- sqlite_file:

  output sql database path

- table_name:

  sql table name

- transform:

  optional function that transforms each chunk

- chunk_size:

  optional chunk size to read/write data, default=1,000,000

- append:

  optional parameter, append to database or overwrite, default=\`FALSE\`

- col_types:

  optional parameter for csv column types

- na:

  na character strings

- text_encoding:

  encoding of csv file (default UTF-8)

- delim:

  (Optional) csv deliminator, default is ","

- ...:

  (Optional) additional parameters passed to
  \`readr::read_delim_chunked\`

## Value

A database connection
