# Collect data from a parquet, feather or sqlite query and normalize cansim table output

Collect data from a parquet, feather or sqlite query and normalize
cansim table output

## Usage

``` r
collect_and_normalize(
  connection,
  replacement_value = "val_norm",
  normalize_percent = TRUE,
  default_month = "07",
  default_day = "01",
  factors = TRUE,
  strip_classification_code = FALSE,
  disconnect = FALSE
)
```

## Arguments

- connection:

  A connection to a local arrow connection as returned by
  `get_cansim_connection`, possibly with filters or other `dplyr` verbs
  applied

- replacement_value:

  (Optional) the name of the column the manipulated value should be
  returned in. Defaults to adding the \`val_norm\` value field.

- normalize_percent:

  (Optional) When `true` (the default) normalizes percentages by
  changing them to rates

- default_month:

  The default month that should be used when creating Date objects for
  annual data (default set to "07")

- default_day:

  The default day of the month that should be used when creating Date
  objects for monthly data (default set to "01")

- factors:

  (Optional) Logical value indicating if dimensions should be converted
  to factors. (Default set to `FALSE`).

- strip_classification_code:

  (Optional) Logical value indicating if classification code should be
  stripped from names. (Default set to `false`).

- disconnect:

  (Optional) Only used when format is sqlite. Logical value to indicate
  if the SQLite database connection should be disconnected. (Default is
  `FALSE`)

## Value

A tibble with the collected and normalized data

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

con <- get_cansim_connection("34-10-0013")
data <- con %>%
  filter(GEO=="Ontario") %>%
  collect_and_normalize()

} # }
```
