# Retrieve the series that changed today

Retrieve the list of data series Statistics Canada changed today, as
vectors together with the table and coordinate they belong to. Where
[`get_cansim_changed_tables()`](https://mountainmath.github.io/cansim/reference/get_cansim_changed_tables.md)
reports which tables were touched, this reports the individual series
inside them, which is the finer grained way to decide what needs
re-downloading.

## Usage

``` r
get_cansim_changed_series_list(timeout = 600)
```

## Arguments

- timeout:

  (Optional) Number of seconds StatCan is allowed to go without sending
  data before the download is abandoned. The default is set high because
  this method is silent while it works.

## Value

A tibble with one row per changed series, carrying the vector, the table
number, the coordinate and the release time

Returns `NULL` if the data could not be retrieved because StatCan is
unavailable.

## Details

StatCan serves this for the current day only and fills it during the
daily update window that ends at 8:30am Eastern. Unlike the changed
tables method there is no way to ask for an earlier day, StatCan answers
a request naming a date with an HTTP 404.

How long this takes depends entirely on how much StatCan released that
morning. The method takes no parameters, so there is no way to ask for a
smaller slice of a busy day, and StatCan works out a whole response
before sending any of it. On a quiet day the answer arrives in well
under a second; on a heavy one the series changing can number in the
hundreds of thousands and the request has been seen to outlive StatCan's
own gateway, coming back as an HTTP 504 after some nine minutes of
silence. That is a limit at StatCan's end which raising `timeout` cannot
lift, so on such a day
[`get_cansim_changed_tables()`](https://mountainmath.github.io/cansim/reference/get_cansim_changed_tables.md)
is the question worth asking instead.

## Examples

``` r
if (FALSE) { # \dontrun{
get_cansim_changed_series_list()
} # }
```
