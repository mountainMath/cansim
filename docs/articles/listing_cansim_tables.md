# Listing Statistics Canada data tables

This vignette details how to use the internal table search functions in
the `cansim` package with a simple example using employment data for
economic regions in British Columbia.

The list of available tables is cached in the current R session to avoid
repeated downloading of data.

### Listing and filtering tables

Calling `list_cansim_cubes` returns a data frame with useful metadata
for available tables. There are 21 fields of metadata for each table
including title, in English and French, keyword sets, notes, and table
numbers.

``` r

library(cansim)

names(list_cansim_cubes())
#> Retrieving cube information from StatCan servers...
#> Warning: StatCan returned table titles or dimension names containing non-breaking spaces
#> or control characters. These render as an ordinary space or as nothing at all,
#> so the names cannot be typed or copy-pasted, the package has replaced them with
#> regular spaces. Repaired 120 names, for example "… end of the fiscal year
#> ending closest to December<U+00A0>31". Nothing on your end causes this and
#> nothing on your end can fix it, the characters are in the data StatCan
#> publishes. This warning will disappear on its own once StatCan stops sending
#> them, which is tracked at https://github.com/mountainMath/cansim/issues/169.
#> Set options(cansim.suppress_repair_warnings=TRUE) to silence this.
#>  [1] "cansim_table_number" "cubeTitleEn"         "cubeTitleFr"        
#>  [4] "productId"           "cansimId"            "cubeStartDate"      
#>  [7] "cubeEndDate"         "releaseTime"         "archived"           
#> [10] "subjectCode"         "surveyCode"          "frequencyCode"      
#> [13] "corrections"         "issueDate"           "dimensionNameEn"    
#> [16] "dimensionNameFr"     "surveyEn"            "surveyFr"           
#> [19] "subjectEn"           "subjectFr"
```

The appropriate table can be found by subsetting or filtering on the
properties we want to use to find the appropriate tables.

``` r

library(dplyr, warn.conflicts = FALSE)

list_cansim_cubes() %>% 
  filter(grepl("Labour force characteristics",cubeTitleEn), 
         grepl("economic region",cubeTitleEn)) %>% 
  select(cansim_table_number,cubeTitleEn)
#> Retrieving cube information from temporary cache.
#> # A tibble: 4 × 2
#>   cansim_table_number cubeTitleEn                                               
#>   <chr>               <chr>                                                     
#> 1 14-10-0090          Labour force characteristics by province, territory and e…
#> 2 14-10-0293          Labour force characteristics by economic region, three-mo…
#> 3 14-10-0462          Labour force characteristics by economic region, three-mo…
#> 4 14-10-0464          Labour force characteristics by province, territory and e…
```

The search came up with two tables. In this example we are interested in
the unemployment rate for 2015 onward for the Lower Mainland, Vancouver
Island, and Okanagan economic regions from the Labour Force
Characteristics table. We use the `tidyr` package here to reshape data
from a long format to a wider format.

``` r

library(tidyr)

selected_table <- "14-10-0293"

data <-get_cansim(selected_table) %>% 
  filter(grepl("Mainland|Vancouver Island|Okanagan", GEO),
         Date>=as.Date("2015-01-01"),
         `Labour force characteristics`=="Unemployment rate") %>%
  select(Date, GEO, Statistics, val_norm) %>%
  spread(key="Statistics", value=val_norm)
#> Accessing CANSIM NDM product 14-10-0293 from Statistics Canada
#> Parsing data
```

We can visualize then results with `ggplot2`.

``` r

library(ggplot2)
ggplot(data, aes(x=Date, group = GEO,y=Estimate)) +
  geom_ribbon(aes(ymin=Estimate - `Standard error of estimate`,
                  ymax=Estimate + `Standard error of estimate`, fill=""),
              alpha=0.8) +
  geom_line(aes(color=GEO)) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(name = "", values="grey80", label="Standard error") +
  theme_bw() + 
  labs(title = "Comparison of unemployment rate by economic region",
       y = "Unemployment Rate", 
       x = "",
       color = "",
       caption=paste0("CANSIM ", selected_table))
```

![Vignette example plot, unemployent
rate](listing_cansim_tables_files/figure-html/unnamed-chunk-4-1.png)
