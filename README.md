# cansim
Wrapper to access CANSIM data

Inspired by the [CANSIM2R package](https://cran.r-project.org/web/packages/CANSIM2R/index.html), with a
couple of modifications to better fit my needs. Tables are cached for the duration of the session to
avoid waits when recompiling code, the "raw" option is the default and variables get automatically read
as numeric/character as appropriate. On read it uses "Windows-1254" ecoding to avoid issues with some labels messing
with R. The package also contains a convenience function that will re-scale and
re-label variables that are reported in thousands or millions.

Caches data for the duration of the current R session.

# Basic Usage
Use old or new table number to download entire cansim tables into a tidy dataframe. 

    data <- get_cansim("051-0057")
    data <- get_cansim("510057")
    data <- get_cansim("17-10-0079")
    data <- get_cansim("17-10-0079-01")
    
all load the same data.
    
To load french language data use
  
    data <- get_cansim("17-10-0079",language="french")
    
CANSIM data values may be scaled by powers of 10. For example, values in the VALUE field may be reported in "millions", so a VALUE of 10 means 10,000,000. The *normailze_cansim_values* function automatically scales the VALUE field to be a number, so the VALUE will be converted from 10 to 10000000 in the example given.

    data <- get_cansim("17-10-0079-01") %>% normailze_cansim_values
    
To retain the original VALUE field and create a new NORMALIZED_VALUE field to contain the normailzed value, pass the *replace=FALSE* option.

    data <- get_cansim("17-10-0079-01") %>% normailze_cansim_values(replace=FALSE)
    
