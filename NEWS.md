# cansim 0.3.7
## Minor changes
* Fix problem with UTF-8 encoding on solaris
* move dbplyr dependence from Imports to Suggests

# cansim 0.3.6
## Major changes
* Fold part of `normalize_cansim_values` into the default table and vector output, in particular always add a scaled variable column called `val_norm` and an imputed `Date` column and covert categories to factors by default.
* New `get_cansim_sqlite` function that stores tables in an SQLite database and facilitates access and managemet of data.

## Minor changes
* Adapt to changes in dplyr, tidyr, and tibble
* fix a bug that would not properly add hierarchies when category names are repeated
* Use system unzip if `getOption("unzip")` is set to enable unzip for files larger than 4GB on unix-like systems

# cansim 0.3.5 

## Minor changes
- Exclude all vignettes and example code from compilation as this may cause CRAN check errors when StatCan servers are down or otherwise temporarily unavailable

# cansim 0.3.4

## Minor changes
- Expand `get_cansim_table_notes()` functionality
- Add functionality to access the new cube list API

# cansim 0.3.3

## Minor changes
- Fix time zone problem when parsing and formatting times for the StatCan API

# cansim 0.3.2

## Minor changes
- Adjust package for changes in StatCan API with different metadata format

# cansim 0.3.1

## Major changes
- Fixes issues arising from StatCan changing their API row limit

## Minor changes
- Optimize vector retrieval by REF_DATE

# cansim 0.3.0

## Minor changes
- Fixes issues arising from StatCan changing their API
- Member Names come concatenated with Classification Code by default, this could break existing code.
- Adds option to change fields to factors
- Adds option to strip Classification Codes from fields
- Exposes timeout limit to deal with slow connections and large tables

# cansim 0.2.3

## Minor changes
- More robust table download functions
- Improved documentation

# cansim 0.2.2

## Major changes
- Initial CRAN release
- French metadata implemented
  
