# cansim 0.3.14
## Minor changes
* Better header parsing to avoid warning messages
* Fix problem with some semi-wide tables

# cansim 0.3.13
## Minor changes
* Speed up access to cached sqlite tables
* Fix problem with `get_cansim_vector_info()`

# cansim 0.3.12
## Major changes
* Fix bug that causes collect_and_normalize not to function on some operating systems

# cansim 0.3.11
## Major changes
* Support for new semi-wide table format, e.g. Census data releases
## Minor changes
* Improvement in offline handling of sqlite tables

# cansim 0.3.10
## Minor changes
* Better error handling when StatCan returns empty tables
* Add Hierachy for Geography in sqlite tables
* Better fallback and warning messages when StatCan table categories are internally inconsistent
* Performance improvements

# cansim 0.3.9
## Major changes
* deprecate `list_cansim_tables` and `serach_cansim_tables` and fallback to corresponding "_cube" methods as Open Data Canada API has changed and similar functionality is available through the "_cube" methods that tie directly into StatCan APIS
## Minor changes
* Fix issues with top level duplicate categories
* Check for expired tables in `list_cansim_sqlite_cached_tables`
* New auto-update feature for sqlite tables

# cansim 0.3.8
## Minor changes
* Exclude vignette from automatic CRAN checks to fix problem of CRAN checks failing when StatCan servers are down and lead to the package being removed from CRAN (checks are still active in local environment and when using GitHub action checks)
* add release date info to cube metadata and cube list calls
* add auto-refresh option for sqlite tables
* remove deprecated `adjust_cansim_values_by_variable` function

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
  
