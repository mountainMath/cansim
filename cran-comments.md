## Test environments
* local OS X install, R 4.3.2
* GitHub Action macOS-latest, windows-lastest (3.6), ubuntu-20.04 (devel, release)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 

## Changes from version 0.2.1

* Redundancies removed from package title
* Implement timeout and retry for API requests to remote servers
* Reduced number of vignettes to be generated during package build--putting them online on an accompanying package website instead.  

## Changes from version 0.2.2

* Correct problem with incorrect encoding

## Changes from version 0.2.3

* Fixes issues arising from StatCan changing their API
* Adds some post-processing options in normalize_cansim_values

## Changes from version 0.3.0

* Fixes issues arising from StatCan changing their API row limit
* Optimize vector retrieval by REF_DATE

## Changes from version 0.3.1

* Adjust package for changes in StatCan API with different metadata format

## Changes from version 0.3.2

* Fix time zone problem when parsing and formatting times for the StatCan API

## Changes from version 0.3.3

* Expand get_cansim_table_notes functionality
* Add functionality to access the new cube list API

## Changes from version 0.3.4

* Exclude all vignettes and example code from compilation as this may cause CRAN check errors when StatCan servers are down or otherwise temporarily unavailable

## Changes from version 0.3.5
* Fold part of `normalize_cansim_values` into the default table and vector output, in particular always add a scaled variable column called `val_norm` and an imputed `Date` column and covert categories to factors by default.
* New `get_cansim_sqlite` function that stores tables in an SQLite database and facilitates access and managemet of data.
* Adapt to changes in dplyr, tidyr, and tibble
* fix a bug that would not properly add hierarchies when category names are repeated
* Use system unzip if `getOption("unzip")` is set to enable unzip for files larger than 4GB on unix-like systems

# Changes from version 0.3.6
* Fix problem with UTF-8 encoding on solaris
* move dbplyr dependence from Imports to Suggests

# Changes from version 0.3.7
## Minor changes
* Exclude vignette from automatic CRAN checks to fix problem of CRAN checks failing when StatCan servers are down and lead to the package being removed from CRAN (checks are still active in local environment and when using GitHub action checks)
* add release date info to cube metadata and cube list calls
* add auto-refresh option for sqlite tables
* remove deprecated `adjust_cansim_values_by_variable` function

# Changes from version 0.3.8
## Major changes
* deprecate `list_cansim_tables` and `serach_cansim_tables` and fallback to corresponding "_cube" methods as Open Data Canada API has changed and similar functionality is available through the "_cube" methods that tie directly into StatCan APIs
* change `\donttest` to `\dontrun` in examples to exclude examples that make API calls running (and occasionally failing) on CRAN
## Minor changes
* Fix issues with top level duplicate categories
* Check for expired tables in `list_cansim_sqlite_cached_tables`
* New auto-update feature for sqlite tables

# Changes from version 0.3.9
## Minor changes
* Better error handling when StatCan returns empty tables
* Add Hierachy for Geography in sqlite tables
* Better fallback and warning messages when StatCan table categories are internally inconsistent
* Performance improvements

# Changes from version 0.3.10
## Major changes
* Support for new semi-wide table format, e.g. Census data releases
## Minor changes
* Improvement in offline handling of sqlite tables

# Changes from version 0.3.11
## Major changes
* Fix bug that causes collect_and_normalize not to function on some operating systems

# Changes from version 0.3.12
## Minor changes
* Speed up access to cached sqlite tables
* Fix problem with `get_cansim_vector_info()`

# Changes from version 0.3.13
## Minor changes
* Better header parsing to avoid warning messages
* Fix problem with some semi-wide tables

# Changes from version 0.3.14
## Minor changes
* accommodate quirks in table 98-10-0017

# Changes from version 0.3.15
## Minor changes
* improve offline handling when StatCan servers are down
* improve metadata handling when Member ID order is mixed up in metadata
* fix problem with refreshing data in get_cansim_vectors

# Changes from version 0.3.16
## Minor changes
* fix problem with reading French tables released by the census division
* restore original column order after converting to factors
* convery geography column to factor if available
* fix problem with `add_provincial_abbreviations` that could lead to mislabelling of provinces in some cases
* improve handling of metadata, enable downloading only metadata instead of only via full table download
* fold metadata into data when accessing via vector or coordinates
* allow for cansim vectors in `view_cansim_webpage` to view vector information from statcan in the browser

# cansim 0.4
## Major changes
* add support for local caching in parquet and feather formats
* uniform interface for sqlite, parquet, and feather caching
* principled approach to column order
## Minor changes
* fix problem with inconsistent type parsing of notes
* better support for french language when accessing data by vector or coordinate
* tests

# cansim 0.4.1
## Minor changes
* fix problem with parsing census data tables
* fix problem with converting to factors when classification codes are attached.

# cansim 0.4.2
## Minor changes
* ensure proper ordering of levels even if StatCan metadata is not ordered
* enable series information by table and coordinate
* generate table template and facitilate adding vector info to aid pinpointed data download
* enable downloading of data by vector and multiple coordinates in get_cansim_data_for_table_coord_periods (breaking changes with change to parameter)
