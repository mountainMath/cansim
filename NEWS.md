# cansim 0.4.5
## Major changes
* data retrieved by vector or by table/coordinate now carries `UOM` and `UOM_ID` columns, taken from the
  cube metadata. StatCan flags a single dimension of each cube as carrying the unit of measure and the unit
  varies by member of that dimension, so the unit is resolved per coordinate. Tables that have no unit of
  measure, for example census tables, get no unit columns, matching the full table download (#170)

* non-breaking spaces and control characters in names returned by StatCan are now replaced with regular
  spaces. These characters render as an ordinary space or as nothing at all, so a column whose name
  contained one could not be reached by typing or copy-pasting what the console displayed. The repair
  covers table downloads, vector and coordinate calls, cube metadata, table templates and the cube list,
  and emits a warning that shows the offending characters by code point, for example
  `Performance<U+00A0> strategy`, together with a count of how many names were repaired. Set
  `options(cansim.suppress_repair_warnings=TRUE)` to silence the warning. Column names of tables cached before this release keep the original characters
  until the table is downloaded again, `get_cansim_connection()` warns when it finds such a cache (#169)

## Deprecations
* `get_cansim_sqlite()`, `list_cansim_sqlite_cached_tables()` and `remove_cansim_sqlite_cached_table()` are now
  also documented as deprecated, matching the deprecation warnings they already emit. Use
  `get_cansim_connection(..., format="sqlite")`, `list_cansim_cached_tables()` and
  `remove_cansim_cached_tables(..., format="sqlite")` instead
* the deprecated `get_cansim_sqlite()`, `list_cansim_sqlite_cached_tables()`, `remove_cansim_sqlite_cached_table()`,
  `list_cansim_tables()` and `search_cansim_tables()` are scheduled for removal in a future release

## Performance
* hierarchy building in metadata parsing no longer re-parses the growing hierarchy paths, hierarchies are built
  one ancestor level at a time across all members at once
* coordinates are split once into a character matrix when folding in metadata and converting to factors
* table templates are built with a single cartesian product instead of joining one dimension at a time

## Minor changes
* fix a `case_when()` deprecation warning emitted by dplyr 1.2.0 on every table read
* the unit of measure columns of French language tables are now ordered with the other value columns,
  as they already were in English language tables
* better connection error handling
* fix `get_cansim_cube_metadata()` and `get_cansim_table_template()` for vectors of table numbers, metadata for
  all tables is still retrieved in a single API call and cached per table
* `get_cansim_cube_metadata()` adds a `cansimTableNumber` column for the "members", "notes" and "corrections" types
* functions that only operate on a single table now fail with an informative message when given several table numbers

# cansim 0.4.4
## Minor changes
* fix a problem with metadata parsing does not work properly for table names
* make documentations more consistent wrt default langauge names
* add convenience functions for setting cache paths for data accessed via get_cansim_connection

# cansim 0.4.3
## Minor changes
* better handling of duplicated levels in metadata, ignore duplication for geography names of census tables but emit warning
* fix issue with accessing tables without footnotes

# cansim 0.4.2
## Minor changes
* ensure proper ordering of levels even if StatCan metadata is not ordered
* better error messages and information on how to disable peer checking when StatCan SSL certificates have problems
* automatically batch vector or coordinate data retrieval in case users request more than 300 series at a time
## Major changes
* enable series information by table and coordinate
* generate table template and facilitate adding vector info to aid pinpointed data download
* enable downloading of data by vector and multiple coordinates in get_cansim_data_for_table_coord_periods (breaking changes with change to parameter)

# cansim 0.4.1
## Minor changes
* fix problem with parsing census data tables
* fix problem with converting to factors when classification codes are attached.

# cansim 0.4
## Major changes
* add support for local caching in parquet and feather formats
* uniform interface for sqlite, parquet, and feather caching
* principled approach to column order
## Minor changes
* fix problem with inconsistent type parsing of notes
* better support for french language when accessing data by vector or coordinate
* tests

# cansim 0.3.17
## Minor changes
* fix problem with reading French tables released by the census division
* restore original column order after converting to factors
* convert geography column to factor if available
* fix problem with `add_provincial_abbreviations` that could lead to mislabelling of provinces in some cases
* improve handling of metadata, enable downloading only metadata instead of only via full table download
* fold metadata into data when accessing via vector or coordinates
* allow for cansim vectors in `view_cansim_webpage` to view vector information from statcan in the browser

# cansim 0.3.16
## Minor changes
* improve offline handling when StatCan servers are down
* improve metadata handling when Member ID order is mixed up in metadata
* fix problem with refreshing data in get_cansim_vectors

# cansim 0.3.15
## Minor changes
* accommodate quirks in table 98-10-0017

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
* Add Hierarchy for Geography in sqlite tables
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
* New `get_cansim_sqlite` function that stores tables in an SQLite database and facilitates access and management of data.

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
  
