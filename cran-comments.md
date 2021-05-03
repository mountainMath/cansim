## Test environments
* local OS X install, R 3.6.2
* ubuntu 14.04.5 LTS (on travis-ci), R 3.6.2

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
* Adapt to changes in dplyr, tidyr, and tibble
* fix a bug that would not properly add hierarchies when category names are repeated
* Use system unzip if `getOption("unzip")` is set to enable unzip for files larger than 4GB on unix-like systems

