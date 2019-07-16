## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04.5 LTS (on travis-ci), R 3.6.0

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

