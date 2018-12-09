## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04.5 LTS (on travis-ci), R 3.5.1

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 

## Changes from previous submissions

* Redundancies removed from package title
* Implement timeout and retry for API requests to remote servers
* Reduced number of vignettes to be generated during package build--putting them online on an accompanying package website instead.  
