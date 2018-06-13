# cansim
R package to download Statistics Canada CANSIM/NDM tables.

This package:

* Downloads CANSIM/NDM tables as analysis-ready tidy data frames
* Works with new NDM tables and legacy CANSIM table catalogue numbers
* Bilingual
* Caches downloaded data for faster loading and less waiting
* Convenience functions for relabelling and rescaling when appropriate

### Installation
```r
# install.packages("devtools")
devtools::install_github("mountainmath/github")
```

### Caching

As many CANSIM table downloads can be quite large in size, the cansim package will temporarily cache data for the duration of the current R session. This reduces unnecessary waiting when recompiling code. 

### Basic Usage

Use old or new table number to download entire CANSIM or NDM tables into a tidy dataframe. The cansim package retains the ability to work with legacy CANSIM table numbers or their new NDM equivalent. Calling either the legacy CANSIM table number or the new NDM number will load the same data. 
```r
# Retrieve data for births table: 17-10-0016-01 (formerly: CANSIM 051-0013)
births <- get_cansim("051-0013")
births <- get_cansim("17-10-0016-01")

# Retrieve data for balance of payment table 36-10-0042-01 (formerly CANSIM  376-8105)
bop <- get_cansim("3768105")
bop <- get_cansim("36-10-0042")
```    

### Bilingual

The cansim package works in either English or French. There is an optional language argument to retrieve CANSIM/NDM tables in French: 

Le paquet cansim fonctionne en anglais ou en français. Il existe un argument de langue optionnel pour récupérer les tables CANSIM / NMD en français:
```r
naissances <- get_cansim("051-0013",language="fr")
```    

### Normalizing values

The package also contains a convenience function that will re-scale and re-label variables that are reported in thousands or millions. CANSIM data values may be scaled by powers of 10. 

For example, values in the VALUE field may be reported in "millions", so a VALUE of 10 means 10,000,000. The `normalize_cansim_values` function automatically scales the VALUE field to be a number, so the VALUE will be converted from 10 to 10000000 in the example given.
```r
data <- get_cansim("17-10-0079-01") %>% normalize_cansim_values
``` 

To retain the original VALUE field pass the *replacement_value = <your field name>* option to create a field to contain the normailzed value.
```r
data <- get_cansim("17-10-0079-01") %>% normalize_cansim_values(replacement_value="normalized value")
```

By default percentages will be converted to rates, so instead of being 0-100 it will be normalized to 0-1. To change that behaviour set *normalize_percent=FALSE*.


### Contributing

This package is still under development and may have some bugs. [Issues](https://github.com/mountainMath/cansim/issues) and [pull requests](https://github.com/mountainMath/cansim/pulls) are highly appreciated. 

### To-do

- [ ] Clean up package and function documentation
- [ ] Provide additional user control over cache location
- [ ] Test for more bugs
- [ ] Submit to CRAN

### Related packages

* There exists a [CANSIM2R package](https://cran.r-project.org/web/packages/CANSIM2R/index.html) on CRAN. This package currently uses some functionality from that package to access legacy CANSIM data. The *CANSIM2R* package currently does not support retrieval of data from Statistics Canada's NDM tables. 

* [CANSIM-dataviewer](https://github.com/bcgov/CANSIM-dataviewer) is another tool that depends on the existing *CANSIM2R* package with a focus on uses for the Province of British Columbia.

* [cancensus](https://github.com/mountainMath/cancensus) is a package designed to access, retrieve, and work with Canadian Census data and geography. A stable version is available on CRAN and a development version is on Github. 


