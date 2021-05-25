# cansim

[![R build status](https://github.com/mountainMath/cansim/workflows/R-CMD-check/badge.svg)](https://github.com/mountainMath/cansim/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/cansim)](https://cran.r-project.org/package=cansim)
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/cansim)](https://cranlogs.r-pkg.org/badges/cansim)

<a href="https://mountainmath.github.io/cansim/index.html"><img src="https://raw.githubusercontent.com/mountainMath/cansim/master/images/cansim-sticker.png" alt="cansim logo" align="right" width = "25%" height = "25%"/></a>

An R package to retrieve and work with public Statistics Canada data tables.

This package:

* Searches for and retrieves data tables and series from Statistics Canada's socioeconomic data repository (previously known as CANSIM) 
* Prepares retrieved data tables as analysis-ready tidy data frames
* Accepts legacy CANSIM table catalogue numbers
* Allows for bilingual data retrieval
* Offers caching of downloaded data for faster loading and less waiting
* Includes convenience functions for relabelling and rescaling as well as tools for working with data hierarchies in downloaded table objects

[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](https://github.com/SNStatComp/awesome-official-statistics-software/)

### Documentation
[Cansim R package home page and reference guide](https://mountainmath.github.io/cansim/index.html)

### Installation

The cansim package is available on CRAN and can be installed directly.
```r
install.packages("cansim")
```
Alternatively, the latest development version can be downloaded from Github using either the `remotes` or `devtools` packages. 
```r
# install.packages("remotes")
remotes::install_github("mountainmath/cansim")
```

### Basic Usage

The package accepts use of both old-format ("051-0013") or new-format ("17-10-0016-01") table catalogue numbers to download entire data tables as tidy data frames. Calling either the legacy CANSIM table number or the new NDM number will load the same data. Since the transition to the new data repository, existing tables will have retained their old-format numbers, but any newly created tables will have only new-format names. 

```r
# Retrieve data for births table: 17-10-0016-01 (formerly: CANSIM 051-0013)
births <- get_cansim("051-0013")
births <- get_cansim("17-10-0016-01")

# Retrieve data for balance of payment table 36-10-0042-01 (formerly CANSIM  376-8105)
bop <- get_cansim("3768105")
bop <- get_cansim("36-10-0042")
```
See more example usage and worfklow in the _Getting started with the cansim package_ vignette. 

### Caching

Many of the data tables available in Statistics Canada's data repository are quite large in size. After downloading tables, the `cansim` package will cache data in a temporary directory for the duration of the current R session. This reduces unnecessary waiting when recompiling code. To force a refresh of the data, pass the `refresh=TRUE` option in the function call. 

### Bilingual

Statistics Canada data tables are provided in either English or French formats, including labels and formats. The `cansim` package allows for download of tables in either English or French. There is an optional language argument to retrieve tables in French: 

Le paquet `cansim` fonctionne en anglais ou en français. Il existe un argument de langue optionnel pour récupérer les tables en français:
```r
naissances <- get_cansim("051-0013",language="fr")
```    

### Normalizing values

The package also contains a convenience function that will re-scale and re-label variables that are reported in thousands or millions. Statistics Canada data table values may be scaled by powers of 10. 

For example, values in the `VALUE` field may be reported in "millions", so a `VALUE` of 10 means 10,000,000. By default the {cansim} package adds a `val_norm` column with scaled values, so to get the value of the `val_norm` the `VALUE` column will be converted from 10 to 10,000,000 in the example given. Similarly, percentages will be converted to rates, so instead of being 0-100 it will be normalized to 0-1 in the `val_norm` column.

Prior to version 0.3.7 this was achieved by calling the `normalize_cansim_values` functions, the use of which is now redundant and is deprecated.

### Vectors

Many of the time-series data available from Statistics Canada have individual vector codes and some users of Canadian statistical data, who are often concerned with specific time series such as the CPI or international arrivals, will typically know the exact series they need. If, for example, you are tracking the Canadian Consumer Price Index (CPI) over time, you might already know the Statistics Canada vector code the seasonally-unadjusted all-items CPI value: *v41690973*. To retrieve just this data series on its own without all of the additional data available in related tables, we can use the `get_cansim_vector()` function with the vector code and the date onwards from which we want to get vector results for.
```{r}
get_cansim_vector("v41690973","2015-01-01")
```

To access metadata for the vectors, use
```{r}
get_cansim_vector_info("v41690973")
```

More detailed usage examples are available in the _Retrieving individual Statistics Canada vectors_ vignette. 

### Table overview metadata

The `get_cansim_table_overview` function displays an overview of table information. If the table is not yet downloaded and cached it will first download the table itself. Let's take a look what's in the table we are interested in.
```{r}
get_cansim_table_overview(36-10-040)
```

### Listing available tables

Calling `list_cansim_tables` returns a data frame with useful metadata for available tables. There are 21 fields of metadata for each table including title, in English and French, keyword sets, notes, and table numbers. 
```{r}
list_cansim_tables()
```
The appropriate table can be found by subsetting or filtering on the properties we want to use to find the appropriate tables. This work well with standard `dplyr` verbs.  
```{r}
list_cansim_tables() %>% 
  filter(grepl("Labour force characteristics",title), grepl("economic region",title)) %>% 
  select("cansim_table_number","title")
```
As table search functions require a full scrape of Statistics Canada's data repository webpages, generating this list can be quite slow so a saved list of tables is included with the package. As Statistics Canada adds additional tables and data products, the list that comes with the package will become out of date and will require refreshing. Tables can be refreshed by specifying `refresh=TRUE` when calling `list_cansim_tables`. The full list of tables can be cached locally to avoid delays and prevent unnecessary web scraping. This can (and should) be enabled by setting `options(cansim.cache_path="your cache path")` option so that table information is cached across R sessions.

The _Listing Statistics Canada data tables_ vignette has additional detail and examples. 

### License

The code in this package is licensed under the MIT license. The bundled table metadata in Sysdata.R, as well as all Statistics Canada data retrieved using this package is made available under the Statistics Canada Open Licence Agreement, a copy of which is included in the `inst` folder. The Statistics Canada Open Licence Agreement requires that: 

```
Subject to this agreement, Statistics Canada grants you a worldwide, royalty-free, non-exclusive licence to:
 
  - use, reproduce, publish, freely distribute, or sell the Information;
  - use, reproduce, publish, freely distribute, or sell Value-added Products; and,
  - sublicence any or all such rights, under terms consistent with this agreement.

In doing any of the above, you shall:
 
  - reproduce the Information accurately;
  - not use the Information in a way that suggests that Statistics Canada endorses you or your use of the Information;
  - not misrepresent the Information or its source;
  - use the Information in a manner that does not breach or infringe any applicable laws;
  - not merge or link the Information with any other databases for the purpose of attempting to identify an individual person, business or organization; and
  - not present the Information in such a manner that gives the appearance that you may have received, or had access to, information held by Statistics Canada about any identifiable individual person, business or organization.
```

### Attribution

Subject to the Statistics Canada Open Licence Agreement, licensed products using Statistics Canada data should employ the following acknowledgement of source:

```
Acknowledgment of Source

(a) You shall include and maintain the following notice on all licensed rights of the Information:

  - Source: Statistics Canada, name of product, reference date. Reproduced and distributed on an "as is" basis with the permission of Statistics Canada.
 
(b) Where any Information is contained within a Value-added Product, you shall include on such Value-added Product the following notice:

  - Adapted from Statistics Canada, name of product, reference date. This does not constitute an endorsement by Statistics Canada of this product.
```

### Why cansim?

CANSIM was the name of Statistics Canada's legacy socio-economic data repository that was widely used by practitioners, academics, and students, with many still calling the new repository by that name. Statistics Canada refers to the current repository simply as "Statistics Canada data" or "StatCan data". We use the CANSIM name for this package as a nostalgic reference.  

### Proxy issues

Some users have reported issues accessing and downloading Statistics Canada tables while behind a proxy as is sometimes the case in office environments. A quick fix for this requires specifying a proxy configuration for the `httr` package. 
```r
httr::set_config(use_proxy(url=http_proxy, port=selected_port, username=your_username,password=your_pass))
```

### Contributing

[Issues](https://github.com/mountainMath/cansim/issues) and [pull requests](https://github.com/mountainMath/cansim/pulls) are highly appreciated. 

If you want to get in touch, we are pretty good at responding via email or via twitter at [@dshkol](https://twitter.com/dshkol) or [@vb_jens](https://twitter.com/vb_jens). 

### Related packages

* There exists a [CANSIM2R package](https://CRAN.R-project.org/package=CANSIM2R) on CRAN that shares some functionality.

* [CANSIM-dataviewer](https://github.com/bcgov/CANSIM-dataviewer) is another tool that depends on the existing *CANSIM2R* package with a focus on uses for the Province of British Columbia.

* [cancensus](https://github.com/mountainMath/cancensus) is a package designed to access, retrieve, and work with Canadian Census data and geography. A stable version is available on CRAN and a development version is on Github. 

### Cite cansim

If you wish to cite the `cansim` package in your work:

  von Bergmann, J., Dmitry Shkolnik (2021). cansim: functions and convenience tools for accessing Statistics Canada data tables. v0.3.8.

A BibTeX entry for LaTeX users is
```
  @Manual{,
    author = {Jens {von Bergmann} and Dmitry Shkolnik},
    title = {cansim: functions and convenience tools for accessing Statistics Canada data tables},
    year = {2021},
    note = {R package version 0.3.8},
    url = {https://mountainmath.github.io/cansim/},
  }
```

