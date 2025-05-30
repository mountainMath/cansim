# cansim

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/cansim)](https://CRAN.R-project.org/package=cansim)
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/cansim)](https://cranlogs.r-pkg.org/badges/cansim)
[![R-CMD-check](https://github.com/mountainMath/cansim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mountainMath/cansim/actions/workflows/R-CMD-check.yaml)
[![DOI](https://img.shields.io/badge/doi-10.32614/CRAN.package.cansim-#d2b24a.svg)](https://doi.org/10.32614/CRAN.package.cansim)
<!-- badges: end -->

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
births <- get_cansim("17-10-0016")

# Retrieve data for balance of payment table 36-10-0042-01 (formerly CANSIM  376-8105)
bop <- get_cansim("3768105")
bop <- get_cansim("36-10-0042")
```

See more example usage and workflow in the _Getting started with the cansim package_ vignette. 

### Caching

Many of the data tables available in Statistics Canada's data repository are quite large in size. After downloading tables, the `cansim` package will cache data in a temporary directory for the duration of the current R session. This reduces unnecessary waiting when recompiling code. To force a refresh of the data, pass the `refresh=TRUE` option in the function call. 

To cache data between sessions the `get_cansim_connection()` function retrieves and caches data in a local database and
returns a database connection. This allows for database level filtering, data manipulation, and summarizing before
calling `collect_and_normalize()` to retrieve the data as a data frame. Data retrieved this way is identical to data
retrieved via `get_cansim()`, possibly up to row order.

The call

```r
births <- get_cansim_connection("17-10-0016") |>
  collect_and_normalize()
```

will give identical output to `get_cansim("17-10-0016")`, more commonly we would filter or otherwise manipulate the data before
calling `collect_and_normalize()` to load the data into memory. For example, to filter the data to only include births
in Canada overall irrespective of gender we could use the following code:


```r
births <- get_cansim_connection("17-10-0016") |>
  dplyr::filter(GEO == "Canada",
                Gender == "Total - gender") |>
  collect_and_normalize()
```

One further difference to just calling `get_cansim()` is that the data is cached before sessions if the
"cansim.cache_path" option variable is set. Typically this will be set in the `.Rprofile` file in the home directory
to share the cache between sessions and between projects.

The function will emit a warning if the package we query is cached and a newer version is available from StatCan.
Setting the `refresh = "auto"` argument will automatically refresh the data if a newer version is available, setting
`refresh = TRUE` forces a refresh irrespective if the cached data is out of date or not.

This approach is especially useful when working with large tables, see more example usage and workflow in
the _Working with large tables_ vignette. 


### Bilingual

Statistics Canada data tables are provided in either English or French formats, including labels and formats. The `cansim` package allows for download of tables in either English or French. There is an optional language argument to retrieve tables in French: 

Le paquet `cansim` fonctionne en anglais ou en français. Il existe un argument de langue optionnel pour récupérer les tables en français:

```r
naissances <- get_cansim("051-0013",language="fr")
```    

### Normalizing values

The package also scales variables that are reported in thousands
or millions. Statistics Canada data table values may be scaled by powers of 10. 

For example, values in the `VALUE` field may be reported in "millions", so a `VALUE` of 10 means 10,000,000. By default
the **cansim** package adds a `val_norm` column with scaled values, so to get the value of the `val_norm` the `VALUE` column
will be converted from 10 to 10,000,000 in the example given. Similarly, percentages will be converted to rates, so instead
of being 0-100 it will be normalized to 0-1 in the `val_norm` column.

### Vectors

Many of the time-series data available from Statistics Canada have individual vector codes and some users of Canadian
statistical data, who are often concerned with specific time series such as the CPI or international arrivals, will
typically know the exact series they need. If, for example, you are tracking the Canadian Consumer Price Index (CPI)
over time, you might already know the Statistics Canada vector code the seasonally-unadjusted all-items CPI value: *v41690973*.
To retrieve just this data series on its own without all of the additional data available in related tables, we can use
the `get_cansim_vector()` function with the vector code and the date onward from which we want to get vector results for.

```r
get_cansim_vector("v41690973","2015-01-01")
```

To access metadata for the vectors, use
```r
get_cansim_vector_info("v41690973")
```

More detailed usage examples are available in the _Retrieving individual Statistics Canada vectors_ vignette. 

### Table overview metadata

The `get_cansim_table_overview` function displays an overview of table information. If the table is not yet downloaded and cached it will first download the table itself. Let's take a look what's in the table we are interested in.

```r
get_cansim_table_overview("36-10-040")
```

### Listing available tables

Calling `list_cansim_cubes` returns a data frame with useful metadata for available tables. There are 21 fields of metadata for each table including title, in English and French, keyword sets, notes, and table numbers. 

```r
list_cansim_cubes()
```

The appropriate table can be found by subsetting or filtering on the properties we want to use to find the appropriate tables. This work well with standard `dplyr` verbs.

```r
list_cansim_cubes() %>% 
  filter(grepl("Labour force characteristics",cubeTitleEn), grepl("economic region",cubeTitleEn)) %>% 
  select("cansim_table_number","cubeTitleEn")
```

As table search functions require a full scrape of Statistics Canada's data repository webpages, generating this list can be quite slow so a saved list of tables is included with the package. As Statistics Canada adds additional tables and data products, the list that comes with the package will become out of date and will require refreshing. Tables can be refreshed by specifying `refresh=TRUE` when calling `list_cansim_tables`. The full list of tables can be cached locally to avoid delays and prevent unnecessary web scraping. This can (and should) be enabled by setting `options(cansim.cache_path="your cache path")` option so that table information is cached across R sessions.

The _Listing Statistics Canada data tables_ vignette has additional detail and examples. 

### License

The code in this package is licensed under the MIT license. All Statistics Canada data retrieved using this package is made available under the Statistics Canada Open Licence Agreement, a copy of which is included in the `inst` folder. The Statistics Canada Open Licence Agreement requires that: 

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

* The [statcanR package](https://CRAN.R-project.org/package=statcanR) is an alternative package providing basic access to StatCan NDM tables and data discovery.

* [cancensus](https://github.com/mountainMath/cancensus) is a package designed to access, retrieve, and work with Canadian Census data and geography. The *cansim* package is designed to work in conjunction with *cancensus* and data can easily be joined on standard geographic identifiers exposed and harmonized by both packages.

* [cmhc](https://github.com/mountainMath/cmhc) is a package designed to access, retrieve, and work with CMHC data.

### Cite cansim

If you wish to cite the `cansim` package in your work:

  von Bergmann, J., Dmitry Shkolnik (2024). cansim: functions and convenience tools for accessing Statistics Canada data tables. v0.4.3. DOI: 10.32614/CRAN.package.cansim

A BibTeX entry for LaTeX users is

```
  @Manual{cansim,
    author = {Jens {von Bergmann} and Dmitry Shkolnik},
    title = {cansim: functions and convenience tools for accessing Statistics Canada data tables},
    year = {2025},
    doi = {10.32614/CRAN.package.cansim},
    note = {R package version 0.4.3},
    url = {https://mountainmath.github.io/cansim/}
  }
```

