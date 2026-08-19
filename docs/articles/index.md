# Articles

### All vignettes

- [Getting started with the cansim
  package](https://mountainmath.github.io/cansim/articles/cansim.md):

  This vignette provides an overview of the `cansim` package, including
  how to install it, how to retrieve data from Statistics Canada’s NDM,
  and how to work with the data.

- [Listing Statistics Canada data
  tables](https://mountainmath.github.io/cansim/articles/listing_cansim_tables.md):

  Data discovery and pinpointing the best data to use is an important
  and often challenging aspect of analysis. The package offers several
  methods to programmatically search through data available via the
  StatCan NDM.

- [Partial table data
  download](https://mountainmath.github.io/cansim/articles/partial_table_data_download.md):

  Data tables can be quite large, in many applications it makes sense to
  only download a small subset of a table. The package can create a
  synthetic template table containing all possible combinations of
  categories in each dimension that can be used for filtering and
  pinpointing data of interest, and then downloading just that data
  instead of the entire table.

- [Retrieving individual Statistics Canada
  vectors](https://mountainmath.github.io/cansim/articles/retrieving_cansim_vectors.md):

  The `cansim` package provides functionality to retrieve individual
  Statistics Canada vectors or access data by table and coordinate,
  which are often used for specific time series data and is generally
  faster than downloading entire tables.

- [Working with Statistics Canada data table object
  hierarchies](https://mountainmath.github.io/cansim/articles/working_with_hierarchies.md):

  Statistics Canada data tables comes with rich metadata, including on
  the hierarchy of categories in each dimension. The packages
  incorporates this metadata and allows for selecting data by hierarchy
  level.

- [Working with large
  tables](https://mountainmath.github.io/cansim/articles/working_with_large_tables.md):

  Very large StatCan tables can pose challenges for memory footprint and
  performace. We show how to overcome these challenges by accessing the
  data through a parquet or SQLite database connection.
