# Package index

## Retrieving data

Retrieve data cubes, tables, and vectors from Statistics Canada

- [`get_cansim()`](https://mountainmath.github.io/cansim/reference/get_cansim.md)
  : Retrieve a Statistics Canada data table using NDM catalogue number
- [`get_cansim_data_for_table_coord_periods()`](https://mountainmath.github.io/cansim/reference/get_cansim_data_for_table_coord_periods.md)
  : Retrieve data for specified Statistics Canada data product for last
  N periods for specific coordinates
- [`get_cansim_vector()`](https://mountainmath.github.io/cansim/reference/get_cansim_vector.md)
  : Retrieve data for a Statistics Canada data vector released within a
  given time frame
- [`get_cansim_vector_for_latest_periods()`](https://mountainmath.github.io/cansim/reference/get_cansim_vector_for_latest_periods.md)
  : Retrieve data for specified Statistics Canada data vector(s) for
  last N periods

## Local database caching

Managing data in local database

- [`get_cansim_connection()`](https://mountainmath.github.io/cansim/reference/get_cansim_connection.md)
  : Retrieve a Statistics Canada data table using NDM catalogue number
  as parquet, feather, or sqlite database connection
- [`collect_and_normalize()`](https://mountainmath.github.io/cansim/reference/collect_and_normalize.md)
  : Collect data from a parquet, feather or sqlite query and normalize
  cansim table output
- [`disconnect_cansim_connection()`](https://mountainmath.github.io/cansim/reference/disconnect_cansim_connection.md)
  : Disconnect from a cansim connection
- [`list_cansim_cached_tables()`](https://mountainmath.github.io/cansim/reference/list_cansim_cached_tables.md)
  : List cached cansim arrow and SQlite databases
- [`remove_cansim_cached_tables()`](https://mountainmath.github.io/cansim/reference/remove_cansim_cached_tables.md)
  : Remove cached cansim SQLite and parquet database
- [`disconnect_cansim_sqlite()`](https://mountainmath.github.io/cansim/reference/disconnect_cansim_sqlite.md)
  : Disconnect from a cansim database connection (deprecated)
- [`cansim_repartition_cached_table()`](https://mountainmath.github.io/cansim/reference/cansim_repartition_cached_table.md)
  : Repartitions a cached cansim table to a new partitioning scheme
- [`set_cansim_cache_path()`](https://mountainmath.github.io/cansim/reference/set_cansim_cache_path.md)
  : Set persistent cansim cache location
- [`show_cansim_cache_path()`](https://mountainmath.github.io/cansim/reference/show_cansim_cache_path.md)
  : View saved cache directory path

## Locating data

Help with data discovery

- [`list_cansim_cubes()`](https://mountainmath.github.io/cansim/reference/list_cansim_cubes.md)
  : Get overview list for all Statistics Canada data cubes
- [`search_cansim_cubes()`](https://mountainmath.github.io/cansim/reference/search_cansim_cubes.md)
  : Search through Statistics Canada data cubes
- [`get_cansim_changed_tables()`](https://mountainmath.github.io/cansim/reference/get_cansim_changed_tables.md)
  : Retrieve a list of modified tables since a given date
- [`get_cansim_table_last_release_date()`](https://mountainmath.github.io/cansim/reference/get_cansim_table_last_release_date.md)
  : Get the latest release data for a StatCan table, if available
- [`get_cansim_key_release_schedule()`](https://mountainmath.github.io/cansim/reference/get_cansim_key_release_schedule.md)
  : Major economic indicator release schedule
- [`get_cansim_changed_series_data_for_vectors()`](https://mountainmath.github.io/cansim/reference/get_cansim_changed_series_data_for_vectors.md)
  : Retrieve data for series that changed, by vector
- [`get_cansim_changed_series_data_for_coordinates()`](https://mountainmath.github.io/cansim/reference/get_cansim_changed_series_data_for_coordinates.md)
  : Retrieve data for series that changed, by table and coordinate

## Metadata and information

Browse metadata and table contents

- [`get_cansim_cube_metadata()`](https://mountainmath.github.io/cansim/reference/get_cansim_cube_metadata.md)
  : Retrieve table metadata from Statistics Canada API
- [`get_cansim_table_info()`](https://mountainmath.github.io/cansim/reference/get_cansim_table_info.md)
  : Retrieve Statistics Canada data table information
- [`get_cansim_table_last_release_date()`](https://mountainmath.github.io/cansim/reference/get_cansim_table_last_release_date.md)
  : Get the latest release data for a StatCan table, if available
- [`get_cansim_table_notes()`](https://mountainmath.github.io/cansim/reference/get_cansim_table_notes.md)
  : Retrieve Statistics Canada data table notes and column categories
- [`get_cansim_table_overview()`](https://mountainmath.github.io/cansim/reference/get_cansim_table_overview.md)
  : Retrieve Statistics Canada data table overview text
- [`get_cansim_table_short_notes()`](https://mountainmath.github.io/cansim/reference/get_cansim_table_short_notes.md)
  : Retrieve Statistics Canada data table short notes
- [`get_cansim_table_subject()`](https://mountainmath.github.io/cansim/reference/get_cansim_table_subject.md)
  : Retrieve Statistics Canada data table subject detail
- [`get_cansim_table_survey()`](https://mountainmath.github.io/cansim/reference/get_cansim_table_survey.md)
  : Retrieve Statistics Canada data table survey detail
- [`get_cansim_table_template()`](https://mountainmath.github.io/cansim/reference/get_cansim_table_template.md)
  : Retrieve table template from Statistics Canada API
- [`get_cansim_table_url()`](https://mountainmath.github.io/cansim/reference/get_cansim_table_url.md)
  : Retrieve a Statistics Canada data table URL given a table number
- [`add_cansim_vectors_to_template()`](https://mountainmath.github.io/cansim/reference/add_cansim_vectors_to_template.md)
  : Retrieve series info for given table id and coordinates
- [`get_cansim_series_info_cube_coord()`](https://mountainmath.github.io/cansim/reference/get_cansim_series_info_cube_coord.md)
  : Retrieve series info for given table id and coordinates
- [`get_cansim_vector_info()`](https://mountainmath.github.io/cansim/reference/get_cansim_vector_info.md)
  : Retrieve metadata for specified Statistics Canada data vectors
- [`get_cansim_column_categories()`](https://mountainmath.github.io/cansim/reference/get_cansim_column_categories.md)
  : Retrieve Statistics Canada data table categories for a specific
  column
- [`get_cansim_column_list()`](https://mountainmath.github.io/cansim/reference/get_cansim_column_list.md)
  : Retrieve Statistics Canada data table column list
- [`view_cansim_webpage()`](https://mountainmath.github.io/cansim/reference/view_cansim_webpage.md)
  : View CANSIM table or vector information in browser
- [`categories_for_level()`](https://mountainmath.github.io/cansim/reference/categories_for_level.md)
  : Use metadata to extract categories for column of specific level

## Miscellaneous

Helper functions

- [`add_provincial_abbreviations()`](https://mountainmath.github.io/cansim/reference/add_provincial_abbreviations.md)
  : Add provincial abbreviations as factor
- [`cansim_old_to_new()`](https://mountainmath.github.io/cansim/reference/cansim_old_to_new.md)
  : Translate deprecated CANSIM table number into new NDM-format table
  catalogue number
- [`get_cansim_code_set()`](https://mountainmath.github.io/cansim/reference/get_cansim_code_set.md)
  : Get NDM code sets
