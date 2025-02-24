TIME_FORMAT <- "%Y-%m-%d %H:%M:%S"



#' Retrieve a Statistics Canada data table using NDM catalogue number as SQLite database connection
#'
#' Retrieves a data table using an NDM catalogue number as an SQLite table. Retrieved table data is
#' cached permanently if a cache path is supplied or for duration of the current R session.
#' The function will check against the lastest release data for the table and emit a warning message
#' if the cached table is out of date.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (defaults to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param auto_refresh (Optional) When set to \code{TRUE}, it will reload of data table if a new version is available (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#' @param cache_path (Optional) Path to where to cache the table permanently. By default, the data is cached
#' in the path specified by `getOption("cansim.cache_path")`, if this is set. Otherwise it will use `tempdir()`.
#  Set to higher values for large tables and slow network connection. (Default is \code{1000}).
#'
#' @return A database connection to a local SQLite database with the StatCan Table data.
#'
#' @examples
#' \dontrun{
#' con <- get_cansim_connection("34-10-0013", format="sqlite")
#'
#' # Work with the data connection
#' gplimpse(con)
#'
#' disconnect_cansim_sqlite(con)
#' }
#' @keywords internal
#' @export
get_cansim_sqlite <- function(cansimTableNumber, language="english", refresh=FALSE, auto_refresh = FALSE,
                              timeout=1000,
                       cache_path=getOption("cansim.cache_path")){
  .Deprecated("get_cansim_connection",
              package="cansim",
              msg="This function has been deprecated, it will be removed in future versions. Please use get_cansim_connection(..., format='sqlite'') instead.")

  if (auto_refresh){
    refresh <- "auto"
  }

  get_cansim_connection(cansimTableNumber=cansimTableNumber,
                language=language,
                format="sqlite",
                refresh=refresh,
                timeout=timeout,
                cache_path=cache_path)
}

#' Disconnect from a cansim database connection
#'
#' @param connection connection to database
#' @return `NULL``
#'
#' @examples
#' \dontrun{
#' con <- get_cansim_sqlite("34-10-0013")
#' disconnect_cansim_sqlite(con)
#' }
#' @export
disconnect_cansim_sqlite <- function(connection){
  if ("tbl_sql" %in% class(connection)) {
    DBI::dbDisconnect(connection$src$con)
  }
  invisible()
}




#' List cached cansim SQLite database
#'
#' @param cache_path Optional, default value is `getOption("cansim.cache_path")`.
#' @param refresh Optional, refresh the last updated date of cached cansim tables
#' @return A tibble with the list of all tables that are currently cached at the given cache path.
#' @examples
#' \dontrun{
#' list_cansim_cached_tables()
#' }
#' @keywords internal
#' @export
list_cansim_sqlite_cached_tables <- function(cache_path=getOption("cansim.cache_path"),refresh=FALSE){
  .Deprecated("list_cansim_cached_tables",
              package="cansim",
              msg="This function has been deprecated, it will be removed in future versions. Please use list_cansim_cached_tables(...) instead.")

  list_cansim_cached_tables(cache_path=cache_path, refresh=refresh) %>%
    filter(.data$dataFormat=="sqlite")
}

#' Remove cached cansim SQLite database
#'
#' @param cansimTableNumber Number of the table to be removed
#' @param language Language for which to remove the cached data. If unspecified (`NULL`) tables for all languages
#' will be removed
#' @param cache_path Optional, default value is `getOption("cansim.cache_path")`
#' @return `NULL``
#'
#' @examples
#' \dontrun{
#' con <- get_cansim_connection("34-10-0013", format="sqlite")
#' disconnect_cansim_sqlite(con)
#' remove_cansim_cached_tables("34-10-0013", format="sqlite")
#' }
#' @keywords internal
#' @export
remove_cansim_sqlite_cached_table <- function(cansimTableNumber,language=NULL,cache_path=getOption("cansim.cache_path")){
  .Deprecated("remove_cansim_cached_tables",
              package="cansim",
              msg="This function has been deprecated, it will be removed in future versions. Please use remove_cansim_cached_tables(..., format='sqlite'') instead.")

  remove_cansim_cached_tables(cansimTableNumber=cansimTableNumber,language=language,format="sqlite",cache_path=cache_path)
}


#' create database index
#'
#' @param connection connection to database
#' @param table_name sql table name
#' @param field name of field to index
#' @return `NULL``
#' @keywords internal
create_index <- function(connection,table_name,field){
  field_index=paste0("index_",gsub("[^[:alnum:]]","_",field))
  query=paste0("CREATE INDEX IF NOT EXISTS ",field_index," ON ",table_name," (`",field,"`)")
  #print(query)
  r<-DBI::dbSendQuery(connection,query)
  DBI::dbClearResult(r)
  NULL
}




#' convert csv to sqlite
#' adapted from https://rdrr.io/github/coolbutuseless/csv2sqlite/src/R/csv2sqlite.R
#'
#' @param csv_file input csv path
#' @param sqlite_file output sql database path
#' @param table_name sql table name
#' @param transform optional function that transforms each chunk
#' @param chunk_size optional chunk size to read/write data, default=1,000,000
#' @param append optional parameter, append to database or overwrite, defaul=`FALSE`
#' @param col_types optional parameter for csv column types
#' @param na na character strings
#' @param text_encoding encoding of csv file (default UTF-8)
#' @param delim (Optional) csv deliminator, default is ","
#' @param ... (Optional) additional parameters passed to `readr::read_delim_chunked`
#'
#' @return A database connection
#' @keywords internal
csv2sqlite <- function(csv_file, sqlite_file, table_name, transform=NULL,chunk_size=5000000,
                       append=FALSE,col_types=NULL,na=c(NA,"..","","...","F"),
                       text_encoding="UTF-8",delim = ",",...) {
  # Connect to database.
  if (!append && file.exists(sqlite_file)) file.remove(sqlite_file)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=sqlite_file)

  chunk_handler <- function(df, pos) {
    if (nrow(readr::problems(df)) > 0) print(readr::problems(df))
    if (!is.null(transform)) df <- df %>% transform()
    DBI::dbWriteTable(con, table_name, as.data.frame(df), append=TRUE)
  }

  readr::read_delim_chunked(csv_file, delim=delim,
                          callback=readr::DataFrameCallback$new(chunk_handler),
                          col_types=col_types,
                          chunk_size = chunk_size,
                          locale=readr::locale(encoding = text_encoding),
                          na=na,
                          ...)

  DBI::dbDisconnect(con)
}
