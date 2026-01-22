TIME_FORMAT <- "%Y-%m-%d %H:%M:%S"


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
#' @param append optional parameter, append to database or overwrite, default=`FALSE`
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
