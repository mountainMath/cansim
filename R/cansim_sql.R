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
#' con <- get_cansim_sqlite("34-10-0013")
#'
#' # Work with the data connection
#' head(con)
#'
#' disconnect_cansim_sqlite(con)
#' }
#' @export
get_cansim_sqlite <- function(cansimTableNumber, language="english", refresh=FALSE, auto_refresh = FALSE,
                              timeout=1000,
                       cache_path=getOption("cansim.cache_path")){
  have_custom_path <- !is.null(cache_path)
  if (!have_custom_path) cache_path <- tempdir()
  cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
  cleaned_language <- cleaned_ndm_language(language)
  base_table <- naked_ndm_table_number(cansimTableNumber)
  table_name<-paste0("cansim_",base_table,"_",cleaned_language)
  cache_path <- file.path(cache_path,table_name)
  if (!dir.exists(cache_path)) dir.create(cache_path)
  path <- paste0(base_path_for_table_language(cansimTableNumber,language),".zip")
  sqlite_path <- paste0(base_path_for_table_language(cansimTableNumber,language,cache_path),".sqlite")

  last_updated <- tryCatch(get_cansim_table_last_release_date(cleaned_number), error=function(cond)return(NA))

  if (is.na(last_updated)) {
    warning("Could not determine if existing table is out of date.")
  } else {
    last_downloaded <- list_cansim_sqlite_cached_tables() %>%
      filter(.data$cansimTableNumber==cleaned_number) %>%
      pull(.data$timeCached)

    if (file.exists(sqlite_path) && auto_refresh && !is.na(last_downloaded) && !is.null(last_updated) &&
        as.numeric(last_downloaded)<as.numeric(last_updated)) {
      message(paste0("A newer version of ",cleaned_number," is available, auto-refreshing the table..."))
      refresh=TRUE
    }
  }

  if (refresh | !file.exists(sqlite_path)){
    if (cleaned_language=="eng")
      message(paste0("Accessing CANSIM NDM product ", cleaned_number, " from Statistics Canada"))
    else
      message(paste0("Acc",intToUtf8(0x00E9),"der au produit ", cleaned_number, " CANSIM NDM de Statistique Canada"))
    url=paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",file_path_for_table_language(cansimTableNumber,language),".zip")

    time_check <- Sys.time()
    response <- get_with_timeout_retry(url,path=path,timeout=timeout)
    if (is.null(response)) return(response)
    data <- NA
    na_strings=c("<NA>",NA,"NA","","F")
    exdir=file.path(tempdir(),file_path_for_table_language(cansimTableNumber,language))
    uzp <- getOption("unzip")
    if (is.null(uzp)) uzp <- "internal"
    utils::unzip(path,exdir=exdir,unzip=uzp)
    unlink(path)

    if(cleaned_language=="eng") {
      message("Parsing data")
      delim <- ","
      value_column="VALUE"
    } else {
      message(paste0("Analyser les donn",intToUtf8(0x00E9),"es"))
      delim <- ";"
      value_column="VALEUR"
    }


    meta <- suppressWarnings(readr::read_delim(file.path(exdir, paste0(base_table, "_MetaData.csv")),
                                               delim=delim,
                                               na=na_strings,
                                               #col_names=FALSE,
                                               locale=readr::locale(encoding="UTF-8"),
                                               col_types = list(.default = "c")))


    headers <- readr::read_delim(file.path(exdir, paste0(base_table, ".csv")),
                                 delim=delim,
                                 col_types = list(.default = "c"),
                                 n_max = 1) %>%
      names()

    hd <- headers[duplicated(toupper(headers))]

    if (length(hd)>0) {
      dupes <- headers[toupper(headers) %in% hd]
      unlink(exdir, recursive=TRUE)
      stop(paste0("This table has duplicated columns names: ",paste0(dupes,collapse = ", "),
                  ".\nThis is not allowed for SQLite databases, please use the 'get_cansim' method for this table."))
    }

    meta_base_path <- paste0(base_path_for_table_language(cansimTableNumber,language,cache_path),".Rda")
    parse_metadata(meta,data_path = meta_base_path)


    to_drop <- intersect(headers,"TERMINATED") # not in use yet


    scale_string <- ifelse(language=="fr","IDENTIFICATEUR SCALAIRE","SCALAR_ID")
    value_string <- ifelse(language=="fr","VALEUR","VALUE")
    # scale_string2 <- ifelse(language=="fr","FACTEUR SCALAIRE","SCALAR_FACTOR")

    dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
    geography_column <- ifelse(cleaned_language=="eng","Geography",paste0("G",intToUtf8(0x00E9),"ographie"))
    data_geography_column <- ifelse(cleaned_language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
    coordinate_column <- ifelse(cleaned_language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))

    meta2 <- readRDS(paste0(meta_base_path,"2"))
    geo_column_pos <- which(pull(meta2,dimension_name_column)==geography_column)
    if (length(geo_column_pos)==0) {
      geography_column <- ifelse(cleaned_language=="eng","Geography of origin",
                                 paste0("G",intToUtf8(0x00E9),"ographie d'origine"))
      geo_column_pos <- which(pull(meta2,dimension_name_column)==geography_column)
    }

    if (length(geo_column_pos)==1) {
      hierarchy_prefix <- ifelse(cleaned_language=="eng","Hierarchy for",paste0("Hi",intToUtf8(0x00E9),"rarchie pour"))
      hierarchy_name <- paste0(hierarchy_prefix," ", data_geography_column)
    }

    csv2sqlite(file.path(exdir, paste0(base_table, ".csv")),
               sqlite_file = sqlite_path,
               table_name=table_name,
               col_types = list(.default = "c"),
               na = na_strings,
               delim = delim,
               transform=function(data){
                 data <- data %>%
                   dplyr::mutate_at(value_string,as.numeric)
                 if (length(geo_column_pos)==1)
                   data <- data %>%
                     fold_in_metadata_for_columns(meta_base_path,geography_column) %>%
                     select(-!!as.name(hierarchy_name))
                 data
               })

    unlink(exdir,recursive = TRUE)

    date_field=ifelse(cleaned_language=="fra",paste0("P",intToUtf8(0x00C9),"RIODE DE R",intToUtf8(0x00C9),"F",intToUtf8(0x00C9),"RENCE"),"REF_DATE")

    fields <- pull(meta2,dimension_name_column) %>%
      gsub(geography_column,data_geography_column,.) %>%
      c(.,date_field,"DGUID")

    if (length(geo_column_pos)==1) fields <- c(fields,"GeoUID")

    con <- DBI::dbConnect(RSQLite::SQLite(), dbname=sqlite_path)
    db_fields <- con %>% tbl(table_name) %>% head(1) %>% collect() %>% names
    for (field in fields) {
      if (!(field %in% db_fields)) {
        geography_column <- ifelse(cleaned_language=="eng","Geography",paste0("G",intToUtf8(0x00E9),"ographie"))
        data_geography_column <- ifelse(cleaned_language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
        if (grepl(geography_column,field) && data_geography_column %in% db_fields) {
          field=data_geography_column
        }
      }
      if (field %in% db_fields) {
        message(paste0("Indexing ",field))
        create_index(con,table_name,field)
      } else {
        warning("did not know how to index field ",field)
      }
    }
    DBI::dbDisconnect(con)

    # saving timestamp
    saveRDS(strftime(time_check,format=TIME_FORMAT),paste0(meta_base_path,"_time"))


  } else {
    if (!is.na(last_updated)) {
      if (is.na(last_downloaded)) message(paste0("Could not accesses date table ",cleaned_number," was cached."))
      if (is.null(last_updated)) message(paste0("Could not accesses date table ",cleaned_number," was last updated."))
      if (!is.na(last_downloaded) && !is.null(last_updated) &&
          as.numeric(last_downloaded)<as.numeric(last_updated)) {
        ld_date <- format(as.POSIXct(last_downloaded), tz="",usetz=FALSE,format="%Y-%m-%d")
        lu_date <- format(as.POSIXct(last_updated), tz="",usetz=FALSE,format="%Y-%m-%d")
        if (ld_date==lu_date) {
          ld_date <- format(as.POSIXct(last_downloaded), tz="",usetz=FALSE,format="%Y-%m-%d %H:%M")
          lu_date <- format(as.POSIXct(last_updated), tz="",usetz=FALSE,format="%Y-%m-%d %H:%M")
        }
        warning(paste0("Cached SQLite table ",cleaned_number," is out of date, it was last downloaded and cached ",ld_date,".\n",
                       "There is a newer version of the table available, it was last updated ",
                       lu_date,".\n",
                       "Consider manually updating the cached version by passing the `refresh=TRUE` option,\n",
                       "or set it to automatically update to the newest version by setting the `auto_refresh=TRUE` option."))
      }
    }
    if (cleaned_language=="eng")
      message(paste0("Reading CANSIM NDM product ",cleaned_number)," from cache.")
    else
      message(paste0("Lecture du produit ",cleaned_number)," de CANSIM NDM ",intToUtf8(0x00E0)," partir du cache.")
  }

  if (have_custom_path||TRUE) {
    meta_base_path <- paste0(base_path_for_table_language(cansimTableNumber,language,cache_path),".Rda")
    meta_grep_string <- basename(meta_base_path)
    meta_dir_name <- dirname(meta_base_path)
    meta_files <- dir(meta_dir_name,pattern=meta_grep_string)
    for (f in meta_files) file.copy(file.path(meta_dir_name,f),file.path(tempdir(),f))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=sqlite_path) %>%
    dplyr::tbl(table_name)

  con
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
  DBI::dbDisconnect(connection$src$con)
}

#' Collect data from connection and normalize cansim table output
#'
#' @param connection A connection to a local StatCan table SQLite database as returned by \code{get_cansim_sqlite},
#' possibly with filters or other \code{dbplyr} verbs applied
#' @param replacement_value (Optional) the name of the column the manipulated value should be returned in. Defaults to adding the `val_norm` value field.
#' @param normalize_percent (Optional) When \code{true} (the default) normalizes percentages by changing them to rates
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "07")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{FALSE}).
#' @param strip_classification_code (Optional) Logical value indicating if classification code should be stripped from names. (Default set to \code{false}).
#' @param disconnect (Optional) Logical value to indicate if the database connection should be disconnected. (Default is \code{FALSE})
#' @return A tibble with the collected and normalized data
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' con <- get_cansim_sqlite("34-10-0013")
#' data <- con %>%
#'   filter(GEO=="Ontario") %>%
#'   collect_and_normalize()
#'
#' disconnect_cansim_sqlite(con)
#' }
#' @export
collect_and_normalize <- function(connection,
                                  replacement_value="val_norm", normalize_percent=TRUE,
                                  default_month="07", default_day="01",
                                  factors=FALSE,strip_classification_code=FALSE,
                                  disconnect=FALSE){
  c <- connection$ops
  while ("x" %in% names(c)) {c <- c$x}
  cansimTableNumber <- c[[1]] %>%
    gsub("^cansim_|_fra$|_eng$","",.) %>%
    cleaned_ndm_table_number()
  d <- connection %>%
    collect()

  if (nrow(d)>0){
    d <- d %>%
      normalize_cansim_values(replacement_value=replacement_value,
                              normalize_percent=normalize_percent,
                              default_month=default_month,
                              default_day=default_day,
                              factors=TRUE,
                              cansimTableNumber = cansimTableNumber)
  } else {
    message("No data selected, try adjusting your filters.")
  }
  if (disconnect) disconnect_cansim_sqlite(connection)
  d
}


#' List cached cansim SQLite database
#'
#' @param cache_path Optional, default value is `getOption("cansim.cache_path")`.
#' @return A tibble with the list of all tables that are currently cached at the given cache path.
#' @examples
#' \dontrun{
#' list_cansim_sqlite_cached_tables()
#' }
#' @export
list_cansim_sqlite_cached_tables <- function(cache_path=getOption("cansim.cache_path")){
  have_custom_path <- !is.null(cache_path)
  if (!have_custom_path) cache_path <- tempdir()
  result <- dplyr::tibble(path=dir(cache_path,"cansim_\\d+_")) %>%
    dplyr::mutate(cansimTableNumber=gsub("^cansim_|_eng$|_fra$","",.data$path) %>% cleaned_ndm_table_number()) %>%
    dplyr::mutate(language=gsub("^cansim_\\d+_","",.data$path)) %>%
    dplyr::mutate(title=NA_character_,
                  timeCached=NA_character_,
                  sqliteSize=NA_character_) %>%
    dplyr::select(.data$cansimTableNumber,.data$language,.data$timeCached,.data$sqliteSize,
                  .data$title,.data$path)

  if (nrow(result)>0) {
    result$timeCached <- do.call("c",
                                 lapply(result$path,function(p){
                                   pp <- dir(file.path(cache_path,p),"\\.Rda_time")
                                   if (length(pp)==1) {
                                     d<-readRDS(file.path(cache_path,p,pp))
                                     dd<- strptime(d,format=TIME_FORMAT)
                                   } else {
                                     dd <- strptime("1900-01-01 01:00:00",format=TIME_FORMAT)
                                   }
                                 }))
    result$sqliteSize <- do.call("c",
                                 lapply(result$path,function(p){
                                   pp <- dir(file.path(cache_path,p),"\\.sqlite")
                                   if (length(pp)==1) {
                                     d<-file.size(file.path(cache_path,p,pp))
                                     dd<- format_file_size(d,"auto")
                                   } else {
                                     dd <- NA_character_
                                   }
                                   dd
                                 }))
    result$title <- do.call("c",
                            lapply(result$path,function(p){
                              pp <- dir(file.path(cache_path,p),"\\.Rda1")
                              if (length(pp)==1) {
                                d <- readRDS(file.path(cache_path,p,pp))
                                dd <- as.character(d[1,1])
                                } else {
                                  dd <- NA_character_
                                }
                                dd
                            }))
  }

  result <- result %>%
    dplyr::mutate(timeReleased=.data$cansimTableNumber %>%
                    lapply(get_cansim_table_last_release_date) %>%
                    do.call(c, .)) %>%
    dplyr::mutate(upToDate=as.numeric(.data$timeReleased)<as.numeric(.data$timeCached))


  result
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
#' con <- get_cansim_sqlite("34-10-0013")
#' disconnect_cansim_sqlite(con)
#' remove_cansim_sqlite_cached_table("34-10-0013")
#' }
#' @export
remove_cansim_sqlite_cached_table <- function(cansimTableNumber,language=NULL,cache_path=getOption("cansim.cache_path")){
  have_custom_path <- !is.null(cache_path)
  if (!have_custom_path) cache_path <- tempdir()
  cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
  cleaned_language <- ifelse(is.null(language),c("eng","fra"),cleaned_ndm_language(language))

  tables <- list_cansim_sqlite_cached_tables(cache_path) %>%
    dplyr::filter(.data$cansimTableNumber==!!cansimTableNumber,
                  .data$language %in% cleaned_language)

  for (index in seq(1,nrow(tables))) {
    path <- tables[index,]$path
    message("Removing cached data for ",tables[index,]$cansimTableNumber," (",tables[index,]$language,")")
    unlink(file.path(cache_path,tables[index,]$path),recursive=TRUE)
  }
  NULL
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
#'
#' @return A database connection
#' @keywords internal
csv2sqlite <- function(csv_file, sqlite_file, table_name, transform=NULL,chunk_size=5000000,
                       append=FALSE,col_types=NULL,na=c(NA,"..","","...","F"),
                       text_encoding="UTF-8",delim = ",") {
  # Connect to database.
  if (!append && file.exists(sqlite_file)) file.remove(sqlite_file)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=sqlite_file)

  chunk_handler <- function(df, pos) {
    if (nrow(readr::problems(df)) > 0) print(readr::problems(df))
    if (!is.null(transform)) df <- df %>% transform
    DBI::dbWriteTable(con, table_name, as.data.frame(df), append=TRUE)
  }

  readr::read_delim_chunked(csv_file, delim=delim,
                          callback=readr::DataFrameCallback$new(chunk_handler),
                          col_types=col_types,
                          chunk_size = chunk_size,
                          locale=readr::locale(encoding = text_encoding),
                          na=na)

  DBI::dbDisconnect(con)
}
