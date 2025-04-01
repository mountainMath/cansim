#' Retrieve a Statistics Canada data table using NDM catalogue number as parquet, feather, or sqlite database connection
#'
#' Retrieves a data table using an NDM catalogue number as parquet, feather, or SQLite database connection. Retrieved table data is
#' cached permanently if a cache path is supplied or for duration of the current R session.
#' If the table is cached the function will check if a newer version is available
#' and emit a warning message if the cached table is out of date.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (defaults to English)
#' @param format (Optional) The format of the data table to retrieve. Either \code{"parquet"}, \code{"feather"}, or \code{sqlite} (default is \code{"parquet"}).
#' @param partitioning (Optional) Partition columns to use for parquet or feather formats.
#' @param refresh (Optional) Valid options are \code{FALSE} (the default), \code{TRUE}, and \code{"auto"}. When set
#' to \code{TRUE}, forces a reload of data table, when set to \code{"auto"} it will refresh the table by downloading
#' the newest version from StatCan if the table is out of date. If set to \code{FALSE} and the table is out of date
#' a warning will be emitted to alert the user that the data is outdated.
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#' @param cache_path (Optional) Path to where to cache the table permanently. By default, the data is cached
#' in the path specified by `getOption("cansim.cache_path")`, if this is set. Otherwise it will use `tempdir()`.
#  Set to higher values for large tables and slow network connection. (Default is \code{1000}).
#'
#' @return A database connection to a local parquet, feather, or sqlite database with the StatCan Table data. The data
#' frames after calling `collect()` or `collect_and_normalize()` are identical up to possibly different row order.
#'
#' @examples
#' \dontrun{
#' con <- get_cansim_connection("34-10-0013")
#'
#' # Work with the data connection
#' glimpse(con)
#'
#' }
#' @export
get_cansim_connection <- function(cansimTableNumber,
                          language="english",
                          format="parquet",
                          partitioning=c(),
                          refresh=FALSE,
                          timeout=1000,
                          cache_path=getOption("cansim.cache_path")){

  if (!c(format %in% c("parquet","feather","sqlite")) || length(format)!=1) {
    stop("format must be either 'parquet', 'feather', or 'sqlite'.")
  }

  auto_refresh <- is.character(refresh) && refresh=="auto"
  refresh <- case_when(is.logical(refresh) && refresh==TRUE ~ TRUE,
                       is.character(refresh) && refresh=="TRUE" ~ TRUE,
                       TRUE ~ FALSE)

  cansimTableNumber <- cleaned_ndm_table_number(cansimTableNumber)
  have_custom_path <- !is.null(cache_path)
  if (!have_custom_path) cache_path <- tempdir()
  cleaned_number <- cansimTableNumber
  cleaned_language <- cleaned_ndm_language(language)
  base_table <- naked_ndm_table_number(cansimTableNumber)
  path_name<-paste0("cansim_",base_table,"_",format,"_",cleaned_language)
  table_name<-paste0("cansim_",base_table,"_",cleaned_language)
  cache_path <- file.path(cache_path,path_name)
  if (!dir.exists(cache_path)) dir.create(cache_path)
  path <- paste0(base_path_for_table_language(cansimTableNumber,language),".zip")
  file_extension <- ifelse(format=="feather","arrow",format)
  db_path <- paste0(base_path_for_table_language(cansimTableNumber,language,cache_path),".",file_extension)

  last_updated <- tryCatch(get_cansim_table_last_release_date(cleaned_number), error=function(cond)return(NA))

  if (is.na(last_updated)) {
    warning("Could not determine if existing table is out of date.")
  } else {
    last_downloaded <- list_cansim_cached_tables() %>%
      filter(.data$cansimTableNumber==cleaned_number, .data$dataFormat==format) %>%
      pull(.data$timeCached)

    if (file.exists(db_path) && auto_refresh && !is.na(last_downloaded) && !is.null(last_updated) &&
        as.numeric(last_downloaded)<as.numeric(last_updated)) {
      message(paste0("A newer version of ",cansimTableNumber," is available, auto-refreshing the table..."))
      refresh=TRUE
    } else if (file.exists(db_path) && auto_refresh && (is.na(last_updated)||is.na(last_downloaded))){
      message(paste0("Could not determine if ",cansimTableNumber," is up to date..."))
    }
  }

  if (refresh || !file.exists(db_path)){
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
      message("Parsing data to ",format,".")
      delim <- ","
      value_column="VALUE"
    } else {
      message(paste0("Analyser les donn",intToUtf8(0x00E9),"es du ",format,"."))
      delim <- ";"
      value_column="VALEUR"
    }


    meta_lines <- readr::read_lines(file.path(exdir, paste0(base_table, "_MetaData.csv")),
                                    locale=readr::locale(encoding="UTF-8"))

    meta_base_path <- paste0(base_path_for_table_language(cansimTableNumber,language,cache_path),".Rda")
    parse_metadata(meta_lines,data_path = meta_base_path)


    scale_string <- ifelse(language=="fr","IDENTIFICATEUR SCALAIRE","SCALAR_ID")
    value_string <- ifelse(language=="fr","VALEUR","VALUE")

    dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
    geography_column <- ifelse(cleaned_language=="eng","Geography",paste0("G",intToUtf8(0x00E9),"ographie"))
    geography_columns <- case_when(cleaned_language=="eng" ~
                                     c("Geography","Geographic name","Geography of origin"),
                                   TRUE ~ c(paste0("G",intToUtf8(0x00E9),"ographie"),
                                            paste0("Nom g",intToUtf8(0x00E9),"ographique"),
                                            paste0("G",intToUtf8(0x00E9),"ographie d'origine")))
    data_geography_column <- ifelse(cleaned_language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
    coordinate_column <- ifelse(cleaned_language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))

    meta2 <- readRDS(paste0(meta_base_path,"2"))
    geo_column_pos <- which(pull(meta2,dimension_name_column) %in% geography_columns)


    if (length(geo_column_pos)>1) geo_column_pos <- geo_column_pos[1]

    if (length(geo_column_pos)==1) {
      hierarchy_prefix <- ifelse(cleaned_language=="eng","Hierarchy for",paste0("Hi",intToUtf8(0x00E9),"rarchie pour"))
      hierarchy_name <- paste0(hierarchy_prefix," ", data_geography_column)
    }


    header <- readr::read_delim(file.path(exdir, paste0(base_table, ".csv")),
                                n_max=1,
                                delim=delim,
                                na=na_strings,
                                locale=readr::locale(encoding="UTF-8"),
                                col_types = list(.default = "c"),
                                col_names = FALSE) %>%
      as.character()

    symbols <- which(header=="Symbol")
    if (length(symbols)==0) {
      symbols <- which(header=="Symbols"|header=="Symboles")
    }


    sl <- length(symbols)

    if (sl>1) {
      header[symbols] <- paste0("Symbol ",seq(1,sl))
    }


    if (!(coordinate_column %in% header)) {
      ci <- which(grepl(coordinate_column,header,ignore.case = TRUE))
      if (length(ci)==0 && (paste0("Coordonn",intToUtf8(0x00E9),"es") %in% header | paste0("Coordonn",intToUtf8(0x00E9),"e") %in% header)) {
        ci <- which(header==paste0("Coordonn",intToUtf8(0x00E9),"es") | header==paste0("Coordonn",intToUtf8(0x00E9),"e"))
      }

      if (length(ci)==1) {
        header[ci] <- coordinate_column
      }
    }


    hd <- header[duplicated(toupper(header))]

    if (length(hd)>0) {
      dupes <- header[toupper(header) %in% hd]
      unlink(exdir, recursive=TRUE)
      stop(paste0("This table has duplicated columns names: ",paste0(dupes,collapse = ", "),
                  ".\nThis is not allowed for SQLite databases, please use the 'get_cansim' method for this table."))
    }

    if (format=="sqlite") {
      chunk_size=ceiling(5000000/pmax(sl,1))

      csv2sqlite(file.path(exdir, paste0(base_table, ".csv")),
                 sqlite_file = db_path,
                 table_name=table_name,
                 col_types = list(.default = "c"),
                 col_names = header,
                 skip=1,
                 na = na_strings,
                 delim = delim,
                 chunk_size=chunk_size,
                 transform=function(data){
                   attr(data,"language") <- cleaned_language
                   attr(data,"cansimTableNumber") <- cleaned_number
                   data <- data %>% transform_value_column(value_string)
                   if (length(geo_column_pos)==1) {
                     data <- data %>%
                       fold_in_metadata_for_columns(meta_base_path,geography_column) %>%
                       select(-!!as.name(hierarchy_name))
                   }
                   if ("DGUID" %in% names(data) && "GeoUID" %in% names(data)) {
                     data <- data %>% relocate("GeoUID",.before="DGUID")
                   }
                   data
                 })
    } else {
      csv2arrow(file.path(exdir, paste0(base_table, ".csv")),
                arrow_file = db_path,
                format = format,
                col_names = header,
                partitioning = partitioning,
                na = na_strings,
                value_column = value_string,
                delim = delim)
    }

    unlink(exdir,recursive = TRUE)

    date_field=ifelse(cleaned_language=="fra",paste0("P",intToUtf8(0x00C9),"RIODE DE R",intToUtf8(0x00C9),"F",intToUtf8(0x00C9),"RENCE"),"REF_DATE")

    if (format=="sqlite") { # add indices
      fields <- pull(meta2,dimension_name_column) %>%
        c(.,date_field,"DGUID")

      if (length(geo_column_pos)==1) fields <- c(fields,"GeoUID")

      con <- DBI::dbConnect(RSQLite::SQLite(), dbname=db_path)
      db_fields <- con %>% tbl(table_name) %>% head(1) %>% collect() %>% names
      for (field in fields) {
        if (!(field %in% db_fields)) {
          geography_column <- ifelse(cleaned_language=="eng","Geography",paste0("G",intToUtf8(0x00E9),"ographie"))
          data_geography_column <- ifelse(cleaned_language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
          if ((grepl(geography_column,field) || field %in% geography_columns )&& data_geography_column %in% db_fields) {
            field=data_geography_column
          }
        }
        if (field %in% db_fields) {
          message(paste0("Indexing ",field))
          create_index(con,table_name,field)
        } else {
          warning("Do not know how to index field ",field)
        }
      }
      DBI::dbDisconnect(con)
    }

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
        warning(paste0("Cached ",format," table ",cleaned_number," is out of date, it was last downloaded and cached ",ld_date,".\n",
                       "There is a newer version of the table available, it was last updated ",
                       lu_date,".\n",
                       "Consider manually updating the cached version by passing the `refresh=TRUE` option,\n",
                       "or set it to automatically update to the newest version by setting the `refresh='auto'` option."))
      }
    }
    if (cleaned_language=="eng")
      message(paste0("Reading CANSIM NDM product ",cleaned_number)," from ",format,".")
    else
      message(paste0("Lecture du produit ",cleaned_number)," de CANSIM NDM ",intToUtf8(0x00E0)," partir du ",format,".")
  }

  if (have_custom_path||TRUE) {
    meta_base_path <- paste0(base_path_for_table_language(cansimTableNumber,language,cache_path),".Rda")
    meta_grep_string <- basename(meta_base_path)
    meta_dir_name <- dirname(meta_base_path)
    meta_files <- dir(meta_dir_name,pattern=meta_grep_string)

    column_files <- meta_files[grepl("_column_",meta_files) & !grepl("_\\d+$",meta_files)]

    # legacy support for old column files
    if (length(column_files)>0) {
      meta2 <- readRDS(file.path(meta_dir_name,meta_files[grepl("\\.Rda2$",meta_files)]))
      for (f in column_files) {
        nn <- gsub(".+_column_","",f)
        id <- meta2[meta2[,2]==nn,1] %>% as.character()
        if (length(id)==1) {
          new_name <- f %>% gsub("_column_.+$",paste0("_column_",id),x=.)
          file.rename(file.path(meta_dir_name,f),file.path(meta_dir_name,new_name))
        }
      }
      meta_files <- dir(meta_dir_name,pattern=meta_grep_string)
    }


    meta_base_path <- table_base_path(cansimTableNumber)
    for (f in meta_files) file.copy(file.path(meta_dir_name,f),file.path(meta_base_path,f))
  }

  if (format %in% c("parquet","feather")) {
    partitioning_path <- file.path(dirname(db_path),paste0(basename(db_path),".partitioning"))
    if (file.exists(partitioning_path)) {
      used_partitioning <- readRDS(partitioning_path)
    } else {
      used_partitioning <- c()
    }

    if (!same_partitioning(used_partitioning,partitioning)) {
      pm <- ifelse(length(used_partitioning)==0,"no partitioning",
                   paste0("[",paste0(used_partitioning,collapse=","),"]"))
      message("Partitioning has changed, using ",pm,". To change the partitioning the arrow file needs to get rebuilt.")
    }

    schema_path <- file.path(dirname(db_path),paste0(basename(db_path),".schema"))
    if (file.exists(schema_path)) { # workaround when arrow drops schema info for partitioning variables
      arrow_schema <- arrow::open_dataset(schema_path,format=format) %>% arrow::schema()
      # workaround for arrow dropping attributes
      arrow_schema <- arrow_schema$WithMetadata(list("cansimTableNumber"=cansimTableNumber,
                                                     "language"=cleaned_language,
                                                     "partitioning"=list(partitioning)))
      con <- arrow::open_dataset(db_path,format=format, schema=arrow_schema)
    } else {
      con <- arrow::open_dataset(db_path,format=format)
    }
  } else {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname=db_path) %>%
      dplyr::tbl(table_name)
  }

  attr(con,"language") <- cleaned_language
  attr(con,"cansimTableNumber") <- cansimTableNumber

  con
}


#' convert csv to arrow
#'
#' @param csv_file input csv path
#' @param arrow_file output arrow database path
#' @param format format of arrow file, "parquet" or "feather" (default parquet)
#' @param col_names column names of the csv file
#' @param value_column name of the value column with numeric data
#' @param partitioning optional partition columns
#' @param na na character strings
#' @param text_encoding encoding of csv file (default UTF-8)
#' @param delim (Optional) csv deliminator, default is ","
#'
#' @return A database connection
#' @keywords internal
csv2arrow <- function(csv_file, arrow_file, format="parquet",
                      col_names, value_column = "VALUE",
                      partitioning = c(),
                       na=c(NA,"..","","...","F"),
                       text_encoding="UTF-8",delim = ",") {

  if (file.exists(arrow_file)) unlink(arrow_file,recursive=TRUE)

  value_columns <- col_names[col_names==value_column| grepl(" \\(\\d+[A-Z]*\\)\\:.+\\[\\d+\\]$",col_names)]

  col_types <- setNames(rep("c",length(col_names)),col_names)
  col_types[value_columns] <- "n"

  arrow_schema <- readr::read_csv(csv_file,col_names=col_names,
                                  col_types = col_types,
                                  n_max = 10,skip=1) %>%
    arrow::arrow_table() %>%
    arrow::schema()



  input <- arrow::read_delim_arrow(csv_file,
                                   skip=1,
                                   delim=delim,
                                   as_data_frame = FALSE,
                                   col_types=arrow_schema,
                                   na=na,
                                   read_options = arrow::csv_read_options(encoding=text_encoding,
                                                                          skip_rows=1,
                                                                          column_names=col_names))

  if ("DGUID" %in% names(input)) {
    input <- input %>%
      mutate(GeoUID=stringr::str_sub(.data$DGUID,10,-1) %>% as.character()) # ,.before=.data$DGUID)
  }

  if (!is.null(partitioning) && length(partitioning)>0) {
    if (setdiff(partitioning,names(input)) %>% length() > 0) {
      stop("Partition columns must be present in the input data")
    }
  }

  if (format=="feather" && arrow::codec_is_available("LZ4_FRAME")) {
    arrow::write_dataset(input, format=format, arrow_file,partition=partitioning,codec=arrow::Codec$create("LZ4_FRAME"))
  } else {
    arrow::write_dataset(input, format=format, arrow_file,partition=partitioning)
  }
  # workaround when arrow drops schema info for partitioning variables
  schema_path <- file.path(dirname(arrow_file),paste0(basename(arrow_file),".schema"))
  partitioning_path <- file.path(dirname(arrow_file),paste0(basename(arrow_file),".partitioning"))
  arrow::write_dataset(input %>% dplyr::slice_head(n=1), format=format, schema_path)
  saveRDS(partitioning,partitioning_path)
}


#' Repartitions a cached cansim table to a new partitioning scheme
#'
#' Repartitions and already downloaded and cached parquet or feather dataset
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (defaults to English)
#' @param format (Optional) The format of the data table to retrieve. Either \code{"parquet"}, \code{"feather"}, or \code{sqlite} (default is \code{"parquet"}).
#' @param new_partitioning (Optional) Partition columns to use for parquet or feather formats.
#' @param cache_path (Optional) Path to where to cache the table permanently. By default, the data is cached
#' in the path specified by `getOption("cansim.cache_path")`, if this is set. Otherwise it will use `tempdir()`.
#  Set to higher values for large tables and slow network connection. (Default is \code{1000}).
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' cansim_repartition_cached_table("34-10-0013",new_partitioning=c("GeoUID"))
#'
#' }
#' @export
cansim_repartition_cached_table <- function(cansimTableNumber,
                                            new_partitioning=c(),
                                            language="english",
                                            format="parquet",
                                            cache_path=getOption("cansim.cache_path")){
  if (!(format %in% c("parquet","feather"))) {
    stop("Format must be one of 'parquet' or 'feather'.")
  }

  cansimTableNumber <- cleaned_ndm_table_number(cansimTableNumber)
  cleaned_language <- cleaned_ndm_language(language)
  base_table <- naked_ndm_table_number(cansimTableNumber)
  path_name<-paste0("cansim_",base_table,"_",format,"_",cleaned_language)
  table_name<-paste0("cansim_",base_table,"_",cleaned_language)

  cache_path <- file.path(cache_path,path_name)
  if (!dir.exists(cache_path)) {
    stop("Table ",cansimTableNumber, " for ",language," and ",format," not found in cache, download it with the desired partitioning using `get_cansim_connection`")
  }
  file_extension <- ifelse(format=="feather","arrow",format)
  db_path <- paste0(base_path_for_table_language(cansimTableNumber,language,cache_path),".",file_extension)
  if (!dir.exists(db_path)) {
    stop("Table not found in cache, download it with the desired partitioning using `get_cansim_connection`")
  }

  schema_path <- file.path(dirname(db_path),paste0(basename(db_path),".schema"))
  partitioning_path <- file.path(dirname(db_path),paste0(basename(db_path),".partitioning"))
  if (file.exists(partitioning_path)) {
    old_partitioning <- readRDS(partitioning_path)

    if (same_partitioning(old_partitioning,new_partitioning)) {
      message("Partitioning is the same as the existing table, no need to repartition")
      return(invisible())
    }
  }

  if (!file.exists(schema_path)) {
    stop("Could not access existing database schema, please redownload the table.")
  }
  arrow_schema <- arrow::open_dataset(schema_path, format=format) %>% arrow::schema()
  old_path <- paste0(db_path,".old")
  file.rename(db_path,old_path)

  old_db <- arrow::open_dataset(old_path, format=format, schema=arrow_schema)
  new_db <- arrow::write_dataset(old_db, format=format, db_path, partition=new_partitioning)

  unlink(old_path,recursive=TRUE)

  saveRDS(new_partitioning,partitioning_path)
  invisible()
}

#' Collect data from a parquet, feather or sqlite query and normalize cansim table output
#'
#' @param connection A connection to a local arrow connection as returned by \code{get_cansim_connection},
#' possibly with filters or other \code{dplyr} verbs applied
#' @param replacement_value (Optional) the name of the column the manipulated value should be returned in. Defaults to adding the `val_norm` value field.
#' @param normalize_percent (Optional) When \code{true} (the default) normalizes percentages by changing them to rates
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "07")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{FALSE}).
#' @param strip_classification_code (Optional) Logical value indicating if classification code should be stripped from names. (Default set to \code{false}).
#' @param disconnect (Optional) Only used when format is sqlite. Logical value to indicate if the SQLite database connection should be disconnected. (Default is \code{FALSE})
#' @return A tibble with the collected and normalized data
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' con <- get_cansim_connection("34-10-0013")
#' data <- con %>%
#'   filter(GEO=="Ontario") %>%
#'   collect_and_normalize()
#'
#' }
#' @export

collect_and_normalize <- function(connection,
                                replacement_value="val_norm", normalize_percent=TRUE,
                                default_month="07", default_day="01",
                                factors=TRUE,strip_classification_code=FALSE,
                                disconnect=FALSE) {

  cansimTableNumber <- attr(connection,"cansimTableNumber")
  language <- attr(connection,"language")

  if ((is.null(language)||is.null(cansimTableNumber))&& ("arrow_dplyr_query" %in% class(connection) || "ArrowObject" %in% class(connection))) {
    md <- arrow::schema(connection)$metadata
    cansimTableNumber <- md$cansimTableNumber
    language <- md$language
    if ((is.null(language)||is.null(cansimTableNumber))) {
      stop("Connection does not have the necessary attributes to normalize the data")
    }
  }

  value_string <- ifelse(language=="fr","VALEUR","VALUE")

  data <- NULL
  if ("tbl_sql" %in% class(connection)) {
    data <- connection %>% dplyr::collect()
    attr(data,"language") <- language
    attr(data,"cansimTableNumber") <- cansimTableNumber

    if (disconnect) disconnect_cansim_sqlite(connection)
  } else if ("arrow_dplyr_query" %in% class(connection) || "ArrowObject" %in% class(connection)){
    data <- connection %>%
      dplyr::as_tibble()
    attr(data,"language") <- language
    attr(data,"cansimTableNumber") <- cansimTableNumber

    if (nrow(data)>0){
      data <- data %>%
        transform_value_column(value_string)
    }
  } else {
    stop("Don't know how to handle connections of class ",paste0(class(connection),collapse=", "))
  }

  if (nrow(data)>0){
    data <- data %>%
      normalize_cansim_values(replacement_value=replacement_value,
                              normalize_percent=normalize_percent,
                              default_month=default_month,
                              default_day=default_day,
                              factors=factors,
                              cansimTableNumber = cansimTableNumber,
                              internal = TRUE)
  } else {
    message("No data selected, try adjusting your filters.")
  }

  data
}


#' List cached cansim arrow and SQlite databases
#'
#' @param cache_path Optional, default value is `getOption("cansim.cache_path")`.
#' @param refresh Optional, refresh the last updated date of cached cansim tables
#' @return A tibble with the list of all tables that are currently cached at the given cache path.
#' @examples
#' \dontrun{
#' list_cansim_cached_tables()
#' }
#' @export
list_cansim_cached_tables <- function(cache_path=getOption("cansim.cache_path"),refresh=FALSE){
  have_custom_path <- !is.null(cache_path)
  if (!have_custom_path) cache_path <- tempdir()

  # check for legacy tables
  result <- dplyr::tibble(path=dir(cache_path,"cansim_\\d+_eng|cansim_\\d+_fra"))
  if (nrow(result)>0) {
    message("Transitioning legacy sqlite tables...")
    for (i in 1:nrow(result)){
      path <- result$path[i]
      full_path <- file.path(cache_path,path)
      files <- dir(full_path)
      if (length(files)==0||length(files[grepl("\\.sqlite$",files)])==0) {
        message("No sqlite files found for ",path, " cleaning up....")
        unlink(full_path, recursive=TRUE)
      } else {
        message("Transitioning ",path," to new format")
        new_path <- path %>%
          gsub("_eng$","_sqlite_eng",x=.) %>%
          gsub("_fra$","_sqlite_fra",x=.)
        full_new_path <- file.path(cache_path,new_path)
        if (dir.exists(full_new_path)) {
          message("Already have new sqlite table, removing legacy table ",path)
          unlink(full_path,recursive=TRUE)
        } else {
          rr <- file.rename(full_path,full_new_path)
          if (!rr) {
            message("Problem transitioning ",path," please try to manually remove the leagacy table at ",full_path)
          }
        }
      }
    }
  }

  result <- dplyr::tibble(path=dir(cache_path,"cansim_\\d+_parquet_eng|cansim_\\d+_parquet_fra|cansim_\\d+_feather_eng|cansim_\\d+_feather_fra|cansim_\\d+_sqlite_eng|cansim_\\d+_sqlite_fra")) %>%
    dplyr::mutate(cansimTableNumber=gsub("^cansim_|_eng$|_fra$|_parquet_eng$|_parquet_fra|_feather_eng$|_feather_fra|_sqlite_eng$|_sqlte_fra$","",.data$path) %>% cleaned_ndm_table_number()) %>%
    dplyr::mutate(dataFormat=case_when(grepl("_parquet",.data$path)~"parquet",
                                     grepl("_feather",.data$path)~"feather",
                                     grepl("_sqlite",.data$path)~"sqlite",
                                     TRUE ~ "UNKNOWN")) %>%
    dplyr::mutate(language=gsub("^cansim_\\d+_sqlite_|^cansim_\\d+_parquet_|^cansim_\\d+_feather_","",.data$path)) %>%
    dplyr::mutate(title=NA_character_,
                  timeCached=NA_character_,
                  rawSize=NA_real_,
                  niceSize=NA_character_)

    if (nrow(result)>0) {
      result <- result %>%
        dplyr::select("cansimTableNumber","language","dataFormat","timeCached","niceSize","rawSize",
                      "title","path")
    }

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
    result$rawSize <- do.call("c",
                           lapply(result$path,function(p){
                             pp <- dir(file.path(cache_path,p),"\\.sqlite$|\\.arrow$|\\.parquet$")
                             if (length(pp)==1) {
                               file_path <- file.path(cache_path,p,pp)
                               if (dir.exists(file_path)) {
                                 d<-list.files(file.path(cache_path,p,pp),full.names = TRUE,recursive = TRUE) %>%
                                   lapply(file.size) %>%
                                   unlist() %>%
                                   sum()
                               } else {
                                d<-file.size(file.path(cache_path,p,pp))
                               }
                             } else {
                               d <- NA_real_
                             }
                             d
                           }))
    result$niceSize <-  do.call("c",lapply(result$rawSize,\(x)ifelse(is.na(x),NA_real_,format_file_size(x,"auto"))))
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

  cube_info <- list_cansim_cubes(lite=TRUE,refresh = refresh,quiet=TRUE)

  if (!is.null(cube_info)) {
    cube_info <- cube_info %>%
      select(cansimTableNumber="cansim_table_number",timeReleased="releaseTime")
    result <- result %>%
      dplyr::left_join(cube_info,by="cansimTableNumber")  %>%
      dplyr::mutate(upToDate=as.numeric(.data$timeReleased)<as.numeric(.data$timeCached))
  } else {
    result <- result %>%
      dplyr::mutate(timeReleased=NA,upToDate=NA)
  }

  result
}



#' Remove cached cansim SQLite and parquet database
#'
#' @param cansimTableNumber Vector of the table(s) to be removed, or a (filtered) table as returned by `list_cansim_cached_tables`
#' with the list of tables to be removed.
#' @param format Format of cache to remove, possible values are `"parquet"`, `"feather"` or `"sqlite"` or a subset of these (the default is all of these)
#' @param language Language for which to remove the cached data. If unspecified (`NULL`) tables for all languages will be removed.
#' @param cache_path Optional, default value is `getOption("cansim.cache_path")`
#' @return `NULL``
#'
#' @examples
#' \dontrun{
#' con <- get_cansim_connection("34-10-0013", format="parquet")
#' remove_cansim_cached_tables("34-10-0013", format="parquet")
#' }
#' @export
remove_cansim_cached_tables <- function(cansimTableNumber, format=c("parquet","feather","sqlite"), language=NULL,
                                              cache_path=getOption("cansim.cache_path")){

  format=tolower(format)
  if (length(setdiff(format,c("parquet","sqlite","feather")))>0) {
    stop("Invalid format, must be a subset of 'parquet', 'sqlite', or 'sqlite'.")
  }
  if (is.null(language)) language <- c("eng","fra")
  cleaned_language <- cleaned_ndm_language(language)

  have_custom_path <- !is.null(cache_path)
  if (!have_custom_path) cache_path <- tempdir()

  if (is.data.frame(cansimTableNumber)) {
    if (!sum(c("cansimTableNumber","language","dataFormat") %in% colnames(cansimTableNumber))) {
      stop("cansimTableNumber must be a character vector or a (filtered) data frame as returned by list_cansim_cached_tables.")
    }
    # ensure that tables actually exist
    tables <- cansimTableNumber %>%
      select("cansimTableNumber","language","dataFormat") %>%
      inner_join(list_cansim_cached_tables(cache_path),by=c("cansimTableNumber","language","dataFormat"))
  } else {
    cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
    tables <- list_cansim_cached_tables(cache_path) %>%
      dplyr::filter(.data$cansimTableNumber %in% cleaned_number,
                    .data$language %in% cleaned_language,
                    .data$dataFormat %in% format)
  }

  if (nrow(tables)==0) {
    message("No cached data found matching specified filter.")
  } else {
    for (index in seq(1,nrow(tables))) {
      path <- tables[index,]$path
      message("Removing ", tables[index,]$dataFormat," cached data for ",tables[index,]$cansimTableNumber," (",tables[index,]$language,")")
      unlink(file.path(cache_path,path),recursive=TRUE)
    }
  }
  invisible()
}

