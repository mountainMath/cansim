# Session-level cache for connection metadata to reduce redundant queries
.cansim_connection_cache <- new.env(parent = emptyenv())

#' Clear connection metadata cache
#'
#' @return NULL
#' @keywords internal
clear_connection_cache <- function() {
  rm(list = ls(envir = .cansim_connection_cache), envir = .cansim_connection_cache)
  invisible(NULL)
}

#' Get cached connection metadata
#'
#' @param cache_key unique key for this connection
#' @return cached metadata or NULL
#' @keywords internal
get_cached_connection_metadata <- function(cache_key) {
  if (exists(cache_key, envir = .cansim_connection_cache)) {
    get(cache_key, envir = .cansim_connection_cache)
  } else {
    NULL
  }
}

#' Set cached connection metadata
#'
#' @param cache_key unique key for this connection
#' @param metadata metadata to cache
#' @return NULL
#' @keywords internal
set_cached_connection_metadata <- function(cache_key, metadata) {
  assign(cache_key, metadata, envir = .cansim_connection_cache)
  invisible(NULL)
}


cleaned_ndm_table_number <- function(cansimTableNumber){
  if (is.numeric(cansimTableNumber)) {
    warning(paste0("The cansim table number ",cansimTableNumber," used in this query is numeric,\n",
                   "it is safer to encode table numbers as character strings."))
    cansimTableNumber <- as.character(cansimTableNumber)
  }
  n<-gsub("-","",cansimTableNumber) %>%
    lapply(function(t){
      if (nchar(t)<=7) {
        tt<-cansim_old_to_new(t)
        message("Legacy table number ",cansimTableNumber,", converting to NDM ",tt)
        t=gsub("-","",tt)
      }
      tn <- paste0(substr(t,1,2),"-",substr(t,3,4),"-",substr(t,5,8))

      if (nchar(t)==10) {
        end_string <- substr(t,9,10)
        if (end_string !="01") {
          warning(paste0("The {cansim} package can only retrieve 'base' tables, those ending in '-01'.\n",
                         "To get derived tables like ",tn,"-",end_string," you will have to perform the\n",
                         "necessary data manipulations manually."))
        }
      }
      tn
    }) %>% unlist

  n
}

naked_ndm_table_number <- function(cansimTableNumber){
  as.character(gsub("-","",cleaned_ndm_table_number(cansimTableNumber)))
}

cleaned_ndm_language <- function(language){
  ifelse(tolower(language) %in% c("english","eng","en"),"eng",ifelse(tolower(language) %in% c("fra","french","fr"),"fra",NA))
}

table_base_path <- function(cansimTableNumber) {
  file.path(tempdir(),paste0("cansim_",naked_ndm_table_number(cansimTableNumber)))
}

file_path_for_table_language <- function(cansimTableNumber, language){
  language <- cleaned_ndm_language(language)
  if (is.na(language)) stop(paste0("Unkown Lanaguage ",language),call.=FALSE)
  base_table <- naked_ndm_table_number(cansimTableNumber)
  file.path(paste0(base_table,"-",language))
}

base_path_for_table_language <- function(cansimTableNumber, language,base_dir = NULL){
  if (is.null(base_dir)) {
    base_dir <- table_base_path(cansimTableNumber)
  }
  if (!dir.exists(base_dir)) {
    dir.create(base_dir)
  }
  file.path(base_dir,file_path_for_table_language(cansimTableNumber,language))
}

response_status_code_translation <- list(
  "0"="Success",
  "1"="Invalid date",
  "2"="Invalid cube and series combination",
  "3"="Request failed",
  "4"="Vector is invalid",
  "5"="Cube product id is invalid",
  "6"="Cube is currently being published. Please try again later.",
  "7"="Cube is currently unavailable. For more information, contact us (toll-free 1-800-263-1136; 514-283-8300; STATCAN.infostats-infostats.STATCAN@canada.ca).",
  "8"="Invalid number of reference periods"
)

response_error_translation <- list(
  "503"="StatCan website is currently unavailable"
)

get_with_timeout_retry <- function(url,timeout=200,retry=3,path=NA,warn_only=FALSE){
  if (!is.na(path)) {
    response <- purrr::safely(httr::GET)(url,encode="json",
                                         httr::add_headers("Content-Type"="application/json"),
                                         httr::timeout(timeout),
                                         httr::write_disk(path,overwrite = TRUE))
  } else {
    response <- purrr::safely(httr::GET)(url,
                                         encode="json",
                                         httr::add_headers("Content-Type"="application/json"),
                                         httr::timeout(timeout))
  }
  if (!is.null(response$error)){
    if ("curl_error_peer_failed_verification" %in% class(response$error)) {
      stop(stringr::str_wrap(gsub(".+\\): ","",as.character(response$error),80)),"\n",
           "This means that the authenticity of the StatCan API server can't be verified.\n",
           "Statistics Canada has a history of failty SSL certificats on their API,\n",
           "if you are reasonably sure that your connection is not getting hijacked you\n",
           "can disable peer checking for the duration of the R session by typing\n\n",
           "httr::set_config(httr::config(ssl_verifypeer=0,ssl_verifystatus=0))","\n\n","into the console.",call.=FALSE)
    }
    if (retry>0) {
      message("Got timeout from StatCan, trying again")
      response <- get_with_timeout_retry(url,timeout=timeout,retry=retry-1,path=path)
    } else {
      message("Got timeout from StatCan, giving up")
    }
  } else if (response$result$status_code %in% names(response_error_translation)){
    if (warn_only) {
      warning(sprintf("%s\nReturned status code %s",response_error_translation[[as.character(response$result$status_code)]], response$result$status_code),call.=FALSE)
      response=response$result
    } else {
      stop(sprintf("%s\nReturned status code %s",response_error_translation[[as.character(response$result$status_code)]], response$result$status_code),call.=FALSE)
    }
  } else if (response$result$status_code != 200){
    if (warn_only) {
      warning(sprintf("Problem downloading data, returned status code %s.",response$result$status_code),call.=FALSE)
      response=response$result
    } else {
      stop(sprintf("Problem downloading data, returned status code %s.",response$result$status_code),call.=FALSE)
    }
  } else {
    response=response$result
  }

  if (is.null(response) && retry == 0) {
    if (warn_only) {
      warning(sprintf("Problem downloading data, multiple timeouts.\nPlease check your network connection. If your connections is fine then StatCan servers might be down."),call.=FALSE)
      response=response$result
    } else {
      stop(sprintf("Problem downloading data, multiple timeouts.\nPlease check your network connection. If your connections is fine then StatCan servers might be down."),call.=FALSE)
    }
  }
  response
}

post_with_timeout_retry <- function(url,body,timeout=200,retry=3,warn_only=FALSE){
  response <- purrr::safely(httr::POST)(url,
                                        body=body,
                                        encode="json",
                                        httr::add_headers("Content-Type"="application/json"),
                                        httr::timeout(timeout))
  if (!is.null(response$error)){
    if ("curl_error_peer_failed_verification" %in% class(response$error)) {
      stop(stringr::str_wrap(gsub(".+\\): ","",as.character(response$error),80)),"\n",
           "This means that the authenticity of the StatCan API server can't be verified.\n",
           "Statistics Canada has a history of failty SSL certificats on their API,\n",
           "if you are reasonably sure that your connection is not getting hijacked you\n",
           "can disable peer checking for the duration of the R session by typing\n\n",
           "httr::set_config(httr::config(ssl_verifypeer=0,ssl_verifystatus=0))","\n\n","into the console.",call.=FALSE)
    }
    if (retry>0) {
      message("Got timeout from StatCan, trying again")
      response <- post_with_timeout_retry(url,body=body,timeout=timeout,retry=retry-1)
    } else {
      message("Got timeout from StatCan, giving up")
      response=response$result
    }
  } else {
    response=response$result
  }

  if (is.null(response) && retry == 0) {
    if (warn_only) {
      warning(sprintf("Problem downloading data, multiple timeouts.\nPlease check your network connection. If your connections is fine then StatCan servers might be down."),call.=FALSE)
      response=NULL
    } else {
      stop(sprintf("Problem downloading data, multiple timeouts.\nPlease check your network connection. If your connections is fine then StatCan servers might be down."),call.=FALSE)
    }
  }
  response
}



short_prov.en <- c(
  "British Columbia"="BC",
  "Alberta"="AB",
  "Saskatchewan"="SK",
  "Manitoba"="MB",
  "Ontario"="ON",
  "Quebec"="QC",
  "New Brunswick"="NB",
  "Prince Edward Island"="PE",
  "Nova Scotia"="NS",
  "Newfoundland and Labrador"="NL",
  "Yukon"="YT",
  "Northwest Territories"="NT",
  "Nunavut"="NU",
  "Northwest Territories including Nunavut"="NTNU",
  "Canada"="CAN"
)


short_prov.fr <- setNames(c(
  "BC",
  "AB",
  "SK",
  "MB",
  "ON",
  "QC",
  "NB",
  "PE",
  "NS",
  "NL",
  "YT",
  "NT",
  "NU",
  "NTNU",
  "CAN"
),c(
  "Colombie-Britannique",
  "Alberta",
  "Saskatchewan",
  "Manitoba",
  "Ontario",
  paste0("Qu",intToUtf8(0x00E9),"bec"),
  "Nouveau-Brunswick",
  paste0(intToUtf8(0x00CE),"le-du-Prince-",intToUtf8(0x00C9),"douard"),
  paste0("Nouvelle-",intToUtf8(0x00C9),"cosse"),
  "Terre-Neuve-et-Labrador",
  "Yukon",
  "Territoires du Nord-Ouest",
  "Nunavut",
  "Territoires du Nord-Ouest incluant Nunavut",
  "Canada"
))



#' Add provincial abbreviations as factor
#' @export
#' @param data A tibble as returned by \code{get_cansim} with provincial level data
#' @return The input tibble with additional factor GEO.abb that contains language-specific provincial abbreviations
#'
#' @examples
#' \dontrun{
#' df <- get_cansim("17-10-0005")
#' df <- add_provincial_abbreviations(df)
#' }
#'
add_provincial_abbreviations <- function(data){
  cleaned_language <- ifelse("VALEUR" %in% names(data),"fra","eng")
  if (cleaned_language=="eng") {
    data_geography_column <- "GEO"
    short_prov <- short_prov.en
  } else {
    data_geography_column <- paste0("G",intToUtf8(0x00C9),"O")
    short_prov <- short_prov.fr
  }

  short_prov_t <- short_prov %>%
    tibble::enframe() %>%
    setNames(c(data_geography_column,"GEO.abb")) %>%
    mutate(GEO.abb=factor(.data$GEO.abb,levels = c("CAN","BC","AB","SK","MB","ON","QC","NB","PE","NS","NL","YT","NT","NU","NTNU")))

  data <- data %>%
    left_join(short_prov_t,by=data_geography_column)
    # mutate(GEO.abb=factor(as.character(short_prov[!!as.name(data_geography_column)]),
    #                       levels=c("CAN","BC","AB","SK","MB","ON","QC","NB","PE","NS","NL","YT","NT","NU","NTNU")))
}


#' Get NDM code sets
#'
#' Useful to get a list of surveys  or subjects and used internally
#' @export
#' @param code_set the code set to retrieve.
#' @param refresh Default is \code{FALSE}, repeated calls during the same session will hit the cached data.
#' To refresh the code list during a running R session set to \code{TRUE}
#'
#' @return A tibble with english and french labels for the given code set
#'
#' @examples
#' \dontrun{
#' get_cansim_code_set("survey")
#' }
get_cansim_code_set <- function(code_set=c("scalar", "frequency", "symbol", "status", "uom", "survey",  "subject", "wdsResponseStatus"),
                                refresh=FALSE){
  code_sets <- c("scalar", "frequency", "symbol", "status", "uom", "survey",  "subject", "wdsResponseStatus")
  if (length(code_set)!=1 | !(code_set %in% code_sets)) {
    stop(paste0("Invalid code set, code_set must be one of ",paste0(code_sets,collapse=", ")),call.=FALSE)
  }
  path=file.path(tempdir(),"cansim_code_sets.Rmd")
  if (refresh | !file.exists(path)) {
    url='https://www150.statcan.gc.ca/t1/wds/rest/getCodeSets'
    r<-get_with_timeout_retry(url)
    if (r$status_code==200) {
      content <- httr::content(r)
      saveRDS(content,path)
    } else {
      warning("Problem downloading code sets.")
      stop(httr::content(r),call.=FALSE)
    }
  } else {
    content <- readRDS(path)
  }
  m<-do.call(rbind, content$object[[code_set]])
  m[m=="NULL"] <- NA
  as_tibble(m) %>%
    mutate_all(unlist)
}

# transforms the value column to nomeric. If table is in semi-wide form it converts the wide for dimension
# to long form and creates and modifies the COORDINATE column as needed.
transform_value_column <- function(data,value_column){
  language <- attr(data,"language")
  cansimTableNumber <- attr(data,"cansimTableNumber")

  symbols <- which(grepl("^Symbol( \\d+)*$",names(data)))
  if (!(value_column %in% names(data)) & length(symbols)>1) {
    #message("\nTransforming to long form.")
    dimension_grep_string <- paste0("^.+ \\(",length(symbols),"[A-Za-z]*\\):.+\\[\\d+\\]$")
    dimensions <- which(grepl(dimension_grep_string,names(data)))
    if (sum(symbols!=dimensions+1)>0) {
      warning("Unable to identify dimensions")
    } else {
      count_type <- stringr::str_match(names(data)[dimensions][1],paste0("(\\(",length(symbols),"[A-Za-z]*\\))"))[1,2]
      dimension_members <- gsub(paste0("^.+ \\(",length(symbols),"[A-Za-z]*\\): *"),"",names(data)[dimensions]) %>%
        gsub(" *\\[\\d+\\]$","",.)
      member_ids <- stringr::str_extract(names(data)[dimensions],"\\[\\d+\\]$") %>% gsub("\\[|\\]","",.)
      dimension_name <- gsub(paste0(" \\(",length(symbols),"[A-Za-z]*\\):.+\\[\\d+\\]"),"",names(data)[dimensions]) %>%
        unique() %>% paste0(.," ",count_type)

      if (length(dimension_name)>1) {
        warning("Unable to identify dimension name")
      } else {
        symbol_string <- "Symbol"
        renames <- c(setNames(names(data)[dimensions],paste0(member_ids," --- ",value_column)),
                     setNames(names(data)[symbols],paste0(member_ids," --- ",symbol_string)))

        member_names <- dplyr::tibble(!!as.name(paste0("Member ID: ",dimension_name)):=member_ids,
                                      !!as.name(dimension_name):=dimension_members)


        if ("arrow_dplyr_query" %in% class(data)) {
          data <- as.data.frame(data)
          attr(data,"language") <- language
          attr(data,"cansimTableNumber") <- cansimTableNumber
        }

        data <- data %>%
          dplyr::rename(!!!renames) %>%
          tidyr::pivot_longer(matches(" --- "), names_pattern="^(.+) --- (.+)$",
                              names_to=c(paste0("Member ID: ",dimension_name),".value")) %>%
          dplyr::left_join(member_names,by=paste0("Member ID: ",dimension_name))

        coordinate_column <- ifelse(language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))

        if (coordinate_column %in% names(data)) {
          data <- data %>%
            dplyr::mutate(!!coordinate_column := paste0(!!as.name(coordinate_column),".",
                                                     !!as.name(paste0("Member ID: ",dimension_name))))
        }

        data <- data %>%
          dplyr::select(-dplyr::all_of(paste0("Member ID: ",dimension_name)))
      }
    }
  }

  if (value_column %in% names(data)) {
    if (!is.numeric(data[[value_column]])) {
      data <- data %>%
        dplyr::mutate(!!value_column:=as.numeric(!!as.name(value_column)))
    }
  } else {
    warning("Unkown table type")
  }
  data
}

same_partitioning <- function(p1,p2) {
  length(p1)==length(p2) && sum(sort(p1)!=sort(p2))==0
}

# copied from unexported utils:::format.object_size
format_file_size <- function (x, units = "b", standard = "auto", digits = 1L, ...)
{
  known_bases <- c(legacy = 1024, IEC = 1024, SI = 1000)
  known_units <- list(SI = c("B", "kB", "MB", "GB", "TB", "PB",
                             "EB", "ZB", "YB"),
                      IEC = c("B", "KiB", "MiB", "GiB",
                              "TiB", "PiB", "EiB", "ZiB", "YiB"),
                      legacy = c("b", "Kb",
                                 "Mb", "Gb", "Tb", "Pb"),
                      LEGACY = c("B", "KB", "MB",
                                                                                                                                           "GB", "TB", "PB"))
  units <- match.arg(units, c("auto", unique(unlist(known_units),
                                             use.names = FALSE)))
  standard <- match.arg(standard, c("auto", names(known_bases)))
  if (is.null(digits))
    digits <- 1L
  if (standard == "auto") {
    standard <- "legacy"
    if (units != "auto") {
      if (endsWith(units, "iB"))
        standard <- "IEC"
      else if (endsWith(units, "b"))
        standard <- "legacy"
      else if (units == "kB")
        stop("For SI units, specify 'standard = \"SI\"'",call.=FALSE)
    }
  }
  base <- known_bases[[standard]]
  units_map <- known_units[[standard]]
  if (units == "auto") {
    power <- if (x <= 0)
      0L
    else min(as.integer(log(x, base = base)), length(units_map) -
               1L)
  }
  else {
    power <- match(toupper(units), toupper(units_map)) -
      1L
    if (is.na(power))
      stop(gettextf("Unit \"%s\" is not part of standard \"%s\"",
                    sQuote(units), sQuote(standard)), domain = NA,call.=FALSE)
  }
  unit <- units_map[power + 1L]
  if (power == 0 && standard == "legacy")
    unit <- "bytes"
  paste(round(x/base^power, digits = digits), unit)
}


#' Get column names de-duplicated and in the correct order
#' @param cansimTableNumber The table number
#' @param language The language of the column names
#' @param column The column name
#' @keywords internal
#' @return A tibble with the column names
get_deduped_column_level_data <- function(cansimTableNumber,language,column) {
  dimension_id_column <- ifelse(language=="eng","Dimension ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification de la dimension"))
  member_id_column <- ifelse(language=="eng","Member ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification du membre"))
  member_name_column <- ifelse(language=="eng","Member Name","Nom du membre")
  parent_member_id_column <- ifelse(language=="eng","Parent Member ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification du membre parent"))

  columns <- get_cansim_column_categories(cansimTableNumber = cansimTableNumber,
                                          column = column,
                                          language = language)

  # full level values from metadata
  level_table <- columns %>%
    select(...dim=!!as.name(dimension_id_column),
           ...id=!!as.name(member_id_column),
           ...name=!!as.name(member_name_column),
           ...parent_id=!!as.name(parent_member_id_column)) %>%
    mutate(...n=as.integer(.data$...id)) %>%
    arrange("...n") %>%
    select(-"...n") %>%
    mutate(...count=n(),.by=c("...dim","...name")) %>%
    mutate(...duplicated=.data$...count>1) %>%
    mutate(...original=!.data$...duplicated) %>%
    mutate(...original_name=.data$...name) %>%
    mutate(...last_parent_id=.data$...parent_id)

  fixed_level_table <- NULL
  # don't try to dedup census geographies, too messy
  if (substr(naked_ndm_table_number(cansimTableNumber),1,4)=="9810" && sum(filter(level_table,.data$...dim=="1")$...duplicated)>0) {
    warning(paste0("Table ",cansimTableNumber," is a census data table that has duplicate geography names, not converting to factors. Treat with caution when accessng geographies by name and check geographic identifiers."))
    fixed_level_table <- level_table %>%
      filter(.data$...dim=="1")
    level_table <- level_table %>%
      filter(.data$...dim!="1")
  }

  # try to dedup
  max_run <- 30
  while (sum(level_table$...duplicated)>0 && max_run>0) { # deals with 36-10-0580
    max_run <- max_run - 1
    level_table <- level_table %>%
      left_join(level_table %>% select("...id","...dim",...parent_name="...original_name",...new_parent_id="...last_parent_id"),
                by=c("...last_parent_id"="...id","...dim"="...dim")) %>%
      mutate(...name=case_when(.data$...duplicated & is.na(.data$...parent_name) ~ paste0(.data$...name," [",.data$...id,"]"),
                               .data$...duplicated & !is.na(.data$...parent_name) ~  paste0(.data$...name," ==> ",.data$...parent_name),
                              TRUE ~ .data$...name)) %>%
      mutate(...last_parent_id=ifelse(.data$...duplicated,
                                      .data$...new_parent_id,
                                      .data$...last_parent_id)) %>%
      mutate(...count=n(),.by=c("...dim","...name")) %>%
      mutate(...duplicated=.data$...count>1) %>%
      select(-any_of(c("...parent_name","...new_parent_id")))

  }

  bind_rows(fixed_level_table,level_table) %>%
    arrange("...dim") %>%
    select("...dim","...id","...name","...original","...original_name")
}


standardize_cansim_column_order <- function(data) {
  language <- attr(data,"language")
  if (is.null(language)|!(language %in% c("eng","fra"))) {
    warning("Don't know how to standardize column order.")
    return(data)
  }

  classification_code_column <- ifelse(language=="eng","Classification Code","Code sur la classification")
  value_string <- ifelse(language=="fra","VALEUR","VALUE")
  scale_string <- ifelse(language=="fra","IDENTIFICATEUR SCALAIRE","SCALAR_ID")
  scale_string2 <- ifelse(language=="fra","FACTEUR SCALAIRE","SCALAR_FACTOR")
  uom_string=ifelse(language=="fra",paste0("UNIT",intToUtf8(0x00C9)," DE MESURE"),"UOM")
  classification_prefix <- ifelse(language=="fra","Code de classification pour ","Classification Code for ")
  hierarchy_prefix <- ifelse(language=="fra",paste0("Hi",intToUtf8(0x00E9),"rarchie pour "),"Hierarchy for ")
  coordinate_column <- ifelse(language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))
  data_geography_column <- ifelse(language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
  date_field=ifelse(language=="fra",paste0("P",intToUtf8(0x00C9),"RIODE DE R",intToUtf8(0x00C9),"F",intToUtf8(0x00C9),"RENCE"),"REF_DATE")

  standard_order1 <- intersect(c("REF_DATE",date_field,"Date","REF_DATE2",data_geography_column,"DGUID","GeoUID") %>%
                                 unique(),names(data))
  standard_order2 <- intersect(c(value_string,"val_norm","UOM","UOM_ID",scale_string2,scale_string,"VECTOR","cansimTableNumber",coordinate_column,
                                 "STATUS","SYMBOL","releaseTime","frequencyCode",
                                 "TERMINATED","DECIMALS"), names(data))
  standard_order3 <- names(data)[grepl(paste0("^",hierarchy_prefix,"|^",classification_prefix),names(data))]

  rest_order <- setdiff(names(data),c(standard_order1,standard_order2,standard_order3))

  data %>%
    select(all_of(c(standard_order1,rest_order,standard_order2,standard_order3)))
}


column_names_for_language <- function(language) {
  date_field=ifelse(language=="fra",paste0("P",intToUtf8(0x00C9),"RIODE DE R",intToUtf8(0x00C9),"F",intToUtf8(0x00C9),"RENCE"),"REF_DATE")
  classification_code_column <- ifelse(language=="eng","Classification Code","Code sur la classification")
  value_string <- ifelse(language=="fra","VALEUR","VALUE")
  scale_string <- ifelse(language=="fra","IDENTIFICATEUR SCALAIRE","SCALAR_ID")
  scale_string2 <- ifelse(language=="fra","FACTEUR SCALAIRE","SCALAR_FACTOR")
  uom_string=ifelse(language=="fra",paste0("UNIT",intToUtf8(0x00C9)," DE MESURE"),"UOM")
  coordinate_column <- ifelse(language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))
  data_geography_column <- ifelse(language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
  column_names <- c(date_field,classification_code_column,value_string,scale_string,scale_string2,
                    uom_string,coordinate_column,data_geography_column)
  column_names
}

rename_columns_for_language <- function(data,from_language,to_language) {
  renames <- setNames(column_names_for_language(to_language),column_names_for_language(from_language))

  renames <- renames[intersect(names(data),names(renames))]

  renames <- setNames(names(renames),as.character(renames))

  data %>%
    rename(!!!renames)

}

geography_colum_names <- function(language) {
  geography_columns <- case_when(language=="eng" ~
                                   c("Geography","Geographic name","Geography of origin"),
                                 TRUE ~ c(paste0("G",intToUtf8(0x00E9),"ographie"),
                                          paste0("Nom g",intToUtf8(0x00E9),"ographique"),
                                          paste0("G",intToUtf8(0x00E9),"ographie d'origine")))
}


normalize_coordinates <- function(coordinates){
  coordinates <- lapply(coordinates,\(coordinate)
                        coordinate %>%
                          strsplit("\\.") %>%
                          unlist() %>%
                          c(., rep(0, pmax(0,10-length(.)))) %>%
                          paste(collapse = ".")
  ) %>% unlist()

}

get_robust_cache_path <- function(cache_path) {
  if (is.null(cache_path) || cache_path=="") {
    cache_path <- Sys.getenv("CANSIM_CACHE_PATH")
    if (cache_path=="") cache_path <- getOption("cansim.cache_path",default="")
    if (cache_path=="") {
      cache_path <- file.path(tempdir(),"cansim_cache")
      if (!dir.exists(cache_path)) dir.create(cache_path)
      message(cansim_no_cache_path_message)
    }
  }
  if (!dir.exists(cache_path)) {
    stop("Cache path ",cache_path," does not exist, please create it first.",call.=FALSE)
  }
  cache_path
}

