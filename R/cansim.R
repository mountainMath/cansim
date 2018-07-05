cleaned_ndm_table_number <- function(cansimTableNumber){
  t<-gsub("-","",as.character(cansimTableNumber))
  if (nchar(t)<=7) {
    tt<-cansim_old_to_new(t)
    message("Legacy table number ",cansimTableNumber,", converting to NDM ",tt)
    cansimTableNumber=tt
  }
  cansimTableNumber
  n=as.character(gsub("-","",cansimTableNumber))
  paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
}

naked_ndm_table_number <- function(cansimTableNumber){
  as.character(gsub("-","",cleaned_ndm_table_number(cansimTableNumber)))
}

cleaned_ndm_language <- function(language){
  ifelse(tolower(language) %in% c("english","eng","en"),"eng",ifelse(tolower(language) %in% c("fra","french","fr"),"fra",NA))
}

file_path_for_table_language <- function(cansimTableNumber,language){
  language <- cleaned_ndm_language(language)
  if (is.na(language)) stop(paste0("Unkown Lanaguage ",language))
  base_table <- naked_ndm_table_number(cansimTableNumber)
  file.path(paste0(base_table,"-",language))
}

base_path_for_table_language <- function(cansimTableNumber,language){
  file.path(tempdir(),file_path_for_table_language(cansimTableNumber,language))
}

#' Get cansim table into tidy dataframe
#'
#' Caches the table data for the current session
#'
#' @param cansimTableNumber the table number to load, accepts old or new NDM table numbers
#' @param language "en" or "fr" for english or french language version. Defaults to english.
#' @param refresh Optionally force reload of cansim data, default is *FALSE*. Cansim data is cached for the duration of the R session only
#'
#' @return a tibble with the cansim table data
#'
#' @export
get_cansim <- function(cansimTableNumber,language="english",refresh=FALSE){
  get_cansim_ndm(cleaned_ndm_table_number(cansimTableNumber),language,refresh)
}


#' Adjust Cansim Value by scaled amount
#' French part does not work, probably encoding issues
#' Legacy function
#' @export
adjust_cansim_values_by_variable <-function(data,var){
  normalize_cansim_values(data)
}

#' normalizes CANSIM values by setting all units to counts/dollars instead of millions, etc.
#'
#' if "replacement_value" is not set, it will replace the *VALUE* field with normailzed values and drop the scale columns,
#' otherwise it keeps the scale columns and created a new column named replacement_value with the normalized value.
#' It will attempt to parse the *REF_DATE* field and create an R date variable. (experimental)
#'
#' @param data A cansim table as returned from *get_cansim*.
#' @param replacement_value Optional name of the column the manipulated value should be returned in. Defaults to replacing the current value field.
#' @param normalize_percent Optional normailze percentages by changing them to rates. *TRUE* by default.
#' @param default_month The default month that should be used when creating Date objects for annual data.
#' @param default_day The defauly day of the month that should be used when creating Date objects for monthly data.
#'
#' @return tibble with adjusted values
#'
#' @export
normalize_cansim_values <- function(data,replacement_value=NA,normalize_percent=TRUE,default_month="01",default_day="01"){
  language <- ifelse("VALEUR" %in% names(data),"fr","en")
  value_string <- ifelse(language=="fr","VALEUR","VALUE")
  scale_string <- ifelse(language=="fr","IDENTIFICATEUR SCALAIRE","SCALAR_ID")
  scale_string2 <- ifelse(language=="fr","FACTEUR SCALAIRE","SCALAR_FACTOR")
  uom_string=ifelse(language=="fr","UNITÉ DE MESURE","UOM")
  percentage_string=ifelse(language=="fr","^Pourcent","^Percent")
  replacement_value_string = ifelse(is.na(replacement_value),value_string,replacement_value)
  data <- data %>%
    mutate(!!as.name(replacement_value_string):=!!as.name(value_string)*(`^`(10,as.integer(!!as.name(scale_string)))))
  if (is.na(replacement_value)) { # remove scale columns
    data <- data %>% select(-one_of(intersect(c(scale_string,scale_string2),names(data))))
  }
  if (normalize_percent & uom_string %in% names(data)) {
    # divide numbers that are percentages by 100 and convert the unit field to "rate"
    data <- data %>%
      mutate(!!as.name(replacement_value_string):=ifelse(grepl(percentage_string,!!as.name(uom_string)),!!as.name(replacement_value_string)/100,!!as.name(replacement_value_string))) %>%
      mutate(!!as.name(uom_string):=ifelse(!!as.name(uom_string)==percentage_string,"Rate",!!as.name(uom_string)))
  }
  date_field=ifelse(language=="fr","PÉRIODE DE RÉFÉRENCE","REF_DATE")
  sample_date <- data[[date_field]] %>% na.omit %>% first()
  if (grepl("^\\d{4}$",sample_date)) {
    # year
    data <- data %>% mutate(Date=as.Date(paste0(!!as.name(date_field),"-",default_month,"-",default_day)))
  } else if (grepl("^\\d{4}/\\d{4}$",sample_date)) {
    # year range, use second year as anchor
    data <- data %>% mutate(Date=as.Date(paste0(gsub("^\\d{4}/","",!!as.name(date_field)),"-",default_month,"-",default_day)))
  } else if (grepl("^\\d{4}-\\d{2}$",sample_date)) {
    # year and month
    data <- data %>% mutate(Date=as.Date(paste0(!!as.name(date_field),"-",default_day)))
  }
  data
}



#' translate from old table number to NDM table number
#'
#' @param oldCansimTableNumber the old cansim table number. Returns the corresponding NDM number
#'
#' @return cansim ndm table number
#'
#' @export
cansim_old_to_new <- function(oldCansimTableNumber){
  path <- file.path(tempdir(),"cansim-correspondence.csv")
  if (!file.exists(path)){
    url="https://www.statcan.gc.ca/eng/developers-developpeurs/cansim_id-product_id-concordance.csv"
    data <- readr::read_csv(url)
    saveRDS(data,file=path)
  }
  data <-readRDS(path)
  cleaned_number=sprintf("%07d", as.numeric(sub("-","",as.character(oldCansimTableNumber))))

  new_number <- data %>%
    filter(CANSIM_ID==as.integer(cleaned_number)) %>%
    pull(PRODUCT_ID)
  if (identical(new_number, integer(0))) {
    stop(paste0("Unable to match old CANSIM table number ",cleaned_number))
  }
  n=as.character(new_number)
  new_table <- paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
  new_table
}


#' Get cansim table via NDM
#' @param cansimTableNumber the NDM table number to load
#' @param language "en" or "fr" for english or french language version. Defaults to english.
#' @param refresh Optionally force reload of cansim data, default is *FALSE*. Cansim data is cached for the duration of the R session only
get_cansim_ndm <- function(cansimTableNumber,language="english",refresh=FALSE){
  cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
  cleaned_language=cleaned_ndm_language(language)
  message(paste0("Accessing CANSIM NDM product ",cleaned_number))
  base_table=naked_ndm_table_number(cansimTableNumber)
  path <- paste0(base_path_for_table_language(cansimTableNumber,language),".zip")
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda")
  if (refresh | !file.exists(data_path)){
    url=paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",file_path_for_table_language(cansimTableNumber,language),".zip")
    httr::GET(url,httr::write_disk(path, overwrite = TRUE))
    data <- NA
    na_strings=c("<NA>",NA,"NA","","F")
    exdir=file.path(tempdir(),file_path_for_table_language(cansimTableNumber,language))
    utils::unzip(path,exdir=exdir)
    unlink(path)
    if(cleaned_language=="eng") {
      message("Parsing data")
      data <- readr::read_csv(file.path(exdir, paste0(base_table, ".csv")),
                              na=na_strings,
                              locale=readr::locale(encoding="UTF8"),
                              col_types = list(.default = "c")) %>%
        dplyr::mutate(VALUE=as.numeric(VALUE))
      message("Folding in metadata")
      meta <- suppressWarnings(readr::read_csv(file.path(exdir, paste0(base_table, "_MetaData.csv")),
                              na=na_strings,
                              #col_names=FALSE,
                              locale=readr::locale(encoding="UTF8"),
                              col_types = list(.default = "c")))
      cut_indices <- grep("Dimension ID",meta$`Cube Title`)
      cut_indices <- c(cut_indices,grep("Symbol Legend",meta$`Cube Title`))
      meta1 <- meta[seq(1,cut_indices[1]-1),]
      saveRDS(meta1,file=paste0(data_path,"1"))
      names2 <- meta[cut_indices[1],]  %>% dplyr::select_if(~sum(!is.na(.)) > 0) %>% as.character()
      meta2 <- meta[seq(cut_indices[1]+1,cut_indices[2]-1),seq(1,length(names2))] %>% set_names(names2)
      saveRDS(meta2,file=paste0(data_path,"2"))
      names3 <- meta[cut_indices[2],]  %>% dplyr::select_if(~sum(!is.na(.)) > 0) %>% as.character()
      meta3 <- meta[seq(cut_indices[2]+1,cut_indices[3]-1),seq(1,length(names3))] %>% set_names(names3)
      additional_indices=c(grep("Survey Code",meta$`Cube Title`),
                           grep("Subject Code",meta$`Cube Title`),
                           grep("Note ID",meta$`Cube Title`),
                           grep("Correction ID",meta$`Cube Title`))
      saveRDS(meta[seq(additional_indices[1]+1,additional_indices[2]-1),c(1,2)] %>%
                set_names(meta[additional_indices[1],c(1,2)]) ,file=paste0(data_path,"3"))
      saveRDS(meta[seq(additional_indices[2]+1,additional_indices[3]-1),c(1,2)] %>%
                set_names(meta[additional_indices[2],c(1,2)]) ,file=paste0(data_path,"4"))
      saveRDS(meta[seq(additional_indices[3]+1,additional_indices[4]-1),c(1,2)] %>%
                set_names(meta[additional_indices[3],c(1,2)]) ,file=paste0(data_path,"5"))
      add_hierarchy <- function(meta_x){
        parent_lookup <- rlang::set_names(meta_x$`Parent Member ID`,meta_x$`Member ID`)
        current_top <- function(c){
          strsplit(c,"\\.") %>% purrr::map(dplyr::first) %>% unlist
        }
        parent_for_current_top <- function(c){
          as.character(parent_lookup[current_top(c)])
        }
        meta_x <- meta_x %>% dplyr::mutate(Hierarchy=`Member ID`)
        added=TRUE
        max_depth=100
        count=0
        while (added & count<max_depth) { # generate hierarchy data from member id and parent member id data
          old <- meta_x$Hierarchy
          meta_x <- meta_x %>%
            dplyr::mutate(p=parent_for_current_top(Hierarchy)) %>%
            dplyr::mutate(Hierarchy=ifelse(is.na(p),Hierarchy,paste0(p,".",Hierarchy))) %>%
            dplyr::select(-p)
          added <- sum(old != meta_x$Hierarchy)>0
          count=count+1
        }
        if (added) warning("Exceeded max depth for hierarchy, hierarchy information may be faulty.")
        meta_x
      }
      for (column_index in seq(1:nrow(meta2))) { # iterate through columns for which we have meta data
        column=meta2[column_index,]
        meta_x <- meta3 %>% dplyr::filter(`Dimension ID`==column$`Dimension ID`) %>%
          add_hierarchy
        saveRDS(meta_x,file=paste0(data_path,"_column_",column$`Dimension name`))
        classification_lookup <- set_names(meta_x$`Classification Code`,meta_x$`Member Name`)
        hierarchy_lookup <- set_names(meta_x$Hierarchy,meta_x$`Member Name`)
        if (grepl("Geography",column$`Dimension name`) &  !(column$`Dimension name` %in% names(data))) {
          data <- data %>% dplyr::mutate(GeoUID=as.character(classification_lookup[GEO]))
        } else if (column$`Dimension name` %in% names(data)){
          classification_name <- paste0("Classification Code for ",column$`Dimension name`) %>% as.name
          hierarchy_name <- paste0("Hierarchy for ",column$`Dimension name`) %>% as.name
          data <- data %>%
            dplyr::mutate(!!classification_name:=as.character(classification_lookup[!!as.name(column$`Dimension name`)]),
                   !!hierarchy_name:=as.character(hierarchy_lookup[!!as.name(column$`Dimension name`)]))
        } else {
          warning(paste0("Don't know how to add metadata for ",column$`Dimension name`,"! Ignoring this dimension."))
        }
      }
    } else {
      data <- readr::read_csv2(file.path(exdir, paste0(base_table, ".csv")),
                               na=na_strings,
                               locale=readr::locale(encoding="UTF8"),
                               col_types = list(.default = "c")) %>%
        dplyr::mutate(VALEUR=as.numeric(VALEUR))
    }
    saveRDS(data,file=data_path)
    unlink(exdir,recursive = TRUE)
  }
  readRDS(file=data_path)
}

#' Get cansim table info
#' @param cansimTableNumber the NDM table number to load
#' @param language "en" or "fr" for english or french language version. Defaults to english.
#' @param refresh Optionally force reload of cansim data, default is *FALSE*. Cansim data is cached for the duration of the R session only
get_cansim_table_info <- function(cansimTableNumber,language="english",refresh=FALSE){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda1")
  if (refresh | !file.exists(data_path)){
    get_cansim_ndm(cansimTableNumber,language=language,refresh = refresh)
  }
  readRDS(file=data_path)
}


#' Get cansim table survey
#' @param cansimTableNumber the NDM table number to load
#' @param language "en" or "fr" for english or french language version. Defaults to english.
#' @param refresh Optionally force reload of cansim data, default is *FALSE*. Cansim data is cached for the duration of the R session only
get_cansim_table_survey <- function(cansimTableNumber,language="english",refresh=FALSE){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda3")
  if (refresh | !file.exists(data_path)){
    get_cansim_ndm(cansimTableNumber,language=language,refresh = refresh)
  }
  readRDS(file=data_path)
}

#' Get cansim table subject
#' @param cansimTableNumber the NDM table number to load
#' @param language "en" or "fr" for english or french language version. Defaults to english.
#' @param refresh Optionally force reload of cansim data, default is *FALSE*. Cansim data is cached for the duration of the R session only
get_cansim_table_subject <- function(cansimTableNumber,language="english",refresh=FALSE){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda4")
  if (refresh | !file.exists(data_path)){
    get_cansim_ndm(cansimTableNumber,language=language,refresh = refresh)
  }
  readRDS(file=data_path)
}

#' Get cansim table notes
#' @param cansimTableNumber the NDM table number to load
#' @param language "en" or "fr" for english or french language version. Defaults to english.
#' @param refresh Optionally force reload of cansim data, default is *FALSE*. Cansim data is cached for the duration of the R session only
get_cansim_table_notes <- function(cansimTableNumber,language="english",refresh=FALSE){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda5")
  if (refresh | !file.exists(data_path)){
    get_cansim_ndm(cansimTableNumber,language=language,refresh = refresh)
  }
  readRDS(file=data_path)
}


#' Get cansim table column list
#' @param cansimTableNumber the NDM table number to load
#' @param language "en" or "fr" for english or french language version. Defaults to english.
#' @param refresh Optionally force reload of cansim data, default is *FALSE*. Cansim data is cached for the duration of the R session only
get_cansim_column_list <- function(cansimTableNumber,language="english",refresh=FALSE){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda2")
  if (refresh | !file.exists(data_path)){
    get_cansim_ndm(cansimTableNumber,language=language,refresh = refresh)
  }
  readRDS(file=data_path)
}

#' Get cansim table column categories
#' @param cansimTableNumber the NDM table number to load
#' @param language "en" or "fr" for english or french language version. Defaults to english.
#' @param refresh Optionally force reload of cansim data, default is *FALSE*. Cansim data is cached for the duration of the R session only
get_cansim_column_categories <- function(cansimTableNumber,column,language="english",refresh=FALSE){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda2")
  if (refresh | !file.exists(data_path)){
    get_cansim_ndm(cansimTableNumber,language=language,refresh = refresh)
  }
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda_column_",column)
  if (!file.exists(data_path)){
    stop(paste0("Unkown column ",column))
  }
  readRDS(file=data_path)
}

#' Get cansim table overview text
#'
#' Needs to load the whole cansim table in order to dusplay overview information. Prints overview as message
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language "en" or "fr" for english or french language version. Defaults to english.
#' @param refresh Optionally force reload of cansim data, default is *FALSE*. Cansim data is cached for the duration of the R session only
#'
#' @return none
#'
#' @export
get_cansim_table_overview <- function(cansimTableNumber,column,language="english",refresh=FALSE){
  info <- cansim:::get_cansim_table_info(cansimTableNumber,language=language,refresh=refresh)
  refresh=FALSE
  text <- paste0(info$`Cube Title`,"\n","CANSIM Table ",cansim:::cleaned_ndm_table_number(cansimTableNumber),"\n",
                 "Start Date: ",info$`Start Reference Period`,", End Date: ",info$`End Reference Period`,", Frequency: ",info$Frequency,"\n")
  columns <- cansim:::get_cansim_column_list(cansimTableNumber,language=language,refresh=refresh)
  for (column in columns$`Dimension name`) {
    text <- paste0(text,"\n","Column ",column)
    categories <- cansim:::get_cansim_column_categories(cansimTableNumber,column,language=language,refresh=refresh)
    text <- paste0(text, " (",nrow(categories),")","\n")
    text <- paste0(text, paste(utils::head(categories$`Member Name`,10),collapse=", "))
    if (nrow(categories)>10) text <- paste0(text, ", ...")
    text <- paste0(text,"\n")
  }
  message(text)
}


#' Use metadate to extract categories for column of specific level.
#'
#' @param data the cansim data as returned from *get_cansim*
#' @param column_name the name of the column to extract categories from
#' @param level the hierarchy level depth to which to extract categories, 0 is top category
#' @param strict flag, *FALSE* by default. If true, only extract that speficit level.
#' @param remove_duplicates flag, *TRUE* by default in which case it will remove higher level grouping categories already captured by lower level hierarchy data.
#'
#' @return A vector of categories
#'
#' @export
categories_for_level <- function(data,column_name,level=NA,strict=FALSE,remove_duplicates=TRUE){
  hierarchy_name=paste0("Hierarchy for ",column_name)
  h <- data %>% dplyr::select(column_name,hierarchy_name) %>%
    unique %>%
    dplyr::mutate(hierarchy_level=(strsplit(!!as.name(hierarchy_name),"\\.") %>% purrr::map(length) %>% unlist)-1)
  max_level=max(h$hierarchy_level,na.rm = TRUE)
  if (is.na(level) | level>max_level) level=max_level
  h <- h %>%
    dplyr::mutate(`Member ID`=strsplit(!!as.name(hierarchy_name),"\\.") %>% purrr::map(last) %>% as.integer) %>%
    dplyr::filter(hierarchy_level<=level)
  strict_hierarchy=h %>% dplyr::filter(hierarchy_level==level) %>% dplyr::pull(hierarchy_name) %>% unique
  if (strict) {
    h <- h %>% dplyr::filter(hierarchy_level==level)
  } else if (remove_duplicates) {
    higher_ids <- strict_hierarchy %>% strsplit("\\.") %>%
      purrr::map(function(x){utils::head(as.integer(x),-1)}) %>%
      unlist %>% unique() %>% as.integer()
    h <- h %>% dplyr::filter(!(`Member ID` %in% higher_ids))
  }
  h[[column_name]]
}


generate_table_metadata <- function(){
  url_for_page <-function(page){paste0("https://www150.statcan.gc.ca/n1/en/type/data?p=",page,"-data/tables#tables")}
  parse_table_data <- function(item){
    product <- item %>%
      html_node(".ndm-result-productid") %>%
      html_text() %>%
      sub("^Table: ","",.)
    if (grepl("^\\d{2}-\\d{2}-\\d{4}",product)) {
      result = tibble(
        title=item %>%
          html_node(".ndm-result-title") %>%
          html_text() %>% sub("^(\\d|,)+\\. ","",.),
        table=product,
        former = item %>%
          html_node(".ndm-result-formerid") %>%
          html_text() %>% trimws %>%
          gsub("^\\(formerly: CANSIM |\\)$","",.),
        geo = item %>%
          html_node(".ndm-result-geo") %>%
          html_text() %>%
          sub("Geography: ","",.),
        description = item %>%
          html_node(".ndm-result-description") %>%
          html_text() %>%
          sub("Description: ","",.),
        release_date = item %>%
          html_node(".ndm-result-date .ndm-result-date") %>%
          html_text() %>%
          as.Date()
      )
    } else {
      result=tibble()
    }
    result
  }
  p <- (xml2::read_html(url_for_page(0)) %>%
          html_nodes(".pagination"))[2]
  l <- p %>% html_nodes("li")
  max_page = (l[length(l)-1] %>%
                html_node("a") %>%
                html_text() %>%
                stringr::str_extract(.,"^(\\d+)") %>%
                as.integer)-1
  pb <- utils::txtProgressBar(0,max_page)
  bind_rows(lapply(seq(0,max_page),function(page){
    utils::setTxtProgressBar(pb, page)
    p <- xml2::read_html(url_for_page(page))
    l <- p %>%
      html_nodes("#ndm-results #tables .ndm-item") %>%
      purrr::map(parse_table_data) %>% bind_rows
  }))
}

#' Get overview list for all CANSIM tables
#'
#' Will generate the table in case it does not exist or refresh option is set
#'
#' @param refresh Default is *FALSE*, will regenerate the table if set to *TRUE*. Takes some time since this is scraping through several
#' hundred we pages to gather the data
#' if option *cache_path* is set it will look for and store the overview table in that directory.
#'
#' @return A tibble with available cansim tables, listing title, cansim table number, old table number, description and geographies covered.
#'
#' @export
list_cansim_tables <- function(refresh=FALSE){
  directory <- getOption("cache_path")
  if (is.null(directory)) {
    result=cansim:::cansim_table_list
    age=(Sys.Date()-attr(result,"date")) %>% as.integer
    if (age>30) {
      message(paste0("Your cansim table overview data is ",age," days old.\nConsider setting options(cache_path=\"your cache path\")\nin your .Rprofile and refreshing the table via list_cansim_tables(refresh=TRUE).\n\n"))
    }
    if (refresh==TRUE) {
      message("The table won't be able to refresh if options(cache_path=\"your cache path\") is not set.")
    }
  } else {
    path <- file.path(directory,"cansim_table_list.Rda")
    if (refresh | !file.exists(path)) {
      message("Generating the table overview data, this may take a while. 15 minutes is not unusual.")
      data <- cansim:::generate_table_metadata()
      attr(data,"date") <- Sys.Date()
      saveRDS(data,path)
    }
    result=readRDS(path)
    age=(Sys.Date()-attr(result,"date")) %>% as.integer
    if (age>30) {
      message(paste0("Your cansim table overview data is ",age," days old.\nConsider refreshing the table via list_cansim_tables(refresh=TRUE)"))
    }
  }
  result
}

#' open cansim table information in browser
#'
#' useful for getting furthe info on cansim table and survey methods
#'
#' @param cansimTableNumber cansim table number
#' @param browser optionally a browser to open the page in
#'
#' @return none
#'
#' @export
view_cansim_webpage <- function(cansimTableNumber,browser = getOption("browser")){
  cansimTableNumber <- paste0(gsub("-","",cleaned_ndm_table_number(cansimTableNumber)),"01")
  url <- paste0("https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=",gsub("-","",cansimTableNumber))
  utils::browseURL(url,browser)
}




#' Get table metadata from API
#' Also accepts a vector of cansim table numbers, new or old.
#' Patience required, StatCan API is very slow. Alternatively
#' one can also use the `list_cansim_tables()` function to retieve
#' a (cached) list of cansim tables with (more limited) metadata
#'
#' @param cansimTableNumber new or old CANSIM table number
#'
#' @return a tibble containing the table metadata
#'
#' @export
get_cansim_cube_metadata <- function(cansimTableNumber){
  table_id=naked_ndm_table_number(cansimTableNumber)
  url="https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata"
  response <- httr::POST(url,
                     #body=jsonlite::toJSON(list("productId"=table_id),auto_unbox =TRUE),
                     body=paste0("[",paste(paste0('{"productId":',table_id,'}'),collapse = ", "),"]"),
                     encode="json",
                     httr::add_headers("Content-Type"="application/json")
  )
  if (response$status_code!=200) {
    stop("Problem downloading data, status code ",response$status_code)
  }
  data <- httr::content(response)
  data1 <- Filter(function(x)x$status=="SUCCESS",data)
  data2 <- Filter(function(x)x$status!="SUCCESS",data)
  if (length(data2)>0) {
    message(paste0("Failed to load metadata for ",length(data2)," tables "))
    data2 %>% purrr::map(function(x){
      message(x$object)
    })
  }
  fields <- c("productId", "cansimId", "cubeTitleEn", "cubeTitleFr", "cubeStartDate", "cubeEndDate", "nbSeriesCube",
              "nbDatapointsCube",  "archiveStatusCode", "archiveStatusEn",   "archiveStatusFr",   "subjectCode",
              "surveyCode",  "dimension")
  l <- lapply(fields, function(field){
    purrr::map(data1,function(d){
      dd<-d$object[[field]]
      if (typeof(dd)=="list") dd <- dd %>% unlist %>% as.character() %>% paste(collapse = ",")
      dd
      }) %>% as.character()
  }) %>%
    purrr::set_names(fields) %>%
    tibble::as.tibble()
}

#' get table url from API, should be more stable than guessing the link
get_cansim_table_url <- function(cansimTableNumber,language){
  l <- cansim:::cleaned_ndm_language(language) %>% substr(1,2)
  url=paste0("https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/",cansim:::naked_ndm_table_number(cansimTableNumber),"/",l)
  response <- httr::GET(url)
  if (response$status_code!=200) {
    stop("Problem downloading data, status code ",response$status_code)
  }
  httr::content(response)$object
}

#' get list of changed tables in date range
#'
#' @param start_date starting date to look for changes that changed on or after that date
#'
#' @return a tible with cansim product ids and release times
#'
#' @export
get_cansim_changed_tables <- function(start_date){
  end_date=NA
  url=paste0("https://www150.statcan.gc.ca/t1/wds/rest/getChangedCubeList/",start_date)
  if (!is.na(end_date)) url = paste0(url,"/",end_date)
  response <- httr::GET(url)
  if (response$status_code!=200) {
    stop("Problem downloading data, status code ",response$status_code)
  }
  httr::content(response)$object %>%
    map(function(o)tibble(productId=o$productId,releaseTime=o$releaseTime)) %>%
    bind_rows
}

#' get list of changed tables in date range
#'
#' @param vecors list of vectors to retrieve
#' @param start_time data release data starting time
#' @param start_time optional data release data ending time, default is current time
#'
#' @return a tible with data for vectors released between start and end time
#'
#' @export
get_cansim_vector<-function(vectors,start_time,end_time=Sys.Date()){
  time_format="%Y-%m-%dT%H:%m"
  vectors=gsub("^v","",vectors) # allow for leading "v" by conditionally stripping it
  url="https://www150.statcan.gc.ca/t1/wds/rest/getBulkVectorDataByRange"
  vectors_string=paste0('"vectorIds":[',paste(purrr::map(vectors,function(x)paste0('"',x,'"')),collapse = ", "),"]")
  time_string=paste0('"startDataPointReleaseDate": "',strftime(start_time,time_format),
                     '","endDataPointReleaseDate": "',strftime(end_time,time_format),'"')
  response <- httr::POST(url,
                         #body=jsonlite::toJSON(list("vectorIds"=vectors)),
                         body=paste0("{",vectors_string,",",time_string,"}"),
                         encode="json",
                         httr::add_headers("Content-Type"="application/json")
  )
  if (response$status_code!=200) {
    stop("Problem downloading data, status code ",response$status_code)
  }
  data <- httr::content(response)
  data1 <- Filter(function(x)x$status=="SUCCESS",data)
  data2 <- Filter(function(x)x$status!="SUCCESS",data)
  if (length(data2)>0) {
    message(paste0("Failed to load metadata for ",length(data2)," tables "))
    data2 %>% purrr::map(function(x){
      message(x$object)
    })
  }
  vf=list("DECIMALS"="decimals",
          "VALUE"="value",
          "REF_DATE"="refPer",
          #"SYMBOL"="symbolCode"
          "SCALAR_ID"="scalarFactorCode")
  result <- purrr::map(data1,function(d){
    value_data = lapply(vf,function(f){purrr::map(d$object$vectorDataPoint,function(cc)cc[[f]]) %>% unlist}) %>%
      tibble::as.tibble() %>%
      mutate(COORDINATE=d$object$coordinate,
             VECTOR=paste0("v",d$object$vectorId))
    value_data
  }) %>%
    dplyr::bind_rows()
  result
}

#' @import dplyr
#' @importFrom rvest html_node
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom rlang set_names
#' @importFrom purrr map
#' @importFrom rlang :=

NULL


