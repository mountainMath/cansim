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

#' Legacy method, get cansim tables based on old table names
#' Get cansim table into tidy dataframe
#' Caches the table data for the current session
#'
get_cansim_old <- function(cansimTableNumber,language="english"){
  lang_ext=ifelse(tolower(language)=="english","-eng",ifelse(tolower(language) %in% c("french"),"-fra",NA))
  if (is.na(lang_ext)) stop(paste0("Unkown Lanaguage ",language))
  cleaned_number=sprintf("%07d", as.numeric(sub("-","",as.character(cansimTableNumber))))
  path <- file.path(tempdir(),paste0(cleaned_number,lang_ext))
  if (!file.exists(path)){
    url <- "http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/"
    cansimTableNumberString <- cleaned_number # pad with zeros if needed
    filename <- paste0("0", cansimTableNumberString, lang_ext)
    url <- paste0(url, filename, ".zip")
    utils::download.file(url, path, quiet = TRUE)
    data <- NA
    na_strings=c("<NA>",NA,"NA","","F")
    if(lang_ext=="-eng")
      data <- readr::read_csv(unz(path, paste0(filename, ".csv")),
                              na=na_strings,
                              locale=readr::locale(encoding="Windows-1254"),
                              col_types = list(.default = "c",Value="d"))
    else
      data <- readr::read_csv2(unz(path, paste0(filename, ".csv")),
                               na=na_strings,
                               locale=readr::locale(encoding="Windows-1254"),
                               col_types = list(.default = "c",Valeur="d"))
    saveRDS(data,file=path)
  }
  readRDS(file=path)
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
  percentage_string=ifelse(language=="fr","Pourcentage","Percentage")
  replacement_value_string = ifelse(is.na(replacement_value),value_string,replacement_value)
  data <- data %>%
    mutate(!!as.name(replacement_value_string):=!!as.name(value_string)*(`^`(10,as.integer(!!as.name(scale_string)))))
  if (is.na(replacement_value)) { # remove scale columns
    data <- data %>% select(-one_of(scale_string,scale_string2))
  }
  if (normalize_percent) {
    # divide numbers that are percentages by 100 and convert the unit field to "rate"
    data <- data %>%
      mutate(!!as.name(replacement_value_string):=ifelse(!!as.name(uom_string)==percentage_string,!!as.name(replacement_value_string)/100,!!as.name(replacement_value_string))) %>%
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
}

#' Adjust Cansim Value by scaled amount
#' French part does not work, probably encoding issues
#'
adjust_cansim_values_by_variable_old <-function(data,var){
  if("Valeur" %in% names(data))
    data <- data %>%
      mutate(Valeur=ifelse(grepl(" \\(x 1 000 000\\)$",rlang::UQ(as.name(var))),1000000*Valeur,Valeur)) %>%
      mutate(!!var:=sub(" \\(x 1 000 000\\)$","",rlang::UQ(as.name(var)))) %>%
      mutate(Valeur=ifelse(grepl(" \\(x 1 000\\)$",rlang::UQ(as.name(var))),1000*Valeur,Valeur)) %>%
      mutate(!!var:=sub(" \\(x 1 000\\)$","",rlang::UQ(as.name(var))))
  else
    data <- data %>%
      mutate(Value=ifelse(grepl(" \\(x 1,000,000\\)$",rlang::UQ(as.name(var))),1000000*Value,Value)) %>%
      mutate(!!var:=sub(" \\(x 1,000,000\\)$","",rlang::UQ(as.name(var)))) %>%
      mutate(Value=ifelse(grepl(" \\(x 1,000\\)$",rlang::UQ(as.name(var))),1000*Value,Value)) %>%
      mutate(!!var:=sub(" \\(x 1,000\\)$","",rlang::UQ(as.name(var))))

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

#' translate from old table number to NDM table number by web search and scraping
#'
#' @param oldCansimTableNumber the old cansim table number
cansim_old_to_new2 <- function(oldCansimTableNumber){
  cleaned_number=sprintf("%07d", as.numeric(sub("-","",as.character(oldCansimTableNumber))))
  cleaned_number <- paste0(substr(cleaned_number,1,3),"-",substr(cleaned_number,4,30))

  p <- xml2::read_html(paste0("https://www150.statcan.gc.ca/n1/en/type/data?text=",cleaned_number))
  new_table <- p %>%
    html_node(".ndm-result-productid") %>%
    html_text() %>%
    sub("^Table:\\s*","",.)
  warning <- p %>% html_node(".alert.alert-warning") %>% html_text()
  if (grepl(cleaned_number,warning)) {
    warning(paste0("Unable to match old CANSIM table number ",cleaned_number))
    stop(warning)
  }
  old_table <-  p %>%
    html_node(".ndm-result-formerid") %>%
    html_text() %>%
    sub("^\\s*\\(formerly: CANSIM ","",.) %>%
    sub("\\)$","",.)
  if (old_table!=cleaned_number) {
    stop(paste0("Unable to match old CANSIM table number ",cleaned_number))
  }
  new_table
}

#' Get cansim table via NDM
#' @param cansimTableNumber the NDM table number to load
#' @param language "en" or "fr" for english or french language version. Defaults to english.
#' @param refresh Optionally force reload of cansim data, default is *FALSE*. Cansim data is cached for the duration of the R session only
get_cansim_ndm <- function(cansimTableNumber,language="english",refresh=FALSE){
  lang_ext=ifelse(tolower(language) %in% c("english","eng","en"),"-eng",ifelse(tolower(language) %in% c("fra","french","fr"),"-fra",NA))
  if (is.na(lang_ext)) stop(paste0("Unkown Lanaguage ",language))
  n=as.character(gsub("-","",cansimTableNumber))
  cleaned_number <- paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
  message(paste0("Accessing CANSIM NDM product ",cleaned_number))
  base_table <- gsub("-","",cleaned_number)
  path <- file.path(tempdir(),paste0(base_table,lang_ext,".zip"))
  data_path <- file.path(tempdir(),paste0(base_table,lang_ext,".Rda"))
  if (refresh | !file.exists(data_path)){
    url=paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",base_table,lang_ext,".zip")
    httr::GET(url,httr::write_disk(path, overwrite = TRUE))
    data <- NA
    na_strings=c("<NA>",NA,"NA","","F")
    exdir=file.path(tempdir(),paste0(base_table,lang_ext))
    utils::unzip(path,exdir=exdir)
    unlink(path)
    if(lang_ext=="-eng") {
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
  lang_ext=ifelse(tolower(language) %in% c("english","eng","en"),"-eng",ifelse(tolower(language) %in% c("fra","french","fr"),"-fra",NA))
  if (is.na(lang_ext)) stop(paste0("Unkown Lanaguage ",language))
  n=as.character(gsub("-","",cansimTableNumber))
  cleaned_number <- paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
  base_table <- gsub("-","",cleaned_number)
  data_path <- file.path(tempdir(),paste0(base_table,lang_ext,".Rda1"))
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
  lang_ext=ifelse(tolower(language) %in% c("english","eng","en"),"-eng",ifelse(tolower(language) %in% c("fra","french","fr"),"-fra",NA))
  if (is.na(lang_ext)) stop(paste0("Unkown Lanaguage ",language))
  n=as.character(gsub("-","",cansimTableNumber))
  cleaned_number <- paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
  base_table <- gsub("-","",cleaned_number)
  data_path <- file.path(tempdir(),paste0(base_table,lang_ext,".Rda3"))
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
  lang_ext=ifelse(tolower(language) %in% c("english","eng","en"),"-eng",ifelse(tolower(language) %in% c("fra","french","fr"),"-fra",NA))
  if (is.na(lang_ext)) stop(paste0("Unkown Lanaguage ",language))
  n=as.character(gsub("-","",cansimTableNumber))
  cleaned_number <- paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
  base_table <- gsub("-","",cleaned_number)
  data_path <- file.path(tempdir(),paste0(base_table,lang_ext,".Rda4"))
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
  lang_ext=ifelse(tolower(language) %in% c("english","eng","en"),"-eng",ifelse(tolower(language) %in% c("fra","french","fr"),"-fra",NA))
  if (is.na(lang_ext)) stop(paste0("Unkown Lanaguage ",language))
  n=as.character(gsub("-","",cansimTableNumber))
  cleaned_number <- paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
  base_table <- gsub("-","",cleaned_number)
  data_path <- file.path(tempdir(),paste0(base_table,lang_ext,".Rda5"))
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
  lang_ext=ifelse(tolower(language) %in% c("english","eng","en"),"-eng",ifelse(tolower(language) %in% c("fra","french","fr"),"-fra",NA))
  if (is.na(lang_ext)) stop(paste0("Unkown Lanaguage ",language))
  n=as.character(gsub("-","",cansimTableNumber))
  cleaned_number <- paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
  base_table <- gsub("-","",cleaned_number)
  data_path <- file.path(tempdir(),paste0(base_table,lang_ext,".Rda2"))
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
  lang_ext=ifelse(tolower(language) %in% c("english","eng","en"),"-eng",ifelse(tolower(language) %in% c("fra","french","fr"),"-fra",NA))
  if (is.na(lang_ext)) stop(paste0("Unkown Lanaguage ",language))
  n=as.character(gsub("-","",cansimTableNumber))
  cleaned_number <- paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
  base_table <- gsub("-","",cleaned_number)
  data_path <- file.path(tempdir(),paste0(base_table,lang_ext,".Rda2"))
  if (refresh | !file.exists(data_path)){
    get_cansim_ndm(cansimTableNumber,language=language,refresh = refresh)
  }
  data_path <- file.path(tempdir(),paste0(base_table,lang_ext,".Rda_column_",column))
  if (!file.exists(data_path)){
    stop(paste0("Unkown column ",column))
  }
  readRDS(file=data_path)
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
#' if option *cache_path* is set it will look for and store the overview table in that directory. Otherwise file will be stored in *tempdir*
#'
#' @return A tibble with available cansim tables, listing title, cansim table number, old table number, description and geographies covered.
#'
#' @export
list_cansim_tables <- function(refresh=FALSE){
  directory <- getOption("cache_path")
  if (is.null(directory)) directory = tempdir()
  path <- file.path(directory,"cansim_table_list.Rda")
  if (refresh | !file.exists(path)) {
    data <- cansim:::generate_table_metadata()
    saveRDS(data,path)
  }
  readRDS(path)
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

#' @import dplyr
#' @importFrom rvest html_node
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom rlang set_names
#' @importFrom purrr map

NULL


