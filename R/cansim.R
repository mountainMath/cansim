
#' Get cansim table into tidy dataframe
#' Caches the table data for the current session
#' @export
get_cansim <- function(cansimTableNumber,language="english"){
  t<-gsub("-","",as.character(cansimTableNumber))
  if (nchar(t)<=7) {
    tt<-cansim_old_to_new(t)
    message("Legacy table number ",cansimTableNumber,", converting to NDM ",tt)
    cansimTableNumber=tt
  }
  get_cansim_ndm(cansimTableNumber,language)
}

#' Legacy method, get cansim tables based on old table names
#' Get cansim table into tidy dataframe
#' Caches the table data for the current session
#' @export
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
    download.file(url, path, quiet = TRUE)
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
#' @export
adjust_cansim_values_by_variable <-function(data,var){
  normailze_cansim_values(data)
}

#' normailzes cancsim values by setting all units to counts/dollars instead of millions, etc.
#' if "replace" is true, it will replace the VALUE field with normailzed values and drop the scale columns,
#' otherwise it keeps the scale columns and creased a new column names "NORMALIZED_VALUE" with the normalized value
#' @export
normailze_cansim_values <- function(data,replace=TRUE){
  scale_string <- ifelse("VALEUR" %in% names(data),"IDENTIFICATEUR SCALAIRE","SCALAR_ID")
  scale_string2 <- ifelse("VALEUR" %in% names(data),"FACTEUR SCALAIRE","SCALAR_FACTOR")
  value_string <- ifelse("VALEUR" %in% names(data),"VALEUR","VALUE")
  if (replace) {
    data <- data %>%
      dplyr::mutate(!!as.name(value_string):=!!as.name(value_string)*(`^`(10,as.integer(!!as.name(scale_string))))) %>%
      dplyr::select(-one_of(scale_string,scale_string2))
  } else {
    data <- data %>%
      dplyr::mutate(NORMALIZED_VALUE=!!as.name(value_string)*(`^`(10,as.integer(!!as.name(scale_string)))))
  }
  data
}

#' Adjust Cansim Value by scaled amount
#' French part does not work, probably encoding issues
#' @export
adjust_cansim_values_by_variable_old <-function(data,var){
  if("Valeur" %in% names(data))
    data <- data %>%
      dplyr::mutate(Valeur=ifelse(grepl(" \\(x 1 000 000\\)$",UQ(as.name(var))),1000000*Valeur,Valeur)) %>%
      dplyr::mutate(!!var:=sub(" \\(x 1 000 000\\)$","",UQ(as.name(var)))) %>%
      dplyr::mutate(Valeur=ifelse(grepl(" \\(x 1 000\\)$",UQ(as.name(var))),1000*Valeur,Valeur)) %>%
      dplyr::mutate(!!var:=sub(" \\(x 1 000\\)$","",UQ(as.name(var))))
  else
    data <- data %>%
      dplyr::mutate(Value=ifelse(grepl(" \\(x 1,000,000\\)$",UQ(as.name(var))),1000000*Value,Value)) %>%
      dplyr::mutate(!!var:=sub(" \\(x 1,000,000\\)$","",UQ(as.name(var)))) %>%
      dplyr::mutate(Value=ifelse(grepl(" \\(x 1,000\\)$",UQ(as.name(var))),1000*Value,Value)) %>%
      dplyr::mutate(!!var:=sub(" \\(x 1,000\\)$","",UQ(as.name(var))))

  data
}


#' translate from old table number to NDM table number
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
    dplyr::filter(CANSIM_ID==as.integer(cleaned_number)) %>%
    dplyr::pull(PRODUCT_ID)
  if (identical(new_number, integer(0))) {
    stop(paste0("Unable to match old CANSIM table number ",cleaned_number))
  }
  n=as.character(new_number)
  new_table <- paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
}

#' translate from old table number to NDM table number by web search and scraping
#' @export
cansim_old_to_new2 <- function(oldCansimTableNumber){
  cleaned_number=sprintf("%07d", as.numeric(sub("-","",as.character(oldCansimTableNumber))))
  cleaned_number <- paste0(substr(cleaned_number,1,3),"-",substr(cleaned_number,4,30))

  p <- xml2::read_html(paste0("https://www150.statcan.gc.ca/n1/en/type/data?text=",cleaned_number))
  new_table <- p %>%
    rvest::html_node(".ndm-result-productid") %>%
    rvest::html_text() %>%
    sub("^Table:\\s*","",.)
  warning <- p %>% rvest::html_node(".alert.alert-warning") %>% rvest::html_text()
  if (grepl(cleaned_number,warning)) {
    warning(paste0("Unable to match old CANSIM table number ",cleaned_number))
    stop(warning)
  }
  old_table <-  p %>%
    rvest::html_node(".ndm-result-formerid") %>%
    rvest::html_text() %>%
    sub("^\\s*\\(formerly: CANSIM ","",.) %>%
    sub("\\)$","",.)
  if (old_table!=cleaned_number) {
    stop(paste0("Unable to match old CANSIM table number ",cleaned_number))
  }
  new_table
}

#' Get cansim table via NDM
#' @export
get_cansim_ndm <- function(cansimTableNumber,language="english"){
  lang_ext=ifelse(tolower(language) %in% c("english","eng","en"),"-eng",ifelse(tolower(language) %in% c("fra","french","fr"),"-fra",NA))
  if (is.na(lang_ext)) stop(paste0("Unkown Lanaguage ",language))
  n=as.character(gsub("-","",cansimTableNumber))
  cleaned_number <- paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
  message(paste0("Accessing CANSIM NDM product ",cleaned_number))
  base_table <- gsub("-","",cleaned_number)
  path <- file.path(tempdir(),paste0(base_table,lang_ext))
  if (!file.exists(path)){
    url=paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",base_table,lang_ext,".zip")
    download.file(url, path, quiet = TRUE)
    data <- NA
    na_strings=c("<NA>",NA,"NA","","F")
    p <- xml2::read_html(url)
    dl <- p %>% rvest::html_nodes(".item-list a") %>% rvest::html_attr("href")
    dl_url <- paste0(urltools::scheme(url),"://",urltools::domain(url),dl)
    download.file(dl_url,path)
    if(lang_ext=="-eng")
      data <- readr::read_csv(unz(path, paste0(base_table, ".csv")),
                              na=na_strings,
                              locale=readr::locale(encoding="UTF8"),
                              col_types = list(.default = "c")) %>%
      mutate(VALUE=as.numeric(VALUE))
    else
      data <- readr::read_csv2(unz(path, paste0(base_table, ".csv")),
                               na=na_strings,
                               locale=readr::locale(encoding="UTF8"),
                               col_types = list(.default = "c")) %>%
      mutate(VALEUR=as.numeric(VALEUR))
    saveRDS(data,file=path)
  }
  readRDS(file=path)
}


#' @importFrom dplyr %>%
#' @importFrom rlang .data
NULL


