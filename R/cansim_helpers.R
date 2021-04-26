cleaned_ndm_table_number <- function(cansimTableNumber){
  n<-gsub("-","",as.character(cansimTableNumber)) %>%
    purrr::map(function(t){
      if (nchar(t)<=7) {
        tt<-cansim_old_to_new(t)
        message("Legacy table number ",cansimTableNumber,", converting to NDM ",tt)
        t=gsub("-","",tt)
      }
      t
    }) %>% unlist
  paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
}

naked_ndm_table_number <- function(cansimTableNumber){
  as.character(gsub("-","",cleaned_ndm_table_number(cansimTableNumber)))
}

cleaned_ndm_language <- function(language){
  ifelse(tolower(language) %in% c("english","eng","en"),"eng",ifelse(tolower(language) %in% c("fra","french","fr"),"fra",NA))
}

file_path_for_table_language <- function(cansimTableNumber, language){
  language <- cleaned_ndm_language(language)
  if (is.na(language)) stop(paste0("Unkown Lanaguage ",language))
  base_table <- naked_ndm_table_number(cansimTableNumber)
  file.path(paste0(base_table,"-",language))
}

base_path_for_table_language <- function(cansimTableNumber, language){
  file.path(tempdir(),file_path_for_table_language(cansimTableNumber,language))
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

get_with_timeout_retry <- function(url,timeout=200,retry=3,path=NA){
  if (!is.na(path)) {
    response <- purrr::safely(httr::GET)(url,httr::timeout(timeout),httr::write_disk(path,overwrite = TRUE))
  } else {
    response <- purrr::safely(httr::GET)(url,httr::timeout(timeout))
  }
  if (!is.null(response$error)){
    if (retry>0) {
      message("Got timeout from StatCan, trying again")
      response <- get_with_timeout_retry(url,timeout=timeout,retry=retry-1,path=path)
    } else {
      message("Got timeout from StatCan, giving up")
      response=response$result
    }
  } else if (response$result$status_code %in% names(response_error_translation)){
    stop(sprintf("%s\nReturned status code %s",response_error_translation[[as.character(response$result$status_code)]], response$result$status_code),call.=FALSE)
  } else if (response$result$status_code != 200){
    stop(sprintf("Problem downloading data, returned status code %s.",response$result$status_code),call.=FALSE)
  } else {
    response=response$result
  }

  if (is.null(response) && retry == 0) {
    stop(sprintf("Problem downloading data, multiple timeouts.\nPlease check your network connection. If your connections is fine then StatCan servers might be down."),call.=FALSE)
  }
  response
}

post_with_timeout_retry <- function(url,body,timeout=200,retry=3){
  response <- purrr::safely(httr::POST)(url,
                                        body=body,
                                        encode="json",
                                        httr::add_headers("Content-Type"="application/json"),
                                        httr::timeout(timeout))
  if (!is.null(response$error)){
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
    stop(sprintf("Problem downloading data, multiple timeouts.\nPlease check your network connection. If your connections is fine then StatCan servers might be down."),call.=FALSE)
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

# short_prov.fr <- c(
#   "Colombie-Britannique"="BC",
#   "Alberta"="AB",
#   "Saskatchewan"="SK",
#   "Manitoba"="MB",
#   "Ontario"="ON",
#   "Qu\U00E9bec"="QC",
#   "Nouveau-Brunswick"="NB",
#   "\u00CEle-du-Prince-\U00C9douard"="PE",
#   "Nouvelle-\U00C9cosse"="NS",
#   "Terre-Neuve-et-Labrador"="NL",
#   "Yukon"="YT",
#   "Territoires du Nord-Ouest"="NT",
#   "Nunavut"="NU",
#   "Territoires du Nord-Ouest incluant Nunavut"="NTNU",
#   "Canada"="CAN"
# )

short_prov.fr <- purrr::set_names(c(
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
#' @param data code{cansim} package data frame with provincial level data
#' @return a code{cansim} package data frame with additional factor GEO.abb that contains language-specific provincial abbreviations
#'
#' @examples
#' \donttest{
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
  data <- data %>%
    mutate(GEO.abb=factor(as.character(short_prov[!!as.name(data_geography_column)]), levels=c("CAN","BC","AB","SK","MB","ON","QC","NB","PE","NS","NL","YT","NT","NU","NTNU")))
}


#' Get NDM code sets
#'
#' Useful to get a list of surveys  or subjects and used internally
#' @export
#' @param code_set the code set to retrieve.
#' @param refresh Default is \code{FALSE}, repeated calls during the same session will hit the cached data.
#' To refresh the code list during a running R session set to \code{TRUE}
#' @return a tibble with english and french labels for the given code set
#'
#' @examples
#' \donttest{
#' get_cansim_code_set("survey")
#' }
get_cansim_code_set <- function(code_set=c("scalar", "frequency", "symbol", "status", "uom", "survey",  "subject", "wdsResponseStatus"),
                                refresh=FALSE){
  code_sets <- c("scalar", "frequency", "symbol", "status", "uom", "survey",  "subject", "wdsResponseStatus")
  if (length(code_set)!=1 | !(code_set %in% code_sets)) {
    stop(paste0("Invalid code set, code_set must be one of ",paste0(code_sets,collapse=", ")))
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
      stop(httr::content(r))
    }
  } else {
    content <- readRDS(path)
  }
  m<-do.call(rbind, content$object[[code_set]])
  m[m=="NULL"] <- NA
  as_tibble(m) %>%
    mutate_all(unlist)
}

