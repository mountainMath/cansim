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


# refresh_cansim_table_list <- function(){
#   cansim_table_list <- list_cansim_tables(refresh = TRUE)
#   usethis::use_data(cansim_table_list, internal = TRUE, overwrite = TRUE)
# }
