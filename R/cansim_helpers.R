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

file_path_for_table_language <- function(cansimTableNumber, language){
  language <- cleaned_ndm_language(language)
  if (is.na(language)) stop(paste0("Unkown Lanaguage ",language))
  base_table <- naked_ndm_table_number(cansimTableNumber)
  file.path(paste0(base_table,"-",language))
}

base_path_for_table_language <- function(cansimTableNumber, language){
  file.path(tempdir(),file_path_for_table_language(cansimTableNumber,language))
}


#' @import dplyr
#' @importFrom tibble as.tibble
#' @importFrom rvest html_node
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom rlang set_names
#' @importFrom rlang .data
#' @importFrom purrr map
#' @importFrom rlang :=
