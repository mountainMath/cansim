#' Retrieve list of all Statistics Canada data tables
#'
#' Internal function to collect an up-to-date and complete list of Statistics Canada data tables
#' @return cansim table list
#' @keywords internal
get_cansim_table_list <- function(){
  start=0
  rows=1000
  stop=FALSE
  result=tibble::tibble()
  while(!stop) {
    new_rows <- get_cansim_table_list_page(start,rows)
    if (nrow(new_rows)>0) {
      result=dplyr::bind_rows(result,new_rows)
      start = start + rows
    } else {
      stop=TRUE
    }
  }
  result
}

#' An internal method to get page of Statistics Canada data tables
#' @param start_offset starting row
#' @param max_rows number of rows, maximum is 1000
#' @return cansim table list
#' @keywords internal
get_cansim_table_list_page <- function(start_offset=0,max_rows=1000){
  if (max_rows>1000) {
    warning("Clipping to 1000 rows")
    max_rows=1000
  }
  url=paste0("https://open.canada.ca/data/api/3/action/package_search?q=owner_org:A0F0FCFC-BC3B-4696-8B6D-E7E411D55BAC",
             "&start=",start_offset,"&rows=",max_rows)
  raw_response <- get_with_timeout_retry(url)
  response <- jsonlite::fromJSON(httr::content(raw_response,type="text",encoding = "UTF-8"))$result
  results=response$results
  if (length(results)==0) return (tibble::tibble())
  string_list <- function(d){
    purrr::map(d,function(l) paste(as.character(unlist(l)),collapse = ", ")) %>% unlist
  }

  url_rows <- dplyr::bind_rows(lapply(results$resources,function(d){
    dd <- tibble::tibble(lang=string_list(d$language),format=d$format,url=d$url) %>%
      dplyr::filter(.data$format=="CSV")
    if (nrow(dd)==2) {
      # have cansim dataset
      url_1=filter(dd,.data$lang=="en")
      url_2=filter(dd,.data$lang=="fr")
      url_1=ifelse(nrow(url_1)==1,url_1$url,"")
      url_2=ifelse(nrow(url_2)==1,url_2$url,"")
      url_data=tibble::tibble(source="CANSIM",
                              url_en=url_1,
                              url_fr=url_2)
    } else {
      url_data=tibble::tibble(source="Other",url_en="",url_fr="")
    }
    table_number_from_url <- function(u){
      cansim_table_number=gsub('^(https://.+/)(\\d+)(-eng.zip)$', '\\2', u)
      if (grepl("^\\d{8}$",cansim_table_number)) cansim_table_number = cleaned_ndm_table_number(cansim_table_number)
      cansim_table_number
    }

    url_data %>%
      dplyr::mutate(cansim_table_number=table_number_from_url(.data$url_en))
  }))

  rows <- tibble::tibble(title=results$title,
                         title_en=results$title_translated$en,
                         title_fr=results$title_translated$fr,
                         keywords_en=string_list(results$keywords$en),
                         keywords=string_list(results$keywords$en),
                         keywords_fr=string_list(results$keywords$en),
                         notes=results$notes,
                         notes_en=results$notes_translated$en,
                         notes_fr=results$notes_translated$fr,
                         state=results$state,
                         subject=string_list(results$subject),
                         date_published=results$date_published,
                         frequency=results$frequency,
                         revision_id=results$revision_id,
                         time_period_coverage_start=results$time_period_coverage_start,
                         time_period_coverage_end=results$time_period_coverage_end,
                         metadata_created=results$metadata_created,
                         metadata_modified=results$metadata_modified) %>%
    dplyr::bind_cols(url_rows)

  rows %>%
    dplyr::filter(.data$source=="CANSIM",
                  grepl("^\\d{2}-\\d{2}-\\d{4}",.data$cansim_table_number)) %>%
    dplyr::select(-source)
}



#' Get overview list for all Statistics Canada data tables
#'
#' Generates an overview table containing metadata of available Statistics Canada data tables. A new and updated table will be generated if this table does not already exist in cached form or if the force refresh option is selected (set to \code{FALSE} by default). This can take some time as this process involves scraping through hundreds of Statistics Canada web pages to gather the required metadata. If option \code{cache_path} is set it will look for and store the overview table in that directory.
#'
#' @param refresh Default is \code{FALSE}, and will regenerate the table if set to \code{TRUE}
#'
#' @return A tibble with available Statistics Canada data tables, listing title, Statistics Canada data table catalogue number, deprecated CANSIM table number, description, and geography
#'
#' @export
list_cansim_tables <- function(refresh=FALSE){
  # flow: if cache_path version exists, use that, otherwise fall back on package version
  # if refresh is TRUE, refresh cache path

  directory <- getOption("cache_path")
  path <- file.path(directory,"cansim_table_list.Rda")
  if (is.null(directory) || (!refresh && !exists(path))) {
    result=cansim_table_list
    age=(Sys.Date()-attr(result,"date")) %>% as.integer
    if (age>30) {
      message_text <- paste0("Your CANSIM table overview data is ",age," days old.\n")
      if (is.null(directory)) message_text <- paste0(message_text,"Consider setting options(cache_path=\"your cache path\")\nin your .Rprofile and refreshing the table via list_cansim_tables(refresh=TRUE).\n\n")
      else message_text <- paste0(message_text,"Consider refreshing the table via list_cansim_tables(refresh=TRUE).\n\n")
      message(paste0("Your CANSIM table overview data is ",age," days old.\nConsider setting options(cache_path=\"your cache path\")\nin your .Rprofile and refreshing the table via list_cansim_tables(refresh=TRUE).\n\n"))
      if (is.null(directory)) {
        message("The table won't be able to be refreshed if options(cache_path=\"your cache path\") is not set.")
      }
    }
  } else {
    if (refresh) {
      message("Generating the table overview data, this may take a minute.")
      data <- get_cansim_table_list()
      attr(data,"date") <- Sys.Date()
      saveRDS(data,path)
    }
    result=readRDS(path)
    age=(Sys.Date()-attr(result,"date")) %>% as.integer
    if (age>30) {
      message(paste0("Your CANSIM table overview data is ",age," days old.\nConsider refreshing the table via list_cansim_tables(refresh=TRUE)"))
    }
  }
  result
}

#' Search through Statistics Canada data tables
#'
#' Searches through Statistics Canada data tables using a search term. A new table is generated if it already does not exist or if refresh option is set to \code{TRUE}. Search-terms are case insensitive, but will accept regular expressions for more advanced searching. The search function can search either through table titles or through table descriptions, depending on the whether or not \code{search_description} is set to \code{TRUE} or not. If \code{refresh = TRUE}, the table will be updated and regenerated using Statistics Canada's latest data. This can take some time since this process involves scraping through several hundred web pages to gather the required metadata. If option \code{cache_path} is set it will look for and store the overview table in that directory.
#'
#' @param search_term User-supplied search term used to find Statistics Canada data tables with matching titles
#' @param search_fields By default, this function will search through table titles and keywords. Setting this parameter to "title" will only search through the title, setting it to "keyword" will only search through keywords
#' @param refresh Default is \code{FALSE}, and will regenerate the table if set to \code{TRUE}
#'
#' @return A tibble with available Statistics Canada data tables, listing title, Statistics Canada data table catalogue number, deprecated CANSIM table number, description and geography
#'
#' @export
search_cansim_tables <- function(search_term, search_fields = "both", refresh=FALSE){
  tables <- list_cansim_tables(refresh = refresh)
  if(search_fields=="title") {
    tables %>%
      filter(grepl(search_term, .data$title, ignore.case = TRUE))
  } else if(search_fields=="keywords") {
    tables %>%
      filter(grepl(search_term, .data$keywords, ignore.case = TRUE))
  } else {
    tables %>%
      filter(grepl(search_term, .data$subject, ignore.case = TRUE) | grepl(search_term, .data$keywords, ignore.case = TRUE) | grepl(search_term, .data$title, ignore.case = TRUE))
  }
}

