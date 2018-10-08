#' get all cansim tables
get_cansim_table_list <- function(){
  start=0
  rows=1000
  stop=FALSE
  result=tibble::tibble()
  while(!stop) {
    new_rows <- cansim:::get_cansim_table_list_page(start,rows)
    if (nrow(new_rows)>0) {
      result=dplyr::bind_rows(result,new_rows)
      start = start + rows
    } else {
      stop=TRUE
    }
  }
  result
}

#' internal method to get page of cansim tables
#' @param start starting row
#' @param rows number of rows, maximum is 1000
get_cansim_table_list_page <- function(start=0,rows=1000){
  if (rows>1000) {
    warning("Clipping to 1000 rows")
    rows=1000
  }
  url=paste0("https://open.canada.ca/data/api/3/action/package_search?q=owner_org:A0F0FCFC-BC3B-4696-8B6D-E7E411D55BAC",
             "&start=",start,"&rows=",rows)
  response <- jsonlite::fromJSON(url)$result
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
      if (grepl("^\\d{8}$",cansim_table_number)) cansim_table_number = cleaned_ndm_table_number(.data$cansim_table_number)
      cansim_table_number
    }

    url_data %>% dplyr::mutate(cansim_table_number=table_number_from_url(.data$url_en))
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
                  grepl("^\\d{2}-\\d{2}-\\d{4}",cansim_table_number)) %>%
    dplyr::select(-source)
}



#' Get overview list for all CANSIM tables
#'
#' Will generate the table in case it does not exist or refresh option is set
#'
#' @param refresh Default is \code{FALSE}, and will regenerate the table if set to \code{TRUE}. This takes some time since this is scraping through several
#' hundred web pages to gather required metadata data. If option \code{cache_path} is set it will look for and store the overview table in that directory.
#'
#' @return A tibble with available CANSIM tables, listing title, CANSIM table number, old table number, description and geographies covered.
#'
#' @export
list_cansim_tables <- function(refresh=FALSE){
  directory <- getOption("cache_path")
  if (is.null(directory)) {
    result=cansim:::cansim_table_list
    age=(Sys.Date()-attr(result,"date")) %>% as.integer
    if (age>30) {
      message(paste0("Your CANSIM table overview data is ",age," days old.\nConsider setting options(cache_path=\"your cache path\")\nin your .Rprofile and refreshing the table via list_cansim_tables(refresh=TRUE).\n\n"))
    }
    if (refresh==TRUE) {
      message("The table won't be able to refresh if options(cache_path=\"your cache path\") is not set.")
    }
  } else {
    path <- file.path(directory,"cansim_table_list.Rda")
    if (refresh | !file.exists(path)) {
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

#' Search through CANSIM tables
#'
#' Searches through CANSIM tables using a search term. A new table is generated if it already does not exist or if refresh option is set to TRUE. Search-terms are case insensitive, but will accept regular expressions for more advanced searching. The search function can search either through table titles or through table descriptions, depending on the whether or not \code{search_description} is set to \code{TRUE} or not. If \code{refresh = TRUE}, the table will be updated and regenerated using Statistics Canada's latest data. This can take some time since this process involves scraping through several hundred web pages to gather the required metadata. If option \code{cache_path} is set it will look for and store the overview table in that directory.
#'
#' @param search_term User-supplied search term used to find CANSIM tables with matching titles
#' @param search_fields By default, this function will search through table titles and keywords. Setting this parameter to "title" will only serach through the title, setting it to "keyword" will only search through keywords.
#' @param refresh Default is \code{FALSE}, and will regenerate the table if set to \code{TRUE}
#'
#' @return A tibble with available CANSIM tables, listing title, CANSIM table number, old table number, description and geographies covered
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

