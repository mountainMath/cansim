#' Get overview list for all Statistics Canada data tables (deprecated)
#'
#' This method is deprecated, please use `list_cansim_cubes` instead.
#' Generates an overview table containing metadata of available Statistics Canada data tables. A new and updated table will be generated if this table does not already exist in cached form or if the force refresh option is selected (set to \code{FALSE} by default). This can take some time as this process involves scraping through hundreds of Statistics Canada web pages to gather the required metadata. If option \code{cansim.cache_path} is set it will look for and store the overview table in that directory.
#'
#' @param refresh Default is \code{FALSE}, and will regenerate the table if set to \code{TRUE}
#'
#' @return A tibble with available Statistics Canada data tables, listing title, Statistics Canada data table catalogue number, deprecated CANSIM table number, description, and geography
#'
#' @examples
#' \dontrun{
#' list_cansim_tables()
#' }
#'
#' @keywords internal
#' @export
list_cansim_tables <- function(refresh=FALSE){
  .Deprecated("list_cansim_cubes",
              package="cansim",
              msg="This function has been deprecated, it will be removed in future versions. Please use list_cansim_cubes(...) instead.")

  list_cansim_cubes(lite=FALSE,refresh=refresh) %>%
    mutate(title=.data$cubeTitleEn,
           subject=.data$subjectEn,
           date_published=as.Date(.data$releaseTime)) %>%
    rename(title_en=.data$cubeTitleEn,
           title_fr=.data$cubeTitleFr,
           time_period_coverage_start=.data$cubeStartDate,
           time_period_coverage_end=.data$cubeEndDate)
}

#' Search through Statistics Canada data tables (deprecated)
#'
#' This method is deprecated, please use `search_cansim_cubes` instead.
#' Searches through Statistics Canada data tables using a search term. A new table is generated if it already does not exist or if refresh option is set to \code{TRUE}. Search-terms are case insensitive, but will accept regular expressions for more advanced searching. The search function can search either through table titles or through table descriptions, depending on the whether or not \code{search_description} is set to \code{TRUE} or not. If \code{refresh = TRUE}, the table will be updated and regenerated using Statistics Canada's latest data. This can take some time since this process involves scraping through several hundred web pages to gather the required metadata. If option \code{cache_path} is set it will look for and store the overview table in that directory.
#'
#' @param search_term User-supplied search term used to find Statistics Canada data tables with matching titles
#' @param search_fields By default, this function will search through table titles and keywords. Setting this parameter to "title" will only search through the title, setting it to "keyword" will only search through keywords
#' @param refresh Default is \code{FALSE}, and will regenerate the table if set to \code{TRUE}
#'
#' @return A tibble with available Statistics Canada data tables, listing title, Statistics Canada data table catalogue number, deprecated CANSIM table number, description and geography that match the search term.
#'
#' @examples
#' \dontrun{
#' search_cansim_tables("Labour force")
#' }
#'
#' @keywords internal
#' @export
search_cansim_tables <- function(search_term, search_fields = "both", refresh=FALSE){
  .Deprecated("search_cansim_cubes",
              package="cansim",
              msg="This function has been deprecated, it will be removed in future versions. Please use search_cansim_cubes(...) instead.")

  search_cansim_cubes(search_term = search_term, refresh=refresh) %>%
    mutate(title=.data$cubeTitleEn,
           subject=.data$subjectEn,
           date_published=as.Date(.data$releaseTime)) %>%
    rename(title_en=.data$cubeTitleEn,
           title_fr=.data$cubeTitleFr,
           time_period_coverage_start=.data$cubeStartDate,
           time_period_coverage_end=.data$cubeEndDate)
}


#' Get overview list for all Statistics Canada data cubes
#'
#' Generates an overview table containing metadata of available Statistics Canada data cubes.
#'
#' @param lite Get the version without cube dimensions and comments for faster retrieval, default is \code{FALSE}.
#' @param refresh Default is \code{FALSE}, repeated calls during the same session will hit the cached data.
#' @param quiet Optional, suppress messages
#' To refresh the code list during a running R session set to \code{TRUE}
#'
#' @return A tibble with available Statistics Canada data cubes, including NDM table number, cube title,
#' start and end dates, achieve status, subject and survey codes, frequency codes and a list of cube dimensions.
#'
#' @examples
#' \dontrun{
#' list_cansim_cubes()
#' }
#'
#' @export
list_cansim_cubes <- function(lite=FALSE,refresh=FALSE,quiet=FALSE){
  data <- NULL
  directory <- tempdir()
  path <- file.path(directory,paste0("cansim_cube_list_",ifelse(lite,"lite","regular"),".Rda"))
  if (refresh | !file.exists(path)) {
    if (!quiet) message("Retrieving cube information from StatCan servers...")
    url=ifelse(lite,"https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesListLite","https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesList")
    r<-get_with_timeout_retry(url,retry=0,warn_only=TRUE)
    if (r$status_code==200) {
      content <- httr::content(r)

      header <- content[[1]] %>%
        tibble::enframe() %>%
        t() %>%
        as.data.frame() %>%
        slice(1) %>%
        unlist() %>%
        as.character()

      # if ("dimesions" %in% header)
      #   header <- c(setdiff(header,"dimensions"),"dimensionNameEn","dimensionNameFr")

      h1 <- setdiff(header,"dimensions")

      surveys <- get_cansim_code_set("survey")
      surveys_en <- setNames(surveys$surveyEn,surveys$surveyCode)
      surveys_fr <- setNames(surveys$surveyFr,surveys$surveyCode)
      subjects <- get_cansim_code_set("subject")
      subjects_en <- setNames(subjects$subjectEn,subjects$subjectCode)
      subjects_fr <- setNames(subjects$subjectEn,subjects$subjectCode)

      if (lite) {
        r<-content %>%
          lapply(function(l){
            lapply(l,function(d)paste0(unlist(d),collapse=", ")) %>%
              as_tibble()
          }) %>%
          bind_rows()
      } else {
        r<-content %>%
          lapply(function(l){
            lapply(l[h1],function(d)paste0(unlist(d),collapse=", ")) %>%
              as_tibble() %>%
              bind_cols(tibble(
                dimensionNameEn=lapply(l[["dimensions"]],function(d)d["dimensionNameEn"]) %>% unlist %>% paste0(.,collapse = ", "),
                dimensionNameFr=lapply(l[["dimensions"]],function(d)d["dimensionNameFr"]) %>% unlist %>% paste0(.,collapse = ", ")))
          }) %>%
          bind_rows()
      }

      data <- r %>%
        mutate_at(vars(ends_with("Date")),as.Date) %>%
        mutate_at(vars(matches("releaseTime")),function(d)readr::parse_datetime(d,
                                                                                #format=STATCAN_TIME_FORMAT,
                                                                                locale=readr::locale(tz=STATCAN_TIMEZONE))) %>%
        mutate(archived=.data$archived==1) %>%
        mutate(cansim_table_number=cleaned_ndm_table_number(.data$productId)) %>%
        select(c("cansim_table_number","cubeTitleEn","cubeTitleFr"),
               setdiff(names(.),c("cansim_table_number","cubeTitleEn","cubeTitleFr"))) %>%
        mutate(surveyEn=lapply(.data$surveyCode,function(d)surveys_en[unlist(strsplit(d,", "))] %>% paste0(collapse="; ")) %>% unlist) %>%
        mutate(surveyFr=lapply(.data$surveyCode,function(d)surveys_fr[unlist(strsplit(d,", "))] %>% paste0(collapse="; ")) %>% unlist) %>%
        mutate(subjectEn=lapply(.data$subjectCode,function(d)subjects_en[unlist(strsplit(d,", "))] %>% paste0(collapse="; ")) %>% unlist) %>%
        mutate(subjectFr=lapply(.data$subjectCode,function(d)subjects_fr[unlist(strsplit(d,", "))] %>% paste0(collapse="; ")) %>% unlist)

      saveRDS(data,path)
    } else {
      warning("Could not retrieve cube list from StatCan servers.")
    }
  } else {
    if (!quiet) message("Retrieving cube information from temporary cache.")
    data <- readRDS(path)
  }
  data
}


#' Search through Statistics Canada data cubes
#'
#' Searches through Statistics Canada data cubes using a search term.
#'
#' @param search_term User-supplied search term used to find Statistics Canada data cubes with matching titles, table numbers, subject and survey codes.
#' @param refresh Default is \code{FALSE}. The underlying cube list is cached for the duration of the R sessions and will regenerate the cube list if set to \code{TRUE}
#'
#' @return A tibble with available Statistics Canada data cubes, listing title, Statistics Canada data cube catalogue number, deprecated CANSIM table number, survey and subject.
#'
#' @examples
#' \dontrun{
#' search_cansim_cubes("Labour force")
#' }
#'
#' @export
search_cansim_cubes <- function(search_term, refresh=FALSE){
  list_cansim_cubes(refresh = refresh) %>%
    filter(grepl(search_term,.data$cubeTitleEn,ignore.case = TRUE) |
             grepl(search_term,.data$cubeTitleFr,ignore.case = TRUE) |
             grepl(search_term,.data$surveyEn,ignore.case = TRUE) |
             grepl(search_term,.data$surveyFr,ignore.case = TRUE) |
             grepl(search_term,.data$subjectEn,ignore.case = TRUE) |
             grepl(search_term,.data$subjectFr,ignore.case = TRUE) |
             grepl(search_term,.data$subjectCode,ignore.case = TRUE) |
             grepl(search_term,.data$surveyCode,ignore.case = TRUE) |
             grepl(search_term,.data$cansim_table_number,ignore.case = TRUE) |
             grepl(search_term,.data$productId,ignore.case = TRUE))
}

#' Major economic indicator release schedule
#'
#' Returns every release date of major economic indicators since March 14, 2012.
#' This also includes scheduled future releases.
#'
#' @return a tibble with data, and details for major economic indicator release
#'
#' @examples
#' \dontrun{
#' get_cansim_key_release_schedule()
#' }
#'
#' @export
get_cansim_key_release_schedule <- function(){
  url <- "https://www150.statcan.gc.ca/n1/dai-quo/ssi/homepage/schedule-key_indicators-eng.json"
  response <- get_with_timeout_retry(url)

  if (response$status_code!=200){
    stop("Problem accessing release schedule.",call.=FALSE)
  }

  httr::content(response) %>%
    lapply(dplyr::as_tibble) %>%
    dplyr::bind_rows() %>%
    mutate(date=strftime(date,STATCAN_TIME_FORMAT_S,tz="UTC") %>% as.Date)
}


