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
    if (is.null(r)||is.null(r$status_code)){
      warning("Could not retrieve cube list from StatCan servers.")
      return(NULL)
    }
    if (!is.null(r$status_code) && r$status_code==200) {
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
  cube_list <- list_cansim_cubes(refresh = refresh)
  if (is.null(cube_list)) {
    stop("Could not retrieve cube list from StatCan servers.",call.=FALSE)
  }
  cube_list %>%
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


