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
  raw_response <- suppressWarnings(get_with_timeout_retry(url))
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



  # rows <- tibble::as_tibble(results) %>%
  #   dplyr::bind_cols(url_rows) %>%
  #   dplyr::filter(.data$source=="CANSIM",
  #                 grepl("^\\d{2}-\\d{2}-\\d{4}",.data$cansim_table_number))
  #
  # dfs <- rows %>% select_if(is.data.frame) %>% names
  # lists <- rows %>% select_if(is.list) %>% names
  #
  # for (n in dfs) {
  #   rows <- rows %>%
  #     dplyr::mutate(!!paste0(n,"_en"):=!!(as.name(n)) %>% pull(.data$en),
  #                   !!paste0(n,"_fr"):=!!(as.name(n)) %>% pull(.data$fr)) %>%
  #     dplyr::select(-!!as.name(n))
  # }
  # for (n in lists) {
  #   rows <- rows %>%
  #     dplyr::mutate(!!paste0(n,"_en"):=!!(as.name(n))[["en"]],
  #                   !!paste0(n,"_fr"):=!!(as.name(n))[["fr"]]) %>%
  #     dplyr::select(-!!as.name(n))
  # }
  #
  #   mutate(title_en=.data$title_translated$en,
  #          title_fr=.data$title_translated$fr,
  #          notes_en=.data$notes_translated$en,
  #          notes_fr=.data$notes_translated$fr,
  #          keywords_en=string_list(.data$keywords$en),
  #          keywords_fr=string_list(.data$keywords$en),
  #          subject=string_list(.data$subject),
  #          organization_en=.data$organization$en,
  #          organization_fr=.data$organization$fr,
  #          program_page_url_en=.data$program_page_url$en,
  #          program_page_url_fr=.data$program_page_url$fr,
  #          data_series_name_en=.data$data_series_name$en,
  #          data_series_name_fr=.data$data_series_name$fr,
  #          org_title_at_publication_en=.data$org_title_at_publication$en,
  #          org_title_at_publication_fr=.data$org_title_at_publication$fr) %>%
  #   select(-.data$title_translated,
  #          -.data$notes_translated,
  #          -.data$keywords,
  #          -.data$organization,
  #          -.data$program_page_url,
  #          -.data$data_series_name,
  #          -.data$org_title_at_publication)

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
  warning("This method is deprecated, please use `list_cansim_cubes` instead.")
  if (FALSE) {
  # flow: if cansim.cache_path version exists, use that, otherwise fall back on package version
  # if refresh is TRUE, refresh cache path

  directory <- getOption("cansim.cache_path")
  #if (is.null(directory)) directory <- getOption("cache_path") # legacy
  path <- file.path(directory,"cansim_table_list.Rda")
  if (is.null(directory) || (!refresh && !file.exists(path))) {
    result=cansim_table_list
    age=(Sys.Date()-attr(result,"date")) %>% as.integer
    if (age>30) {
      message_text <- paste0("Your CANSIM table overview data is ",age," days old.\n")
      if (is.null(directory)) message_text <- paste0(message_text,"Consider setting options(cansim.cache_path=\"your cache path\")\nin your .Rprofile and refreshing the table via list_cansim_tables(refresh=TRUE).\n\n")
      else message_text <- paste0(message_text,"Consider refreshing the table via list_cansim_tables(refresh=TRUE).\n\n")
      message(paste0("Your CANSIM table overview data is ",age," days old.\nConsider setting options(cansim.cache_path=\"your cache path\")\nin your .Rprofile and refreshing the table via list_cansim_tables(refresh=TRUE).\n\n"))
      if (is.null(directory)) {
        message("The table won't be able to be refreshed if options(cansim.cache_path=\"your cache path\") is not set.")
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
  warning("This method is deprecated, please use `search_cansim_cubes` instead.")
  if (FALSE) {
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
list_cansim_cubes <- function(lite=FALSE,refresh=FALSE){
  directory <- tempdir() #getOption("cansim.cache_path")
  path <- file.path(directory,paste0("cansim_cube_list_",ifelse(lite,"lite","regular"),".Rda"))
  if (refresh | !file.exists(path)) {
    message("Retrieving cube information from StatCan servers...")
    url=ifelse(lite,"https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesListLite","https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesList")
    r<-get_with_timeout_retry(url)
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
                                                                                format=STATCAN_TIME_FORMAT,
                                                                                locale=readr::locale(tz="UTC"))) %>%
        mutate(archived=.data$archived==1) %>%
        mutate(cansim_table_number=cleaned_ndm_table_number(.data$productId)) %>%
        select(c("cansim_table_number","cubeTitleEn","cubeTitleFr"),
               setdiff(names(.),c("cansim_table_number","cubeTitleEn","cubeTitleFr"))) %>%
        mutate(surveyEn=lapply(.data$surveyCode,function(d)surveys_en[unlist(strsplit(d,", "))] %>% paste0(collapse="; ")) %>% unlist) %>%
        mutate(surveyFr=lapply(.data$surveyCode,function(d)surveys_fr[unlist(strsplit(d,", "))] %>% paste0(collapse="; ")) %>% unlist) %>%
        mutate(subjectEn=lapply(.data$subjectCode,function(d)subjects_en[unlist(strsplit(d,", "))] %>% paste0(collapse="; ")) %>% unlist) %>%
        mutate(subjectFr=lapply(.data$subjectCode,function(d)subjects_fr[unlist(strsplit(d,", "))] %>% paste0(collapse="; ")) %>% unlist)
      # if (lite) {
      #   dd<-content %>% purrr::map(function(l) {
      #     tibble::enframe(l) %>%
      #       mutate(value=purrr::map(.data$value,function(v)
      #         paste0(unlist(v),collapse = ", ")) %>%
      #           unlist)
      #   })
      # } else {
      #   dd <- content %>% purrr::map(function(l) {
      #     df <- tibble::enframe(l)
      #     dimensions <- filter(df,.data$name=="dimensions")$value
      #     c(df %>%
      #         filter(.data$name!=dimensions) %>%
      #         mutate(value=purrr::map(.data$value,function(v)
      #           paste0(unlist(v),collapse = ", ")) %>%
      #             unlist),
      #       purrr::map(dimensions[[1]],function(a)a$dimensionNameEn) %>% paste0(collapse=", "),
      #       purrr::map(dimensions[[1]],function(a)a$dimensionNameFr) %>% paste0(collapse=", "))
      #   })
      # }

      # data <- lapply(dd,function(d)d$value) %>%
      #   do.call(rbind,.) %>%
      #   as_tibble() %>%
      #   set_names(header) %>%
      #   mutate_at(vars(ends_with("Date")),as.Date) %>%
      #   mutate(archived=.data$archived==1) %>%
      #   mutate(cansim_table_number=cleaned_ndm_table_number(.data$productId)) %>%
      #   select(c("cansim_table_number","cubeTitleEn","cubeTitleFr"),
      #          setdiff(names(.),c("cansim_table_number","cubeTitleEn","cubeTitleFr"))) %>%
      #   left_join(surveys,by="surveyCode") %>%
      #   left_join(subjects,by="subjectCode")

      saveRDS(data,path)
    }
  } else {
    message("Retrieving cube information from temporary cache.")
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
    stop("Problem accessing release schedule.")
  }

  httr::content(response) %>%
    lapply(dplyr::as_tibble) %>%
    dplyr::bind_rows() %>%
    mutate(date=strftime(date,STATCAN_TIME_FORMAT_S,tz="UTC") %>% as.Date)
}


