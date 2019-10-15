MAX_PERIODS = 6000
STATCAN_TIMEZONE = "America/Toronto"
STATCAN_TIME_FORMAT="%Y-%m-%dT%H:%M"


extract_vector_data <- function(data1){
  vf=list("DECIMALS"="decimals",
          "VALUE"="value",
          "REF_DATE"="refPer",
          "REF_DATE_2"="refPer2",
          "releaseTime"="releaseTime",
          "SYMBOL"="symbolCode",
          "frequencyCode"="frequencyCode",
          "SCALAR_ID"="scalarFactorCode")
  result <- purrr::map(data1,function(d){
    vdp <- d$object$vectorDataPoint
    if (length(vdp)==0) {return(NULL)}
    value_data = lapply(vf,function(f){
      x=purrr::map(vdp,function(cc)cc[[f]])
      x[sapply(x, is.null)] <- NA
      unlist(x)
    }) %>%
      tibble::as.tibble() %>%
      mutate(COORDINATE=d$object$coordinate,
             VECTOR=paste0("v",d$object$vectorId))
    value_data
  }) %>%
    dplyr::bind_rows()
  ref_date_2 <- unique(result$REF_DATE_2) %>% unique
  if (length(ref_date_2)==1 && ref_date_2=="") result <- result %>% dplyr::select(-.data$REF_DATE_2)
  result
}

extract_vector_metadata <- function(data1){
  vf=list("DECIMALS"="decimals",
          "VECTOR"="vectorId",
          "table"="productId",
          "COORDINATE"="coordinate",
          "title_en"="SeriesTitleEn",
          "title_fr"="SeriesTitleFr",
          "UOM"="memberUomCode",
          "frequencyCode"="frequencyCode",
          "SCALAR_ID"="scalarFactorCode")
  result <- purrr::map(data1,function(d){
    value_data = lapply(vf,function(f){
      x=d$object[[f]]
      if (is.null(x)) x <- NA
      x
    }) %>%
      tibble::as.tibble()
    value_data
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(VECTOR=paste0("v",.data$VECTOR)) %>%
    dplyr::mutate(title=.data$title_en) %>%
    dplyr::mutate(table=cleaned_ndm_table_number(table))

  result
}

rename_vectors <- function(data,vectors){
  if (!is.null(names(vectors))) {
    vectors2 <- rlang::set_names(names(vectors),paste0("v",as.character(vectors)))
    data <- data %>%
      mutate(label=recode(.data$VECTOR,!!!vectors2))
  }
  data
}

#' Retrieve data for a Statistics Canada data vector released within a given time frame
#'
#' @param vectors The list of vectors to retrieve
#' @param start_time Starting date in \code{YYYY-MM-DD} format, applies to \code{REF_DATE} or \code{releaseTime}, depending on \code{use_ref_date} parameter
#' @param end_time Set an optional end time filter in \code{YYYY-MM-DD} format (defaults to current system time)
#' @param use_ref_date Optional, \code{TRUE} by default. When set to \code{TRUE}, uses \code{REF_DATE} of vector data to filter, otherwise it uses Statistics Canada's \code{releaseDate} value for filtering the specified vectors
#'
#' @return A tibble with data for vectors released between start and end time
#'
#' @examples
#' get_cansim_vector("v41690973","2015-01-01")
#'
#' @export
get_cansim_vector<-function(vectors, start_time, end_time=Sys.time(), use_ref_date=TRUE){
  start_time=as.Date(start_time)
  original_end_time=as.Date(end_time)
  if (use_ref_date) end_time=pmax(Sys.time(),end_time) else end_time=original_end_time
  vectors=gsub("^v","",vectors) # allow for leading "v" by conditionally stripping it
  url="https://www150.statcan.gc.ca/t1/wds/rest/getBulkVectorDataByRange"
  vectors_string=paste0('"vectorIds":[',paste(purrr::map(as.character(vectors),function(x)paste0('"',x,'"')),collapse = ", "),"]")
  time_string=paste0('"startDataPointReleaseDate": "',strftime(start_time,STATCAN_TIME_FORMAT,tz=STATCAN_TIMEZONE),
                     '","endDataPointReleaseDate": "',strftime(end_time,STATCAN_TIME_FORMAT,tz=STATCAN_TIMEZONE),'"')
  response <- post_with_timeout_retry(url, body=paste0("{",vectors_string,",",time_string,"}"))
  if (is.null(response)) return(response)
  if (response$status_code!=200) {
    stop("Problem downloading data, status code ",response$status_code,"\n",httr::content(response))
  }
  data <- httr::content(response)
  data1 <- Filter(function(x)x$status=="SUCCESS",data)
  data2 <- Filter(function(x)x$status!="SUCCESS",data)
  if (length(data2)>0) {
    message(paste0("Failed to load data for ",length(data2)," vector(s)."))
    data2 %>% purrr::map(function(x){
      message(paste0("Problem downloading data: ",response_status_code_translation[as.character(x$object$responseStatusCode)]))
    })
  }

  if (length(data1)>0)
    result <- extract_vector_data(data1) %>% rename_vectors(vectors)
  else
    result <- tibble::tibble()
  if (use_ref_date) {
    result <- result %>%
      filter(as.Date(.data$REF_DATE)>=start_time,as.Date(.data$REF_DATE)<=original_end_time)
  }
  result
}

#' Retrieve data for specified Statistics Canada data vector(s) for last N periods
#'
#' Allows for the retrieval of data for specified vector series for the N most-recently released periods.
#'
#' @param vectors The list of vectors to retrieve
#' @param periods Numeric value for number of latest periods to retrieve data for
#'
#' @return A tibble with data for specified vector(s) for the last N periods
#'
#' @examples
#' get_cansim_vector_for_latest_periods("v41690973",10)
#'
#' @export
get_cansim_vector_for_latest_periods<-function(vectors, periods=1){
  if (periods*length(vectors)>MAX_PERIODS) {
    periods=pmin(periods,floor(as.numeric(MAX_PERIODS)/length(vectors)))
    warning(paste0("Can access at most ",MAX_PERIODS," data points, capping value to ",periods," periods per vector."))
  }
  vectors=gsub("^v","",vectors) # allow for leading "v" by conditionally stripping it
  url="https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods"
  vectors_string=paste0("[",paste(purrr::map(as.character(vectors),function(x)paste0('{"vectorId":',x,',"latestN":',periods,'}')),collapse = ", "),"]")
  response <- post_with_timeout_retry(url, body=vectors_string)
  if (is.null(response)) return(response)
  if (response$status_code!=200) {
    stop("Problem downloading data, status code ",response$status_code,"\n",httr::content(response))
  }
  data <- httr::content(response)
  data1 <- Filter(function(x)x$status=="SUCCESS",data)
  data2 <- Filter(function(x)x$status!="SUCCESS",data)
  if (length(data2)>0) {
    message(paste0("Failed to load data for ",length(data2)," vector(s)."))
    data2 %>% purrr::map(function(x){
      message(paste0("Problem downloading data: ",response_status_code_translation[as.character(x$object$responseStatusCode)]))
    })
  }
  if (length(data1)>0)
    result <- extract_vector_data(data1) %>%
      rename_vectors(vectors)
  else
    result <- tibble::tibble()
  result
}


#' Retrieve data for specified Statistics Canada data product for last N periods for specific coordinates
#'
#' Allows for the retrieval of data for a Statistics Canada data table with specific coordinates for the N most-recently released periods. Caution: coordinates are concatenations of table member ID values and require familiarity with the TableMetadata data structure. Coordinates have a maximum of ten dimensions.
#'
#' @param cansimTableNumber Statistics Canada data table number
#' @param coordinate A string of table coordinates in the form \code{"1.1.1.36.1.0.0.0.0.0"}
#' @param periods Numeric value for number of latest periods to retrieve data for
#'
#' @return A tibble with data matching specified coordinate and period input arguments
#'
#' @export
get_cansim_data_for_table_coord_periods<-function(cansimTableNumber, coordinate, periods=1){
  table=naked_ndm_table_number(cansimTableNumber)
  url="https://www150.statcan.gc.ca/t1/wds/rest/getDataFromCubePidCoordAndLatestNPeriods"
  body_string=paste0('[{"productId":',table,',"coordinate":"',coordinate,'","latestN":',periods,'}]')
  response <- post_with_timeout_retry(url, body=body_string)
  if (response$status_code!=200) {
    stop("Problem downloading data, status code ",response$status_code,"\n",httr::content(response))
  }
  data <- httr::content(response)
  data1 <- Filter(function(x)x$status=="SUCCESS",data)
  data2 <- Filter(function(x)x$status!="SUCCESS",data)
  if (length(data2)>0) {
    message(paste0("Failed to load metadata for ",length(data2)," tables "))
    data2 %>% purrr::map(function(x){
      message(x$object)
    })
  }

  extract_vector_data(data1)
}


#' Retrieve metadata for specified Statistics Canada data vectors
#'
#' Allows for the retrieval of metadatadata for Statistics Canada data vectors
#'
#' @param vectors a vector of cansim vectors
#'
#' @return A tibble with metadata for selected vectors
#'
#' @export
get_cansim_vector_info <- function(vectors){
  vectors=gsub("^v","",vectors) # allow for leading "v" by conditionally stripping it
  url="https://www150.statcan.gc.ca/t1/wds/rest/getSeriesInfoFromVector"
  vectors_string=paste0("[",paste(purrr::map(as.character(vectors),function(x)paste0('{"vectorId":',x,'}')),collapse = ", "),"]")
  response <- post_with_timeout_retry(url, body=vectors_string)
  if (response$status_code!=200) {
    stop("Problem downloading data, status code ",response$status_code,"\n",httr::content(response))
  }
  data <- httr::content(response)
  data1 <- Filter(function(x)x$status=="SUCCESS",data)
  data2 <- Filter(function(x)x$status!="SUCCESS",data)
  if (length(data2)>0) {
    message(paste0("Failed to load metadata for ",length(data2)," tables "))
    data2 %>% purrr::map(function(x){
      message(x$object)
    })
  }

  extract_vector_metadata(data1)
}


