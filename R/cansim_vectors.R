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
    value_data = lapply(vf,function(f){
      x=purrr::map(d$object$vectorDataPoint,function(cc)cc[[f]])
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
  if (length(ref_date_2)==1 && ref_date_2=="") result <- result %>% dplyr::select(-REF_DATE_2)
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

#' Retrieve data for a CANSIM vector released within a given time frame
#'
#' @param vectors The list of vectors to retrieve
#' @param start_time Starting date in \code{YYYY-MM-DD} format, applies to \code{REF_DATE} or \code{releaseTime}, depending on \code{use_ref_date} parameter
#' @param end_time Set an optional end time filter in \code{YYYY-MM-DD} format (defaults to current system time)
#' @param use_ref_date Optional, \code{TRUE} by default. When set to \code{TRUE}, uses \code{REF_DATE} of vector data to filter, otherwise it uses Statistics Canada's \code{releaseDate} value for filtering the specified vectors
#'
#' @return A tibble with data for vectors released between start and end time
#'
#' @export
get_cansim_vector<-function(vectors,start_time,end_time=Sys.Date(),use_ref_date=TRUE){
  start_time=as.Date(start_time)
  end_time=as.Date(end_time)
  if (!use_ref_date) {
    time_format="%Y-%m-%dT%H:%m"
    vectors=gsub("^v","",vectors) # allow for leading "v" by conditionally stripping it
    url="https://www150.statcan.gc.ca/t1/wds/rest/getBulkVectorDataByRange"
    vectors_string=paste0('"vectorIds":[',paste(purrr::map(as.character(vectors),function(x)paste0('"',x,'"')),collapse = ", "),"]")
    time_string=paste0('"startDataPointReleaseDate": "',strftime(start_time,time_format),
                       '","endDataPointReleaseDate": "',strftime(end_time,time_format),'"')
    response <- httr::POST(url,
                           body=paste0("{",vectors_string,",",time_string,"}"),
                           encode="json",
                           httr::add_headers("Content-Type"="application/json")
    )
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

    result <- extract_vector_data(data1) %>%
      rename_vectors(vectors)
  } else {
    result <- get_cansim_vector_for_latest_periods(vectors,periods=10000) %>%
      filter(as.Date(.data$REF_DATE)>=start_time,as.Date(.data$REF_DATE)<=end_time)
  }
  result
}

#' Retrieve data for specified CANSIM vector(s) for last N periods
#'
#' Allows for the retrieval of data for specified vector series for the N most-recently released periods.
#'
#' @param vectors The list of vectors to retrieve
#' @param periods Numeric value for number of latest periods to retrieve data for
#'
#' @return A tibble with data for specified vector(s) for the last N periods
#'
#' @export
get_cansim_vector_for_latest_periods<-function(vectors,periods=1){
  vectors=gsub("^v","",vectors) # allow for leading "v" by conditionally stripping it
  url="https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods"
  vectors_string=paste0("[",paste(purrr::map(as.character(vectors),function(x)paste0('{"vectorId":',x,',"latestN":',periods,'}')),collapse = ", "),"]")
  response <- httr::POST(url,
                         body=vectors_string,
                         encode="json",
                         httr::add_headers("Content-Type"="application/json")
  )
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

  extract_vector_data(data1) %>%
    rename_vectors(vectors)
}


#' Retrieve data for specified CANSIM table for last N periods for specific coordinates
#'
#' Allows for the retrieval of data for a CANSIM table with specific coordinates for the N most-recently released periods. Caution: coordinates are concatenations of table member ID values and require familiarity with the TableMetadata data structure. Coordinates have a maximum of ten dimensions.
#'
#' @param cansimTableNumber CANSIM table number
#' @param coordinate A string CANSIM coordinates in the form \code{"1.1.1.36.1.0.0.0.0.0"}
#' @param periods Numeric value for number of latest periods to retrieve data for
#'
#' @return A tibble with data matching specified coordinate and period input arguments
#'
#' @export
get_cansim_data_for_table_coord_periods<-function(cansimTableNumber,coordinate,periods=1){
  table=naked_ndm_table_number(cansimTableNumber)
  url="https://www150.statcan.gc.ca/t1/wds/rest/getDataFromCubePidCoordAndLatestNPeriods"
  body_string=paste0('[{"productId":',table,',"coordinate":"',coordinate,'","latestN":',periods,'}]')
  response <- httr::POST(url,
                         body=body_string,
                         encode="json",
                         httr::add_headers("Content-Type"="application/json")
  )
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

