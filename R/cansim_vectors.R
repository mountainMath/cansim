MAX_PERIODS = 6000
STATCAN_TIMEZONE = "America/Toronto"
STATCAN_TIME_FORMAT="%Y-%m-%dT%H:%M"
STATCAN_TIME_FORMAT_S="%Y-%m-%dT%H:%M:%S"

extract_vector_data <- function(data1){
  vf=list("DECIMALS"="decimals",
          "VALUE"="value",
          "REF_DATE"="refPer",
          "REF_DATE_2"="refPer2",
          "releaseTime"="releaseTime",
          "SYMBOL"="symbolCode",
          "frequencyCode"="frequencyCode",
          "SCALAR_ID"="scalarFactorCode")
  ctn <- cleaned_ndm_table_number(as.character(data1[[1]]$object$productId))
  result <- purrr::map(data1,function(d){
    vdp <- d$object$vectorDataPoint
    if (length(vdp)==0) {return(NULL)}
    value_data = lapply(vf,function(f){
      x=purrr::map(vdp,function(cc)cc[[f]])
      x[sapply(x, is.null)] <- NA
      unlist(x)
    }) %>%
      tibble::as_tibble() %>%
      mutate(COORDINATE=d$object$coordinate,
             VECTOR=paste0("v",d$object$vectorId))
    value_data
  }) %>%
    dplyr::bind_rows()
  if ("REF_DATE_2" %in% names(result)) {
    ref_date_2 <- unique(result$REF_DATE_2) %>% unique
    if (length(ref_date_2)==1 && ref_date_2=="")
      result <- result %>% dplyr::select(-.data$REF_DATE_2)
  }
  result %>%
    mutate(cansimTableNumber=ctn)
}


metadata_for_coordinates <- function(cansimTableNumber,coordinates,language) {
  unique(coordinates) %>%
    purrr::map_dfr(\(coord)metadata_for_coordinate(cansimTableNumber,coord,language))
}

metadata_for_coordinate <- function(cansimTableNumber,coordinate,language) {
  cleaned_language <- cleaned_ndm_language(language)
  members <- get_cansim_cube_metadata(cansimTableNumber,type="members")
  coordinates <- coordinate %>% strsplit("\\.") %>% unlist()
  dimensions <- members %>% pull(.data$dimensionPositionId) %>% unique()
  result <- tibble::tibble(cansimTableNumber=cansimTableNumber, COORDINATE=coordinate)

  if (cleaned_language=="fra") {
    members <- members %>%
      select(.data$dimensionPositionId,.data$memberId,dimensionName=.data$dimensionNameFr,memberName=.data$memberNameFr)
  } else {
    members <- members %>%
      select(.data$dimensionPositionId,.data$memberId,dimensionName=.data$dimensionNameEn,memberName=.data$memberNameEn)
  }

  for (dimension in dimensions) {
    member_pos <- coordinates[dimension] %>% as.integer()
    dm<-members %>%
      filter(.data$dimensionPositionId==dimension) %>%
      mutate(n=n(),.by = .data$memberName) %>%
      mutate(nn=row_number(),.by=.data$memberName) %>%
      mutate(memberLevel=if_else(.data$n==1,.data$memberName,paste0(.data$memberName," (",.data$nn,")")))

    data_geography_column <- ifelse(cleaned_language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
    geography_columns <- case_when(cleaned_language=="eng" ~
                                     c("Geography","Geographic name","Geography of origin"),
                                   TRUE ~ c(paste0("G",intToUtf8(0x00E9),"ographie"),
                                            paste0("Nom g",intToUtf8(0x00E9),"ographique"),
                                            paste0("G",intToUtf8(0x00E9),"ographie d'origine")))

    m<-dm %>%
      filter(.data$memberId==member_pos)

    if (dimension==1 && (m$dimensionName %in% geography_columns)) {
      m$dimensionName <- data_geography_column
    }

    dn <- m$dimensionName

    result_new <- m %>%
      select(.data$dimensionName,.data$memberLevel) %>%
      tidyr::pivot_wider(names_from="dimensionName",values_from="memberLevel") %>%
      mutate(!!dn:=factor(!!as.name(dn),levels=dm$memberLevel))

    result <- result %>%
      bind_cols(result_new)
  }
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
      tibble::as_tibble()
    value_data
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(VECTOR=paste0("v",.data$VECTOR)) %>%
    dplyr::mutate(title=.data$title_en) %>%
    dplyr::mutate(table=cleaned_ndm_table_number(as.character(table)))

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
#' @param use_ref_date Optional, \code{TRUE} by default. When set to \code{TRUE}, uses \code{REF_DATE} of vector data to filter, otherwise it uses StatisticsCanada's \code{releaseDate} value for filtering the specified vectors.
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (defaults to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{TRUE}).
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "07")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#'
#' @return A tibble with data for vectors released between start and end time
#'
#' @examples
#' \dontrun{
#' get_cansim_vector("v41690973","2015-01-01")
#' }
#' @export
get_cansim_vector<-function(vectors, start_time = as.Date("1800-01-01"), end_time = Sys.time(), use_ref_date = TRUE,
                            language="english",
                            refresh = FALSE, timeout = 200,
                            factors = TRUE, default_month = "07", default_day = "01"){
  cleaned_language <- cleaned_ndm_language(language)
  start_time=as.Date(start_time)
  original_end_time=as.Date(end_time)
  vectors=gsub("^v","",vectors) # allow for leading "v" by conditionally stripping it
  #if (use_ref_date) end_time=as.Date(pmax(Sys.time(),end_time))+1 else end_time=original_end_time
  if (use_ref_date){
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorByReferencePeriodRange"
    vectors_string=paste0('vectorIds=',paste(lapply(as.character(vectors),function(x)paste0('"',x,'"')),collapse = ","),"")
    time_string=paste0('startRefPeriod=',strftime(start_time,"%Y-%m-%d",tz=STATCAN_TIMEZONE),
                       '&endReferencePeriod=',strftime(end_time,"%Y-%m-%d",tz=STATCAN_TIMEZONE),'')
    body=paste0(vectors_string,"&",time_string)
    # vectors_string=paste0('"vectorIds":[',paste(purrr::map(as.character(vectors),function(x)paste0('"',x,'"')),collapse = ", "),"]")
    # time_string=paste0('"startRefPeriod": "',strftime(start_time,"%Y-%m-%d",tz=STATCAN_TIMEZONE),
    #                    '","endReferencePeriod": "',strftime(end_time,"%Y-%m-%d",tz=STATCAN_TIMEZONE),'"')
    # body=paste0("{",vectors_string,",",time_string,"}")
  } else {
    url="https://www150.statcan.gc.ca/t1/wds/rest/getBulkVectorDataByRange"
    vectors_string=paste0('"vectorIds":[',paste(purrr::map(as.character(vectors),function(x)paste0('"',x,'"')),collapse = ", "),"]")
    time_string=paste0('"startDataPointReleaseDate": "',strftime(start_time,STATCAN_TIME_FORMAT,tz=STATCAN_TIMEZONE),
                       '","endDataPointReleaseDate": "',strftime(end_time,STATCAN_TIME_FORMAT,tz=STATCAN_TIMEZONE),'"')
    body=paste0("{",vectors_string,",",time_string,"}")
  }
  cache_path <- file.path(tempdir(), paste0("cansim_cache_",digest::digest(list(vectors_string,time_string), algo = "md5"), ".rda"))
  if (!file.exists(cache_path)||refresh) {
    message(paste0("Accessing CANSIM NDM vectors from Statistics Canada"))
    if (use_ref_date){
      response <- get_with_timeout_retry(paste0(url,"?",body),
                                         timeout = timeout)
    } else {
      response <- post_with_timeout_retry(url, body=body,
                                          timeout = timeout)
    }
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
      result <- extract_vector_data(data1)
    else
      result <- tibble::tibble()
    saveRDS(result,cache_path)
  } else {
    message(paste0("Reading CANSIM NDM vectors from temporary cache"))
    result <- readRDS(cache_path)
  }
  # if (use_ref_date) {
  #   result <- result %>%
  #     filter(as.Date(.data$REF_DATE)>=start_time,as.Date(.data$REF_DATE)<=original_end_time)
  # }

  attr(result,"language") <- cleaned_language


  metadata <- result %>%
    select(.data$cansimTableNumber,.data$COORDINATE) %>%
    unique() %>%
    group_by(.data$cansimTableNumber) %>%
    group_map(~ metadata_for_coordinates(cansimTableNumber=.y$cansimTableNumber,coordinates=.x$COORDINATE,language=cleaned_language)) %>%
    bind_rows()

    #metadata_for_coordinates(attr(result,"cansimTableNumber"),coordinates=unique(result$COORDINATE),language=language)

  if (nrow(result)>0) {
    result <-  result %>%
      rename_vectors(vectors)  %>%
      normalize_cansim_values(replacement_value = "val_norm", factors = factors,
                              default_month = default_month, default_day = default_day)
  }



  result %>% left_join(metadata,by=c("cansimTableNumber","COORDINATE"))
}

#' Retrieve data for specified Statistics Canada data vector(s) for last N periods
#'
#' Allows for the retrieval of data for specified vector series for the N most-recently released periods.
#'
#' @param vectors The list of vectors to retrieve
#' @param periods Numeric value for number of latest periods to retrieve data for
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (defaults to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{TRUE}).
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "07")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#'
#' @return A tibble with data for specified vector(s) for the last N periods
#'
#' @examples
#' \dontrun{
#' get_cansim_vector_for_latest_periods("v41690973",10)
#' }
#' @export
get_cansim_vector_for_latest_periods<-function(vectors, periods=1,
                                               language="english",
                                               refresh = FALSE, timeout = 200,
                                               factors = TRUE, default_month = "07", default_day = "01"){
  cleaned_language <- cleaned_ndm_language(language)
  if (periods*length(vectors)>MAX_PERIODS) {
    periods=pmin(periods,floor(as.numeric(MAX_PERIODS)/length(vectors)))
    warning(paste0("Can access at most ",MAX_PERIODS," data points, capping value to ",periods," periods per vector."))
  }
  vectors=gsub("^v","",vectors) # allow for leading "v" by conditionally stripping it
  url="https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods"
  vectors_string=paste0("[",paste(purrr::map(as.character(vectors),function(x)paste0('{"vectorId":',x,',"latestN":',periods,'}')),collapse = ", "),"]")
  cache_path <- file.path(tempdir(), paste0("cansim_cache_",digest::digest(vectors_string, algo = "md5"), ".rda"))
  if (refresh || !file.exists(cache_path)) {
    message(paste0("Accessing CANSIM NDM vectors from Statistics Canada"))
    response <- post_with_timeout_retry(url, body=vectors_string, timeout = timeout)
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
    saveRDS(result,cache_path)
  } else {
    message(paste0("Reading CANSIM NDM vectors from temporary cache"))
    result <- readRDS(cache_path)
  }

  metadata <- result %>%
    select(.data$cansimTableNumber,.data$COORDINATE) %>%
    unique() %>%
    group_by(.data$cansimTableNumber) %>%
    group_map(~ metadata_for_coordinates(cansimTableNumber=.y$cansimTableNumber,coordinates=.x$COORDINATE,language=cleaned_language)) %>%
    bind_rows()

  result %>%
    normalize_cansim_values(replacement_value = "val_norm", factors = factors,
                            default_month = default_month, default_day = default_day) %>%
    left_join(metadata,by=c("cansimTableNumber","COORDINATE"))
}


#' Retrieve data for specified Statistics Canada data product for last N periods for specific coordinates
#'
#' Allows for the retrieval of data for a Statistics Canada data table with specific coordinates for the N most-recently released periods. Caution: coordinates are concatenations of table member ID values and require familiarity with the TableMetadata data structure. Coordinates have a maximum of ten dimensions.
#'
#' @param cansimTableNumber Statistics Canada data table number
#' @param coordinate A string of table coordinates in the form \code{"1.1.1.36.1.0.0.0.0.0"}
#' @param periods Numeric value for number of latest periods to retrieve data for
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (defaults to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{TRUE}).
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "07")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#'
#' @return A tibble with data matching specified coordinate and period input arguments
#'
#' @examples
#' \dontrun{
#' get_cansim_data_for_table_coord_periods("35-10-0003",coordinate="1.12.0.0.0.0.0.0.0.0",periods=3)
#' }
#' @export
get_cansim_data_for_table_coord_periods<-function(cansimTableNumber, coordinate, periods=1,
                                                  language="english",
                                                  refresh = FALSE, timeout = 200,
                                                  factors=TRUE, default_month="07", default_day="01"){
  cleaned_language <- cleaned_ndm_language(language)
  table=naked_ndm_table_number(cansimTableNumber)
  url="https://www150.statcan.gc.ca/t1/wds/rest/getDataFromCubePidCoordAndLatestNPeriods"
  body_string=paste0('[{"productId":',table,',"coordinate":"',coordinate,'","latestN":',periods,'}]')
  cache_path <- file.path(tempdir(), paste0("cansim_cache_",digest::digest(body_string, algo = "md5"), ".rda"))
  if (refresh || !file.exists(cache_path)) {
    message(paste0("Accessing CANSIM NDM vectors from Statistics Canada"))
    response <- post_with_timeout_retry(url, body=body_string, timeout = timeout)
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
    saveRDS(data1,cache_path)
  } else {
    message(paste0("Reading CANSIM NDM vectors from temporary cache"))
    data1 <- readRDS(cache_path)
  }

  result <- extract_vector_data(data1)

  metadata <- result %>%
    select(.data$cansimTableNumber,.data$COORDINATE) %>%
    unique() %>%
    group_by(.data$cansimTableNumber) %>%
    group_map(~ metadata_for_coordinates(cansimTableNumber=.y$cansimTableNumber,coordinates=.x$COORDINATE,language=cleaned_language)) %>%
    bind_rows()

  result %>%
    normalize_cansim_values(replacement_value = "val_norm", factors = factors,
                            default_month = default_month, default_day = default_day) %>%
    left_join(metadata,by=c("cansimTableNumber","COORDINATE"))
}


#' Retrieve metadata for specified Statistics Canada data vectors
#'
#' Allows for the retrieval of metadatadata for Statistics Canada data vectors
#'
#' @param vectors a vector of cansim vectors
#'
#' @return A tibble with metadata for selected vectors
#'
#' @examples
#' \dontrun{
#' get_cansim_vector_info("v41690973")
#' }
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


