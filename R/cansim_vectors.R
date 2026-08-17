MAX_PERIODS = 1000000L
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
  # ctn <- cleaned_ndm_table_number(as.character(data1[[1]]$object$productId))
  result <- purrr::map(data1,function(d){
    ctn <- cleaned_ndm_table_number(as.character(d$object$productId))
    vdp <- d$object$vectorDataPoint
    if (length(vdp)==0) {return(NULL)}
    value_data <- lapply(vf,function(f){
      x=purrr::map(vdp,function(cc)cc[[f]])
      # P8: Use vapply instead of sapply for type-safe, faster null check
      x[vapply(x, is.null, logical(1))] <- NA
      unlist(x)
    }) %>%
      tibble::as_tibble() %>%
      mutate(COORDINATE=d$object$coordinate,
             VECTOR=paste0("v",d$object$vectorId)) %>%
      mutate(cansimTableNumber=ctn) %>%
      mutate(VECTOR=na_if(.data$VECTOR,"v0"))

    value_data
  }) %>%
    dplyr::bind_rows()
  if ("REF_DATE_2" %in% names(result)) {
    ref_date_2 <- unique(result$REF_DATE_2) %>% unique
    if (length(ref_date_2)==1 && ref_date_2=="")
      result <- result %>% dplyr::select(-"REF_DATE_2")
  }
  result
}


# The member table of a dimension, and with it the disambiguation of duplicated member names and the
# resulting factor levels, is the same for every coordinate of a table. It is therefore built once per
# dimension and all coordinates are looked up against it in a single pass, rather than rebuilding it
# for every coordinate in turn.
metadata_for_coordinates <- function(cansimTableNumber,coordinates,language) {
  cleaned_language <- cleaned_ndm_language(language)
  coordinate_column <- ifelse(cleaned_language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))
  coordinates <- unique(coordinates)

  members <- get_cansim_cube_metadata(cansimTableNumber,type="members")
  result <- tibble::tibble(cansimTableNumber=cansimTableNumber, !!coordinate_column:=coordinates)

  # without cube metadata the data still stands on its own, it just carries no dimension names
  if (!is.null(members)) {
    dimensions <- members %>% pull(.data$dimensionPositionId) %>% unique()

    if (cleaned_language=="fra") {
      members <- members %>%
        select("dimensionPositionId","memberId",dimensionName="dimensionNameFr",memberName="memberNameFr")
    } else {
      members <- members %>%
        select("dimensionPositionId","memberId",dimensionName="dimensionNameEn",memberName="memberNameEn")
    }

    data_geography_column <- ifelse(cleaned_language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
    geography_columns <- geography_colum_names(cleaned_language)

    # split the coordinates once, the member ids of a dimension are then a column of this matrix
    coordinate_parts <- max(c(0,stringr::str_count(coordinates,"\\.")),na.rm=TRUE)+1
    coordinate_matrix <- stringr::str_split_fixed(coordinates,"\\.",coordinate_parts)

    dimension_columns <- lapply(dimensions, function(dimension) {
      dm <- members %>%
        filter(.data$dimensionPositionId==dimension) %>%
        mutate(n=n(),.by="memberName") %>%
        mutate(nn=row_number(),.by="memberName") %>%
        mutate(memberLevel=if_else(.data$n==1,.data$memberName,paste0(.data$memberName," (",.data$nn,")")))

      position <- as.integer(dimension)
      if (position<=ncol(coordinate_matrix)) {
        member_ids <- coordinate_matrix[,position]
        member_ids[is.na(member_ids) | member_ids==""] <- NA_character_
      } else { # coordinates that are shorter than the cube has dimensions carry nothing for this one
        member_ids <- rep(NA_character_,length(coordinates))
      }

      index <- match(member_ids,as.character(dm$memberId))
      # NA member ids come from coordinates that do not reach this dimension, only real ids are worth a warning
      for (missing_id in setdiff(unique(member_ids[is.na(index)]),NA_character_)) {
        warning("Could not find metadata for dimension ",unique(dm$dimensionName)," member ",missing_id,
                " in table ",cansimTableNumber)
      }
      # a dimension no coordinate resolves against adds a column of nothing but NA
      if (all(is.na(index))) return(NULL)

      dn <- dm$dimensionName[1]
      if (position==1 && (dn %in% geography_columns)) dn <- data_geography_column

      tibble::tibble(!!dn:=factor(dm$memberLevel[index],levels=dm$memberLevel))
    })

    dimension_columns <- Filter(Negate(is.null),dimension_columns)
    if (length(dimension_columns)>0) result <- bind_cols(c(list(result),dimension_columns))
  }

  result %>%
    add_uom_for_coordinates(cansimTableNumber,language)
}

# StatCan flags a single dimension of each cube as carrying the unit of measure, the unit itself
# varies by member of that dimension. The member id at the matching position of the coordinate
# therefore determines the unit, and the "uom" code set resolves that code to a name. This is done
# for all coordinates of a table at once so the cube metadata and the code set are only consulted once.
add_uom_for_coordinates <- function(data,cansimTableNumber,language) {
  cleaned_language <- cleaned_ndm_language(language)
  coordinate_column <- ifelse(cleaned_language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))
  uom_column <- ifelse(cleaned_language=="fra",paste0("UNIT",intToUtf8(0x00C9)," DE MESURE"),"UOM")
  uom_id_column <- ifelse(cleaned_language=="fra",paste0("IDENTIFICATEUR D'UNIT",intToUtf8(0x00C9)," DE MESURE"),"UOM_ID")

  # never overwrite a unit of measure that is already present, including a dimension that happens
  # to carry the same name as the unit columns
  if (nrow(data)==0 || !(coordinate_column %in% names(data)) ||
      uom_column %in% names(data) || uom_id_column %in% names(data)) return(data)

  members <- tryCatch(get_cansim_cube_metadata(cansimTableNumber,type="members"),
                      error=function(e) NULL, warning=function(w) NULL)
  if (is.null(members) || !all(c("hasUom","memberUomCode","dimensionPositionId","memberId") %in% names(members))) {
    return(data)
  }

  uom_members <- members %>% filter(.data$hasUom)
  uom_position <- unique(as.integer(uom_members$dimensionPositionId))
  if (nrow(uom_members)==0 || length(uom_position)!=1 || is.na(uom_position)) return(data)

  coordinates <- pull(data,coordinate_column)
  coordinate_parts <- max(c(0,stringr::str_count(coordinates,"\\.")),na.rm=TRUE)+1
  # coordinates that do not reach the unit dimension carry no unit information
  if (coordinate_parts<uom_position) return(data)
  member_ids <- stringr::str_split_fixed(coordinates,"\\.",coordinate_parts)[,uom_position]

  code_lookup <- rlang::set_names(as.character(uom_members$memberUomCode),
                                  as.character(uom_members$memberId))
  uom_ids <- unname(code_lookup[member_ids])
  # StatCan uses code 0 to mark members that have no unit of measure
  uom_ids[!is.na(uom_ids) & uom_ids=="0"] <- NA_character_

  if (all(is.na(uom_ids))) return(data)

  uom_names <- rep(NA_character_,length(uom_ids))
  uom_codes <- tryCatch(get_cansim_code_set("uom"), error=function(e) NULL)
  if (!is.null(uom_codes)) {
    uom_name_column <- ifelse(cleaned_language=="fra","memberUomFr","memberUomEn")
    name_lookup <- rlang::set_names(as.character(uom_codes[[uom_name_column]]),
                                    as.character(uom_codes$memberUomCode))
    uom_names <- unname(name_lookup[uom_ids])
  }

  data %>%
    mutate(!!uom_column:=uom_names,
           !!uom_id_column:=uom_ids)
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
    dplyr::mutate(table=cleaned_ndm_table_number(as.character(table))) %>%
    dplyr::mutate(COORDINATE=gsub("(\\.0)+$","",.data$COORDINATE)) # strip trailing zeros

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
#' Allows for the retrieval of data for specified vector series for a given time window.
#' Accessing data by vector allows for targeted extraction of time series. Discovering vectors of interest can be achieved
#' using the StatCan table web interface or using \code{get_cansim_table_template}
#' function to help pinpoint data series of interest, and then chaining the \code{add_cansim_vectors_to_template} function to add
#' cansim vector information to the template data.
#' The StatCan API can only process 300 coordinates at a time,
#' if more than 300 coordinates are specified the function will batch the requests to the API.
#'
#'
#' @param vectors The list of vectors to retrieve
#' @param start_time Starting date in \code{YYYY-MM-DD} format, applies to \code{REF_DATE} or \code{releaseTime}, depending on \code{use_ref_date} parameter
#' @param end_time Set an optional end time filter in \code{YYYY-MM-DD} format (defaults to current system time)
#' @param use_ref_date Optional, \code{TRUE} by default. When set to \code{TRUE}, uses \code{REF_DATE} of vector data to filter, otherwise it uses StatisticsCanada's \code{releaseDate} value for filtering the specified vectors.
#' @param language \code{"english"} (the default) or \code{"french"}. Short forms such as \code{"en"}, \code{"eng"}, \code{"fr"} or \code{"fra"} are accepted, as are the French names \code{"anglais"} and \code{"francais"}; case and accents are ignored
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{TRUE}).
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "07")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#'
#' @return A tibble with data for vectors released between start and end time
#'
#' Returns \code{NULL} if the data could not be retrieved because StatCan is unavailable.
#' @examples
#' \donttest{
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

  batches <- split(vectors, ceiling(seq_along(vectors)/300))
  # Keep batches separate so the accumulated rows are copied only once.
  batch_results <- vector("list", length(batches))
  for (batch_number in seq_along(batches)) {
    addition=""
    if (length(batches)>1) {
      addition=paste0(" (batch ",batch_number," of ",length(batches),")")
    }
    vecs <- batches[[batch_number]]
    if (use_ref_date){
      url = "https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorByReferencePeriodRange"
      vectors_string=paste0('vectorIds=',paste(lapply(as.character(vecs),function(x)paste0('"',x,'"')),collapse = ","),"")
      time_string=paste0('startRefPeriod=',strftime(start_time,"%Y-%m-%d",tz=STATCAN_TIMEZONE),
                         '&endReferencePeriod=',strftime(end_time,"%Y-%m-%d",tz=STATCAN_TIMEZONE),'')
      body=paste0(vectors_string,"&",time_string)
    } else {
      url="https://www150.statcan.gc.ca/t1/wds/rest/getBulkVectorDataByRange"
      vectors_string=paste0('"vectorIds":[',paste(purrr::map(as.character(vecs),function(x)paste0('"',x,'"')),collapse = ", "),"]")
      time_string=paste0('"startDataPointReleaseDate": "',strftime(start_time,STATCAN_TIME_FORMAT,tz=STATCAN_TIMEZONE),
                         '","endDataPointReleaseDate": "',strftime(end_time,STATCAN_TIME_FORMAT,tz=STATCAN_TIMEZONE),'"')
      body=paste0("{",vectors_string,",",time_string,"}")
    }
    cache_path <- file.path(tempdir(), paste0("cansim_cache_",digest::digest(list(vectors_string,time_string), algo = "md5"), ".rda"))
    if (!file.exists(cache_path)||refresh) {
      message(paste0("Accessing CANSIM NDM vectors from Statistics Canada",addition))
      if (use_ref_date){
        response <- get_with_timeout_retry(paste0(url,"?",body),
                                           timeout = timeout)
      } else {
        response <- post_with_timeout_retry(url, body=body,
                                            timeout = timeout)
      }
      if (is.null(response)) return(response)
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
        result_new <- extract_vector_data(data1)
      else
        result_new <- tibble::tibble()
      saveRDS(result_new,cache_path)
    } else {
      message(paste0("Reading CANSIM NDM vectors from temporary cache",addition))
      result_new <- readRDS(cache_path)
    }

    batch_results[[batch_number]] <- result_new
  }
  result <- bind_rows(batch_results)

  attr(result,"language") <- cleaned_language
  coordinate_column <- ifelse(cleaned_language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))

  if (cleaned_language=="fra") { # need to rename columns
    result <- result %>%
      rename_columns_for_language("eng",cleaned_language)
  }

  metadata <- result %>%
    select("cansimTableNumber",all_of(coordinate_column)) %>%
    unique() %>%
    group_by(.data$cansimTableNumber) %>%
    group_map(~ metadata_for_coordinates(cansimTableNumber=.y$cansimTableNumber,
                                         coordinates=.x[[coordinate_column]],
                                         language=cleaned_language)) %>%
    bind_rows()

    #metadata_for_coordinates(attr(result,"cansimTableNumber"),coordinates=unique(result$COORDINATE),language=language)


  if (nrow(result)>0) {
    result <-  result %>%
      left_join(metadata,by=c("cansimTableNumber",coordinate_column)) %>%
      rename_vectors(vectors)  %>%
      normalize_cansim_values(replacement_value = "val_norm", factors = factors,
                              default_month = default_month, default_day = default_day, internal=TRUE)
  }

  result %>%
    mutate(across(all_of(coordinate_column),~gsub("(\\.0)+$","",.x)))
}

#' Retrieve data for specified Statistics Canada data vector(s) for last N periods
#'
#' Allows for the retrieval of data for specified vector series for the N most-recently released periods.
#' Accessing data by vector allows for targeted extraction of time series. Discovering vectors of interest can be achieved
#' using the StatCan table web interface or using \code{get_cansim_table_template}
#' function to help pinpoint data series of interest, and then chaining the \code{add_cansim_vectors_to_template} function to add
#' cansim vector information to the template data.
#' The StatCan API can only process 300 coordinates at a time,
#' if more than 300 coordinates are specified the function will batch the requests to the API.
#'
#' @param vectors The list of vectors to retrieve
#' @param periods Numeric value for number of latest periods to retrieve data for, but default all data is retrieved.
#' @param language \code{"english"} (the default) or \code{"french"}. Short forms such as \code{"en"}, \code{"eng"}, \code{"fr"} or \code{"fra"} are accepted, as are the French names \code{"anglais"} and \code{"francais"}; case and accents are ignored
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{TRUE}).
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "07")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#'
#' @return A tibble with data for specified vector(s) for the last N periods
#'
#' Returns \code{NULL} if the data could not be retrieved because StatCan is unavailable.
#' @examples
#' \donttest{
#' get_cansim_vector_for_latest_periods("v41690973",10)
#' }
#' @export
get_cansim_vector_for_latest_periods<-function(vectors, periods=NULL,
                                               language="english",
                                               refresh = FALSE, timeout = 200,
                                               factors = TRUE, default_month = "07", default_day = "01"){
  if (is.null(periods) || is.na(periods)) {periods <- MAX_PERIODS}
  periods <- as.integer(periods)
  cleaned_language <- cleaned_ndm_language(language)

  vectors=gsub("^v","",vectors) # allow for leading "v" by conditionally stripping it
  url="https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods"

  batches <- split(vectors, ceiling(seq_along(vectors)/300))
  # Keep batches separate so the accumulated rows are copied only once.
  batch_results <- vector("list", length(batches))
  for (batch_number in seq_along(batches)) {
    addition=""
    if (length(batches)>1) {
      addition=paste0(" (batch ",batch_number," of ",length(batches),")")
    }
    vecs <- batches[[batch_number]]

    vectors_string=paste0("[",paste(purrr::map(as.character(vecs),function(x)paste0('{"vectorId":',x,',"latestN":',periods,'}')),collapse = ", "),"]")
    cache_path <- file.path(tempdir(), paste0("cansim_cache_",digest::digest(vectors_string, algo = "md5"), ".rda"))
    if (refresh || !file.exists(cache_path)) {
      message(paste0("Accessing CANSIM NDM vectors from Statistics Canada",addition))
      response <- post_with_timeout_retry(url, body=vectors_string, timeout = timeout)
      if (is.null(response)) return(response)
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
        result_new <- extract_vector_data(data1)
      else
        result_new <- tibble::tibble()
      saveRDS(result_new,cache_path)
    } else {
      message(paste0("Reading CANSIM NDM vectors from temporary cache",addition))
      result_new <- readRDS(cache_path)
    }
    batch_results[[batch_number]] <- result_new

  }
  result <- bind_rows(batch_results)

  attr(result,"language") <- cleaned_language
  coordinate_column <- ifelse(cleaned_language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))

  if (cleaned_language=="fra") { # need to rename columns
    result <- result %>%
      rename_columns_for_language("eng",cleaned_language)
  }

  metadata <- result %>%
    select("cansimTableNumber",all_of(coordinate_column)) %>%
    unique() %>%
    group_by(.data$cansimTableNumber) %>%
    group_map(~ metadata_for_coordinates(cansimTableNumber=.y$cansimTableNumber,
                                         coordinates=.x[[coordinate_column]],
                                         language=cleaned_language)) %>%
    bind_rows()

  if (nrow(result)>0) {
    result <-  result %>%
      left_join(metadata,by=c("cansimTableNumber",coordinate_column)) %>%
      rename_vectors(vectors)  %>%
      normalize_cansim_values(replacement_value = "val_norm", factors = factors,
                              default_month = default_month, default_day = default_day, internal=TRUE)
  }

  result %>%
    mutate(across(all_of(coordinate_column),~gsub("(\\.0)+$","",.x)))
}


#' Retrieve data for specified Statistics Canada data product for last N periods for specific coordinates
#'
#' Allows for the retrieval of data for a Statistics Canada data table with specific table and coordinates.
#' This allows partial targeted download of tables and can be effectively combined with the \code{get_cansim_table_template}
#' function to help pinpoint data series of interest.
#' The StatCan API can only process 300 coordinates at a time,
#' if more than 300 coordinates are specified the function will batch the requests to the API.
#'
#' @param tableCoordinates Either a list with vectors of coordinates by table number, or a
#' (filtered) data frame as returned by \code{get_cansim_table_template}.
#' @param periods Optional numeric value for number of latest periods to retrieve data for, default is \code{NULL} in which case data for all periods is downloaded.
#' Alternatively this can be specified by
#' coordinate if tableCoordinates is a data frame, this argument will be ignored if that data frame as a "periods" column.
#' @param language \code{"english"} (the default) or \code{"french"}. Short forms such as \code{"en"}, \code{"eng"}, \code{"fr"} or \code{"fra"} are accepted, as are the French names \code{"anglais"} and \code{"francais"}; case and accents are ignored
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{TRUE}).
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "07")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#'
#' @return A tibble with data matching specified coordinate and period input arguments
#'
#' Returns \code{NULL} if the data could not be retrieved because StatCan is unavailable.
#' @examples
#' \donttest{
#' get_cansim_data_for_table_coord_periods(list("35-10-0003"=c("1.1","1.12")),periods=3)
#' }
#' @export
get_cansim_data_for_table_coord_periods<-function(tableCoordinates, periods=NULL,
                                                  language="english",
                                                  refresh = FALSE, timeout = 200,
                                                  factors=TRUE, default_month="07", default_day="01"){
  CENSUS_TABLE_STARTING_STRING <- "9810"
  if (is.null(periods) || is.na(periods)) {periods <- MAX_PERIODS}
  periods <- as.integer(periods)

  # pad coordinate if needed
  if ("list" %in% class(tableCoordinates)) {
    tableCoordinates <- tibble::enframe(tableCoordinates) %>%
      setNames(c("cansimTableNumber","COORDINATE")) %>%
      tidyr::unnest_longer("COORDINATE")
  }
  tableCoordinates <- tableCoordinates %>%
    mutate(cansimTableNumber=naked_ndm_table_number(.data$cansimTableNumber)) %>%
    mutate(COORDINATE = normalize_coordinates(.data$COORDINATE)) %>%
    mutate(is_census_table=substr(.data$cansimTableNumber,1,4)==CENSUS_TABLE_STARTING_STRING) %>%
    mutate(batch=paste0(.data$is_census_table,"_",.data$cansimTableNumber)) %>%
    mutate(n=row_number(),.by="batch") %>%
    mutate(b=(n-1) %% 300 == 0) %>%
    mutate(batch=paste0(.data$batch,"_",cumsum(.data$b)),.by="batch")

  if (!("periods") %in% names(tableCoordinates)) {
    tableCoordinates <- tableCoordinates %>%
      mutate(periods = !!periods)
  } else {
    tableCoordinates <- tableCoordinates %>%
      mutate(periods = coalesce(.data$periods, MAX_PERIODS))
  }

  tableCoordinates <- tableCoordinates %>%
    select(any_of(c("cansimTableNumber","COORDINATE","periods","is_census_table","batch"))) %>%
    arrange(.data$is_census_table, .data$cansimTableNumber, .data$COORDINATE)

  cleaned_language <- cleaned_ndm_language(language)
  url="https://www150.statcan.gc.ca/t1/wds/rest/getDataFromCubePidCoordAndLatestNPeriods"
  result <- NULL
  batches <- unique(tableCoordinates$batch)
  batch_number=0
  failed_coordinates <- NULL
  for (batch in batches) {
    batch_number <- batch_number + 1
    working_data <- tableCoordinates %>%
      filter(.data$batch==!!batch)  %>%
      mutate(body_string=paste0('{"productId":',.data$cansimTableNumber,
                                ',"coordinate":"',.data$COORDINATE,
                                '","latestN":',.data$periods,'}'))
    body_string=paste0(working_data$body_string, collapse = ", ") %>%
      paste0("[",.,"]")
    cache_path <- file.path(tempdir(), paste0("cansim_cache_",digest::digest(body_string, algo = "md5"), ".rda"))
    if (refresh || !file.exists(cache_path)) {
      addition=""
      if (length(batches)>1) {
        addition=paste0(" (batch ",batch_number," of ",length(batches),")")
      }
      message(paste0("Accessing CANSIM NDM coordinates from Statistics Canada",addition))
      response <- post_with_timeout_retry(url, body=body_string, timeout = timeout)
      if (is.null(response)) {return(response)}
      data <- httr::content(response)
      data1 <- Filter(function(x)x$status=="SUCCESS",data)
      data2 <- Filter(function(x)x$status!="SUCCESS",data)
      new_failed_coordinates <- NULL
      if (length(data2)>0) {
        # message(paste0("Failed to load for ",length(data2)," coordinates "))
        new_failed_coordinates <- data2 %>% purrr::map(function(x){x$object$coordinate}) %>% unlist()
        new_failed_coordinates <- tibble::tibble(cansimTableNumber=unique(working_data$cansimTableNumber),
                                                 COORDINATE=new_failed_coordinates)

        # if (substr(batch,7,10) == CENSUS_TABLE_STARTING_STRING) {
        #   warning(paste0("Table ",.data$cansimTableNumber,
        #                  " is a census data table that does not coform to usual NDM standards, this likely means that the data for the queried data point is zero."))
        # }
        attr(data1,"failed_coordinates") <- new_failed_coordinates
      }
      saveRDS(data1,cache_path)
    } else {
      addition=""
      if (length(batches)>1) {
        addition=paste0(" (batch ",batch_number," of ",length(batches),")")
      }
      message(paste0("Reading CANSIM NDM coordinates from temporary cache",addition))
      data1 <- readRDS(cache_path)
      new_failed_coordinates <- attr(data1,"failed_coordinates")
    }

    failed_coordinates <- bind_rows(failed_coordinates, new_failed_coordinates)
    new_result <- extract_vector_data(data1)

    if (nrow(new_result)==0) {
      new_result <- NULL
    } else {
      coordinate_column <- ifelse(cleaned_language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))

      if (cleaned_language=="fra") { # need to rename columns
        new_result <- new_result %>%
          rename_columns_for_language("eng",cleaned_language)
      }

      metadata <- new_result %>%
        select("cansimTableNumber",all_of(coordinate_column)) %>%
        unique() %>%
        group_by(.data$cansimTableNumber) %>%
        group_map(~ metadata_for_coordinates(cansimTableNumber=.y$cansimTableNumber,
                                             coordinates=.x[[coordinate_column]],
                                             language=cleaned_language)) %>%
        bind_rows()

      new_result <- new_result %>%
        left_join(metadata,by=c("cansimTableNumber",coordinate_column)) %>%
        normalize_cansim_values(replacement_value = "val_norm", factors = factors,
                                default_month = default_month, default_day = default_day, internal=TRUE) %>%
        mutate(across(all_of(coordinate_column),~gsub("(\\.0)+$","",.x)))
    }
    result <- bind_rows(result,new_result)
  }

  attr(result,"language") <- cleaned_language
  if (!is.null(failed_coordinates) && nrow(failed_coordinates) > 0) {
    regular_fails <- failed_coordinates %>%
      filter(substr(.data$cansimTableNumber,1,4) != CENSUS_TABLE_STARTING_STRING)
    census_fails <- failed_coordinates %>%
      filter(substr(.data$cansimTableNumber,1,4) == CENSUS_TABLE_STARTING_STRING)

    if (nrow(regular_fails)>0) {
    attr(result,"failed_coordinates") <- regular_fails
    message(paste0("Could not access data for ",nrow(regular_fails)," coordinates, see attr(<result>, 'failed_coordinates') for details.",
                   "\n","This occurs when combinations of coordinates aren't available in the data table, values can safely be assumed to be NA."))
    }
    if (nrow(census_fails) > 0) {
      attr(result,"failed_coordinates_census") <- census_fails
      start_string <- "There were"
      if (nrow(regular_fails) > 0) {
        start_string <- "Additionally there were"
      }
      message(paste0(start_string," ",nrow(census_fails)," coordinates from Census data tables that were not available, this generally means that",
                     "\n","the corresponding data values are zero. This is due to the fact that Census data tables do not conform to usual NDM standards.",
                     "\n","Hopefully this will get fixed on StatCan servers in the future so data can be accessed more reliably.","\n",
                     "See attr(<result>, 'failed_coordinates_census') for details on which coordinates had failed data download and likely zero value."))
    }
  }
  result
}


#' Retrieve metadata for specified Statistics Canada data vectors
#'
#' Allows for the retrieval of metadata for Statistics Canada data vectors
#'
#' @param vectors a vector of cansim vectors
#'
#' @return A tibble with metadata for selected vectors
#'
#' Returns \code{NULL} if the data could not be retrieved because StatCan is unavailable.
#' @examples
#' \donttest{
#' get_cansim_vector_info("v41690973")
#' }
#' @export
get_cansim_vector_info <- function(vectors){
  vectors=gsub("^v","",vectors) # allow for leading "v" by conditionally stripping it
  url="https://www150.statcan.gc.ca/t1/wds/rest/getSeriesInfoFromVector"
  vectors_string=paste0("[",paste(purrr::map(as.character(vectors),function(x)paste0('{"vectorId":',x,'}')),collapse = ", "),"]")
  response <- post_with_timeout_retry(url, body=vectors_string)
  if (is.null(response)){return(response)}
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
