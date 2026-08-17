# StatCan reports a request naming only series that did not change with an HTTP 404 carrying a
# "Not data found" body. Nothing went wrong in that case, the answer is simply that there is nothing,
# so the two methods that name the series they ask about have that status come back as an empty result
# rather than as the failure the shared handling would otherwise make of it.
CHANGED_SERIES_NO_DATA_STATUS <- 404L

# Retrieve the list of data series StatCan changed today, as vectors together with the table and
# coordinate they belong to. StatCan serves this for the current day only and fills it during the
# daily update window that ends at 8:30am Eastern. Unlike the changed tables method there is no way to
# ask for an earlier day, StatCan answers a request naming a date with an HTTP 404.
#
# This is deliberately not exported yet. The method is frequently unable to answer at all: StatCan
# works out the whole response before sending any of it, and the series changing on a given day can
# number in the hundreds of thousands, so the request regularly outlives StatCan's own gateway and
# comes back as an HTTP 504 after some nine minutes of silence. Raising `timeout` does not help, the
# limit being exceeded is at StatCan's end. Until it is clear whether that is a fault worth working
# around or simply how the method behaves, exporting it would be handing users something that mostly
# does not work, and `get_cansim_changed_tables()` answers the coarser version of the same question in
# a fraction of a second. The plan is to watch it for a while and export it in a later release once
# there is a clear picture of what to expect.
#
# `timeout` is the number of seconds StatCan may go without sending data before the call is
# abandoned, and is set high because this method is silent while it works.
get_cansim_changed_series_list <- function(timeout=600){
  url <- "https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesList"

  # no `empty_status` here, unlike the two data methods below. Their 404 means that none of the series
  # asked about changed, but this method names no series, so nothing it could report as absent. A 404
  # from it is StatCan not serving the route, which is worth telling the caller about rather than
  # quietly passing off as a day on which nothing changed.
  response <- get_with_timeout_retry(url,timeout=timeout)
  if (is.null(response)) return(NULL)

  data <- statcan_response_json(response)
  if (length(data$object)==0) return(empty_changed_series_list())

  # this method has been seen to answer both in the wrapped shape the other list methods use, each
  # series sitting in the `object` of a record carrying its own status, and with the series themselves
  # as the entries of `object`. The presence of that per-record status is what tells the two apart,
  # and it has to be settled before asking for the successful records, because putting the unwrapped
  # shape through that check would report every series in it as a failure
  wrapped <- !is.null(data$object[[1]]$status)
  series <- if (wrapped) {
    purrr::map(successful_wds_records(data$object,"changed series"),\(x) x$object)
  } else {
    Filter(\(x) is.list(x) && !is.null(x$vectorId),data$object)
  }

  if (length(series)==0) return(empty_changed_series_list())
  changed_series_tibble(series)
}

empty_changed_series_list <- function(){
  tibble::tibble(VECTOR=character(0),cansimTableNumber=character(0),
                 COORDINATE=character(0),releaseTime=character(0))
}

changed_series_tibble <- function(series){
  field <- function(x,name,default=NA_character_){
    value <- x[[name]]
    if (length(value)!=1 || is.null(value)) return(default)
    as.character(value)
  }
  tibble::tibble(
    VECTOR=paste0("v",purrr::map_chr(series,field,"vectorId")),
    cansimTableNumber=purrr::map_chr(series,\(x){
      product_id <- field(x,"productId")
      if (is.na(product_id)) NA_character_ else cleaned_ndm_table_number(product_id)
    }),
    COORDINATE=gsub("(\\.0)+$","",purrr::map_chr(series,field,"coordinate")),
    releaseTime=purrr::map_chr(series,field,"releaseTime"))
}

# Shared body of the two methods that fetch the data of series that changed. They differ only in the
# endpoint they call and in how the caller names the series it is asking about.
changed_series_data <- function(url,bodies,vectors,language,timeout,factors,
                                default_month,default_day){
  cleaned_language <- cleaned_ndm_language(language)

  batches <- batch_items(bodies)
  batch_results <- vector("list", length(batches))
  for (batch_number in seq_along(batches)) {
    addition <- if (length(batches)>1) paste0(" (batch ",batch_number," of ",length(batches),")") else ""
    message(paste0("Accessing changed CANSIM NDM series from Statistics Canada",addition))

    body <- paste0("[",paste(batches[[batch_number]],collapse=", "),"]")
    response <- post_with_timeout_retry(url,body=body,timeout=timeout,
                                        empty_status=CHANGED_SERIES_NO_DATA_STATUS)
    if (is.null(response)) return(NULL)
    # none of the series in this batch changed, which says nothing about the other batches
    if (statcan_no_data(response)) next

    records <- successful_wds_records(statcan_response_json(response),"changed series data")
    if (length(records)>0) batch_results[[batch_number]] <- extract_vector_data(records)
  }

  result <- bind_rows(batch_results)
  # an empty answer here means nothing changed, which is an ordinary thing to report rather than the
  # sign of a problem the vector methods warn about
  finalize_vector_data(result,vectors,cleaned_language,factors,default_month,default_day,
                       warn_if_empty=FALSE)
}

#' Retrieve data for series that changed, by vector
#'
#' Retrieve the data points Statistics Canada changed for the given vectors. Series among the ones
#' asked about that did not change contribute no rows, and if none of them changed the result is an
#' empty table rather than an error. The StatCan API can only process 300 vectors at a time, if more
#' than 300 vectors are specified the function will batch the requests to the API.
#'
#' @param vectors The list of vectors to retrieve changed data for
#' @param language \code{"english"} (the default) or \code{"french"}. Short forms such as \code{"en"}, \code{"eng"}, \code{"fr"} or \code{"fra"} are accepted, as are the French names \code{"anglais"} and \code{"francais"}; case and accents are ignored
#' @param timeout (Optional) Number of seconds StatCan is allowed to go without sending data before the download is abandoned, to work around scenarios where StatCan servers drop the network connection. This does not limit how long a download may take overall, a transfer that keeps delivering data is left alone. StatCan prepares a whole response before sending any of it, which for large requests can take the better part of a minute, so values much below the default of 200 risk cutting off legitimate requests.
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{TRUE}).
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "07")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#'
#' @return A tibble with the changed data for the specified vector(s)
#'
#' Returns \code{NULL} if the data could not be retrieved because StatCan is unavailable.
#' @examples
#' \dontrun{
#' get_cansim_changed_series_data_for_vectors("v41690973")
#' }
#' @export
get_cansim_changed_series_data_for_vectors <- function(vectors, language="english", timeout=200,
                                                       factors=TRUE, default_month="07",
                                                       default_day="01"){
  naked_vectors <- gsub("^v","",vectors) # allow for leading "v" by conditionally stripping it
  bodies <- paste0('{"vectorId":',naked_vectors,'}')

  changed_series_data("https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesDataFromVector",
                      bodies,vectors,language,timeout,factors,default_month,default_day)
}

#' Retrieve data for series that changed, by table and coordinate
#'
#' Retrieve the data points Statistics Canada changed for the given coordinates of a table.
#' Coordinates among the ones asked about that did not change contribute no rows, and if none of them
#' changed the result is an empty table rather than an error. The StatCan API can only process 300
#' coordinates at a time, if more than 300 coordinates are specified the function will batch the
#' requests to the API.
#'
#' @param cansimTableNumber The table number the coordinates belong to
#' @param coordinates The coordinates to retrieve changed data for
#' @param language \code{"english"} (the default) or \code{"french"}. Short forms such as \code{"en"}, \code{"eng"}, \code{"fr"} or \code{"fra"} are accepted, as are the French names \code{"anglais"} and \code{"francais"}; case and accents are ignored
#' @param timeout (Optional) Number of seconds StatCan is allowed to go without sending data before the download is abandoned, to work around scenarios where StatCan servers drop the network connection. This does not limit how long a download may take overall, a transfer that keeps delivering data is left alone. StatCan prepares a whole response before sending any of it, which for large requests can take the better part of a minute, so values much below the default of 200 risk cutting off legitimate requests.
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{TRUE}).
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "07")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#'
#' @return A tibble with the changed data for the specified coordinates
#'
#' Returns \code{NULL} if the data could not be retrieved because StatCan is unavailable.
#' @examples
#' \dontrun{
#' get_cansim_changed_series_data_for_coordinates("34-10-0013","1.1")
#' }
#' @export
get_cansim_changed_series_data_for_coordinates <- function(cansimTableNumber, coordinates,
                                                           language="english", timeout=200,
                                                           factors=TRUE, default_month="07",
                                                           default_day="01"){
  validate_single_table_number(cansimTableNumber)
  product_id <- naked_ndm_table_number(cleaned_ndm_table_number(cansimTableNumber))
  # the API wants coordinates spelled out to all ten dimensions
  coordinates <- normalize_coordinates(coordinates)
  bodies <- paste0('{"productId":',product_id,', "coordinate":"',coordinates,'"}')

  changed_series_data("https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesDataFromCubePidCoord",
                      bodies,NULL,language,timeout,factors,default_month,default_day)
}
