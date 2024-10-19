cleaned_ndm_table_number <- function(cansimTableNumber){
  if (is.numeric(cansimTableNumber)) {
    warning(paste0("The cansim table number ",cansimTableNumber," used in this query is numeric,\n",
                   "it is safer to encode table numbers as character strings."))
    cansimTableNumber <- as.character(cansimTableNumber)
  }
  n<-gsub("-","",cansimTableNumber) %>%
    purrr::map(function(t){
      if (nchar(t)<=7) {
        tt<-cansim_old_to_new(t)
        message("Legacy table number ",cansimTableNumber,", converting to NDM ",tt)
        t=gsub("-","",tt)
      }
      tn <- paste0(substr(t,1,2),"-",substr(t,3,4),"-",substr(t,5,8))

      if (nchar(t)==10) {
        end_string <- substr(t,9,10)
        if (end_string !="01") {
          warning(paste0("The {cansim} package can only retrieve 'base' tables, those ending in '-01'.\n",
                         "To get derived tables like ",tn,"-",end_string," you will have to perform the\n",
                         "necessary data manipulations manually."))
        }
      }
      tn
    }) %>% unlist

  n
}

naked_ndm_table_number <- function(cansimTableNumber){
  as.character(gsub("-","",cleaned_ndm_table_number(cansimTableNumber)))
}

cleaned_ndm_language <- function(language){
  ifelse(tolower(language) %in% c("english","eng","en"),"eng",ifelse(tolower(language) %in% c("fra","french","fr"),"fra",NA))
}

table_base_path <- function(cansimTableNumber) {
  file.path(tempdir(),paste0("cansim_",naked_ndm_table_number(cansimTableNumber)))
}

file_path_for_table_language <- function(cansimTableNumber, language){
  language <- cleaned_ndm_language(language)
  if (is.na(language)) stop(paste0("Unkown Lanaguage ",language))
  base_table <- naked_ndm_table_number(cansimTableNumber)
  file.path(paste0(base_table,"-",language))
}

base_path_for_table_language <- function(cansimTableNumber, language,base_dir = NULL){
  if (is.null(base_dir)) {
    base_dir <- table_base_path(cansimTableNumber)
  }
  if (!dir.exists(base_dir)) {
    dir.create(base_dir)
  }
  file.path(base_dir,file_path_for_table_language(cansimTableNumber,language))
}

response_status_code_translation <- list(
  "0"="Success",
  "1"="Invalid date",
  "2"="Invalid cube and series combination",
  "3"="Request failed",
  "4"="Vector is invalid",
  "5"="Cube product id is invalid",
  "6"="Cube is currently being published. Please try again later.",
  "7"="Cube is currently unavailable. For more information, contact us (toll-free 1-800-263-1136; 514-283-8300; STATCAN.infostats-infostats.STATCAN@canada.ca).",
  "8"="Invalid number of reference periods"
)

response_error_translation <- list(
  "503"="StatCan website is currently unavailable"
)

get_with_timeout_retry <- function(url,timeout=200,retry=3,path=NA,warn_only=FALSE){
  if (!is.na(path)) {
    response <- purrr::safely(httr::GET)(url,encode="json",
                                         httr::add_headers("Content-Type"="application/json"),
                                         httr::timeout(timeout),
                                         httr::write_disk(path,overwrite = TRUE))
  } else {
    response <- purrr::safely(httr::GET)(url,
                                         encode="json",
                                         httr::add_headers("Content-Type"="application/json"),
                                         httr::timeout(timeout))
  }
  if (!is.null(response$error)){
    if (retry>0) {
      message("Got timeout from StatCan, trying again")
      response <- get_with_timeout_retry(url,timeout=timeout,retry=retry-1,path=path)
    } else {
      message("Got timeout from StatCan, giving up")
    }
  } else if (response$result$status_code %in% names(response_error_translation)){
    if (warn_only) {
      warning(sprintf("%s\nReturned status code %s",response_error_translation[[as.character(response$result$status_code)]], response$result$status_code),call.=FALSE)
      response=response$result
    } else {
      stop(sprintf("%s\nReturned status code %s",response_error_translation[[as.character(response$result$status_code)]], response$result$status_code),call.=FALSE)
    }
  } else if (response$result$status_code != 200){
    if (warn_only) {
      warning(sprintf("Problem downloading data, returned status code %s.",response$result$status_code),call.=FALSE)
      response=response$result
    } else {
      stop(sprintf("Problem downloading data, returned status code %s.",response$result$status_code),call.=FALSE)
    }
  } else {
    response=response$result
  }

  if (is.null(response) && retry == 0) {
    if (warn_only) {
      warning(sprintf("Problem downloading data, multiple timeouts.\nPlease check your network connection. If your connections is fine then StatCan servers might be down."),call.=FALSE)
      response=response$result
    } else {
      stop(sprintf("Problem downloading data, multiple timeouts.\nPlease check your network connection. If your connections is fine then StatCan servers might be down."),call.=FALSE)
    }
  }
  response
}

post_with_timeout_retry <- function(url,body,timeout=200,retry=3,warn_only=FALSE){
  response <- purrr::safely(httr::POST)(url,
                                        body=body,
                                        encode="json",
                                        httr::add_headers("Content-Type"="application/json"),
                                        httr::timeout(timeout))
  if (!is.null(response$error)){
    if (retry>0) {
      message("Got timeout from StatCan, trying again")
      response <- post_with_timeout_retry(url,body=body,timeout=timeout,retry=retry-1)
    } else {
      message("Got timeout from StatCan, giving up")
      response=response$result
    }
  } else {
    response=response$result
  }

  if (is.null(response) && retry == 0) {
    if (warn_only) {
      warning(sprintf("Problem downloading data, multiple timeouts.\nPlease check your network connection. If your connections is fine then StatCan servers might be down."),call.=FALSE)
      response=NULL
    } else {
      stop(sprintf("Problem downloading data, multiple timeouts.\nPlease check your network connection. If your connections is fine then StatCan servers might be down."),call.=FALSE)
    }
  }
  response
}



short_prov.en <- c(
  "British Columbia"="BC",
  "Alberta"="AB",
  "Saskatchewan"="SK",
  "Manitoba"="MB",
  "Ontario"="ON",
  "Quebec"="QC",
  "New Brunswick"="NB",
  "Prince Edward Island"="PE",
  "Nova Scotia"="NS",
  "Newfoundland and Labrador"="NL",
  "Yukon"="YT",
  "Northwest Territories"="NT",
  "Nunavut"="NU",
  "Northwest Territories including Nunavut"="NTNU",
  "Canada"="CAN"
)

# short_prov.fr <- c(
#   "Colombie-Britannique"="BC",
#   "Alberta"="AB",
#   "Saskatchewan"="SK",
#   "Manitoba"="MB",
#   "Ontario"="ON",
#   "Qu\U00E9bec"="QC",
#   "Nouveau-Brunswick"="NB",
#   "\u00CEle-du-Prince-\U00C9douard"="PE",
#   "Nouvelle-\U00C9cosse"="NS",
#   "Terre-Neuve-et-Labrador"="NL",
#   "Yukon"="YT",
#   "Territoires du Nord-Ouest"="NT",
#   "Nunavut"="NU",
#   "Territoires du Nord-Ouest incluant Nunavut"="NTNU",
#   "Canada"="CAN"
# )

short_prov.fr <- purrr::set_names(c(
  "BC",
  "AB",
  "SK",
  "MB",
  "ON",
  "QC",
  "NB",
  "PE",
  "NS",
  "NL",
  "YT",
  "NT",
  "NU",
  "NTNU",
  "CAN"
),c(
  "Colombie-Britannique",
  "Alberta",
  "Saskatchewan",
  "Manitoba",
  "Ontario",
  paste0("Qu",intToUtf8(0x00E9),"bec"),
  "Nouveau-Brunswick",
  paste0(intToUtf8(0x00CE),"le-du-Prince-",intToUtf8(0x00C9),"douard"),
  paste0("Nouvelle-",intToUtf8(0x00C9),"cosse"),
  "Terre-Neuve-et-Labrador",
  "Yukon",
  "Territoires du Nord-Ouest",
  "Nunavut",
  "Territoires du Nord-Ouest incluant Nunavut",
  "Canada"
))



#' Add provincial abbreviations as factor
#' @export
#' @param data A tibble as returned by \code{get_cansim} with provincial level data
#' @return The input tibble with additional factor GEO.abb that contains language-specific provincial abbreviations
#'
#' @examples
#' \dontrun{
#' df <- get_cansim("17-10-0005")
#' df <- add_provincial_abbreviations(df)
#' }
#'
add_provincial_abbreviations <- function(data){
  cleaned_language <- ifelse("VALEUR" %in% names(data),"fra","eng")
  if (cleaned_language=="eng") {
    data_geography_column <- "GEO"
    short_prov <- short_prov.en
  } else {
    data_geography_column <- paste0("G",intToUtf8(0x00C9),"O")
    short_prov <- short_prov.fr
  }

  short_prov_t <- short_prov %>%
    tibble::enframe() %>%
    setNames(c(data_geography_column,"GEO.abb")) |>
    mutate(GEO.abb=factor(.data$GEO.abb,levels = c("CAN","BC","AB","SK","MB","ON","QC","NB","PE","NS","NL","YT","NT","NU","NTNU")))

  data <- data %>%
    left_join(short_prov_t,by=data_geography_column)
    # mutate(GEO.abb=factor(as.character(short_prov[!!as.name(data_geography_column)]),
    #                       levels=c("CAN","BC","AB","SK","MB","ON","QC","NB","PE","NS","NL","YT","NT","NU","NTNU")))
}


#' Get NDM code sets
#'
#' Useful to get a list of surveys  or subjects and used internally
#' @export
#' @param code_set the code set to retrieve.
#' @param refresh Default is \code{FALSE}, repeated calls during the same session will hit the cached data.
#' To refresh the code list during a running R session set to \code{TRUE}
#'
#' @return A tibble with english and french labels for the given code set
#'
#' @examples
#' \dontrun{
#' get_cansim_code_set("survey")
#' }
get_cansim_code_set <- function(code_set=c("scalar", "frequency", "symbol", "status", "uom", "survey",  "subject", "wdsResponseStatus"),
                                refresh=FALSE){
  code_sets <- c("scalar", "frequency", "symbol", "status", "uom", "survey",  "subject", "wdsResponseStatus")
  if (length(code_set)!=1 | !(code_set %in% code_sets)) {
    stop(paste0("Invalid code set, code_set must be one of ",paste0(code_sets,collapse=", ")))
  }
  path=file.path(tempdir(),"cansim_code_sets.Rmd")
  if (refresh | !file.exists(path)) {
    url='https://www150.statcan.gc.ca/t1/wds/rest/getCodeSets'
    r<-get_with_timeout_retry(url)
    if (r$status_code==200) {
      content <- httr::content(r)
      saveRDS(content,path)
    } else {
      warning("Problem downloading code sets.")
      stop(httr::content(r))
    }
  } else {
    content <- readRDS(path)
  }
  m<-do.call(rbind, content$object[[code_set]])
  m[m=="NULL"] <- NA
  as_tibble(m) %>%
    mutate_all(unlist)
}

# transforms the value column to nomeric. If table is in semi-wide form it converts the wide for dimension
# to long form and creates and modifies the COORDINATE column as needed.
transform_value_column <- function(data,value_column){
  language <- attr(data,"language")

  symbols <- which(grepl("^Symbol( \\d+)*$",names(data)))
  if (!(value_column %in% names(data)) & length(symbols)>1) {
    #message("\nTransforming to long form.")
    dimension_grep_string <- paste0("^.+ \\(",length(symbols),"[A-Za-z]*\\):.+\\[\\d+\\]$")
    dimensions <- which(grepl(dimension_grep_string,names(data)))
    if (sum(symbols!=dimensions+1)>0) {
      warning("Unable to identify dimensions")
    } else {
      count_type <- stringr::str_match(names(data)[dimensions][1],paste0("(\\(",length(symbols),"[A-Za-z]*\\))"))[1,2]
      dimension_members <- gsub(paste0("^.+ \\(",length(symbols),"[A-Za-z]*\\): *"),"",names(data)[dimensions]) %>%
        gsub(" *\\[\\d+\\]$","",.)
      member_ids <- stringr::str_extract(names(data)[dimensions],"\\[\\d+\\]$") %>% gsub("\\[|\\]","",.)
      dimension_name <- gsub(paste0(" \\(",length(symbols),"[A-Za-z]*\\):.+\\[\\d+\\]"),"",names(data)[dimensions]) %>%
        unique() %>% paste0(.," ",count_type)

      if (length(dimension_name)>1) {
        warning("Unable to identify dimension name")
      } else {
        symbol_string <- "Symbol"
        renames <- c(setNames(names(data)[dimensions],paste0(member_ids," --- ",value_column)),
                     setNames(names(data)[symbols],paste0(member_ids," --- ",symbol_string)))

        member_names <- dplyr::tibble(!!as.name(paste0("Member ID: ",dimension_name)):=member_ids,
                                      !!as.name(dimension_name):=dimension_members)

        data <- data %>%
          dplyr::rename(!!!renames) %>%
          tidyr::pivot_longer(matches(" --- "), names_pattern="^(.+) --- (.+)$",
                              names_to=c(paste0("Member ID: ",dimension_name),".value")) %>%
          dplyr::left_join(member_names,by=paste0("Member ID: ",dimension_name))

        coordinate_column <- ifelse(language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))

        if (coordinate_column %in% names(data)) {
          data <- data %>%
            dplyr::mutate(!!coordinate_column := paste0(!!as.name(coordinate_column),".",
                                                     !!as.name(paste0("Member ID: ",dimension_name))))
        }

        data <- data %>%
          dplyr::select(-dplyr::all_of(paste0("Member ID: ",dimension_name)))
      }
    }
  }

  if (value_column %in% names(data)) {
    data <- data %>%
      dplyr::mutate(!!value_column:=as.numeric(.data[[value_column]]))
  } else {
    warning("Unkown table type")
  }
  data
}

# copied from unexported utils:::format.object_size
format_file_size <- function (x, units = "b", standard = "auto", digits = 1L, ...)
{
  known_bases <- c(legacy = 1024, IEC = 1024, SI = 1000)
  known_units <- list(SI = c("B", "kB", "MB", "GB", "TB", "PB",
                             "EB", "ZB", "YB"),
                      IEC = c("B", "KiB", "MiB", "GiB",
                              "TiB", "PiB", "EiB", "ZiB", "YiB"),
                      legacy = c("b", "Kb",
                                 "Mb", "Gb", "Tb", "Pb"),
                      LEGACY = c("B", "KB", "MB",
                                                                                                                                           "GB", "TB", "PB"))
  units <- match.arg(units, c("auto", unique(unlist(known_units),
                                             use.names = FALSE)))
  standard <- match.arg(standard, c("auto", names(known_bases)))
  if (is.null(digits))
    digits <- 1L
  if (standard == "auto") {
    standard <- "legacy"
    if (units != "auto") {
      if (endsWith(units, "iB"))
        standard <- "IEC"
      else if (endsWith(units, "b"))
        standard <- "legacy"
      else if (units == "kB")
        stop("For SI units, specify 'standard = \"SI\"'")
    }
  }
  base <- known_bases[[standard]]
  units_map <- known_units[[standard]]
  if (units == "auto") {
    power <- if (x <= 0)
      0L
    else min(as.integer(log(x, base = base)), length(units_map) -
               1L)
  }
  else {
    power <- match(toupper(units), toupper(units_map)) -
      1L
    if (is.na(power))
      stop(gettextf("Unit \"%s\" is not part of standard \"%s\"",
                    sQuote(units), sQuote(standard)), domain = NA)
  }
  unit <- units_map[power + 1L]
  if (power == 0 && standard == "legacy")
    unit <- "bytes"
  paste(round(x/base^power, digits = digits), unit)
}
