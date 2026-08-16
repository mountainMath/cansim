# StatCan returns some names containing characters that are either invisible or that render as an
# ordinary space, most importantly the non-breaking space U+00A0. A name holding one of these cannot
# be reached by typing or copy-pasting what the console displays, which makes the corresponding
# column inaccessible in an R session. Line feeds and other control characters cause the same problem.
ZERO_WIDTH_CODE_POINTS <- c(0x200B,0x200C,0x200D,0xFEFF)
SPACE_LIKE_CODE_POINTS <- c(0x0009,0x000A,0x000B,0x000C,0x000D,0x00A0,0x1680,
                            0x2000:0x200A,0x2028,0x2029,0x202F,0x205F,0x3000)
ZERO_WIDTH_CHARACTERS <- paste0("[",intToUtf8(ZERO_WIDTH_CODE_POINTS),"]")
SPACE_LIKE_CHARACTERS <- paste0("[",intToUtf8(SPACE_LIKE_CODE_POINTS),"]")

# Zero width characters are dropped, everything else that behaves like a space becomes a regular
# space. Strings that contain none of these are returned untouched, so that the squishing and
# trimming below can never alter a name StatCan spelled with ordinary characters.
repair_statcan_strings <- function(x) {
  if (length(x)==0 || !is.character(x)) return(x)
  needs_repair <- !is.na(x) & grepl(paste0(ZERO_WIDTH_CHARACTERS,"|",SPACE_LIKE_CHARACTERS),x)
  if (!any(needs_repair)) return(x)

  x[needs_repair] <- x[needs_repair] %>%
    gsub(ZERO_WIDTH_CHARACTERS,"",.) %>%
    gsub(SPACE_LIKE_CHARACTERS," ",.) %>%
    gsub(" {2,}"," ",.) %>%
    trimws()

  x
}

# Renders the offending characters as their code points. A report that showed the repaired name
# would hide the very thing it is reporting on, since these characters are invisible on screen.
escape_statcan_characters <- function(x) {
  for (code_point in c(ZERO_WIDTH_CODE_POINTS,SPACE_LIKE_CODE_POINTS)) {
    x <- gsub(intToUtf8(code_point),sprintf("<U+%04X>",code_point),x,fixed=TRUE)
  }
  x
}

# Keeps the report readable when the offending character sits in the middle of a long table title,
# by showing a window around it rather than the whole string.
abbreviate_around_escape <- function(x,width=60) {
  if (is.na(x) || nchar(x)<=width) return(x)
  ellipsis <- intToUtf8(0x2026)
  at <- regexpr("<U+",x,fixed=TRUE)
  if (at<0) return(paste0(substr(x,1,width),ellipsis))
  end <- min(nchar(x),max(width,at-1+floor(width/2)))
  start <- max(1,end-width+1)
  paste0(if (start>1) ellipsis else "",substr(x,start,end),if (end<nchar(x)) ellipsis else "")
}

# R prints a warning as it was assembled, so a message built from several sentences arrives as one
# long line. Wrapping it to the console width the way ordinary console output is wrapped keeps it
# readable. Only for messages that are plain prose, anything laid out by hand keeps its own breaks.
wrap_warning_text <- function(...) {
  paste(strwrap(paste0(...),width=max(40,getOption("width",80))),collapse="\n")
}

# `original_values` are the names as StatCan sent them, before repair
warn_statcan_repairs <- function(original_values,context) {
  if (length(original_values)==0 || isTRUE(getOption("cansim.suppress_repair_warnings"))) return(invisible(NULL))
  example <- original_values[1] %>% escape_statcan_characters() %>% abbreviate_around_escape()
  warning(wrap_warning_text(
            "StatCan returned ",context," containing non-breaking spaces or control characters. ",
            "These render as an ordinary space or as nothing at all, so the names cannot be typed or ",
            "copy-pasted, the package has replaced them with regular spaces. ",
            if (length(original_values)==1) paste0("Repaired \"",example,"\".")
            else paste0("Repaired ",length(original_values)," names, for example \"",example,"\"."),
            " Set options(cansim.suppress_repair_warnings=TRUE) to silence this."),
          call.=FALSE)
  invisible(NULL)
}

# repairs a character vector of names and reports once on what changed
repair_statcan_names <- function(x,context=NULL) {
  repaired <- repair_statcan_strings(x)
  if (!is.null(context)) {
    warn_statcan_repairs(unique(x[!is.na(x) & repaired!=x]),context)
  }
  repaired
}

# repairs the given columns of a table and reports once across all of them
repair_statcan_columns <- function(data,columns,context=NULL) {
  columns <- intersect(columns,names(data))
  changed <- character(0)
  for (column in columns) {
    original <- data[[column]]
    repaired <- repair_statcan_strings(original)
    if (!identical(repaired,original)) {
      changed <- c(changed,original[!is.na(original) & repaired!=original])
      data[[column]] <- repaired
    }
  }
  if (!is.null(context)) warn_statcan_repairs(unique(changed),context)
  data
}

# The member names in the metadata are repaired, so the labels in the data have to be repaired the
# same way or the two no longer match and every row carrying an affected label turns into NA when the
# dimension is converted to a factor. Dimension columns hold a handful of distinct labels repeated
# across millions of rows, so only the distinct values are scanned and the rows are read back through
# an index. Scanning every row instead costs about seventy times as much on a large table.
repair_statcan_values <- function(x) {
  if (length(x)==0 || !is.character(x)) return(x)
  values <- unique(x)
  repaired <- repair_statcan_strings(values)
  if (identical(repaired,values)) return(x)
  repaired[match(x,values)]
}

# The dimension columns are the ones whose labels come from the metadata, and the only ones that need
# repairing. Everything else is either numeric, an identifier, or the coordinate column, which holds
# one distinct value per series and would make the scan above the expensive thing it avoids.
dimension_columns_in_data <- function(data_columns,dimension_names,cleaned_language) {
  geography_column <- ifelse(cleaned_language=="eng","Geography|Geographic name",
                             paste0("G",intToUtf8(0x00E9),"ographie|Nom g",intToUtf8(0x00E9),"ographique"))
  data_geography_column <- ifelse(cleaned_language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
  geography_columns <- geography_colum_names(cleaned_language)

  columns <- vapply(dimension_names, function(field) {
    if (field %in% data_columns) return(field)
    # StatCan names the geography dimension in the metadata but calls the column GEO in the data
    if ((grepl(geography_column,field) || field %in% geography_columns) &&
        data_geography_column %in% data_columns) return(data_geography_column)
    NA_character_
  }, character(1), USE.NAMES=FALSE)

  unique(columns[!is.na(columns)])
}

# repairs the dimension columns of a table, silently, the caller has already reported on the names
repair_statcan_dimension_values <- function(data,dimension_names,cleaned_language) {
  columns <- dimension_columns_in_data(names(data),dimension_names,cleaned_language)
  for (column in columns) {
    data[[column]] <- repair_statcan_values(data[[column]])
  }
  data
}

# Two things about a cached table are not in the data itself, when it was downloaded and which
# version of the package parsed it. The first says whether StatCan has newer data, the second says
# whether this version of the package would read the same files the same way. They are kept together
# in one file next to the data, `.Rda_info`, as a named list so that further entries can join them.
CACHE_INFO_SUFFIX <- "_info"

# up to 0.4.4 the timestamp lived on its own in `.Rda_time` and there was no version at all
LEGACY_CACHE_TIME_SUFFIX <- "_time"

# non-breaking spaces and control characters in column names and member labels are repaired as of
# this version, anything cached before it still carries the characters StatCan sent
VALUE_REPAIR_VERSION <- package_version("0.4.5")

write_cache_info <- function(meta_base_path,time_cached) {
  tryCatch(
    saveRDS(list(timeCached=strftime(time_cached,format=TIME_FORMAT),
                 cansimVersion=as.character(utils::packageVersion("cansim"))),
            paste0(meta_base_path,CACHE_INFO_SUFFIX)),
    error = function(e) warning("Failed to save cache info: ", e$message)
  )
  # a table refreshed into a cache that still has the old timestamp file leaves it behind stale
  legacy_file <- paste0(meta_base_path,LEGACY_CACHE_TIME_SUFFIX)
  if (file.exists(legacy_file)) unlink(legacy_file)
  invisible(NULL)
}

# Always returns both entries, either of them `NA` when the cache does not say. A cache written
# before 0.4.5 has the timestamp on its own and no version, which is itself the tell that the files
# were parsed by a version that predates everything the version is consulted about.
read_cache_info <- function(cache_dir) {
  info_file <- dir(cache_dir,paste0("\\.Rda",CACHE_INFO_SUFFIX,"$"))
  if (length(info_file)==1) {
    info <- tryCatch(readRDS(file.path(cache_dir,info_file)),error=function(e) NULL)
    if (is.list(info)) {
      entry <- function(name) if (length(info[[name]])==1) as.character(info[[name]]) else NA_character_
      return(list(timeCached=entry("timeCached"),cansimVersion=entry("cansimVersion")))
    }
  }

  time_file <- dir(cache_dir,paste0("\\.Rda",LEGACY_CACHE_TIME_SUFFIX,"$"))
  time_cached <- NA_character_
  if (length(time_file)==1) {
    time_cached <- tryCatch(as.character(readRDS(file.path(cache_dir,time_file))),
                            error=function(e) NA_character_)
  }
  list(timeCached=time_cached,cansimVersion=NA_character_)
}

# `NULL` when the cache does not record one, the caller decides what an unmarked cache means
read_cache_version <- function(cache_dir) {
  version <- read_cache_info(cache_dir)$cansimVersion
  if (is.na(version)) return(NULL)
  tryCatch(package_version(version),error=function(e) NULL)
}

cache_predates_value_repair <- function(cache_dir) {
  version <- read_cache_version(cache_dir)
  is.null(version) || version < VALUE_REPAIR_VERSION
}

# The dimension names and member labels cached with an old table carry the same unrepaired characters
# as its data, so they are what an old cache can be checked against. The footnotes and the table title
# are not looked at, a line feed inside a footnote is part of the text rather than a defect.
CACHE_METADATA_LABEL_PATTERN <- "\\.Rda2$|\\.Rda_column_"

# The names and labels the cached metadata holds that the repair would change. Reading them back is
# what tells us whether a cache that predates the repair is actually affected, most tables are not.
stale_cached_labels <- function(cache_dir) {
  files <- dir(cache_dir,CACHE_METADATA_LABEL_PATTERN,full.names=TRUE)
  labels <- lapply(files, function(file) {
    tryCatch({
      meta <- readRDS(file)
      values <- c(names(meta),unlist(lapply(meta, function(column) {
        if (is.factor(column)) levels(column) else if (is.character(column)) unique(column) else NULL
      }),use.names=FALSE))
      values[!is.na(values) & values!=repair_statcan_strings(values)]
    }, error=function(e) character(0))
  })
  unique(unlist(labels,use.names=FALSE))
}

cleaned_ndm_table_number <- function(cansimTableNumber){
  if (is.numeric(cansimTableNumber)) {
    warning(paste0("The cansim table number ",cansimTableNumber," used in this query is numeric,\n",
                   "it is safer to encode table numbers as character strings."))
    cansimTableNumber <- as.character(cansimTableNumber)
  }
  n<-gsub("-","",cansimTableNumber) %>%
    lapply(function(t){
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

# several functions only operate on a single table, guard against silently
# processing just the first entry when a vector of table numbers is passed
validate_single_table_number <- function(cansimTableNumber){
  if (length(cansimTableNumber)>1) {
    stop("This function only accepts a single table number, but ",length(cansimTableNumber),
         " table numbers were given.",call.=FALSE)
  }
  invisible(cansimTableNumber)
}

file_path_for_table_language <- function(cansimTableNumber, language){
  validate_single_table_number(cansimTableNumber)
  language <- cleaned_ndm_language(language)
  if (is.na(language)) stop(paste0("Unknown Lanaguage ",language),call.=FALSE)
  base_table <- naked_ndm_table_number(cansimTableNumber)
  file.path(paste0(base_table,"-",language))
}

base_path_for_table_language <- function(cansimTableNumber, language,base_dir = NULL){
  validate_single_table_number(cansimTableNumber)
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
  "429"="StatCan is rate limiting requests, please try again later",
  "502"="StatCan website is currently unreachable",
  "503"="StatCan website is currently unavailable",
  "504"="StatCan website did not respond in time"
)

# StatCan servers time out, go down for maintenance, or serve error pages often enough that treating
# it as a fatal error is the wrong default. Every failure to get a usable answer out of StatCan is
# reported through here, which warns loudly and returns NULL so the calling function can return NULL
# in turn. Erroring instead is what repeatedly got the package pulled from CRAN, since a check run
# started while StatCan was down would fail on examples and vignettes that are not at fault.
# Set options(cansim.error_on_unavailable=TRUE) to get an error rather than a warning.
statcan_unavailable <- function(...){
  message <- paste0(...)
  if (isTRUE(getOption("cansim.error_on_unavailable"))) stop(message,call.=FALSE)
  warning(message,call.=FALSE)
  NULL
}

# Shared failure handling for the GET and POST helpers. Returns the response on success, and NULL on
# any failure, so that callers only ever have to check for NULL rather than inspect status codes.
# `again` retries the request that produced `response`, it takes the remaining retry count.
check_statcan_response <- function(response,retry,again){
  if (!is.null(response$error)) {
    if ("curl_error_peer_failed_verification" %in% class(response$error)) {
      return(statcan_unavailable(
        stringr::str_wrap(gsub(".+\\): ","",as.character(response$error)),80),"\n",
        "This means that the authenticity of the StatCan API server can't be verified.\n",
        "Statistics Canada has a history of faulty SSL certificates on their API,\n",
        "if you are reasonably sure that your connection is not getting hijacked you\n",
        "can disable peer checking for the duration of the R session by typing\n\n",
        "httr::set_config(httr::config(ssl_verifypeer=0,ssl_verifystatus=0))","\n\n","into the console."))
    }
    if (retry>0) {
      message("Got timeout from StatCan, trying again")
      return(again(retry-1))
    }
    message("Got timeout from StatCan, giving up")
    return(statcan_unavailable("Problem downloading data, multiple timeouts.\n",
                               "Please check your network connection. If your connection is fine then ",
                               "StatCan servers might be down."))
  }

  status_code <- response$result$status_code
  if (is.null(status_code)) {
    return(statcan_unavailable("Problem downloading data, StatCan did not return a response."))
  }
  if (status_code!=200) {
    translation <- response_error_translation[[as.character(status_code)]]
    return(statcan_unavailable(if (is.null(translation)) "" else paste0(translation,"\n"),
                               "Problem downloading data, StatCan returned status code ",status_code,"."))
  }
  response$result
}

get_with_timeout_retry <- function(url,timeout=200,retry=3,path=NA){
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
  check_statcan_response(response,retry=retry,
                         again=\(r)get_with_timeout_retry(url,timeout=timeout,retry=r,path=path))
}

post_with_timeout_retry <- function(url,body,timeout=200,retry=3){
  response <- purrr::safely(httr::POST)(url,
                                        body=body,
                                        encode="json",
                                        httr::add_headers("Content-Type"="application/json"),
                                        httr::timeout(timeout))
  check_statcan_response(response,retry=retry,
                         again=\(r)post_with_timeout_retry(url,body=body,timeout=timeout,retry=r))
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


short_prov.fr <- setNames(c(
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
    setNames(c(data_geography_column,"GEO.abb")) %>%
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
#' Returns \code{NULL} if the data could not be retrieved because StatCan is unavailable.
#' @examples
#' \donttest{
#' get_cansim_code_set("survey")
#' }
get_cansim_code_set <- function(code_set=c("scalar", "frequency", "symbol", "status", "uom", "survey",  "subject", "wdsResponseStatus"),
                                refresh=FALSE){
  code_sets <- c("scalar", "frequency", "symbol", "status", "uom", "survey",  "subject", "wdsResponseStatus")
  if (length(code_set)!=1 | !(code_set %in% code_sets)) {
    stop(paste0("Invalid code set, code_set must be one of ",paste0(code_sets,collapse=", ")),call.=FALSE)
  }
  path=file.path(tempdir(),"cansim_code_sets.Rmd")
  if (refresh | !file.exists(path)) {
    url='https://www150.statcan.gc.ca/t1/wds/rest/getCodeSets'
    r<-get_with_timeout_retry(url)
    if (is.null(r)) return(NULL)
    content <- httr::content(r)
    saveRDS(content,path)
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
  cansimTableNumber <- attr(data,"cansimTableNumber")

  symbols <- which(grepl("^Symbol( \\d+)*$|^Symbole( \\d+)*$",names(data)))
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
        symbol_string <- ifelse(language=="fra","Symbole","Symbol")
        renames <- c(setNames(names(data)[dimensions],paste0(member_ids," --- ",value_column)),
                     setNames(names(data)[symbols],paste0(member_ids," --- ",symbol_string)))

        member_names <- dplyr::tibble(!!as.name(paste0("Member ID: ",dimension_name)):=member_ids,
                                      !!as.name(dimension_name):=dimension_members)


        if ("arrow_dplyr_query" %in% class(data)) {
          data <- as.data.frame(data)
          attr(data,"language") <- language
          attr(data,"cansimTableNumber") <- cansimTableNumber
        }

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
    if (!is.numeric(data[[value_column]])) {
      data <- data %>%
        dplyr::mutate(!!value_column:=as.numeric(!!as.name(value_column)))
    }
  } else {
    warning("Unknown table type")
  }
  data
}

same_partitioning <- function(p1,p2) {
  length(p1)==length(p2) && sum(sort(p1)!=sort(p2))==0
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
        stop("For SI units, specify 'standard = \"SI\"'",call.=FALSE)
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
                    sQuote(units), sQuote(standard)), domain = NA,call.=FALSE)
  }
  unit <- units_map[power + 1L]
  if (power == 0 && standard == "legacy")
    unit <- "bytes"
  paste(round(x/base^power, digits = digits), unit)
}


#' Get column names de-duplicated and in the correct order
#' @param cansimTableNumber The table number
#' @param language The language of the column names
#' @param column The column name
#' @keywords internal
#' @return A tibble with the column names
get_deduped_column_level_data <- function(cansimTableNumber,language,column) {
  dimension_id_column <- ifelse(language=="eng","Dimension ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification de la dimension"))
  member_id_column <- ifelse(language=="eng","Member ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification du membre"))
  member_name_column <- ifelse(language=="eng","Member Name","Nom du membre")
  parent_member_id_column <- ifelse(language=="eng","Parent Member ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification du membre parent"))

  columns <- get_cansim_column_categories(cansimTableNumber = cansimTableNumber,
                                          column = column,
                                          language = language)

  # full level values from metadata - combine mutates for efficiency
  level_table <- columns %>%
    select(...dim=!!as.name(dimension_id_column),
           ...id=!!as.name(member_id_column),
           ...name=!!as.name(member_name_column),
           ...parent_id=!!as.name(parent_member_id_column))

  # Sort once using base R for efficiency
  level_table <- level_table[order(as.integer(level_table$...id)), ]

  # Compute duplicates in one pass
  level_table <- level_table %>%
    mutate(...count=n(),
           ...duplicated=.data$...count>1,
           ...original=.data$...count==1,
           ...original_name=.data$...name,
           ...last_parent_id=.data$...parent_id,
           .by=c("...dim","...name"))

  fixed_level_table <- NULL
  # don't try to dedup census geographies, too messy
  if (substr(naked_ndm_table_number(cansimTableNumber),1,4)=="9810" && sum(filter(level_table,.data$...dim=="1")$...duplicated)>0) {
    warning(paste0("Table ",cansimTableNumber," is a census data table that has duplicate geography names, not converting to factors. Treat with caution when accessng geographies by name and check geographic identifiers."))
    fixed_level_table <- level_table %>%
      filter(.data$...dim=="1")
    level_table <- level_table %>%
      filter(.data$...dim!="1")
  }

  # try to dedup - only if there are duplicates
  max_run <- 30
  while (sum(level_table$...duplicated)>0 && max_run>0) { # deals with 36-10-0580
    max_run <- max_run - 1
    # Use join-based approach for deduplication as it handles dynamic parent chains efficiently
    level_table <- level_table %>%
      left_join(level_table %>% select("...id","...dim",...parent_name="...original_name",...new_parent_id="...last_parent_id"),
                by=c("...last_parent_id"="...id","...dim"="...dim")) %>%
      mutate(...name=case_when(.data$...duplicated & is.na(.data$...parent_name) ~ paste0(.data$...name," [",.data$...id,"]"),
                               .data$...duplicated & !is.na(.data$...parent_name) ~  paste0(.data$...name," ==> ",.data$...parent_name),
                               TRUE ~ .data$...name),
             ...last_parent_id=ifelse(.data$...duplicated, .data$...new_parent_id, .data$...last_parent_id)) %>%
      mutate(...count=n(), ...duplicated=.data$...count>1, .by=c("...dim","...name")) %>%
      select(-any_of(c("...parent_name","...new_parent_id")))
  }

  result <- bind_rows(fixed_level_table,level_table)
  # Sort by dim and id for final output
  result <- result[order(as.integer(result$...dim), as.integer(result$...id)), ]
  result %>%
    select("...dim","...id","...name","...original","...original_name")
}


standardize_cansim_column_order <- function(data) {
  language <- attr(data,"language")
  if (is.null(language)|!(language %in% c("eng","fra"))) {
    warning("Don't know how to standardize column order.")
    return(data)
  }

  classification_code_column <- ifelse(language=="eng","Classification Code","Code sur la classification")
  value_string <- ifelse(language=="fra","VALEUR","VALUE")
  scale_string <- ifelse(language=="fra","IDENTIFICATEUR SCALAIRE","SCALAR_ID")
  scale_string2 <- ifelse(language=="fra","FACTEUR SCALAIRE","SCALAR_FACTOR")
  uom_string=ifelse(language=="fra",paste0("UNIT",intToUtf8(0x00C9)," DE MESURE"),"UOM")
  uom_id_string=ifelse(language=="fra",paste0("IDENTIFICATEUR D'UNIT",intToUtf8(0x00C9)," DE MESURE"),"UOM_ID")
  classification_prefix <- ifelse(language=="fra","Code de classification pour ","Classification Code for ")
  hierarchy_prefix <- ifelse(language=="fra",paste0("Hi",intToUtf8(0x00E9),"rarchie pour "),"Hierarchy for ")
  coordinate_column <- ifelse(language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))
  data_geography_column <- ifelse(language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
  date_field=ifelse(language=="fra",paste0("P",intToUtf8(0x00C9),"RIODE DE R",intToUtf8(0x00C9),"F",intToUtf8(0x00C9),"RENCE"),"REF_DATE")

  standard_order1 <- intersect(c("REF_DATE",date_field,"Date","REF_DATE2",data_geography_column,"DGUID","GeoUID") %>%
                                 unique(),names(data))
  standard_order2 <- intersect(c(value_string,"val_norm",uom_string,uom_id_string,scale_string2,scale_string,"VECTOR","cansimTableNumber",coordinate_column,
                                 "STATUS","SYMBOL","releaseTime","frequencyCode",
                                 "TERMINATED","DECIMALS"), names(data))
  standard_order3 <- names(data)[grepl(paste0("^",hierarchy_prefix,"|^",classification_prefix),names(data))]

  rest_order <- setdiff(names(data),c(standard_order1,standard_order2,standard_order3))

  data %>%
    select(all_of(c(standard_order1,rest_order,standard_order2,standard_order3)))
}


column_names_for_language <- function(language) {
  date_field=ifelse(language=="fra",paste0("P",intToUtf8(0x00C9),"RIODE DE R",intToUtf8(0x00C9),"F",intToUtf8(0x00C9),"RENCE"),"REF_DATE")
  classification_code_column <- ifelse(language=="eng","Classification Code","Code sur la classification")
  value_string <- ifelse(language=="fra","VALEUR","VALUE")
  scale_string <- ifelse(language=="fra","IDENTIFICATEUR SCALAIRE","SCALAR_ID")
  scale_string2 <- ifelse(language=="fra","FACTEUR SCALAIRE","SCALAR_FACTOR")
  uom_string=ifelse(language=="fra",paste0("UNIT",intToUtf8(0x00C9)," DE MESURE"),"UOM")
  uom_id_string=ifelse(language=="fra",paste0("IDENTIFICATEUR D'UNIT",intToUtf8(0x00C9)," DE MESURE"),"UOM_ID")
  coordinate_column <- ifelse(language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))
  data_geography_column <- ifelse(language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
  column_names <- c(date_field,classification_code_column,value_string,scale_string,scale_string2,
                    uom_string,uom_id_string,coordinate_column,data_geography_column)
  column_names
}

rename_columns_for_language <- function(data,from_language,to_language) {
  renames <- setNames(column_names_for_language(to_language),column_names_for_language(from_language))

  renames <- renames[intersect(names(data),names(renames))]

  renames <- setNames(names(renames),as.character(renames))

  data %>%
    rename(!!!renames)

}

geography_colum_names <- function(language) {
  # `language` is a single value, an unrecognized (NA) language falls back to the French names
  if (isTRUE(language=="eng")) {
    c("Geography","Geographic name","Geography of origin")
  } else {
    c(paste0("G",intToUtf8(0x00E9),"ographie"),
      paste0("Nom g",intToUtf8(0x00E9),"ographique"),
      paste0("G",intToUtf8(0x00E9),"ographie d'origine"))
  }
}


normalize_coordinates <- function(coordinates){
  coordinates <- lapply(coordinates,\(coordinate)
                        coordinate %>%
                          strsplit("\\.") %>%
                          unlist() %>%
                          c(., rep(0, pmax(0,10-length(.)))) %>%
                          paste(collapse = ".")
  ) %>% unlist()

}

get_robust_cache_path <- function(cache_path) {
  if (is.null(cache_path) || cache_path=="") {
    cache_path <- Sys.getenv("CANSIM_CACHE_PATH")
    if (cache_path=="") cache_path <- getOption("cansim.cache_path",default="")
    if (cache_path=="") {
      cache_path <- file.path(tempdir(),"cansim_cache")
      if (!dir.exists(cache_path)) dir.create(cache_path)
      message(cansim_no_cache_path_message)
    }
  }
  if (!dir.exists(cache_path)) {
    stop("Cache path ",cache_path," does not exist, please create it first.",call.=FALSE)
  }
  cache_path
}

