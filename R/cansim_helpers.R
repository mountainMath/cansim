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

ISSUE_169_URL <- "https://github.com/mountainMath/cansim/issues/169"

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
            " Nothing on your end causes this and nothing on your end can fix it, the characters are ",
            "in the data StatCan publishes. This warning will disappear on its own once StatCan stops ",
            "sending them, which is tracked at ",ISSUE_169_URL,". ",
            "Set options(cansim.suppress_repair_warnings=TRUE) to silence this."),
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

# StatCan publishes in both languages and so do the people using this package, so a language can be
# named in either one. Everything is folded to lower case and stripped of accents before it is
# matched, which is what lets "Francais", accented or not, and "FRA" all name the same language.
ACCENTED_LETTERS <- intToUtf8(c(0x00E0,0x00E1,0x00E2,0x00E3,0x00E4,0x00E5,0x00E7,0x00E8,0x00E9,
                                0x00EA,0x00EB,0x00EE,0x00EF,0x00F4,0x00F6,0x00F9,0x00FB,0x00FC))
UNACCENTED_LETTERS <- "aaaaaaceeeeiioouuu"

ENGLISH_LANGUAGE_NAMES <- c("en","eng","engl","english","ang","angl","anglais","anglaise")
FRENCH_LANGUAGE_NAMES <- c("fr","fre","fren","french","fra","fran","franc","francais","francaise")

# Errors on anything it does not recognize rather than passing an NA on. An unrecognized language used
# to travel as far as the name of a cache directory or the tail of a StatCan URL, where it surfaced as
# a download failure or as a column that could not be found, neither of which points at the argument
# that caused it. Vectors are allowed, `remove_cansim_cached_tables()` asks for both languages at once.
cleaned_ndm_language <- function(language){
  normalized <- language %>% as.character() %>% trimws() %>% tolower() %>%
    chartr(ACCENTED_LETTERS,UNACCENTED_LETTERS,.)
  cleaned <- ifelse(normalized %in% ENGLISH_LANGUAGE_NAMES,"eng",
                    ifelse(normalized %in% FRENCH_LANGUAGE_NAMES,"fra",NA_character_))

  unknown <- unique(language[is.na(cleaned)])
  if (length(unknown)>0) {
    stop("Unknown language ",paste0('"',unknown,'"',collapse=", "),
         '. Use "english" (or "en", "eng", "anglais") for English and ',
         '"french" (or "fr", "fra", "francais") for French, case and accents are ignored.',
         call.=FALSE)
  }

  cleaned
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

# StatCan rejects a request that carries more than this many items with an HTTP 416 that names the
# limit, so every method taking a list of vectors, coordinates or tables is sent in batches of at
# most this size. The limit is enforced by the API but is not stated in the WDS user guide.
MAX_BATCH_SIZE <- 300L

batch_items <- function(items,size=MAX_BATCH_SIZE){
  unname(split(items,ceiling(seq_along(items)/size)))
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
  "409"=paste0("StatCan is publishing this table right now, or the daily update window ",
               "(midnight to 8:30am Eastern) has not finished, please try again later"),
  "416"="The request asked for more items than StatCan accepts in a single call",
  "429"="StatCan is rate limiting requests, please try again later",
  "502"="StatCan website is currently unreachable",
  "503"=paste0("StatCan website is currently unavailable, either for scheduled maintenance or ",
               "because of an outage. This lasts longer than it is worth waiting for, so the ",
               "request was not retried, please try again later"),
  "504"="StatCan website did not respond in time"
)

# A WDS method that takes a list of items answers with one record per item, each carrying its own
# status, and the API does not signal a bad item the same way everywhere. Some methods mark the
# record `"status":"FAILED"`, others answer `"status":"SUCCESS"` and put the reason in
# `responseStatusCode`, where anything other than 0 means the record carries no data. Asking only
# about `status` is what let an invalid vector through `get_cansim_vector_info()` as a row of NAs
# that was indistinguishable from real metadata, so both are checked here.
wds_record_code <- function(record){
  object <- record$object
  # a record that failed outright can carry a sentence in place of the object
  if (!is.list(object)) return(NA_integer_)
  code <- object$responseStatusCode
  if (length(code)!=1) return(NA_integer_)
  suppressWarnings(as.integer(code))
}

wds_record_succeeded <- function(record){
  if (!identical(as.character(record$status),"SUCCESS")) return(FALSE)
  code <- wds_record_code(record)
  is.na(code) || code==0L
}

split_wds_records <- function(data){
  if (length(data)==0) return(list(success=list(),failed=list()))
  succeeded <- vapply(data,wds_record_succeeded,logical(1))
  list(success=data[succeeded],failed=data[!succeeded])
}

wds_record_reason <- function(record){
  object <- record$object
  if (is.character(object) && length(object)==1) return(object)
  code <- wds_record_code(record)
  if (is.na(code)) return("no reason given")
  translation <- response_status_code_translation[[as.character(code)]]
  # the API emits codes that are not in its own wdsResponseStatus code set, such as the 9 it
  # answers with when a request exceeds the item limit
  if (is.null(translation)) return(paste0("StatCan response status code ",code))
  translation
}

# Names the item a failed record answers for, so that a report can say which vector or coordinate
# was dropped rather than only how many.
wds_record_id <- function(record){
  object <- record$object
  if (!is.list(object)) return(NA_character_)
  present <- function(x) length(x)==1 && !is.na(x) && x!=0
  if (present(object$vectorId)) return(paste0("v",object$vectorId))
  if (present(object$productId)) {
    # spelled out rather than routed through cleaned_ndm_table_number(), which warns and messages
    # on its own and has no business doing so from inside a failure report
    product_id <- as.character(object$productId)
    table_number <- paste0(substr(product_id,1,2),"-",substr(product_id,3,4),"-",substr(product_id,5,8))
    if (length(object$coordinate)==1 && !is.na(object$coordinate)) {
      return(paste0(table_number," ",gsub("(\\.0)+$","",object$coordinate)))
    }
    return(table_number)
  }
  NA_character_
}

report_failed_wds_records <- function(failed,context){
  if (length(failed)==0) return(invisible(NULL))
  reasons <- vapply(failed,wds_record_reason,character(1))
  ids <- vapply(failed,wds_record_id,character(1))
  message("Failed to load ",context," for ",length(failed)," of the requested items.")
  # a batch of 300 bad vectors is one problem, not 300, so identical reasons are reported together
  for (reason in unique(reasons)) {
    named <- as.character(na.omit(ids[reasons==reason]))
    shown <- head(named,5)
    message("  ",reason,
            if (length(shown)>0) paste0(" (",paste(shown,collapse=", "),
                                        if (length(named)>length(shown)) ", ..." else "",")") else "")
  }
  invisible(NULL)
}

# Returns the records that carry data and reports on the rest. `ignore_codes` is for the callers to
# which a given failure is an expected answer rather than a problem worth mentioning.
successful_wds_records <- function(data,context,ignore_codes=integer(0)){
  records <- split_wds_records(data)
  if (length(records$failed)>0) {
    reported <- Filter(\(record) !(wds_record_code(record) %in% ignore_codes),records$failed)
    report_failed_wds_records(reported,context)
  }
  records$success
}

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

# The body of every WDS answer is JSON, and this is the only place that is assumed, so that the
# fourteen call sites reading a response do not each have to say so. `check_type=FALSE` because a few
# of the endpoints the package uses, the key release schedule among them, have been seen to label
# their JSON as plain text.
statcan_response_json <- function(response){
  httr2::resp_body_json(response,check_type=FALSE)
}

# A response StatCan refuses carries a JSON body saying why, which the status code on its own does
# not: the 409 served outside the daily release window explains that the product is not released
# yet, and the 416 served for an oversized request names the item limit it went past. Anything that
# is not JSON with a message in it, an HTML error page or a part-written download, yields NULL.
statcan_response_message <- function(response){
  parsed <- tryCatch(statcan_response_json(response),error=function(e) NULL)
  if (!is.list(parsed)) return(NULL)
  detail <- parsed$message
  if (is.null(detail) && is.character(parsed$object)) detail <- parsed$object
  if (length(detail)!=1 || !is.character(detail) || is.na(detail) || detail=="") return(NULL)
  detail
}

# StatCan documents a limit of 25 requests per second per IP address, and 50 across all callers. The
# package makes its requests one after the other so it rarely comes close, but a script looping over
# many tables or vectors can, and being throttled locally is better than being answered with an
# HTTP 429.
STATCAN_REQUESTS_PER_SECOND <- 25

# Statuses worth trying again a moment later, being the ones a busy or briefly confused server
# recovers from within seconds. Deliberately not in the list:
#   409, the nightly update window, which lasts until 8:30am Eastern
#   416, a request carrying more items than StatCan accepts, which stays oversized however often it
#        is sent
#   503, StatCan being down for maintenance or an outage, which lasts far longer than any retry
#        budget worth spending
# Retrying any of these would only fail more slowly, so they are reported to the caller instead,
# with a message saying what to do about it.
STATCAN_TRANSIENT_STATUS <- c(429L,500L,502L,504L)

# An upper bound on the wall clock time one request may spend being retried, counted from the start of
# the first attempt and so covering the requests themselves as well as the waiting between them. The
# backoff sleeps for somewhere between one and 2^n seconds before the nth retry, which adds up to at
# most 14 seconds across the three retries a request gets by default, so 30 seconds leaves room for
# the full sequence to play out along with the requests it separates. Its purpose is the case that
# sequence does not cover: a request that hangs rather than failing, where the default 200 second
# timeout would otherwise let a single call sit for the better part of quarter of an hour before
# giving up. One attempt that runs to that timeout now uses up the budget on its own, which is the
# intended trade, since a connection StatCan has left hanging rarely comes back on an immediate retry.
STATCAN_MAX_RETRY_SECONDS <- 30

# The `timeout` every download function takes used to be a hard cap on the whole transfer, which
# cannot tell a connection StatCan has stopped answering on from a large table that is simply taking a
# while to arrive, and cut both off alike. It is now the length of time StatCan may go without sending
# anything useful, which is the distinction the argument was always described as making. A transfer
# that keeps delivering is left alone however long it runs, and one that goes quiet is dropped.
#
# The floor is set well under any real transfer, an 11.8MB table download runs at about 3MB/s, while
# still being high enough that a connection dribbling a byte at a time to stay alive does not hold the
# session open forever.
STATCAN_MINIMUM_SPEED <- 100

# StatCan answers a request by working out the whole response and only then sending it, so the wait
# for the first byte grows with the size of the request: about 0.11 seconds per vector, putting the
# 300 item batches this package sends at roughly 35 seconds of silence before any data arrives. That
# silence counts against the timeout above, which is why the default is left far above it rather than
# tightened to the few seconds a healthy connection needs. Establishing the connection is the one part
# that is quick regardless, and is bounded separately so an unreachable host fails fast.
STATCAN_CONNECT_TIMEOUT <- 10

cansim_user_agent <- function(){
  paste0("cansim/",utils::packageVersion("cansim")," (https://github.com/mountainMath/cansim)")
}

# The shape every request to StatCan has in common. Retry and throttling are handled by httr2 rather
# than by hand: `req_retry()` backs off exponentially with jitter between attempts and honours a
# `Retry-After` header if StatCan sends one, where the package used to retry immediately and only
# ever on a connection failure, never on a status code that says to come back later.
statcan_request <- function(url,timeout=200,retry=3){
  req <- httr2::request(url) %>%
    httr2::req_user_agent(cansim_user_agent()) %>%
    # sent on GET as well as POST, which is what the package has always done
    httr2::req_headers("Content-Type"="application/json") %>%
    # `timeout` bounds how long StatCan may go without sending anything, not how long the whole
    # transfer may take, see the note on STATCAN_MINIMUM_SPEED above
    httr2::req_options(low_speed_limit=STATCAN_MINIMUM_SPEED,
                       low_speed_time=timeout,
                       connecttimeout=STATCAN_CONNECT_TIMEOUT) %>%
    # every status is translated below rather than thrown, so that a caller only has to check for NULL
    httr2::req_error(is_error=function(response) FALSE) %>%
    httr2::req_throttle(capacity=STATCAN_REQUESTS_PER_SECOND,fill_time_s=1,realm="statcan") %>%
    httr2::req_retry(max_tries=retry+1,
                     max_seconds=STATCAN_MAX_RETRY_SECONDS,
                     retry_on_failure=TRUE,
                     is_transient=function(response) httr2::resp_status(response) %in% STATCAN_TRANSIENT_STATUS)

  if (isTRUE(getOption("cansim.disable_ssl_verification"))) {
    req <- httr2::req_options(req,ssl_verifypeer=0,ssl_verifystatus=0)
  }
  req
}

# The condition httr2 raises for a request that never got an answer wraps the underlying curl error
# as its parent, so the reason has to be looked for down the chain rather than on the condition
# itself.
condition_classes <- function(cond){
  classes <- character(0)
  while (inherits(cond,"condition")) {
    classes <- c(classes,class(cond))
    cond <- cond$parent
  }
  classes
}

# Distinguishes "StatCan answered, and the answer is that there is nothing" from "StatCan did not
# answer", which is the NULL every failure returns. The changed-series methods need the difference:
# they report that none of the series asked about changed with an HTTP 404, which is an ordinary
# answer for them rather than a sign that anything is wrong.
STATCAN_NO_DATA <- structure(list(),class="statcan_no_data")

statcan_no_data <- function(x) inherits(x,"statcan_no_data")

perform_statcan_request <- function(req,path=NA,empty_status=integer(0)){
  response <- tryCatch(if (is.na(path)) httr2::req_perform(req) else httr2::req_perform(req,path=path),
                       error=function(e) e)
  check_statcan_response(response,empty_status=empty_status)
}

# Shared failure handling for the GET and POST helpers. Takes what performing the request produced,
# either a response or the condition raised when there was none, returns the response on success and
# NULL on any failure, so that callers only ever have to check for NULL rather than inspect statuses.
check_statcan_response <- function(response,empty_status=integer(0)){
  if (inherits(response,"condition")) {
    if ("curl_error_peer_failed_verification" %in% condition_classes(response)) {
      return(statcan_unavailable(
        stringr::str_wrap(gsub(".+\\): ","",conditionMessage(response)),80),"\n",
        "This means that the authenticity of the StatCan API server can't be verified.\n",
        "Statistics Canada has a history of faulty SSL certificates on their API,\n",
        "if you are reasonably sure that your connection is not getting hijacked you\n",
        "can disable peer checking for the duration of the R session by typing\n\n",
        "options(cansim.disable_ssl_verification=TRUE)","\n\n","into the console."))
    }
    return(statcan_unavailable("Problem downloading data, StatCan did not answer.\n",
                               "Please check your network connection. If your connection is fine then ",
                               "StatCan servers might be down.\n",
                               conditionMessage(response)))
  }
  if (!inherits(response,"httr2_response")) {
    return(statcan_unavailable("Problem downloading data, StatCan did not return a response."))
  }

  status_code <- httr2::resp_status(response)
  # a status the caller asked for as meaning "nothing to report" rather than "something went wrong"
  if (status_code %in% empty_status) return(STATCAN_NO_DATA)
  if (status_code!=200) {
    translation <- response_error_translation[[as.character(status_code)]]
    detail <- statcan_response_message(response)
    return(statcan_unavailable(if (is.null(translation)) "" else paste0(translation,"\n"),
                               "Problem downloading data, StatCan returned status code ",status_code,".",
                               if (is.null(detail)) "" else paste0("\n","StatCan says: ",detail)))
  }
  response
}

# `query` is given as a named list rather than pasted onto the url by the caller, because httr2 sends
# a url exactly as handed to it. StatCan's vector-by-reference-period method takes its vector ids
# quoted, and a raw double quote in a query string is answered with an HTTP 400, so the values have
# to be percent-encoded on the way out.
get_with_timeout_retry <- function(url,timeout=200,retry=3,path=NA,query=NULL,empty_status=integer(0)){
  req <- statcan_request(url,timeout=timeout,retry=retry)
  if (!is.null(query)) req <- httr2::req_url_query(req,!!!query)
  perform_statcan_request(req,path=path,empty_status=empty_status)
}

post_with_timeout_retry <- function(url,body,timeout=200,retry=3,empty_status=integer(0)){
  req <- statcan_request(url,timeout=timeout,retry=retry) %>%
    httr2::req_body_raw(body,type="application/json")
  perform_statcan_request(req,empty_status=empty_status)
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
    content <- statcan_response_json(r)
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

