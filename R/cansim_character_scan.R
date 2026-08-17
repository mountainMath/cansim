# Instrumentation for #169, meant to be deleted along with its tests once StatCan stops publishing
# non-breaking spaces and control characters in the names it returns. Everything else in the package
# repairs those characters on the way in, which by design hides them, so watching whether the
# upstream problem is shrinking needs a reader that looks at the metadata as StatCan sends it. The
# repair itself, and the warning it emits, become dead weight once a scan comes back empty.

CUBE_METADATA_SCAN_URL <- "https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata"

# The names of one metadata level for one language, one element per dimension or member, NA where
# StatCan sent nothing at all. Reading them as a vector is what lets the scan grep a whole dimension
# of members at once, there are 2.3 million of them across the catalogue.
scan_names_of <- function(items,key) {
  vapply(items,
         function(x) {
           value <- x[[key]]
           if (is.null(value) || length(value)!=1 || is.na(value)) NA_character_ else as.character(value)
         },
         character(1))
}

# A clean scan finds nothing, so the columns have to come from somewhere other than the hits
empty_character_scan_hits <- function() {
  tibble(cansimTableNumber=character(0),level=character(0),language=character(0),
         dimension=character(0),value=character(0))
}

# One cube as StatCan returned it, no repair applied. Returns the offending names and the number of
# names looked at, the latter is the denominator the hit counts are meaningless without.
scan_cube_object <- function(object) {
  problem_characters <- paste0(ZERO_WIDTH_CHARACTERS,"|",SPACE_LIKE_CHARACTERS)
  cansimTableNumber <- cleaned_ndm_table_number(as.character(object$productId))
  hits <- list()

  record <- function(level,language,dimension,values) {
    if (length(values)==0) return(invisible(NULL))
    bad <- which(!is.na(values) & grepl(problem_characters,values))
    if (length(bad)==0) return(invisible(NULL))
    hits[[length(hits)+1]] <<- tibble(cansimTableNumber=cansimTableNumber,
                                      level=level,
                                      language=language,
                                      dimension=dimension,
                                      value=values[bad])
    invisible(NULL)
  }

  for (language in c("eng","fra")) {
    suffix <- ifelse(language=="eng","En","Fr")
    record("title",language,NA_character_,scan_names_of(list(object),paste0("cubeTitle",suffix)))
  }

  dimensions <- object$dimension
  members <- 0L
  for (dimension in dimensions) {
    # the English name identifies the dimension in both languages, a French hit is easier to place
    # when it is labelled with the name the rest of the package uses
    name <- scan_names_of(list(dimension),"dimensionNameEn")
    members <- members + length(dimension$member)
    for (language in c("eng","fra")) {
      suffix <- ifelse(language=="eng","En","Fr")
      record("dimension",language,name,scan_names_of(list(dimension),paste0("dimensionName",suffix)))
      record("member",language,name,scan_names_of(dimension$member,paste0("memberName",suffix)))
    }
  }

  list(hits=hits,
       scanned=tibble(cansimTableNumber=cansimTableNumber,
                      dimensions=length(dimensions),
                      members=members))
}

# Scans the names StatCan publishes for the characters `repair_statcan_strings()` removes, reading
# the metadata straight off the API rather than through `get_cansim_cube_metadata()` so that the
# repair does not hide what is being counted. Defaults to the whole catalogue, which is around 165
# API calls and takes some twenty minutes, pass table numbers to scan a survey or a single table.
#
# Returns a list of
#   `hits`, one row per offending name, holding the name as StatCan sent it and the same name with
#     its offending characters written out as `<U+00A0>` and friends
#   `scanned`, one row per table, carrying the number of dimensions and members looked at and, when
#     the whole catalogue was scanned, the survey the table belongs to
#   `failed`, the tables StatCan would not return metadata for
#
# Hand the result to `summarize_statcan_character_problems()` for the counts by level, language and
# survey. Returns NULL when StatCan cannot be reached, as the rest of the package does.
scan_statcan_character_problems <- function(cansimTableNumber=NULL,batch_size=50,quiet=FALSE) {
  cubes <- NULL
  if (is.null(cansimTableNumber)) {
    cubes <- suppressWarnings(list_cansim_cubes())
    if (is.null(cubes)) return(NULL)
    cansimTableNumber <- cubes$cansim_table_number
  }
  cansimTableNumber <- cleaned_ndm_table_number(cansimTableNumber)

  batches <- split(cansimTableNumber,ceiling(seq_along(cansimTableNumber)/batch_size))
  if (!quiet) message("Scanning ",length(cansimTableNumber)," tables in ",length(batches)," calls")

  hits <- list()
  scanned <- list()
  failed <- character(0)
  started <- Sys.time()

  for (i in seq_along(batches)) {
    body <- paste0("[",paste(paste0('{"productId":',naked_ndm_table_number(batches[[i]]),'}'),
                             collapse=", "),"]")
    response <- post_with_timeout_retry(CUBE_METADATA_SCAN_URL,body=body)
    if (is.null(response)) return(NULL)

    data <- statcan_response_json(response)
    succeeded <- Filter(function(x)x$status=="SUCCESS",data)
    for (x in succeeded) {
      scan <- scan_cube_object(x$object)
      hits <- c(hits,scan$hits)
      scanned[[length(scanned)+1]] <- scan$scanned
    }
    failed <- c(failed,setdiff(batches[[i]],
                               purrr::map_chr(succeeded,
                                              \(x)cleaned_ndm_table_number(as.character(x$object$productId)))))

    if (!quiet && (i %% 10 == 0 || i==length(batches))) {
      message(sprintf("%d/%d calls, %d tables, %d affected names, %.1f minutes elapsed",
                      i,length(batches),length(scanned),
                      sum(vapply(hits,nrow,integer(1))),
                      as.numeric(difftime(Sys.time(),started,units="mins"))))
    }
  }

  hits <- bind_rows(hits,empty_character_scan_hits()) %>%
    mutate(escaped=escape_statcan_characters(.data$value))

  scanned <- bind_rows(scanned,tibble(cansimTableNumber=character(0),
                                      dimensions=integer(0),members=integer(0)))
  if (!is.null(cubes)) {
    scanned <- scanned %>%
      left_join(cubes %>% select("cansim_table_number","surveyCode","surveyEn"),
                by=c("cansimTableNumber"="cansim_table_number"))
  }

  list(hits=hits,scanned=scanned,failed=failed)
}

# A cube can list several surveys and is counted under each of them, so the survey rows add up to
# more than the number of tables scanned. Survey names come from the cubes that list a single code,
# the cube list joins the names of several surveys with the same comma it joins their codes with and
# survey names have commas of their own, so a cube listing several cannot be taken apart again.
survey_lookup_from_cubes <- function(scanned) {
  scanned %>%
    filter(!grepl(",",.data$surveyCode),.data$surveyCode!="") %>%
    transmute(surveyCode=trimws(.data$surveyCode),survey=.data$surveyEn) %>%
    filter(!is.na(.data$survey)) %>%
    distinct(.data$surveyCode,.keep_all=TRUE)
}

# Counts for a `scan_statcan_character_problems()` result, as a list of
#   `overall`, one row per metadata level with the names scanned and the hits in each language
#   `tables`, how many tables are affected and on which language side
#   `characters`, how often each offending code point occurs, a name can hold several
#   `surveys`, the same counts per survey, only present when the whole catalogue was scanned
summarize_statcan_character_problems <- function(scan) {
  hits <- scan$hits
  scanned <- scan$scanned
  levels <- c("title","dimension","member")
  count_hits <- function(level,language) sum(hits$level==level & hits$language==language)

  overall <- tibble(level=levels,
                    scanned=c(nrow(scanned),sum(scanned$dimensions),sum(scanned$members)),
                    english=unname(vapply(levels,\(l)count_hits(l,"eng"),integer(1))),
                    french=unname(vapply(levels,\(l)count_hits(l,"fra"),integer(1)))) %>%
    mutate(total=.data$english+.data$french)

  per_table <- scanned %>%
    select("cansimTableNumber") %>%
    left_join(hits %>%
                group_by(.data$cansimTableNumber) %>%
                summarize(english=sum(.data$language=="eng"),
                          french=sum(.data$language=="fra"),.groups="drop"),
              by="cansimTableNumber") %>%
    mutate(across(c("english","french"),\(x)tidyr::replace_na(x,0L)),
           total=.data$english+.data$french)

  tables <- tibble(tables=nrow(per_table),
                   affected=sum(per_table$total>0),
                   english_only=sum(per_table$english>0 & per_table$french==0),
                   french_only=sum(per_table$english==0 & per_table$french>0),
                   both=sum(per_table$english>0 & per_table$french>0))

  code_points <- unlist(lapply(hits$value,\(v){
    points <- utf8ToInt(v)
    points[points %in% c(ZERO_WIDTH_CODE_POINTS,SPACE_LIKE_CODE_POINTS)]
  }))
  characters <- tibble(code_point=sprintf("U+%04X",code_points)) %>%
    count(.data$code_point,name="occurrences",sort=TRUE)

  summary <- list(overall=overall,tables=tables,characters=characters)

  if ("surveyCode" %in% names(scanned)) {
    summary$surveys <- scanned %>%
      select("cansimTableNumber","surveyCode") %>%
      mutate(surveyCode=strsplit(.data$surveyCode,"\\s*,\\s*")) %>%
      tidyr::unnest("surveyCode") %>%
      filter(.data$surveyCode!="") %>%
      left_join(per_table,by="cansimTableNumber") %>%
      group_by(.data$surveyCode) %>%
      summarize(tables=n(),
                affected_tables=sum(.data$total>0),
                english=sum(.data$english),
                french=sum(.data$french),
                total=sum(.data$total),.groups="drop") %>%
      left_join(survey_lookup_from_cubes(scanned),by="surveyCode") %>%
      select("survey","surveyCode","tables","affected_tables","english","french","total") %>%
      arrange(desc(.data$total))
  }

  summary
}
