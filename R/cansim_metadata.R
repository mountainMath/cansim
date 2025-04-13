

#' Parse metadata
#' @param meta the raw metadata table
#' @param data_path base path to save parsed metadata
#'
#' @return NULL
#' @keywords internal
parse_metadata <- function(meta,data_path){
  cleaned_language <- basename(data_path) %>% gsub("^.+-|\\..+$","",.)
  cube_title_column <- ifelse(cleaned_language=="eng","Cube Title","Titre du cube")
  dimension_id_column <- ifelse(cleaned_language=="eng","Dimension ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification de la dimension"))
  dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
  classification_code_column <- ifelse(cleaned_language=="eng","Classification Code","Code sur la classification")
  member_name_column <- ifelse(cleaned_language=="eng","Member Name","Nom du membre")
  geography_column <- ifelse(cleaned_language=="eng","Geography",paste0("G",intToUtf8(0x00E9),"ographie"))
  data_geography_column <- ifelse(cleaned_language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
  symbol_legend_grepl_field <- ifelse(cleaned_language=="eng","Symbol Legend",paste0("L",intToUtf8(0x00E9),"gende Symbole"))
  survey_code_grepl_field <- ifelse(cleaned_language=="eng","Survey Code",paste0("Code d'enqu",intToUtf8(0x00EA),"te"))
  subject_code_grepl_field <- ifelse(cleaned_language=="eng","Subject Code","Code du sujet")
  note_id_grepl_field <- ifelse(cleaned_language=="eng","Note ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification de la note"))
  correction_id_grepl_field <- ifelse(cleaned_language=="eng","Correction ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification de la correction"))
  member_id_column <- ifelse(cleaned_language=="eng","Member ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification du membre"))
  parent_member_id_column <- ifelse(cleaned_language=="eng","Parent Member ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification du membre parent"))
  hierarchy_column <- ifelse(cleaned_language=="eng","Hierarchy",paste0("Hi",intToUtf8(0x00E9),"rarchie"))
  classification_code_prefix <- ifelse(cleaned_language=="eng","Classification Code for","Code de classification pour")
  exceeded_hierarchy_warning_message <- ifelse(cleaned_language=="eng","Exceeded max depth for hierarchy, hierarchy information may be faulty.",
                                               paste0("Profondeur maximale d",intToUtf8(0x00E9),"pass",intToUtf8(0x00E9),"e pour la hi",intToUtf8(0x00E9),"rarchie, les informations de hi",intToUtf8(0x00E9),"rarchie peuvent ",intToUtf8(0x00EA),"tre erron",intToUtf8(0x00E9),"es."))
  hierarchy_prefix <- ifelse(cleaned_language=="eng","Hierarchy for",paste0("Hi",intToUtf8(0x00E9),"rarchie pour"))

  table_delim <- ifelse(cleaned_language=="fra",";",",")

  read_meta <- function(meta_part) {
    while (meta_part[length(meta_part)]=="") {
      meta_part <- meta_part[-length(meta_part)]
    }
    if (TRUE) {
      # This is a workaround for problems with StatCan Metadata found in Table 17-10-0016
      if (length(grep("\u201C|\u201D",meta_part))>0){
        meta_part <- meta_part %>% gsub("\u201C|\u201D",'"',x=.)
      }
      utils::read.delim(text=meta_part,sep=table_delim,header=TRUE,stringsAsFactors=FALSE,
                        quote="\"",na.strings="",
                 colClasses="character",check.names=FALSE) %>%
        as_tibble()
    } else {
      suppressWarnings(readr::read_delim(paste0(meta_part,collapse="\n"),
                                         delim=table_delim, col_types = readr::cols(.default="c")))
    }
  }

  read_notes <- function(meta_part) {
    while (meta_part[length(meta_part)]=="") {
      meta_part <- meta_part[-length(meta_part)]
    }
    if (length(grep("\u201C|\u201D",meta_part))>0){
      meta_part <- meta_part %>% gsub("\u201C|\u201D",'"',x=.)
    }
    h <- utils::read.delim(text=meta_part[1],sep=table_delim,header=TRUE,stringsAsFactors=FALSE,
                           quote="\"",na.strings="",
                           colClasses="character",check.names=FALSE) %>%
      names()
    notes <- tibble(!!h[1]:=meta_part[-1] %>% lapply(\(x)gsub(",.+","",x)) %>% unlist(),
                    !!h[2]:=meta_part[-1] %>% lapply(\(x)gsub("^\\d+,","",x) %>% gsub("^\"|\"$","",.)) %>% unlist())

  }

  cut_indices <- setdiff(which(grepl(paste0('^"',dimension_id_column,'"|^',symbol_legend_grepl_field,''),meta)),length(meta))

  meta1 <- read_meta(meta[seq(1,cut_indices[1]-1)])
  saveRDS(meta1,file=paste0(data_path,"1"))
  meta2 <- read_meta(meta[seq(cut_indices[1],cut_indices[2]-1)])
  saveRDS(meta2,file=paste0(data_path,"2"))
  meta3 <- read_meta(meta[seq(cut_indices[2],cut_indices[3]-1)])
  saveRDS(meta3,file=paste0(data_path,"2m"))
  correction_index <- grep(paste0('^"',correction_id_grepl_field,'"'),meta)
  if (length(correction_index)==0) correction_index=length(meta)
  additional_indices=c(grep(paste0('^"',survey_code_grepl_field,'"'),meta),
                       grep(paste0('^"',subject_code_grepl_field,'"'),meta),
                       grep(paste0('^"',note_id_grepl_field,'"'),meta),
                       correction_index)
  saveRDS(read_meta(meta[seq(additional_indices[1],additional_indices[2]-1)]), file=paste0(data_path,"3"))
  saveRDS(read_meta(meta[seq(additional_indices[2],additional_indices[3]-1)]), file=paste0(data_path,"4"))
  if (length(additional_indices)>3) {
    saveRDS(read_notes(meta[seq(additional_indices[3],additional_indices[4]-1)]),file=paste0(data_path,"5"))
  }

  column_ids <- dplyr::pull(meta2,dimension_id_column)
  column_names <- dplyr::pull(meta2,dimension_name_column)
  for (column_index in column_ids) { # iterate through columns for which we have meta data
    column <- meta2 %>% dplyr::filter(.data[[dimension_id_column]]==column_index)
    is_geo_column <- grepl(geography_column,column[[dimension_name_column]]) & !(column[[dimension_name_column]] %in% column_names)
    meta_x <- meta3 %>%
      dplyr::filter(.data[[dimension_id_column]]==column_index) %>%
      add_hierarchy(parent_member_id_column=parent_member_id_column,
                    member_id_column=member_id_column,
                    hierarchy_column=hierarchy_column,
                    exceeded_hierarchy_warning_message=exceeded_hierarchy_warning_message) %>%
      mutate(name=ifelse(is.na(!!as.name(classification_code_column)) | is_geo_column,
                         !!as.name(member_name_column),
                         paste0(!!as.name(member_name_column)," ",!!as.name(classification_code_column))))
    saveRDS(meta_x,file=paste0(data_path,"_column_",column_index))
  }
  NULL
}


add_hierarchy <- function(meta_x,parent_member_id_column,member_id_column,hierarchy_column,exceeded_hierarchy_warning_message){
  meta_x <- meta_x %>% mutate(across(all_of(c(member_id_column,parent_member_id_column)),as.character))
  parent_lookup <- rlang::set_names(meta_x[[parent_member_id_column]],meta_x[[member_id_column]])
  current_top <- function(c){
    strsplit(c,"\\.") %>%
      purrr::map(dplyr::first) %>%
      unlist
  }
  parent_for_current_top <- function(c){
    as.character(parent_lookup[current_top(c)])
  }
  meta_x <- meta_x %>%
    dplyr::mutate(!!as.name(hierarchy_column):=.data[[member_id_column]])
  added=TRUE
  max_depth=100
  count=0
  while (added & count<max_depth) { # generate hierarchy data from member id and parent member id data
    old <- meta_x[[hierarchy_column]]
    meta_x <- meta_x %>%
      dplyr::mutate(p=parent_for_current_top(.data[[hierarchy_column]])) %>%
      dplyr::mutate(!!as.name(hierarchy_column):=ifelse(is.na(.data$p),.data[[hierarchy_column]],paste0(.data$p,".",.data[[hierarchy_column]]))) %>%
      dplyr::select(-"p")
    added <- sum(old != meta_x[[hierarchy_column]])>0
    count=count+1
  }
  if (added) {
    warning(exceeded_hierarchy_warning_message)
  }
  meta_x
}



#' Retrieve table metadata from Statistics Canada API
#'
#' Retrieves table metadata given an input table number or vector of table numbers using either the new or old table number format. Patience is suggested as the Statistics Canada API can be very slow. The `list_cansim_tables()` function can be used as an alternative to retrieve a (cached) list of CANSIM tables with (more limited) metadata.
#'
#' @param cansimTableNumber A new or old CANSIM/NDM table number or a vector of table numbers
#' @param type Which type of metadata to get, options are "overview", "members", "notes", or "corrections".
#' @param refresh Refresh the data from the Statistics Canada API
#'
#' @return a tibble containing the table metadata
#'
#' @examples
#' \dontrun{
#' get_cansim_cube_metadata("34-10-0013")
#' }
#' @export
get_cansim_cube_metadata <- function(cansimTableNumber, type="overview",refresh=FALSE){
  type <- type[1]
  if (!(type %in% c("overview", "members", "notes", "corrections"))) {
    stop("type must be one of 'overview', 'members', 'notes', or 'corrections'")
  }
  tmp_base <- table_base_path(cansimTableNumber)
  if (!dir.exists(tmp_base)) dir.create(tmp_base)
  cansimTableNumber <- cleaned_ndm_table_number(cansimTableNumber)
  tmp <- file.path(tmp_base, paste0(cansimTableNumber,"_metadata", ".Rda"))
  if (!file.exists(tmp) || refresh) {
    table_id <- naked_ndm_table_number(cansimTableNumber)
    url <- "https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata"
    response <- httr::POST(url,
                           #body=jsonlite::toJSON(list("productId"=table_id),auto_unbox =TRUE),
                           body=paste0("[",paste(paste0('{"productId":',table_id,'}'),collapse = ", "),"]"),
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
    d <- data[[1]]$object
    saveRDS(data1, tmp)
  } else {
    data1 <- readRDS(tmp)
  }
  d <- data1[[1]]$object


  meta1_path <- file.path(tmp_base, paste0(cansimTableNumber, "_cubemeta1.Rda"))
  meta2_path <- file.path(tmp_base, paste0(cansimTableNumber, "_cubemeta2.Rda"))
  meta3_path <- file.path(tmp_base, paste0(cansimTableNumber, "_cubemeta3.Rda"))
  meta4_path <- file.path(tmp_base, paste0(cansimTableNumber, "_cubemeta4.Rda"))
  meta5_path <- file.path(tmp_base, paste0(cansimTableNumber, "_cubemeta5.Rda"))


  if (!file.exists(meta1_path)||refresh) {
    m1 <- d %>% tibble::enframe() %>%
      mutate(l=lapply(.data$value,class) %>% unlist()) %>%
      filter(.data$l!="list" | .data$name %in% c("surveyCode","subjectCode")) %>%
      select(-"l") %>%
      tidyr::pivot_wider() %>%
      mutate_all(\(x)paste0(unlist(x), collapse=", "))
    saveRDS(m1, meta1_path)
  } else {
    m1 <- readRDS(meta1_path)
  }


  if (!file.exists(meta2_path)||refresh) {
    m2 <- d$dimension %>%
      purrr::map_df(\(x){
        tibble::as_tibble(x) %>%
          tidyr::unnest_wider("member")  %>%
          mutate(across(where(is.integer),as.character))
      }) %>%
      arrange(as.integer(.data$dimensionPositionId),as.integer(.data$memberId))
    saveRDS(m2, meta2_path)
  } else {
    m2 <- readRDS(meta2_path)
  }

  if (!file.exists(meta3_path)||refresh) {
    m3 <- d$footnote %>%
      purrr::map_df(\(x){
        tibble::as_tibble(x) %>%
          left_join(as_tibble(.$link),by="footnoteId") %>%
          dplyr::select(-"link")  %>%
          mutate(across(where(is.integer),as.character)) %>%
          arrange(as.integer(.data$footnoteId))
      }) %>%
      unique() %>%
      arrange(as.integer(.data$footnoteId),as.integer(.data$dimensionPositionId),as.integer(.data$memberId))
    saveRDS(m3, meta3_path)
  } else {
    m3 <- readRDS(meta3_path)
  }

  if (!file.exists(meta4_path)||refresh) {
    m4 <- d$correctionFootnote %>%
      purrr::map_df(\(x){
        tibble::as_tibble(x)   %>%
          mutate(across(is.integer,as.character))
      })
    saveRDS(m4, meta4_path)
  } else {
    m4 <- readRDS(meta4_path)
  }


  if (FALSE) {
    short_language <- c("eng"="En","fra"="Fr")[[language]]

    m1_renames <- c(
      "Cube Title"=paste0("cubeTitle",short_language),
      "Product Id"="productId",
      "CANSIM Id"="cansimId",
      "URL"="URL",
      "Cube Notes"="cubeNotes",
      "Archive Status"=paste0("archiveStatus",short_language),
      "Frequency"=paste0("frequencyDesc",short_language),
      "Start Reference Period"="cubeStartDate",
      "End Reference Period"="cubeEndDate",
      "Total number of dimensions"="nbDatapointsCube"
    )

    frequency_codes <- get_cansim_code_set("frequency")

    meta1 <- m1 %>%
      left_join(frequency_codes,by="frequencyCode") %>%
      mutate(URL=paste0("https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=",productId)) %>%
      mutate(cubeNotes=m3 %>% filter(dimensionPositionId==0,memberId==0) %>% pull(footnoteId) %>% paste0(collapse=", ")) %>%
      rename(!!!m1_renames) %>%
      relocate(names(m1_renames))

    writeRDS(meta1, paste0(base_path_for_table_language(cansimTableNumber, language), ".Rda1"))
  }

  if (type=="overview") {

    if (FALSE) { # experimental code
    fields <- c("productId", "cansimId", "cubeTitleEn", "cubeTitleFr", "cubeStartDate", "cubeEndDate", "nbSeriesCube",
                "nbDatapointsCube",  "archiveStatusCode", "archiveStatusEn",   "archiveStatusFr",   "subjectCode",
                "surveyCode",  "dimension","releaseTime")
    result <- lapply(fields, function(field){
      purrr::map(data1,function(d){
        dd<-d$object[[field]]
        if (typeof(dd)=="list") dd <- dd %>% unlist %>% as.character() %>% paste(collapse = ",")
        dd
      }) %>% as.character()
    }) %>%
      purrr::set_names(fields) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(productId=cleaned_ndm_table_number(.data$productId)) %>%
      dplyr::mutate(releaseTime=readr::parse_datetime(.data$releaseTime,
                                                      format=STATCAN_TIME_FORMAT,
                                                      locale=readr::locale(tz=STATCAN_TIMEZONE)))
    } else {
    result <- m1 %>%
      dplyr::mutate(productId=cleaned_ndm_table_number(.data$productId)) %>%
      dplyr::mutate(releaseTime=readr::parse_datetime(.data$releaseTime,
                                                      format=STATCAN_TIME_FORMAT,
                                                      locale=readr::locale(tz=STATCAN_TIMEZONE)))
    }
  } else if (type=="notes") {
    result <- m3
  } else if (type=="members") {
    result <- m2
  } else if (type=="corrections") {
    result <- m4
  }
  result
}

#' Retrieve table template from Statistics Canada API
#'
#' A table templase consists of the dimensions and members and coordinates of a table that can be used to explore
#' and filter table data before downloading subsets of the table. To add vector Ids to (a possibly filtered) template
#' the `add_cansim_vectors_to_template` function can be used.
#'
#' @param cansimTableNumber A new or old CANSIM/NDM table number or a vector of table numbers
#' @param language Language for the dimension and memebr names, either "eng" or "fra"
#' @param refresh Refresh the data from the Statistics Canada API
#'
#' @return a tibble containing the table template
#'
#' @examples
#' \dontrun{
#' get_cansim_table_template("34-10-0013")
#' }
#' @export
get_cansim_table_template <- function(cansimTableNumber, language="eng",refresh=FALSE){
  member_info <- get_cansim_cube_metadata(cansimTableNumber, type="members", refresh=refresh)

  language <- cleaned_ndm_language(language)

  if (language=="fra") {
    member_info <- member_info %>%
      select("dimensionPositionId",dimensionName="dimensionNameFr","memberId",memberName="memberNameFr",
             "classificationCode","geoLevel")
  } else {
    member_info <- member_info %>%
      select("dimensionPositionId",dimensionName="dimensionNameEn","memberId",memberName="memberNameEn",
             "classificationCode","geoLevel")
  }

  dimensions <- member_info %>%
    select("dimensionPositionId", "dimensionName") %>%
    unique() %>%
    arrange("dimensionPositionId")

  result <- tibble(...link="link",COORDINATE="")

  for (i in seq_len(nrow(dimensions))) {
    dim <- dimensions[i,]
    dim_name <- dim$dimensionName
    member <- member_info %>%
      filter(.data$dimensionPositionId==dim$dimensionPositionId) %>%
      select("memberId", "memberName") %>%
      unique() %>%
      arrange("memberId") %>%
      rename(!!dim_name:="memberName") %>%
      mutate(...link="link")

    result <- result %>%
      full_join(member, by="...link",
                relationship = "many-to-many") %>%
      mutate(COORDINATE=ifelse(.data$COORDINATE=="", .data$memberId, paste0(.data$COORDINATE, ".", .data$memberId))) %>%
      select(-any_of("memberId"))
  }

  result <- result %>%
    select(-any_of("...link"))

  attr(result, "cansimTableNumber") <- cansimTableNumber
  attr(result, "langauge") <- language

  result
}


#' Retrieve series info for given table id and coordinates
#'
#' Retrieves series information by coordinates
#'
#' @param cansimTableNumber A new or old CANSIM/NDM table number or a vector of table numbers
#' @param coordinates A vector of coordinates
#' @param timeout Timeout for the API call
#' @param refresh Refresh the data from the Statistics Canada API
#'
#' @return a tibble containing the table template
#'
#' @examples
#' \dontrun{
#' get_cansim_table_template("34-10-0013")
#' }
#' @export
get_cansim_series_info_cube_coord <- function(cansimTableNumber,coordinates, timeout=1000, refresh=FALSE){

  productId <- naked_ndm_table_number(cansimTableNumber)

  coordinates <- sort(normalize_coordinates(coordinates))

  chuncksize <- 300
  batches = split(coordinates, cumsum((1:length(coordinates)-1)%%chuncksize==0))

  info <- purrr::map_dfr(batches, \(coordinates){
    body <- paste0("{\"productId\": ",productId,", \"coordinate\": \"",coordinates,"\"}") %>%
      paste0(.,collapse=", ") %>%
      paste0("[",.,"]")

    tmp_base <- tempdir()
    tmp <- file.path(tmp_base, paste0("series_coord_info_",productId,"_",digest::digest(body), ".Rda"))

    if (!file.exists(tmp) || refresh) {
      url <- "https://www150.statcan.gc.ca/t1/wds/rest/getSeriesInfoFromCubePidCoord"
      response <- httr::POST(url,
                             body=body,
                             encode="json",
                             httr::add_headers("Content-Type"="application/json"),
                             httr::timeout(timeout)
      )
      if (response$status_code!=200) {
        stop("Problem downloading data, status code ",response$status_code,"\n",httr::content(response))
      }
      data <- httr::content(response)
      data1 <- Filter(function(x)x$status=="SUCCESS",data)
      data2 <- Filter(function(x)x$status!="SUCCESS",data)

      info <- data1 %>%
        purrr::map_df(\(x){
          o <- x$object
          as_tibble(Filter(Negate(is.null),o))
          })

      saveRDS(info, tmp)
    } else {
      info <- readRDS(tmp)
    }
    info
  })

  info  %>%
    filter(responseStatusCode==0) %>%
    select(-"responseStatusCode")
}

#' Retrieve series info for given table id and coordinates
#'
#' Retrieves series information by coordinates
#'
#' @param template A (possibly filtered) cansim table template as returned by `get_cansim_table_template`
#' @param refresh Refresh the data from the Statistics Canada API
#'
#' @return a tibble containing the table template with added vector info
#'
#' @examples
#' \dontrun{
#' template <- get_cansim_table_template("34-10-0013")
#' template |> filter(Geography=="Canada") |>
#'   add_cansim_vectors_to_template()
#' }
#' @export
#'
add_cansim_vectors_to_template <- function(template, refresh=FALSE) {
  cansimTableNumber <- attr(template, "cansimTableNumber")
  if (is.null(cansimTableNumber)) {
    stop("The template does not have a cansimTableNumber attribute")
  }

  if (!"COORDINATE" %in% names(template)) {
    stop("The template does not have a COORDINATE column")
  }

  vector_info <- get_cansim_series_info_cube_coord(cansimTableNumber, template$COORDINATE, refresh=refresh) %>%
    select(COORDINATE="coordinate", VECTOR=vectorId) %>%
    mutate(VECTOR=paste0("v",.data$VECTOR)) %>%
    mutate(COORDINATE=gsub("(.0)+$","",.data$COORDINATE))

  template %>%
    inner_join(vector_info,
               by="COORDINATE") %>%
    relocate("VECTOR", .after="COORDINATE")

}
