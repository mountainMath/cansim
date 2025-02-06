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

  m<-suppressWarnings(setdiff(grep(dimension_id_column,meta[[cube_title_column]]),nrow(meta)))
  m<-NULL

  cut_indices <- setdiff(grep(dimension_id_column,meta[[cube_title_column]]),nrow(meta))
  cut_indices <- c(cut_indices,grep(symbol_legend_grepl_field,meta[[cube_title_column]])) %>% sort()
  meta1 <- meta[seq(1,cut_indices[1]-1),]
  saveRDS(meta1,file=paste0(data_path,"1"))
  names2 <- meta[cut_indices[1],]  %>%
    dplyr::select_if(function(d)sum(!is.na(d)) > 0) %>%
    as.character()
  meta2 <- meta[seq(cut_indices[1]+1,cut_indices[2]-1),seq(1,length(names2))] %>%
    rlang::set_names(names2)
  saveRDS(meta2,file=paste0(data_path,"2"))
  names3 <- meta[cut_indices[2],]  %>%
    dplyr::select_if(function(d)sum(!is.na(d)) > 0) %>%
    as.character()
  meta3 <- meta[seq(cut_indices[2]+1,cut_indices[3]-1),seq(1,length(names3))] %>%
    rlang::set_names(names3)
  saveRDS(meta3,file=paste0(data_path,"2m"))
  correction_index <- grep(correction_id_grepl_field,meta[[cube_title_column]])
  if (length(correction_index)==0) correction_index=nrow(meta)
  additional_indices=c(grep(survey_code_grepl_field,meta[[cube_title_column]]),
                       grep(subject_code_grepl_field,meta[[cube_title_column]]),
                       grep(note_id_grepl_field,meta[[cube_title_column]]),
                       correction_index)
  saveRDS(meta[seq(additional_indices[1]+1,additional_indices[2]-1),c(1,2)] %>%
            rlang::set_names(meta[additional_indices[1],c(1,2)]) ,file=paste0(data_path,"3"))
  saveRDS(meta[seq(additional_indices[2]+1,additional_indices[3]-1),c(1,2)] %>%
            rlang::set_names(meta[additional_indices[2],c(1,2)]) ,file=paste0(data_path,"4"))
  if (length(additional_indices)>3)
    saveRDS(meta[seq(additional_indices[3]+1,additional_indices[4]-1),c(1,2)] %>%
              rlang::set_names(meta[additional_indices[3],c(1,2)]) ,file=paste0(data_path,"5"))

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
      dplyr::select(-.data$p)
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


  if (!file.exists(meta1_path)) {
    m1 <- d %>% tibble::enframe() %>%
      mutate(l=lapply(.data$value,class) %>% unlist()) %>%
      filter(.data$l!="list" | .data$name %in% c("surveyCode","subjectCode")) %>%
      select(-.data$l) %>%
      tidyr::pivot_wider() %>%
      mutate_all(\(x)paste0(unlist(x), collapse=", "))
    saveRDS(m1, meta1_path)
  } else {
    m1 <- readRDS(meta1_path)
  }


  if (!file.exists(meta2_path)) {
    m2 <- d$dimension %>%
      purrr::map_df(\(x){
        tibble::as_tibble(x) %>%
          tidyr::unnest_wider(.data$member)  %>%
          mutate(across(is.integer,as.character))
      })
    saveRDS(m2, meta2_path)
  } else {
    m2 <- readRDS(meta2_path)
  }

  if (!file.exists(meta3_path)) {
    m3 <- d$footnote %>%
      purrr::map_df(\(x){
        tibble::as_tibble(x) %>%
          left_join(as_tibble(.$link),by="footnoteId") %>%
          dplyr::select(-.data$link)  %>%
          mutate(across(is.integer,as.character))
      }) %>%
      unique()
    saveRDS(m3, meta3_path)
  } else {
    m3 <- readRDS(meta3_path)
  }

  if (!file.exists(meta4_path)) {
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



