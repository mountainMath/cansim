
#' Retrieves a Statistics Canada data table using CANSIM code or NDM table number
#'
#' Retrieves a data table as a tidy dataframe using either an old-style CANSIM code or a new-format NDM table number. This function will automatically convert old-style CANSIM codes into their new equivalents. Retrieved table data is cached for the duration of the current R session only by default.
#'
#' Deprecated, use `get_cansim` instead. This will be removed in future releases.
#'
#' @param cansimTableNumber the table number to load, accepts old CANSIM or new NDM table numbers
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (default is set to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#  Set to higher values for large tables and slow network connection. (Default is \code{200}).
#'
#' @return A tibble with the StatCan table data
#'
#' @examples
#' \dontrun{
#' # Retrieve a table with an NDM code
#' get_cansim("34-10-0013")
#' # Retrieve a table with an old-style CANSIM code
#' get_cansim("026-0018")
#' }
#' @export
get_cansim_ndm <- function(cansimTableNumber, language="english", refresh=FALSE, timeout = 200){
  warning("The `get_cansim_ndm` method is deprecated and will be removed in future releases, please use `get_cansim` instead.")
  get_cansim(cleaned_ndm_table_number(cansimTableNumber), language, refresh, timeout = timeout)
}



#' Normalize retrieved data table values to appropriate scales
#'
#' Facilitates working with Statistics Canada data table values retrieved using the package by setting all units to counts/dollars instead of millions, etc. If "replacement_value" is not set, it will replace the \code{VALUE} field with normalized values and drop the \code{scale} column. Otherwise it will keep the scale columns and create a new column named replacement_value with the normalized value. It will attempt to parse the \code{REF_DATE} field and create an R date variable. This is currently experimental.
#'
#' @param data A retrieved data table as returned from \code{get_cansim()} pr \code{get_cansim_ndm()}
#' @param replacement_value (Optional) the name of the column the manipulated value should be returned in. Defaults to replacing the current value field
#' @param normalize_percent (Optional) When \code{true} (the default) normalizes percentages by changing them to rates
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "01")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{false}).
#' @param strip_classification_code (strip_classification_code) Logical value indicating if classification code should be stripped from names. (Default set to \code{false}).
#' @param cansimTableNumber (Optional) Only needed when operating on results of SQLite connections.
#'
#' @return Returns the input tibble with with adjusted values
#'
#' @examples
#' \dontrun{
#' cansim_table <- get_cansim("34-10-0013")
#' normalize_cansim_values(cansim_table)
#' }
#' @export
normalize_cansim_values <- function(data, replacement_value=NA, normalize_percent=TRUE,
                                    default_month="01", default_day="01",
                                    factors=FALSE,strip_classification_code=FALSE,
                                    cansimTableNumber=NULL){
  language <- ifelse("VALEUR" %in% names(data),"fr","en")
  value_string <- ifelse(language=="fr","VALEUR","VALUE")
  scale_string <- ifelse(language=="fr","IDENTIFICATEUR SCALAIRE","SCALAR_ID")
  scale_string2 <- ifelse(language=="fr","FACTEUR SCALAIRE","SCALAR_FACTOR")
  uom_string=ifelse(language=="fr",paste0("UNIT",intToUtf8(0x00C9)," DE MESURE"),"UOM")
  percentage_string=ifelse(language=="fr","^Pourcent","^Percent")
  classification_prefix <- ifelse(language=="fr","Code de classification pour ","Classification Code for ")
  hierarchy_prefix <- ifelse(language=="fr",paste0("Hi",intToUtf8(0x00E9),"rarchie pour "),"Hierarchy for ")
  replacement_value_string = ifelse(is.na(replacement_value),value_string,replacement_value)

  if (!is.null(getOption("cansim.debug"))) message('Normalizing cansim values')

  data <- data %>%
    mutate(!!as.name(replacement_value_string):=!!as.name(value_string)*(`^`(10,as.integer(!!as.name(scale_string)))))
  if (is.na(replacement_value)) { # remove scale columns
    data <- data %>%
      select(-one_of(intersect(c(scale_string,scale_string2),names(data))))
  }
  if (normalize_percent & uom_string %in% names(data)) {
    # divide numbers that are percentages by 100 and convert the unit field to "rate"
    data <- data %>%
      mutate(!!as.name(replacement_value_string):=ifelse(grepl(percentage_string,!!as.name(uom_string)),!!as.name(replacement_value_string)/100,!!as.name(replacement_value_string))) %>%
      mutate(!!as.name(uom_string):=ifelse(!!as.name(uom_string)==percentage_string,"Rate",!!as.name(uom_string)))
  }
  date_field=ifelse(language=="fr",paste0("P",intToUtf8(0x00C9),"RIODE DE R",intToUtf8(0x00C9),"F",intToUtf8(0x00C9),"RENCE"),"REF_DATE")
  sample_date <- data[[date_field]] %>%
    na.omit %>%
    first()
  if (grepl("^\\d{4}$",sample_date)) {
    # year
    data <- data %>%
      mutate(Date=as.Date(paste0(!!as.name(date_field),"-",default_month,"-",default_day)))
  } else if (grepl("^\\d{4}/\\d{4}$",sample_date)) {
    # year range, use second year as anchor
    data <- data %>%
      mutate(Date=as.Date(paste0(gsub("^\\d{4}/","",!!as.name(date_field)),"-",default_month,"-",default_day)))
  } else if (grepl("^\\d{4}-\\d{2}$",sample_date)) {
    # year and month
    data <- data %>% mutate(Date=as.Date(paste0(!!as.name(date_field),"-",default_day)))
  } else if (grepl("^\\d{4}-\\d{2}-\\d{2}$",sample_date)) {
    # year, month and day
    data <- data %>%
      mutate(Date=as.Date(!!as.name(date_field)))
  }

  if (!is.null(cansimTableNumber)) {
    cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
    cleaned_language <- cleaned_ndm_language(language)
    geography_column <- ifelse(cleaned_language=="eng","Geography",paste0("G",intToUtf8(0x00E9),"ographie"))
    base_table <- naked_ndm_table_number(cansimTableNumber)
    path <- paste0(base_path_for_table_language(cansimTableNumber,language),".zip")
    data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda")
    meta2 <- readRDS(paste0(data_path,"2"))
    dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
    fields <- pull(meta2,  dimension_name_column)
    #fields <- setdiff(fields,geography_column)
    data <- fold_in_metadata_for_columns(data,data_path,fields)
    fields <- setdiff(fields,geography_column)
  } else {
    fields= gsub(classification_prefix,"",names(data)[grepl(classification_prefix,names(data))])
  }


  if (strip_classification_code){
    for (field in fields) {
      if (sum(!is.na(data[[paste0(classification_prefix,field)]]))>0) {
        data <- data %>%
          mutate(!!field:=gsub(" \\[.+\\]$","",!!as.name(field)))
      }
    }
  }

  if (factors){
    if (!is.null(getOption("cansim.debug"))) message('Converting to factors')

    parent_hierarchy <- function(hs){
      hs %>%
        strsplit("\\.") %>%
        lapply(function(d)head(d,-1)) %>%
        lapply(function(d)paste0(d,collapse = ".")) %>%
        unlist
    }
    hierarchy_order <- function(hs){
      hs %>%
        strsplit("\\.") %>%
        lapply(function(d)stringr::str_pad(d,20,side="left",pad="0")) %>%
        lapply(function(d)paste0(d,collapse = ".")) %>%
        unlist
    }
    # for (field in fields) {
    #   data <- data %>%
    #     mutate(!!field:=factor(!!as.name(field),levels=levels_for_field(field)))
    # }
    for (field in fields) {
      if (!is.null(getOption("cansim.debug"))) message(paste0('Converting ',field,' to factors'))
      tryCatch({
        hierarchy_field <- paste0(hierarchy_prefix,field)
        parent_field <- paste0("parent ",field)
        levels_data <- data %>%
          select(all_of(c(field,hierarchy_field))) %>%
          unique %>%
          rename(...name = !!as.name(field)) %>%
          mutate(...dupes=.data$...name %in% filter(.,duplicated(.data$...name))$...name) %>%
          mutate(...parent_hierarchy=parent_hierarchy(pull(.,hierarchy_field))) %>%
          mutate(...original_name = .data$...name)

        h <- levels_data[,hierarchy_field]
        if (nrow(h) != nrow(unique(h))) {
           warning(paste0("There is inconsitent naming of categories in column ",field,", cannot convert to factors.\n",
                   "This is likely a problem with StatCan data, proceed with caution when filtering on this field\n",
                   "to ensure data integrity. If this turns out to be a problem with the {cansim} package rather\n",
                   "than with StatCan, or if this problem can't be resolved, please flag this as an issue in the\n",
                   "{cansim} repository at https://github.com/mountainMath/cansim/issues."))
        } else {
          max_iterations <- 20 # failsafe
          while (nrow(levels_data %>%filter(.data$...dupes,.data$...parent_hierarchy!=""))>0 & max_iterations > 0) {
            if (!is.null(getOption("cansim.debug"))) message(paste0('Deduping ',field,' categories'))
            levels_data <- levels_data %>%
              mutate(...parent_hierarchy=parent_hierarchy(pull(.,hierarchy_field))) %>%
              select(setdiff(names(.),parent_field)) %>%
              left_join(select(.,all_of(c("...name",hierarchy_field)))%>%
                          rename(!!!rlang::set_names("...name",parent_field)),
                        by=c(...parent_hierarchy=hierarchy_field)) %>%
              mutate(...name=ifelse(.data$...dupes & .data$...parent_hierarchy != "",
                                    paste0(.data$...original_name," (",!!as.name(parent_field),")"),
                                    .data$...name)) %>%
              mutate(...dupes=.data$...name %in% filter(.,duplicated(.data$...name))$...name) %>%
              mutate(...parent_hierarchy=parent_hierarchy(.data$...parent_hierarchy))
            max_iterations <- max_iterations -1
          }
          if (max_iterations == 0) warning(paste0("Maximum iterations reached for field ",field,"."))
          if (sum(levels_data$...dupes)>0) {
            levels_data <- levels_data %>%
              group_by(.data$...name) %>%
              mutate(n=n(),nn=row_number()) %>%
              mutate(...name=ifelse(n>1,paste0(.data$...name," - ",.data$nn),.data$...name))
          }

          # make sure order of factors is right
          levels_data <- levels_data %>%
            mutate(...h=hierarchy_order(!!as.name(hierarchy_field))) %>%
            arrange(.data$...h)


          data <- data %>%
            select(-all_of(field)) %>%
            left_join(levels_data %>%
                        select(all_of(c(hierarchy_field,"...name"))) %>%
                        rename(!!!rlang::set_names("...name",field)),by=hierarchy_field)

          if (!is.null(getOption("cansim.debug"))) message(paste0('Converting ',field,' values to factor.'))
          data <- data %>%
            mutate_at(field,function(d)factor(d,levels=levels_data$...name))
        }
      },
      error=function(cond){
        warning(paste0("Could not convert field ",field, " to a factor, please flag this as an issue in the {cansim} repository at https://github.com/mountainMath/cansim/issues."))
      },
      warning=function(cond) {
        warning(cond$message)
        #warning(paste0("Encountered a warning when converting field ",field, " to a factor, please flag this as an issue in the {cansim} repository at https://github.com/mountainMath/cansim/issues."))
      })
    }
  }

  data
}

#' Translate deprecated CANSIM table number into new NDM-format table catalogue number
#'
#' Returns NDM table catalogue equivalent given a standard old-format CANSIM table number
#'
#' @param oldCansimTableNumber deprecated style CANSIM table number (e.g. "427-0001")
#'
#' @return A character string with the new-format NDM table number
#'
#' @examples
#' \dontrun{
#' cansim_old_to_new("026-0018")
#' }
#' @export
cansim_old_to_new <- function(oldCansimTableNumber){
  # cache the file as data, old table numbers should not change
  #
  # path <- file.path(tempdir(),"cansim-correspondence.csv")
  # if (!file.exists(path)){
  #   url="https://www.statcan.gc.ca/eng/developers-developpeurs/cansim_id-product_id-concordance.csv"
  #   data <- readr::read_csv(url)
  #   saveRDS(data,file=path)
  # }
  # data <-readRDS(path)
  cleaned_number=sprintf("%07d", as.numeric(sub("-","",as.character(oldCansimTableNumber))))

  new_number <- cansim::correspondence %>%
    filter(.data$CANSIM_ID == as.integer(cleaned_number)) %>%
    pull(.data$PRODUCT_ID)
  if (identical(new_number, integer(0))) {
    stop(paste0("Unable to match old CANSIM table number ",cleaned_number))
  }
  n=as.character(new_number)
  new_table <- paste0(substr(n,1,2),"-",substr(n,3,4),"-",substr(n,5,8))
  new_table
}


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
  exceeded_hierarchy_warning_message <- ifelse(cleaned_language=="eng","Exceeded max depth for hierarchy, hierarchy information may be faulty.",
                                               paste0("Profondeur maximale d",intToUtf8(0x00E9),"pass",intToUtf8(0x00E9),"e pour la hi",intToUtf8(0x00E9),"rarchie, les informations de hi",intToUtf8(0x00E9),"rarchie peuvent ",intToUtf8(0x00EA),"tre erron",intToUtf8(0x00E9),"es."))
  classification_code_prefix <- ifelse(cleaned_language=="eng","Classification Code for","Code de classification pour")
  hierarchy_prefix <- ifelse(cleaned_language=="eng","Hierarchy for",paste0("Hi",intToUtf8(0x00E9),"rarchie pour"))

  m<-suppressWarnings(setdiff(grep(dimension_id_column,meta[[cube_title_column]]),nrow(meta)))
  m<-NULL

  cut_indices <- setdiff(grep(dimension_id_column,meta[[cube_title_column]]),nrow(meta))
  cut_indices <- c(cut_indices,grep(symbol_legend_grepl_field,meta[[cube_title_column]]))
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
  saveRDS(meta[seq(additional_indices[3]+1,additional_indices[4]-1),c(1,2)] %>%
            rlang::set_names(meta[additional_indices[3],c(1,2)]) ,file=paste0(data_path,"5"))

  add_hierarchy <- function(meta_x){
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
    if (added) warning(exceeded_hierarchy_warning_message)
    meta_x
  }
  column_names <- dplyr::pull(meta2,dimension_name_column)
  for (column_index in seq(1:nrow(meta2))) { # iterate through columns for which we have meta data
    column <- meta2[column_index,]
    is_geo_column <- grepl(geography_column,column[[dimension_name_column]]) &  !(column[[dimension_name_column]] %in% column_names)
    meta_x <- meta3 %>%
      dplyr::filter(.data[[dimension_id_column]]==column[[dimension_id_column]]) %>%
      add_hierarchy %>%
      mutate(name=ifelse(is.na(!!as.name(classification_code_column)) | is_geo_column,!!as.name(member_name_column),paste0(!!as.name(member_name_column)," ",!!as.name(classification_code_column))))
    saveRDS(meta_x,file=paste0(data_path,"_column_",column[[dimension_name_column]]))
  }
  NULL
}

#' Fold in metadata and for selected columns
#' @param data A tibble with StatCan table data as e.g. returned by \code{get_cansim}.
#' @param data_path base path to save parsed metadata
#' @param column_names the names of the columns
#'
#' @return A tibble including the metadata information
#' @keywords internal
fold_in_metadata_for_columns <- function(data,data_path,column_names){
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
  exceeded_hierarchy_warning_message <- ifelse(cleaned_language=="eng","Exceeded max depth for hierarchy, hierarchy information may be faulty.",
                                               paste0("Profondeur maximale d",intToUtf8(0x00E9),"pass",intToUtf8(0x00E9),"e pour la hi",intToUtf8(0x00E9),"rarchie, les informations de hi",intToUtf8(0x00E9),"rarchie peuvent ",intToUtf8(0x00EA),"tre erron",intToUtf8(0x00E9),"es."))
  classification_code_prefix <- ifelse(cleaned_language=="eng","Classification Code for","Code de classification pour")
  hierarchy_prefix <- ifelse(cleaned_language=="eng","Hierarchy for",paste0("Hi",intToUtf8(0x00E9),"rarchie pour"))
  coordinate_column <- ifelse(cleaned_language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))


  meta2 <- readRDS(paste0(data_path,"2"))

  if (!is.null(getOption("cansim.debug"))) message('Generating base hierarchy')
  hierarchy_data <- tibble(X=pull(data,coordinate_column) %>% unique) %>%
    setNames(coordinate_column) %>%
    mutate(...pos=strsplit(!!as.name(coordinate_column),"\\."))

  for (column_name in column_names) {
    if (!is.null(getOption("cansim.debug"))) message(paste0("Generating ",column_name," hierarchy"))
    column_index <- which(pull(meta2,dimension_name_column)==column_name)

    column <- meta2[column_index,]
    is_geo_column <- grepl(geography_column,column[[dimension_name_column]]) &  !(column[[dimension_name_column]] %in% names(data))
    meta_x=readRDS(paste0(data_path,"_column_",column[[dimension_name_column]]))

    if (is_geo_column) {
      hierarchy_name <- paste0(hierarchy_prefix," ", data_geography_column)
      join_column <- meta_x %>%
        mutate(GeoUID=gsub("\\[|\\]","",!!as.name(classification_code_column)),
               !!hierarchy_name:=!!as.name(hierarchy_column)) %>%
        select(setdiff(c(member_id_column,"GeoUID",hierarchy_name),names(data)))

      hierarchy_data <- hierarchy_data %>%
        mutate(!!member_id_column:=lapply(.data$...pos,function(d)d[column_index]) %>% unlist) %>%
        dplyr::left_join(join_column,by=member_id_column) %>%
        dplyr::select(-!!as.name(member_id_column))
    } else if (column[[dimension_name_column]] %in% names(data)){
      classification_name <- paste0(classification_code_prefix," ",column[[dimension_name_column]])
      hierarchy_name <- paste0(hierarchy_prefix," ",column[[dimension_name_column]])
      join_column <- meta_x %>%
        mutate(!!classification_name:=!!as.name(classification_code_column),
               !!hierarchy_name:=!!as.name(hierarchy_column)) %>%
        select(setdiff(c(member_id_column,classification_name,hierarchy_name),names(data)))

      hierarchy_data <- hierarchy_data %>%
        mutate(!!member_id_column:=lapply(.data$...pos,function(d)d[column_index]) %>% unlist) %>%
        dplyr::left_join(join_column,by=member_id_column) %>%
        dplyr::select(-!!as.name(member_id_column))
    } else {
      if (cleaned_language=="eng")
        warning(paste0("Don't know how to add metadata for ",column[[dimension_name_column]],"! Ignoring this dimension."))
      else
        warning(paste0("Je ne sais pas comment ajouter des m",intToUtf8(0x00E9),"tadonn",intToUtf8(0x00E9),"es pour ",column[[dimension_name_column]],"! Ignorer cette dimension."))
    }
  }
  if (!is.null(getOption("cansim.debug"))) message('Folding in hierarchy')
  data %>% dplyr::left_join(hierarchy_data %>% dplyr::select(-.data$...pos), by=coordinate_column)
}

#' The correspondence file for old to new StatCan table numbers is included in the package
#'
#' @name correspondence
#' @docType data
#' @author Statistics Canada
#' @references \url{https://www.statcan.gc.ca/eng/developers-developpeurs/cansim_id-product_id-concordance.csv}
#' @keywords data
NULL


#' Retrieve a Statistics Canada data table using NDM catalogue number
#'
#' Retrieves a data table using an NDM catalogue number as a tidy data frame. Retrieved table data is cached for the duration of the current R session only by default.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (defaults to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{TRUE}).
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "07")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#'  Set to higher values for large tables and slow network connection. (Default is \code{200}).
#'
#' @return A tibble with StatCan Table data and added \code{Date} column with inferred date objects and
#' added \code{val_norm} column with normalized value from the \code{VALUE} column.
#'
#' @examples
#' \dontrun{
#' get_cansim("34-10-0013")
#' }
#' @export
get_cansim <- function(cansimTableNumber, language="english", refresh=FALSE, timeout=200,
                       factors=TRUE, default_month="07", default_day="01"){
  cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
  cleaned_language <- cleaned_ndm_language(language)
  base_table <- naked_ndm_table_number(cansimTableNumber)
  path <- paste0(base_path_for_table_language(cansimTableNumber,language),".zip")
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda")
  if (refresh | !file.exists(data_path)){
    if (cleaned_language=="eng")
      message(paste0("Accessing CANSIM NDM product ", cleaned_number, " from Statistics Canada"))
    else
      message(paste0("Acc",intToUtf8(0x00E9),"der au produit ", cleaned_number, " CANSIM NDM de Statistique Canada"))
    url=paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",file_path_for_table_language(cansimTableNumber,language),".zip")
    response <- get_with_timeout_retry(url,path=path,timeout=timeout)
    if (is.null(response)) return(response)
    data <- NA
    na_strings=c("<NA>",NA,"NA","","F")
    exdir=file.path(tempdir(),file_path_for_table_language(cansimTableNumber,language))
    uzp <- getOption("unzip")
    if (is.null(uzp)) uzp <- "internal"
    utils::unzip(path,exdir=exdir,unzip=uzp)
    unlink(path)
    if(cleaned_language=="eng") {
      message("Parsing data")
      csv_reader <- readr::read_csv
      value_column="VALUE"
    } else {
      message(paste0("Analyser les donn",intToUtf8(0x00E9),"es"))
      csv_reader <- readr::read_csv2
      value_column="VALEUR"
    }

    data <- csv_reader(file.path(exdir, paste0(base_table, ".csv")),
                       na=na_strings,
                       locale=readr::locale(encoding="UTF-8"),
                       col_types = list(.default = "c")) %>%
      dplyr::mutate(!!value_column:=as.numeric(.data[[value_column]]))


    meta <- suppressWarnings(csv_reader(file.path(exdir, paste0(base_table, "_MetaData.csv")),
                                             na=na_strings,
                                             #col_names=FALSE,
                                             locale=readr::locale(encoding="UTF-8"),
                                             col_types = list(.default = "c")))
    tryCatch({
      #data <- parse_and_fold_in_metadata(data,meta,data_path)
      parse_metadata(meta,data_path)
      meta2 <- readRDS(paste0(data_path,"2"))
      dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
      data <- fold_in_metadata_for_columns(data,data_path,pull(meta2,dimension_name_column))
    }, error = function(e) {
      warning("Could not fold in metadata")
      if (nrow(data)==0) warning(paste0("StatCan returned zero rows of data for table ",cleaned_number,
                                        ", this is likely a problem with StatCan."))
    })

    if (!is.null(getOption("cansim.debug"))) message('saving data')
    saveRDS(data,file=data_path)
    unlink(exdir,recursive = TRUE)
  } else {
    if (cleaned_language=="eng")
      message(paste0("Reading CANSIM NDM product ",cleaned_number)," from cache.")
    else
      message(paste0("Lecture du produit ",cleaned_number)," de CANSIM NDM ",intToUtf8(0x00E0)," partir du cache.")
    data <- readRDS(file=data_path)
  }

  if (!is.null(getOption("cansim.debug"))) message('Initiating normalization')
  data  %>%
    normalize_cansim_values(replacement_value = "val_norm", factors = factors,
                            default_month = default_month, default_day = default_day)
}

#' Retrieve Statistics Canada data table information
#'
#' Returns table information given an NDM table catalogue number in English or French. Retrieved table information data is cached for the duration of the R session only.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (default set to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#  Set to higher values for large tables and slow network connection. (Default is \code{200}).
#'
#' @return A tibble with the table overview information
#'
#' @examples
#' \dontrun{
#' get_cansim_table_info("34-10-0013")
#' }
#' @export
get_cansim_table_info <- function(cansimTableNumber, language="english", refresh=FALSE, timeout=200){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda1")
  if (refresh | !file.exists(data_path)){
    get_cansim(cansimTableNumber,language=language,refresh = refresh,timeout=timeout)
  }
  readRDS(file=data_path)
}


#' Retrieve Statistics Canada data table survey detail
#'
#' Returns table survey detail given an NDM table number in English or French. Retrieved table information data is cached for the duration of the R session only.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (default set to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#  Set to higher values for large tables and slow network connection. (Default is \code{200}).
#'
#' @return A tibble with the table survey code and name
#'
#' @examples
#' \dontrun{
#' get_cansim_table_survey("34-10-0013")
#' }
#' @export
get_cansim_table_survey <- function(cansimTableNumber, language="english", refresh=FALSE, timeout=200){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda3")
  if (refresh | !file.exists(data_path)){
    get_cansim(cansimTableNumber,language=language,refresh = refresh, timeout = timeout)
  }
  readRDS(file=data_path)
}

#' Retrieve Statistics Canada data table subject detail
#'
#' Returns table subject detail given an NDM table number in English or French. Retrieved table information data is cached for the duration of the R session only.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (default set to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#  Set to higher values for large tables and slow network connection. (Default is \code{200}).
#'
#' @return A tibble with the table subject code and name.
#'
#' @examples
#' \dontrun{
#' get_cansim_table_subject("34-10-0013")
#' }
#' @export
get_cansim_table_subject <- function(cansimTableNumber, language="english", refresh=FALSE, timeout = 200){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda4")
  if (refresh | !file.exists(data_path)){
    get_cansim(cansimTableNumber,language=language,refresh = refresh, timeout = timeout)
  }
  readRDS(file=data_path)
}

#' Retrieve Statistics Canada data table short notes
#'
#' Returns table notes given an NDM table number in English or French. Retrieved table information data is cached for the duration of the R session only.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (default set to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#  Set to higher values for large tables and slow network connection. (Default is \code{200}).
#'
#' @return A tibble with the StatCan Notes for the table
#'
#' @examples
#' \dontrun{
#' get_cansim_table_short_notes("34-10-0013")
#' }
#' @export
get_cansim_table_short_notes <- function(cansimTableNumber, language="english", refresh=FALSE, timeout = 200){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda5")
  if (refresh | !file.exists(data_path)){
    get_cansim(cansimTableNumber,language=language,refresh = refresh, timeout = timeout)
  }
  readRDS(file=data_path)
}

#' Retrieve Statistics Canada data table column list
#'
#' Returns table column details given an NDM table number in English or French. Retrieved table information data is cached for the duration of the R session only.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (default set to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#  Set to higher values for large tables and slow network connection. (Default is \code{200}).
#'
#' @return A tibble listing the column names of the StatCan table.
#'
#' @examples
#' \dontrun{
#' get_cansim_column_list("34-10-0013")
#' }
#' @export
get_cansim_column_list <- function(cansimTableNumber, language="english", refresh=FALSE, timeout= 200){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda2")
  if (refresh | !file.exists(data_path)){
    get_cansim(cansimTableNumber,language=language,refresh = refresh, timeout = timeout)
  }
  readRDS(file=data_path)
}

#' Retrieve Statistics Canada data table categories for a specific column
#'
#' Returns table column details given an NDM table number in English or French. Retrieved table information data is cached for the duration of the R session only.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param column the specified column for which to retrieve category information for
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (default set to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#  Set to higher values for large tables and slow network connection. (Default is \code{200}).
#'
#' @return A tibble with detailed information on StatCan table categories for the specified field
#'
#' @examples
#' \dontrun{
#' get_cansim_column_categories("34-10-0013", "Geography")
#' }
#' @export
get_cansim_column_categories <- function(cansimTableNumber, column, language="english", refresh=FALSE, timeout = 200){
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda2")
  if (refresh | !file.exists(data_path)){
    get_cansim(cansimTableNumber,language=language,refresh = refresh, timeout = 200)
  }
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda_column_",column)
  if (!file.exists(data_path)){
    stop(paste0("Unkown column ",column))
  }
  readRDS(file=data_path)
}

#' Retrieve Statistics Canada data table overview text
#'
#' Prints table overview information as console output. In order to display table overview information, the selected CANSIM table must be loaded entirely to display overview information. Overview information is printed in console an in English or French, as specified.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (default set to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#'
#' @return none
#'
#' @examples
#' \dontrun{
#' get_cansim_table_overview("34-10-0013")
#' }
#' @export
get_cansim_table_overview <- function(cansimTableNumber, language="english", refresh=FALSE){
  cansimTableNumber <- cleaned_ndm_table_number(cansimTableNumber)
  info <- cansim::get_cansim_table_info(cansimTableNumber,language=language,refresh=refresh)
  #refresh=FALSE
  cleaned_language <- cleaned_ndm_language(language)
  cube_title_column <- ifelse(cleaned_language=="eng","Cube Title","Titre du cube")
  start_period_column <- ifelse(cleaned_language=="eng","Start Reference Period",paste0("D",intToUtf8(0x00E9),"but de la p",intToUtf8(0x00E9),"riode de r",intToUtf8(0x00E9),"f",intToUtf8(0x00E9),"rence"))
  end_period_column <- ifelse(cleaned_language=="eng","End Reference Period",paste0("Fin de la p",intToUtf8(0x00E9),"riode de r",intToUtf8(0x00E9),"f",intToUtf8(0x00E9),"rence"))
  frequency_column <- ifelse(cleaned_language=="eng","Frequency",paste0("Fr",intToUtf8(0x00E9),"quence"))
  dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
  member_name_column <- ifelse(cleaned_language=="eng","Member Name","Nom du membre")

  text <- paste0(info[[cube_title_column]],"\n","CANSIM Table ",cleaned_ndm_table_number(cansimTableNumber),"\n",
                 start_period_column,": ",info[[start_period_column]],", ",
                 end_period_column,": ",info[[end_period_column]],", ",
                 frequency_column,": ",info[[frequency_column]],"\n")
  columns <- get_cansim_column_list(cansimTableNumber,language=language,refresh=refresh)
  for (column in columns[[dimension_name_column]]) {
    text <- paste0(text,"\n","Column ",column)
    categories <- get_cansim_column_categories(cansimTableNumber,column,language=language,refresh=refresh)
    text <- paste0(text, " (",nrow(categories),")","\n")
    text <- paste0(text, paste(utils::head(categories[[member_name_column]],10),collapse=", "))
    if (nrow(categories)>10) text <- paste0(text, ", ...")
    text <- paste0(text,"\n")
  }
  message(text)
}

#' Use metadata to extract categories for column of specific level
#'
#' For tables with data with hierarchical categories, metadata containing hierarchy level descriptions is used to extract categories at a specified level of hierarchy only.
#'
#' @param data data table object as returned from \code{get_cansim()}
#' @param column_name the quoted name of the column to extract categories from
#' @param level the hierarchy level depth to which to extract categories, where 0 is top category
#' @param strict (default \code{FALSE}) when \code{TRUE} will only extract that specific hierarchy level
#' @param remove_duplicates (default \code{TRUE}) When set to \code{TRUE} higher level grouping categories already captured by lower level hierarchy data will be removed
#'
#' @return A vector of categories
#'
#' @examples
#' \dontrun{
#' data <- get_cansim("16-10-0117")
#' categories_for_level(data,"North American Industry Classification System (NAICS)",level=2)
#' }
#' @export
categories_for_level <- function(data,column_name, level=NA, strict=FALSE, remove_duplicates=TRUE){
  hierarchy_name=paste0("Hierarchy for ",column_name)
  h <- data %>% dplyr::select(column_name,hierarchy_name) %>%
    unique %>%
    dplyr::mutate(hierarchy_level=(strsplit(!!as.name(hierarchy_name),"\\.") %>% purrr::map(length) %>% unlist)-1)
  max_level=max(h$hierarchy_level,na.rm = TRUE)
  if (is.na(level) | level>max_level) level=max_level
  h <- h %>%
    dplyr::mutate(`Member ID`=strsplit(!!as.name(hierarchy_name),"\\.") %>% purrr::map(last) %>% as.integer) %>%
    dplyr::filter(.data$hierarchy_level<=level)
  #strict_hierarchy=h %>% dplyr::filter(.data$hierarchy_level==level) %>% dplyr::pull(hierarchy_name) %>% unique
  if (strict) {
    h <- h %>% dplyr::filter(.data$hierarchy_level==level)
  } else if (remove_duplicates) {
    higher_ids <- h %>% pull(hierarchy_name) %>% #strict_hierarchy %>%
      as.character() %>%
      strsplit("\\.") %>%
      purrr::map(function(x){utils::head(as.integer(x),-1)}) %>%
      unlist %>% unique() %>% as.integer()
    h <- h %>% dplyr::filter(!(.data$`Member ID` %in% higher_ids))
  }
  h[[column_name]] %>% as.character()
}


generate_table_metadata <- function(){
  url_for_page <-function(page){paste0("https://www150.statcan.gc.ca/n1/en/type/data?p=",page,"-data/tables#tables")}
  parse_table_data <- function(item){
    product <- item %>%
      html_node(".ndm-result-productid") %>%
      html_text() %>%
      sub("^Table: ","", .data)
    if (grepl("^\\d{2}-\\d{2}-\\d{4}",product)) {
      result = tibble(
        title=item %>%
          html_node(".ndm-result-title") %>%
          html_text() %>% sub("^(\\d|,)+\\. ","", .data),
        table=product,
        former = item %>%
          html_node(".ndm-result-formerid") %>%
          html_text() %>% trimws %>%
          gsub("^\\(formerly: CANSIM |\\)$","", .data),
        geo = item %>%
          html_node(".ndm-result-geo") %>%
          html_text() %>%
          sub("Geography: ","", .data),
        description = item %>%
          html_node(".ndm-result-description") %>%
          html_text() %>%
          sub("Description: ","", .data),
        release_date = item %>%
          html_node(".ndm-result-date .ndm-result-date") %>%
          html_text() %>%
          as.Date()
      )
    } else {
      result=tibble()
    }
    result
  }
  p <- (xml2::read_html(url_for_page(0)) %>%
          html_nodes(".pagination"))[2]
  l <- p %>% html_nodes("li")
  max_page = (l[length(l)-1] %>%
                html_node("a") %>%
                html_text() %>%
                stringr::str_extract(.data,"^(\\d+)") %>%
                as.integer)-1
  pb <- utils::txtProgressBar(0,max_page)
  bind_rows(lapply(seq(0,max_page),function(page){
    utils::setTxtProgressBar(pb, page)
    p <- xml2::read_html(url_for_page(page))
    l <- p %>%
      html_nodes("#ndm-results #tables .ndm-item") %>%
      purrr::map(parse_table_data) %>%
      bind_rows
  }))
}


#' View CANSIM table information in browser
#'
#' Opens CANSIM table on Statistics Canada's website using default browser. This may be useful for getting further info on CANSIM table and survey methods.
#'
#' @param cansimTableNumber CANSIM or NDM table number. If no number is provided, the vector search
#' page on the Statistic Canada website will be opened.
#'
#' @return none
#'
#' @examples
#' \dontrun{
#' view_cansim_webpage("34-10-0013")
#' }
#' @export
view_cansim_webpage <- function(cansimTableNumber = NULL){
  browser <- getOption("browser")

  if (is.null(cansimTableNumber)) {
    url <- 'https://www150.statcan.gc.ca/t1/tbl1/en/sbv.action#tables'
  } else {
    cansimTableNumber <- paste0(gsub("-","",cleaned_ndm_table_number(cansimTableNumber)),"01")
    url <- paste0("https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=",gsub("-","",cansimTableNumber))
  }

  utils::browseURL(url,browser)
}

#' Retrieve table metadata from Statistics Canada API
#'
#' Retrieves table metadata given an input table number or vector of table numbers using either the new or old table number format. Patience is suggested as the Statistics Canada API can be very slow. The `list_cansim_tables()` function can be used as an alternative to retrieve a (cached) list of CANSIM tables with (more limited) metadata.
#'
#' @param cansimTableNumber A new or old CANSIM/NDM table number or a vector of table numbers
#'
#' @return a tibble containing the table metadata
#'
#' @examples
#' \dontrun{
#' get_cansim_cube_metadata("34-10-0013")
#' }
#' @export
get_cansim_cube_metadata <- function(cansimTableNumber){
  table_id=naked_ndm_table_number(cansimTableNumber)
  url="https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata"
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
  fields <- c("productId", "cansimId", "cubeTitleEn", "cubeTitleFr", "cubeStartDate", "cubeEndDate", "nbSeriesCube",
              "nbDatapointsCube",  "archiveStatusCode", "archiveStatusEn",   "archiveStatusFr",   "subjectCode",
              "surveyCode",  "dimension","releaseTime")
  l <- lapply(fields, function(field){
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
  l
}

#' Retrieve a Statistics Canada data table URL given a table number
#'
#' Retrieve URL of a table from the API given a table number. Offers a more stable approach than manually guessing the URL of the table.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (defaults to English)
#'
#' @return String object containing URL for specified table number
#'
#' @examples
#' \dontrun{
#' get_cansim_table_url("34-10-0013")
#' get_cansim_table_url("34-10-0013", language = "fr")
#' }
#' @export
get_cansim_table_url <- function(cansimTableNumber, language = "en"){
  l <- cleaned_ndm_language(language) %>% substr(1,2)
  url=paste0("https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/",naked_ndm_table_number(cansimTableNumber),"/",l)
  response <- httr::GET(url)
  if (response$status_code!=200) {
    stop("Problem downloading data, status code ",response$status_code,"\n",httr::content(response))
  }
  httr::content(response)$object
}

#' Retrieve a list of modified tables since a given date
#'
#' Retrieve a list of tables that have been modified or updated since the specified date.
#'
#' @param start_date Starting date in \code{YYYY-MM-DD} format to look for changes that changed on or after that date
#' @param end_date Optional end date in \code{YYYY-MM-DD} format to look for changes that changed on or before that date,
#' default is same as start date
#'
#' @return A tibble with Statistics Canada data table product ids and their release times
#'
#' @examples
#' \dontrun{
#' get_cansim_changed_tables("2018-08-01")
#' }
#' @export
get_cansim_changed_tables <- function(start_date,end_date=NULL){
  last_available_date <- Sys.Date()
  if (Sys.time()<as.POSIXct(paste0(Sys.Date()," 09:00:00"),tz="America/Toronto")) {
    last_available_date = last_available_date  -1
  }
  if (start_date>last_available_date) {
    stop(paste0("Last available date is ",last_available_date,", please try with a start date on or before that date."))
  }
  if (is.null(end_date)) end_date=start_date
  if (as.Date(end_date) > last_available_date) {
    message(paste0("Capping end date to last available date ",last_available_date,"."))
    end_date=last_available_date
  }
  if (start_date>end_date) {
    message("End date is earlier than start date, switching the order.")
    d <-start_date
    start_date <- end_date
    end_date <-d
  }
  if (difftime(end_date,start_date,"days")>31) {
    message("Querying for long time intervals may be slow.")
  }
  seq(as.Date(start_date),as.Date(end_date),"days") %>%
    lapply(function(date){
      url=paste0("https://www150.statcan.gc.ca/t1/wds/rest/getChangedCubeList/",strftime(date,"%Y-%m-%d"))
      response <- httr::GET(url)
      if (response$status_code!=200) {
        stop("Problem downloading data, status code ",response$status_code,"\n",httr::content(response))
      }
      httr::content(response)$object %>%
        map(function(o)tibble(productId=o$productId,releaseTime=o$releaseTime)) %>%
        bind_rows
    }) %>%
    bind_rows
}


#' Retrieve Statistics Canada data table notes and column categories
#'
#' Returns table notes given an NDM table number in English or French. Retrieved table information data is cached for the duration of the R session only.
#'
#' @param cansimTableNumber the NDM table number to load
#' @param language \code{"en"} or \code{"english"} for English and \code{"fr"} or \code{"french"} for French language versions (default set to English)
#' @param refresh (Optional) When set to \code{TRUE}, forces a reload of data table (default is \code{FALSE})
#' @param timeout (Optional) Timeout in seconds for downloading cansim table to work around scenarios where StatCan servers drop the network connection.
#  Set to higher values for large tables and slow network connection. (Default is \code{200}).
#' @return A tibble with table notes.
#'
#' @examples
#' \dontrun{
#' get_cansim_table_notes("34-10-0013")
#' }
#' @export
get_cansim_table_notes <- function(cansimTableNumber,language="en",refresh=FALSE, timeout = 200) {
  cleaned_language <- cleaned_ndm_language(language)
  dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
  dimenion_note_column <- ifelse(cleaned_language=="eng","Dimension Notes","Notes sur la dimension")
  member_name_column <- ifelse(cleaned_language=="eng","Member Name","Nom du membre")
  member_note_column <- ifelse(cleaned_language=="eng","Member Notes","Notes sur le membre")
  note_id_column <- ifelse(cleaned_language=="eng","Note ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification de la note"))
  notes <- get_cansim_table_short_notes(cansimTableNumber,language=language,refresh=refresh,timeout=timeout)
  columns <- get_cansim_column_list(cansimTableNumber,language=language)
  full_notes <- columns %>%
    select(!!as.name(dimension_name_column),!!note_id_column:=!!as.name(dimenion_note_column)) %>%
    bind_rows(
      pull(.,dimension_name_column) %>% lapply(function(c) {
        get_cansim_column_categories(cansimTableNumber,column=c,language = language) %>%
          mutate(!!dimension_name_column:=c) %>%
          select(!!as.name(dimension_name_column),!!as.name(member_name_column),
                 !!note_id_column:=!!as.name(member_note_column))
      }) %>%
        bind_rows) %>%
    filter(!is.na(!!as.name(note_id_column))) %>%
    full_join(notes,by=note_id_column) %>%
    arrange(!!as.name(note_id_column))
  full_notes
}


#' Get the latest release data for a StatCan table, if available
#'
#' This can be used to check when a table has last been updated.
#'
#' @param cansimTableNumber the NDM table number
#' @return A datatime object if a release data is available, NULL otherwise.
#'
#' @examples
#' \dontrun{
#' get_cansim_table_last_release_date("34-10-0013")
#' }
#' @export
get_cansim_table_last_release_date <- function(cansimTableNumber){
  cansimTableNumber <- cleaned_ndm_table_number(cansimTableNumber)
  pid <- paste0(naked_ndm_table_number(cansimTableNumber),"01")
  url <- "https://www150.statcan.gc.ca/n1/en/metadata.json"
  response <- purrr::safely(httr::GET)(url,query=list(productid=pid))
  if (!is.null(response$error) || response$result$status_code!=200) {
    warning(paste0("Could not access information for table ",cansimTableNumber,
                   " (productID: ",pid,").\n",
                   response$error))
    release_date <- NULL
  } else {
    c <- httr::content(response$result)
    r<-c$result
    if (length(r)>0) {
      rd <- unique(unlist(lapply(r,function(rr)rr$releasedate)))
      release_date <- strptime(rd,format = STATCAN_TIME_FORMAT,tz="UTC") %>%
        max()
    } else {
      release_date <- NULL
    }
  }
  release_date
  #get_cansim_cube_metadata(cansimTableNumber) %>% pull(releaseTime)
}


#' @import dplyr
#' @importFrom tibble as.tibble
#' @importFrom rvest html_node
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom rlang set_names
#' @importFrom purrr map
#' @importFrom rlang :=
#' @importFrom stats setNames
#' @importFrom utils head

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))



