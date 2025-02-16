#' Normalize retrieved data table values to appropriate scales
#'
#' Facilitates working with Statistics Canada data table values retrieved using the package by setting all units to counts/dollars instead of millions, etc. If "replacement_value" is not set, it will replace the \code{VALUE} field with normalized values and drop the \code{scale} column. Otherwise it will keep the scale columns and create a new column named replacement_value with the normalized value. It will attempt to parse the \code{REF_DATE} field and create an R date variable. This is currently experimental.
#'
#' @param data A retrieved data table as returned from \code{get_cansim()} pr \code{get_cansim_ndm()}
#' @param replacement_value (Optional) the name of the column the manipulated value should be returned in. Defaults to "val_norm"
#' @param normalize_percent (Optional) When \code{TRUE} (the default) normalizes percentages by changing them to rates
#' @param default_month The default month that should be used when creating Date objects for annual data (default set to "01")
#' @param default_day The default day of the month that should be used when creating Date objects for monthly data (default set to "01")
#' @param factors (Optional) Logical value indicating if dimensions should be converted to factors. (Default set to \code{TRUE}).
#' @param strip_classification_code (strip_classification_code) Logical value indicating if classification code should be stripped from names. (Default set to \code{false}).
#' @param cansimTableNumber (Optional) Only needed when operating on results of SQLite connections.
#'
#' @return Returns a tibble with with adjusted values.
#'
#' @examples
#' \dontrun{
#' cansim_table <- get_cansim("34-10-0013")
#' normalize_cansim_values(cansim_table)
#' }
#' @export
normalize_cansim_values <- function(data, replacement_value="val_norm", normalize_percent=TRUE,
                                    default_month="01", default_day="01",
                                    factors=TRUE,strip_classification_code=FALSE,
                                    cansimTableNumber=NULL){

  language <- attr(data,"language")
  if (is.null(cansimTableNumber)) {
    cansimTableNumber <- attr(data,"cansimTableNumber")
  }

  if (is.null(language)) {
    language <- ifelse("VALEUR" %in% names(data),"fra","eng")
  }
  value_string <- ifelse(language=="fra","VALEUR","VALUE")
  scale_string <- ifelse(language=="fra","IDENTIFICATEUR SCALAIRE","SCALAR_ID")
  scale_string2 <- ifelse(language=="fra","FACTEUR SCALAIRE","SCALAR_FACTOR")
  uom_string=ifelse(language=="fra",paste0("UNIT",intToUtf8(0x00C9)," DE MESURE"),"UOM")
  percentage_string=ifelse(language=="fra","^Pourcent","^Percent")
  classification_prefix <- ifelse(language=="fra","Code de classification pour ","Classification Code for ")
  hierarchy_prefix <- ifelse(language=="fra",paste0("Hi",intToUtf8(0x00E9),"rarchie pour "),"Hierarchy for ")
  replacement_value_string = ifelse(is.na(replacement_value),value_string,replacement_value)
  coordinate_column <- ifelse(language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))

  trad_cansim <- scale_string %in% names(data)

  if (!is.null(getOption("cansim.debug"))) message('Normalizing cansim values')

  if (nrow(data)==0) {
    message("No data, try to adjust filters.")
    return (data)
  }

  data <- data |> as_tibble()

  attr(data,"cansimTableNumber") <- cansimTableNumber
  attr(data,"language") <- language


  if (trad_cansim) {
    data <- data %>%
      mutate(!!as.name(replacement_value_string):=!!as.name(value_string)*(`^`(10,as.integer(!!as.name(scale_string)))))
  }
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


  date_field=ifelse(language=="fra",paste0("P",intToUtf8(0x00C9),"RIODE DE R",intToUtf8(0x00C9),"F",intToUtf8(0x00C9),"RENCE"),"REF_DATE")

  sample_date <- data[1:10,date_field] |> pull(date_field) |> na.omit() |> first()
  if (is.na(sample_date)) {
    sample_date <- pull(date_field) |> na.omit() |> first()

  }
  # sample_date <- data[[date_field]] %>%
  #   na.omit %>%
  #   first()


  if (!trad_cansim) {
    # do nothing
  } else if (grepl("^\\d{4}$",sample_date)) {
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

  cansimTableNumber <- cleaned_ndm_table_number(cansimTableNumber)
  cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
  cleaned_language <- cleaned_ndm_language(language)
  geography_columns <- case_when(cleaned_language=="eng" ~
                              c("Geography","Geographic name","Geography of origin"),
                              TRUE ~ c(paste0("G",intToUtf8(0x00E9),"ographie"),
                                paste0("Nom g",intToUtf8(0x00E9),"ographique"),
                                paste0("G",intToUtf8(0x00E9),"ographie d'origine")))

  base_table <- naked_ndm_table_number(cansimTableNumber)
  path <- paste0(base_path_for_table_language(cansimTableNumber,language),".zip")
  data_path <- paste0(base_path_for_table_language(cansimTableNumber,language),".Rda")
  if (!is.null(cansimTableNumber) && file.exists(paste0(data_path,"2"))) {
    meta2 <- readRDS(paste0(data_path,"2"))
    dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
    fields <- pull(meta2,  dimension_name_column)
    #fields <- setdiff(fields,geography_column)
    data <- fold_in_metadata_for_columns(data,data_path,fields)
    #fields <- setdiff(fields,geography_columns)
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

    for (field in fields) {
      if (!is.null(getOption("cansim.debug"))) message(paste0('Converting ',field,' to factors'))
      tryCatch({
        level_table <- get_deduped_column_level_data(cansimTableNumber = cansimTableNumber,language=language,column=field)
        if (!(field %in% names(data))) {
          geography_column <- ifelse(cleaned_language=="eng","Geography|Geographic name",paste0("G",intToUtf8(0x00E9),"ographie|Nom g",intToUtf8(0x00E9),"ographique"))
          data_geography_column <- ifelse(language=="eng","GEO",paste0("G",intToUtf8(0x00C9),"O"))
          if ((grepl(geography_column,field) | field %in% geography_columns) &&
              data_geography_column %in% names(data)) {
            field=data_geography_column
          }
        }

        dimension_id <- level_table$...dim |> unique() |> as.integer()

        if (sum(!level_table$...original)>0) {
          # need to rename data

          column_position <- which(names(data)==field)
          column_before <- names(data)[column_position-1]

          data$`...id` <- stringr::str_split(data[[coordinate_column]],"\\.") |> lapply(\(x)x[dimension_id]) |> unlist()

          data <- data |>
            select(-all_of(field)) |>
            left_join(level_table |> select("...id","...name") |> rename(!!field:="...name"),by="...id") |>
            relocate(!!as.name(field),.after=!!as.name(column_before)) |>
            select(-"...id")
        }

        h <- level_table$...name
        if (length(h) != length(unique(h))) {
          warning(paste0("There is inconsitent naming of categories in column ",field,", cannot convert to factors.\n",
                         "This is likely a problem with StatCan data, proceed with caution when filtering on this field\n",
                         "to ensure data integrity. If this turns out to be a problem with the {cansim} package rather\n",
                         "than with StatCan, or if this problem can't be resolved, please flag this as an issue in the\n",
                         "{cansim} repository at https://github.com/mountainMath/cansim/issues."))
        } else {
          data <- data |>
            mutate(!!field:=factor(!!as.name(field),levels=level_table$...name))
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

  # column order
  if (replacement_value_string != value_string) {
    data <- data %>%
      relocate(!!as.name(replacement_value_string),.after=!!as.name(value_string))
  }
  if ("GeoUID" %in% names(data) && "DGUID" %in% names(data)) {
    data <- data %>%
      relocate("GeoUID",.after="DGUID")
  }
  if ("Date" %in% names(data) && "REF_DATE" %in% names(data)) {
    data <- data %>%
      relocate("Date",.after="REF_DATE")
  }


  data |>
    standardize_cansim_column_order()
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
  geography_column <- ifelse(cleaned_language=="eng","Geography|Geographic name",paste0("G",intToUtf8(0x00E9),"ographie|Nom g",intToUtf8(0x00E9),"ographique"))
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
    #column_index <- which(pull(meta2,dimension_name_column)==column_name)
    column_index <- meta2 %>%
      filter(!!as.name(dimension_name_column)==column_name) %>%
      pull(!!as.name(dimension_id_column)) %>%
      as.integer()

    is_geo_column <- grepl(geography_column,column_name) &  !(column_name %in% names(data))
    if (length(column_index)==0) { # failsafe
      column_index <- which(grepl(geography_column,pull(meta2,dimension_name_column))) %>% first()
    }

    if (length(column_index)!=1) { # failsafe
      warning("Problem with column index, trying to find column index by order in metadata. This may be a problem with the cansim package, please flag this on the {cansim} repository at https://github.com/mountainMath/cansim/issues.")
      column_index <- which(pull(meta2,dimension_name_column)==column_name)
    }

    column <- meta2 %>% filter(!!as.name(dimension_id_column)==column_index)
    # is_geo_column <- grepl(geography_column,column[[dimension_name_column]]) &  !(column[[dimension_name_column]] %in% names(data))
    meta_x=readRDS(paste0(data_path,"_column_",column_index))

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
  data %>% dplyr::left_join(hierarchy_data %>% dplyr::select(-"...pos"), by=coordinate_column)
}

#' The correspondence file for old to new StatCan table numbers is included in the package
#'
#' @name correspondence
#' @docType data
#' @author Statistics Canada
#' @references \url{https://www.statcan.gc.ca/eng/developers-developpeurs/cansim_id-product_id-concordance.csv}
#' @keywords data internal
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
  base_table <- naked_ndm_table_number(cleaned_number)
  path <- paste0(base_path_for_table_language(cleaned_number,language),".zip")
  data_path <- paste0(base_path_for_table_language(cleaned_number,language),".Rda")
  if (refresh | !file.exists(data_path)){
    if (cleaned_language=="eng")
      message(paste0("Accessing CANSIM NDM product ", cleaned_number, " from Statistics Canada"))
    else
      message(paste0("Acc",intToUtf8(0x00E9),"der au produit ", cleaned_number, " CANSIM NDM de Statistique Canada"))
    url=paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",file_path_for_table_language(cleaned_number,language),".zip")
    response <- get_with_timeout_retry(url,path=path,timeout=timeout)
    if (is.null(response)) return(response)
    data <- NA
    na_strings=c("<NA>",NA,"NA","","F")
    exdir=file.path(tempdir(),file_path_for_table_language(cleaned_number,language))
    uzp <- getOption("unzip")
    if (is.null(uzp)) uzp <- "internal"
    utils::unzip(path,exdir=exdir,unzip=uzp)
    unlink(path)
    if(cleaned_language=="eng") {
      message("Parsing data")
      csv_reader <- readr::read_csv
      value_column <- "VALUE"
    } else {
      message(paste0("Analyser les donn",intToUtf8(0x00E9),"es"))
      csv_reader <- readr::read_csv2
      value_column <- "VALEUR"
    }

    header <- csv_reader(file.path(exdir, paste0(base_table, ".csv")), n_max=1,
                                 na=na_strings,
                                 locale=readr::locale(encoding="UTF-8",
                                                      decimal_mark = ",",
                                                      grouping_mark = "."),
                                 col_types = list(.default = "c"),
                                 col_names = FALSE) %>%
      as.character()

    symbols <- which(header=="Symbol")
    if (length(symbols)==0) {
      symbols <- which(header=="Symbols"|header=="Symboles")
    }

    if (length(symbols)>1) {
      header[symbols] <- paste0("Symbol ",seq(1,length(symbols)))
    }

    coordinate_column <- ifelse(cleaned_language=="eng","COORDINATE",paste0("COORDONN",intToUtf8(0x00C9),"ES"))
    if (!(coordinate_column %in% header)) {
      ci <- which(grepl(coordinate_column,header,ignore.case = TRUE))
      if (length(ci)==0 && (paste0("Coordonn",intToUtf8(0x00E9),"es") %in% header | paste0("Coordonn",intToUtf8(0x00E9),"e") %in% header)) {
        ci <- which(header==paste0("Coordonn",intToUtf8(0x00E9),"es") | header==paste0("Coordonn",intToUtf8(0x00E9),"e"))
      }

      if (length(ci)==1) {
        header[ci] <- coordinate_column
      }
    }


    data <- csv_reader(file.path(exdir, paste0(base_table, ".csv")),
                       na=na_strings,
                       locale=readr::locale(encoding="UTF-8",
                                            decimal_mark = ",",
                                            grouping_mark = "."),
                       col_types = list(.default = "c"),
                       skip=1,
                       col_names = header)

    attr(data,"language") <- cleaned_language
    attr(data,"cansimTableNumber") <- cleaned_number

    data <- data %>% transform_value_column(value_column)


    meta_lines <- readr::read_lines(file.path(exdir, paste0(base_table, "_MetaData.csv")),
                                    locale=readr::locale(encoding="UTF-8"))

    tryCatch({
      parse_metadata(meta_lines,data_path)
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
  data <- data  %>%
    normalize_cansim_values(replacement_value = "val_norm", factors = factors,
                            default_month = default_month, default_day = default_day)

  data
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
  cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
  data_path <- paste0(base_path_for_table_language(cleaned_number,language),".Rda1")
  if (!refresh && file.exists(data_path)){
    message(paste0("Reading CANSIM NDM product ",cleaned_number," information from cache."))
    result <- readRDS(file=data_path)
  } else {
    # get_cansim(cleaned_number,language=language,refresh = refresh,timeout=timeout)
    cleaned_language <- cleaned_ndm_language(language)
    cube_title_column <- ifelse(cleaned_language=="eng","Cube Title","Titre du cube")
    start_period_column <- ifelse(cleaned_language=="eng","Start Reference Period",paste0("D",intToUtf8(0x00E9),"but de la p",intToUtf8(0x00E9),"riode de r",intToUtf8(0x00E9),"f",intToUtf8(0x00E9),"rence"))
    end_period_column <- ifelse(cleaned_language=="eng","End Reference Period",paste0("Fin de la p",intToUtf8(0x00E9),"riode de r",intToUtf8(0x00E9),"f",intToUtf8(0x00E9),"rence"))
    frequency_column <- ifelse(cleaned_language=="eng","Frequency",paste0("Fr",intToUtf8(0x00E9),"quence"))
    dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
    member_name_column <- ifelse(cleaned_language=="eng","Member Name","Nom du membre")
    archived_column <- "Archive Status"

    d <- get_cansim_cube_metadata(cansimTableNumber, type="overview",refresh=refresh)

    if (cleaned_language=="fra") {
      result <- d %>%
        select(!!cube_title_column:=.data$cubeTitleFr,
               `Product Id`=.data$productId, `CANSIM Id`=.data$cansimId,
               !!archived_column:=.data$archiveStatusFr,
               !!frequency_column:=.data$frequencyCode,
               !!start_period_column:=.data$cubeStartDate,!!end_period_column:=.data$cubeEndDate)
    } else {
      result <- d %>%
        select(!!cube_title_column:=.data$cubeTitleEn,
               `Product Id`=.data$productId, `CANSIM Id`=.data$cansimId,
               !!archived_column:=.data$archiveStatusEn,
               !!frequency_column:=.data$frequencyCode,
               !!start_period_column:=.data$cubeStartDate,!!end_period_column:=.data$cubeEndDate)

    }
  }
  result
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
  cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
  data_path <- paste0(base_path_for_table_language(cleaned_number,language),".Rda3")
  if (!refresh && file.exists(data_path)) {
    result <- readRDS(file=data_path)
  } else {
    cleaned_language <- cleaned_ndm_language(language)
    survey_code_grepl_field <- ifelse(cleaned_language=="eng","Survey Code",paste0("Code d'enqu",intToUtf8(0x00EA),"te"))
    result<-get_cansim_cube_metadata(cansimTableNumber,type="overview",refresh=refresh) %>% select(!!survey_code_grepl_field:=.data$surveyCode)
  }
  result
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
  cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
  data_path <- paste0(base_path_for_table_language(cleaned_number,language),".Rda4")
  if (!refresh && file.exists(data_path)) {
    result <- readRDS(file=data_path)
  } else {
    cleaned_language <- cleaned_ndm_language(language)
    subject_code_grepl_field <- ifelse(cleaned_language=="eng","Subject Code","Code du sujet")
    result<-get_cansim_cube_metadata(cansimTableNumber,type="overview",refresh=refresh) %>%
      select(.data$subjectCode) %>%
      mutate(subjectCode=strsplit(.data$subjectCode,", ")) %>%
      tidyr::unnest_longer(.data$subjectCode) %>%
      select(!!subject_code_grepl_field:=.data$subjectCode)
  }
  result
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
  cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
  data_path <- paste0(base_path_for_table_language(cleaned_number,language),".Rda5")
  if (!refresh && file.exists(data_path)) {
    notes <- readRDS(file=data_path)
  } else if (!file.exists(data_path)) {
    notes <- get_cansim_cube_metadata(cansimTableNumber,refresh=refresh,type="notes")
  }
  if (refresh || !file.exists(data_path)){
    notes <- get_cansim_cube_metadata(cansimTableNumber,refresh=refresh,type="notes")
    cleaned_language <- cleaned_ndm_language(language)
    note_id_grepl_field <- ifelse(cleaned_language=="eng","Note ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification de la note"))

    if (cleaned_language=="fra") {
      notes <- notes %>%
        select(.data$footnoteId,Note=.data$footnotesFr)
    } else {
      notes <- notes %>%
        select(.data$footnoteId,Note=.data$footnotesEn)
    }
    notes <- notes %>%
      unique() %>%
      rename(!!note_id_grepl_field:=.data$footnoteId)
    #get_cansim(cleaned_number,language=language,refresh = refresh, timeout = timeout)
  }

  notes
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
  cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
  data_path <- paste0(base_path_for_table_language(cleaned_number,language),".Rda2")
  if (!refresh && file.exists(data_path)) {
    result <- readRDS(file=data_path)
  } else {
    cleaned_language <- cleaned_ndm_language(language)
    dimension_id_column <- ifelse(cleaned_language=="eng","Dimension ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification de la dimension"))
    dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
    d <- get_cansim_cube_metadata(cansimTableNumber,type="members",refresh=refresh)

    if (cleaned_language=="fra") {
     result <- d %>%
      select(!!dimension_id_column:=.data$dimensionPositionId,!!dimension_name_column:=.data$dimensionNameFr) %>%
       unique()
    } else {
      result <- d %>%
        select(!!dimension_id_column:=.data$dimensionPositionId,!!dimension_name_column:=.data$dimensionNameEn)  %>%
        unique()
    }
    #get_cansim(cleaned_number,language=language,refresh = refresh, timeout = timeout)
    #result <- readRDS(file=data_path)
  }

  result
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
  cleaned_number <- cleaned_ndm_table_number(cansimTableNumber)
  cleaned_language <- cleaned_ndm_language(language)
  data_path <- paste0(base_path_for_table_language(cleaned_number,language),".Rda2")
  if (!refresh && file.exists(data_path)) {
    meta2 <- readRDS(data_path)
    dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
    dimension_id_column <- ifelse(cleaned_language=="eng","Dimension ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification de la dimension"))
    column_index <- meta2 %>%
      dplyr::filter(!!as.name(dimension_name_column) == column) %>%
      dplyr::pull(!!as.name(dimension_id_column))
    data_path <- paste0(base_path_for_table_language(cleaned_number,language),".Rda_column_",column_index)
    if (!file.exists(data_path)){
      stop(paste0("Unkown column ",column))
    }
    result <- readRDS(file=data_path)
  } else {
    cleaned_language <- cleaned_ndm_language(language)
    dimension_id_column <- ifelse(cleaned_language=="eng","Dimension ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification de la dimension"))
    dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
    member_name_column <- ifelse(cleaned_language=="eng","Member Name","Nom du membre")
    member_id_column <- ifelse(cleaned_language=="eng","Member ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification du membre"))
    parent_member_id_column <- ifelse(cleaned_language=="eng","Parent Member ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification du membre parent"))
    hierarchy_column <- ifelse(cleaned_language=="eng","Hierarchy",paste0("Hi",intToUtf8(0x00E9),"rarchie"))
    terminated_column <- ifelse(cleaned_language=="eng","Terminated",paste0("Termin",intToUtf8(0x00E9)))
    exceeded_hierarchy_warning_message <- ifelse(cleaned_language=="eng","Exceeded max depth for hierarchy, hierarchy information may be faulty.",
                                                 paste0("Profondeur maximale d",intToUtf8(0x00E9),"pass",intToUtf8(0x00E9),"e pour la hi",intToUtf8(0x00E9),"rarchie, les informations de hi",intToUtf8(0x00E9),"rarchie peuvent ",intToUtf8(0x00EA),"tre erron",intToUtf8(0x00E9),"es."))
    d <- get_cansim_cube_metadata(cansimTableNumber,type="members",refresh=refresh)

    if (cleaned_language=="fra") {
      result <- d %>%
        select(!!dimension_id_column:=.data$dimensionPositionId,!!dimension_name_column:=.data$dimensionNameFr,
               !!member_id_column:=.data$memberId, !!member_name_column:=.data$memberNameFr,
               !!parent_member_id_column:=.data$parentMemberId, !!terminated_column:=.data$terminated)
    } else {
      result <- d %>%
        select(!!dimension_id_column:=.data$dimensionPositionId,!!dimension_name_column:=.data$dimensionNameEn,
               !!member_id_column:=.data$memberId, !!member_name_column:=.data$memberNameEn,
               !!parent_member_id_column:=.data$parentMemberId, !!terminated_column:=.data$terminated)
    }
    result <- result %>%
      filter(!!as.name(dimension_name_column)==column) %>%
      add_hierarchy(parent_member_id_column=parent_member_id_column,
                    member_id_column=member_id_column,
                    hierarchy_column=hierarchy_column,
                    exceeded_hierarchy_warning_message=exceeded_hierarchy_warning_message)

    if (nrow(result)==0){
      stop(paste0("Unkown column ",column))
    }
  }

  result
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
  info <- get_cansim_table_info(cansimTableNumber,language=language,refresh=refresh)
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
    dplyr::mutate(hierarchy_level=(strsplit(!!as.name(hierarchy_name),"\\.") %>% lapply(length) %>% unlist)-1)
  max_level=max(h$hierarchy_level,na.rm = TRUE)
  if (is.na(level) | level>max_level) level=max_level
  h <- h %>%
    dplyr::mutate(`Member ID`=strsplit(!!as.name(hierarchy_name),"\\.") %>% lapply(last) %>% as.integer) %>%
    dplyr::filter(.data$hierarchy_level<=level)
  #strict_hierarchy=h %>% dplyr::filter(.data$hierarchy_level==level) %>% dplyr::pull(hierarchy_name) %>% unique
  if (strict) {
    h <- h %>% dplyr::filter(.data$hierarchy_level==level)
  } else if (remove_duplicates) {
    higher_ids <- h %>% pull(hierarchy_name) %>% #strict_hierarchy %>%
      as.character() %>%
      strsplit("\\.") %>%
      lapply(function(x){utils::head(as.integer(x),-1)}) %>%
      unlist() %>% unique() %>% as.integer()
    h <- h %>% dplyr::filter(!(.data$`Member ID` %in% higher_ids))
  }
  h[[column_name]] %>% as.character()
}





#' View CANSIM table or vector information in browser
#'
#' Opens CANSIM table or vector on Statistics Canada's website using default browser. This may be useful for getting further info on CANSIM table and survey methods.
#'
#' @param cansimTableNumber CANSIM or NDM table number or cansim vectors with "v" prefix. If no number is provided, the vector search
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
  cansimTableNumber <- tolower(cansimTableNumber)

  if (is.null(cansimTableNumber)) {
    url <- 'https://www150.statcan.gc.ca/t1/tbl1/en/sbv.action#tables'
  } else if (grepl("^v\\d+$",cansimTableNumber)) {
    url <- paste0("https://www150.statcan.gc.ca/t1/tbl1/en/sbv.action?vectorNumbers=",cansimTableNumber)
  } else {
    cansimTableNumber <- paste0(gsub("-","",cleaned_ndm_table_number(cansimTableNumber)),"01")
    url <- paste0("https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=",gsub("-","",cansimTableNumber))
  }

  utils::browseURL(url,browser)
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
  cansimTableNumber <- cleaned_ndm_table_number(cansimTableNumber)
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
  cansimTableNumber <- cleaned_ndm_table_number(cansimTableNumber)
  cleaned_language <- cleaned_ndm_language(language)
  dimension_name_column <- ifelse(cleaned_language=="eng","Dimension name","Nom de la dimension")
  dimenion_note_column <- ifelse(cleaned_language=="eng","Dimension Notes","Notes sur la dimension")
  member_name_column <- ifelse(cleaned_language=="eng","Member Name","Nom du membre")
  member_note_column <- ifelse(cleaned_language=="eng","Member Notes","Notes sur le membre")
  note_id_column <- ifelse(cleaned_language=="eng","Note ID",paste0("Num",intToUtf8(0x00E9),"ro d'identification de la note"))
  notes <- get_cansim_table_short_notes(cansimTableNumber,language=language,refresh=refresh,timeout=timeout)
  columns <- get_cansim_column_list(cansimTableNumber,language=language)

  if (dimenion_note_column %in% names(columns)) {
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
      mutate(!!note_id_column:=strsplit(!!as.name(note_id_column),";")) %>%
      tidyr::unnest_longer(!!note_id_column) %>%
      mutate(!!note_id_column:=as.character(!!as.name(note_id_column))) %>%
      full_join(notes %>% mutate(!!note_id_column:=as.character(!!as.name(note_id_column))),by=note_id_column) %>%
      arrange(!!as.integer(as.name(note_id_column)))
  } else {
    full_notes <- get_cansim_cube_metadata(cansimTableNumber,type="notes",refresh=refresh)
    members <- get_cansim_cube_metadata(cansimTableNumber,type="members",refresh = refresh)

    if (cleaned_language=="fra") {
      members <- members %>%
        select(.data$dimensionPositionId,!!dimension_name_column:=.data$dimensionNameFr,
               .data$memberId,!!member_name_column:=.data$memberNameFr) %>%
        unique()
      full_notes <- full_notes %>%
        select(!!note_id_column:=.data$footnoteId,Note=.data$footnotesFr,.data$dimensionPositionId, .data$memberId) %>%
        left_join(members,by=c("dimensionPositionId","memberId")) %>%
        select(-.data$dimensionPositionId,-.data$memberId)
    } else {
      members <- members %>%
        select(.data$dimensionPositionId,!!dimension_name_column:=.data$dimensionNameEn,
               .data$memberId,!!member_name_column:=.data$memberNameEn) %>%
        unique()
      full_notes <- full_notes %>%
        select(!!note_id_column:=.data$footnoteId,Note=.data$footnotesEn,.data$dimensionPositionId, .data$memberId) %>%
        left_join(members,by=c("dimensionPositionId","memberId")) %>%
        select(-.data$dimensionPositionId,-.data$memberId)
    }
  }
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
    release_date <- NA
  } else {
    c <- httr::content(response$result)
    r<-c$result
    if (length(r)>0) {
      rd <- unique(unlist(lapply(r,function(rr)rr$releasedate)))
      release_date <- strptime(rd,format = STATCAN_TIME_FORMAT,tz="UTC") %>%
        max()
    } else {
      release_date <- NA
    }
  }
  release_date
  #get_cansim_cube_metadata(cansimTableNumber) %>% pull(releaseTime)
}


#' @import dplyr
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom rlang set_names
#' @importFrom purrr map
#' @importFrom rlang :=
#' @importFrom stats setNames
#' @importFrom utils head

# silence warning that dbplyer is not used (explicitly)
ignore_unused_imports <- function(){
  dbplyr::sql(NULL)
}

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))



