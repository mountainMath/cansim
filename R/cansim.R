

#' Get cansim table into tidy dataframe
#' Caches the table data for the current session
#' @export
get_cansim <- function(cansimTableNumber,language="english"){
  lang_ext=ifelse(tolower(language)=="english","-eng",ifelse(tolower(language) %in% c("french"),"-fra",NA))
  if (is.na(lang_ext)) stop(paste0("Unkown Lanaguage ",language))
  cleaned_number=sprintf("%07d", as.numeric(sub("-","",as.character(cansimTableNumber))))
  path <- file.path(tempdir(),paste0(cleaned_number,lang_ext))
  if (!file.exists(path)){
    url <- "http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/"
    cansimTableNumberString <- cleaned_number # pad with zeros if needed
    filename <- paste0("0", cansimTableNumberString, lang_ext)
    url <- paste0(url, filename, ".zip")
    download.file(url, path, quiet = TRUE)
    data <- NA
    na_strings=c("<NA>",NA,"NA","")
    if(lang_ext=="-eng")
      data <- readr::read_csv(unz(path, paste0(filename, ".csv")),
                              na=na_strings,
                              locale=readr::locale(encoding="Windows-1254"),
                              col_types = list(.default = "c",Value="d"))
    else
      data <- readr::read_csv2(unz(path, paste0(filename, ".csv")),
                               na=na_strings,
                               locale=readr::locale(encoding="Windows-1254"),
                               col_types = list(.default = "c",Valeur="d"))
    saveRDS(data,file=path)
  }
  readRDS(file=path)
}

#' Adjust Cansim Value by scaled amount
#' French part does not work, probably encoding issues
#' @export
adjust_cansim_values_by_variable <-function(data,var){
 if("Valeur" %in% names(data))
   data <- data %>%
    mutate(Valeur=ifelse(grepl(" \\(x 1 000 000\\)$",UQ(as.name(var))),1000000*Valeur,Valeur)) %>%
    mutate(!!var:=sub(" \\(x 1 000 000\\)$","",UQ(as.name(var)))) %>%
    mutate(Valeur=ifelse(grepl(" \\(x 1 000\\)$",UQ(as.name(var))),1000*Valeur,Valeur)) %>%
    mutate(!!var:=sub(" \\(x 1 000\\)$","",UQ(as.name(var))))
 else
   data <- data %>%
     mutate(Value=ifelse(grepl(" \\(x 1,000,000\\)$",UQ(as.name(var))),1000000*Value,Value)) %>%
     mutate(!!var:=sub(" \\(x 1,000,000\\)$","",UQ(as.name(var)))) %>%
     mutate(Value=ifelse(grepl(" \\(x 1,000\\)$",UQ(as.name(var))),1000*Value,Value)) %>%
     mutate(!!var:=sub(" \\(x 1,000\\)$","",UQ(as.name(var))))

  data
}
