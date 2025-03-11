

get_cansim_members <- function(cansimTableNumber, language="english",
                               latestN=NULL,
                               members=list(),
                               refresh=FALSE,
                               timeout=1000,
                        cache_path=getOption("cansim.cache_path")){

  language <- cleaned_ndm_language(language)
  cansimTableNumber <- cleaned_ndm_table_number(cansimTableNumber)
  naked_table_number <- naked_ndm_table_number(cansimTableNumber)

  base_url <- "https://www150.statcan.gc.ca/t1/tbl1/en/dtl!downloadDbLoadingData-nonTraduit.action"

  "startDate=&endDate=&csvLocale=en&selectedMembers=[[],[2,3,4],[],[],[],[1,2]]&checkedLevels=0D1,1D1,2D1,3D1,3D2,4D1"


  if (is.null(latestN)) {latestN=100000}

  cl <- get_cansim_column_list(cansimTableNumber, language=language, refresh=refresh, timeout=timeout)

  selectedMembers <- cl %>%
    arrange(as.integer(1)) %>%
    pull(1) %>%
    lapply(\(did){

    })

    members %>%
    #seq_along() %>%
    lapply(\(i,el,n){

    })

  url <- paste0(base_url,
                "?","pid=",naked_table_number,"01",
                "&","latestN=",latestN,
                "&","csvLocale=",substr(language,1,2))

  readr::read_csv()

}
