# Functions for managing user settings, API keys, cache locations



#' Set persistent cansim cache location
#'
#' @description Cansim provides session caching for retrieved data. This function will create a persistent cache across sessions for data accessed via `get_cansim_connection` and caches data in a database across sessions..
#'
#' @param cache_path a local directory to use for saving cached data
#' @param overwrite Option to overwrite any existing cache path already stored locally.
#' @param install Option to install permanently for use across sessions.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' set_cansim_cache_path("~/cansim_cache")
#'
#' # This will set the cache path permanently until overwritten again
#' set_cansim_cache_path("~/cancensus_cache", install = TRUE)
#' }
set_cansim_cache_path <- function(cache_path, overwrite = FALSE, install = FALSE){
  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(!file.exists(renv)){
      file.create(renv)
    } else{
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
      if(isTRUE(overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=readLines(renv)
        newenv <- oldenv[-grep("CANSIM_CACHE_PATH", oldenv)]
        writeLines(newenv, renv, sep = "\n")
      } else{
        tv <- readLines(renv)
        if(any(grepl("CANSIM_CACHE_PATH",tv))){
          stop("A saved cache already exists. You can overwrite it with the argument overwrite=TRUE.", call.=FALSE)
        }
      }
    }

    keyconcat <- paste0("CANSIM_CACHE_PATH='", cache_path, "'")
    # Append cache path .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message('Your cache path has been stored in your .Renviron and can be accessed by Sys.getenv("CANSIM_CACHE_PATH").')
    Sys.setenv('CANSIM_CACHE_PATH' = cache_path)
  } else {
    message("Cache set for duration of session. To permanently add your cache path for use across sessions, run this function with install = TRUE.")
    Sys.setenv('CANSIM_CACHE_PATH' = cache_path)
  }
  cache_path
}


#' View saved cache directory path
#'
#' @description View saved cache path'
#'
#' @export
#'
#' @examples
#' show_cansim_cache_path()
show_cansim_cache_path <- function() {
  path <- Sys.getenv('CANSIM_CACHE_PATH')
  if (path==""){
    path <- getOption("cansim.cache_path")
    if (is.null(path)) {
      message("No cache path set")
    } else {
      message("Cache path is set via legacy option 'cansim.cache_path',\n",
      "please consider setting the cache path via environment variables using\n",
      "`set_cansim_cache_path(getOption('cansim.cache_path'), install = TRUE)`\n")
    }
  }
  path
}

cansim_no_cache_path_message <- paste0(
  "Cansim data retrieved via `get_cansim_connection` is currently stored temporarily.\n\n",
  "In order to speed up performance and share data across sessions and projects to avoid\n",
  "unnecessary network calls, please set up a persistent cache directory via ",
  "`set_cansim_cache_path('<local cache path>', install = TRUE)`.\n",
  "This will add your cache directory as environment varianble to your .Renviron to be ",
  "used across sessions and projects.\n\n",
  "To ensure that cached data is always up to date pass the refresh=TRUE argument to `get_cansim_connection`.\n"
)
