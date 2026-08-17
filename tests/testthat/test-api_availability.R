# StatCan servers time out or go down often enough that the package has to keep working around it.
# A dead proxy makes every request fail immediately, which stands in for StatCan being unreachable.
with_statcan_down <- function(code) {
  httr::with_config(httr::use_proxy("127.0.0.1", port=9), code, override=TRUE)
}

test_that("failures are reported as warnings and yield NULL", {
  fail <- list(error=structure(list(message="could not connect"), class=c("error","condition")),
               result=NULL)

  expect_warning(result <- cansim:::check_statcan_response(fail, retry=0, again=\(r)NULL),
                 "multiple timeouts")
  expect_null(result)

  # a non-200 answer is an availability problem too, StatCan serves error pages under load
  expect_warning(result <- cansim:::check_statcan_response(list(error=NULL, result=list(status_code=503)),
                                                           retry=0, again=\(r)NULL),
                 "currently unavailable")
  expect_null(result)
  expect_warning(result <- cansim:::check_statcan_response(list(error=NULL, result=list(status_code=404)),
                                                           retry=0, again=\(r)NULL),
                 "status code 404")
  expect_null(result)

  # a good response passes through untouched
  ok <- list(error=NULL, result=list(status_code=200, content="data"))
  expect_no_warning(result <- cansim:::check_statcan_response(ok, retry=0, again=\(r)NULL))
  expect_equal(result, ok$result)
})

test_that("retries are exhausted before giving up", {
  attempts <- 0
  fail <- list(error=structure(list(message="timeout"), class=c("error","condition")), result=NULL)
  again <- function(retry) {
    attempts <<- attempts + 1
    cansim:::check_statcan_response(fail, retry=retry, again=again)
  }

  suppressMessages(expect_warning(result <- cansim:::check_statcan_response(fail, retry=3, again=again)))
  expect_null(result)
  expect_equal(attempts, 3)
})

test_that("erroring can be restored through an option", {
  fail <- list(error=structure(list(message="could not connect"), class=c("error","condition")),
               result=NULL)

  old <- options(cansim.error_on_unavailable=TRUE)
  on.exit(options(old), add=TRUE)
  expect_error(cansim:::check_statcan_response(fail, retry=0, again=\(r)NULL), "multiple timeouts")
})

test_that("every function that talks to StatCan returns NULL when StatCan is unreachable", {
  skip_on_cran()

  # a cache path of its own, otherwise cached tables would legitimately satisfy some of these calls;
  # get_cansim_connection() serves a cached table when a refresh download fails, and it reads the
  # environment variable rather than the option, so that has to point at the private path too
  old <- options(cansim.cache_path=file.path(tempdir(),"cansim_unavailable_test"))
  dir.create(getOption("cansim.cache_path"), showWarnings=FALSE)
  on.exit(options(old), add=TRUE)
  old_env <- Sys.getenv("CANSIM_CACHE_PATH", unset=NA)
  Sys.setenv(CANSIM_CACHE_PATH=getOption("cansim.cache_path"))
  on.exit(if (is.na(old_env)) Sys.unsetenv("CANSIM_CACHE_PATH") else
    Sys.setenv(CANSIM_CACHE_PATH=old_env), add=TRUE)

  # a table no other test touches, so that nothing it needs is already sitting in the session cache
  TABLE <- "17-10-0005"

  calls <- list(
    get_cansim                         = \() get_cansim(TABLE, refresh=TRUE),
    get_cansim_connection              = \() get_cansim_connection(TABLE, refresh=TRUE),
    get_cansim_cube_metadata           = \() get_cansim_cube_metadata(TABLE, refresh=TRUE),
    get_cansim_table_info              = \() get_cansim_table_info(TABLE, refresh=TRUE),
    get_cansim_table_notes             = \() get_cansim_table_notes(TABLE, refresh=TRUE),
    get_cansim_table_short_notes       = \() get_cansim_table_short_notes(TABLE, refresh=TRUE),
    get_cansim_table_survey            = \() get_cansim_table_survey(TABLE, refresh=TRUE),
    get_cansim_table_subject           = \() get_cansim_table_subject(TABLE, refresh=TRUE),
    get_cansim_table_overview          = \() get_cansim_table_overview(TABLE, refresh=TRUE),
    get_cansim_column_list             = \() get_cansim_column_list(TABLE, refresh=TRUE),
    get_cansim_column_categories       = \() get_cansim_column_categories(TABLE,"Geography", refresh=TRUE),
    get_cansim_table_template          = \() get_cansim_table_template(TABLE, refresh=TRUE),
    get_cansim_table_url               = \() get_cansim_table_url(TABLE),
    get_cansim_table_last_release_date = \() get_cansim_table_last_release_date(TABLE),
    get_cansim_changed_tables          = \() get_cansim_changed_tables("2022-01-01"),
    get_cansim_code_set                = \() get_cansim_code_set("uom", refresh=TRUE),
    get_cansim_key_release_schedule    = \() get_cansim_key_release_schedule(),
    list_cansim_cubes                  = \() list_cansim_cubes(refresh=TRUE),
    search_cansim_cubes                = \() search_cansim_cubes("labour", refresh=TRUE),
    get_cansim_vector                  = \() get_cansim_vector("v41690973", refresh=TRUE),
    get_cansim_vector_for_latest_periods = \() get_cansim_vector_for_latest_periods("v41690973", periods=3, refresh=TRUE),
    get_cansim_vector_info             = \() get_cansim_vector_info("v41690973"),
    get_cansim_series_info_cube_coord  = \() get_cansim_series_info_cube_coord(TABLE,"1.1", refresh=TRUE),
    get_cansim_data_for_table_coord_periods = \() get_cansim_data_for_table_coord_periods(
                                                    rlang::list2(!!TABLE:="1.1"), periods=3, refresh=TRUE)
  )

  # a call can fail on several requests and warn once for each, so the warnings are collected rather
  # than matched one at a time, which would leave the rest to bubble up as unexpected warnings
  call_while_down <- function(call) {
    warnings <- character()
    value <- withCallingHandlers(suppressMessages(call()),
                                 warning=function(w){
                                   warnings <<- c(warnings,conditionMessage(w))
                                   invokeRestart("muffleWarning")
                                 })
    list(value=value,warnings=warnings)
  }

  with_statcan_down({
    for (name in names(calls)) {
      result <- call_while_down(calls[[name]])
      expect_gt(length(result$warnings), 0, label=paste0(name," warnings"))
      expect_null(result$value, label=paste0(name," returns"))
    }
  })
})
