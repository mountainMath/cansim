# StatCan servers time out or go down often enough that the package has to keep working around it.
# A dead proxy makes every request fail immediately, which stands in for StatCan being unreachable.
# curl reads the proxy out of the environment, which is how httr2 requests are reached from outside.
with_statcan_down <- function(code) {
  withr::with_envvar(c(http_proxy="http://127.0.0.1:9", https_proxy="http://127.0.0.1:9"), code)
}

# what httr2 raises when a request never got an answer at all
connection_failure <- function(message="Failed to perform HTTP request.") {
  rlang::error_cnd(c("httr2_failure","httr2_error"), message=message)
}

statcan_response <- function(status_code, body=NULL) {
  httr2::response(status_code=status_code,
                  headers=if (is.null(body)) list() else list(`Content-Type`="application/json"),
                  body=if (is.null(body)) raw() else charToRaw(body))
}

test_that("failures are reported as warnings and yield NULL", {
  expect_warning(result <- cansim:::check_statcan_response(connection_failure()),
                 "StatCan did not answer")
  expect_null(result)

  # a non-200 answer is an availability problem too, StatCan serves error pages under load
  expect_warning(result <- cansim:::check_statcan_response(statcan_response(503)),
                 "currently unavailable")
  expect_null(result)
  expect_warning(result <- cansim:::check_statcan_response(statcan_response(404)),
                 "status code 404")
  expect_null(result)

  # a good response passes through untouched
  ok <- statcan_response(200, '{"status":"SUCCESS"}')
  expect_no_warning(result <- cansim:::check_statcan_response(ok))
  expect_equal(result, ok)
})

test_that("the reason StatCan gives for refusing is passed on", {
  # the status code alone does not say which of the two things a 409 means
  expect_warning(cansim:::check_statcan_response(
                   statcan_response(409, '{"message":"The product is not released yet"}')),
                 "StatCan says: The product is not released yet")
  # and the translation of the code is shown alongside it
  expect_warning(cansim:::check_statcan_response(
                   statcan_response(409, '{"message":"The product is not released yet"}')),
                 "midnight to 8:30am Eastern")

  # a body that is not JSON, an HTML error page for instance, is not reported as a reason
  warnings <- warning_text(cansim:::check_statcan_response(statcan_response(500, "<html>oops</html>")))
  expect_match(warnings, "status code 500")
  expect_false(grepl("StatCan says", warnings))
})

test_that("transient statuses are retried and permanent ones are not", {
  is_transient <- function(status) {
    cansim:::STATCAN_TRANSIENT_STATUS |> (\(codes) status %in% codes)()
  }
  # StatCan serves these under load, so coming back a moment later can work
  expect_true(all(vapply(c(429,500,502,504), is_transient, logical(1))))
  # the nightly window lasts until 8:30am, an oversized request stays oversized, and an outage
  # outlasts any retry budget, so none of these is worth retrying, and a 404 never becomes a 200
  expect_false(any(vapply(c(409,416,503,404,400), is_transient, logical(1))))

  # the ones that are not retried have to say what to do instead
  expect_match(warning_text(cansim:::check_statcan_response(statcan_response(503))),
               "was not retried, please try again later")
})

test_that("requests carry a user agent and a retry budget", {
  req <- cansim:::statcan_request("https://www150.statcan.gc.ca/t1/wds/rest/getCodeSets", retry=3)

  expect_match(req$options$useragent, "^cansim/")
  expect_match(req$options$useragent, "github.com/mountainMath/cansim")
  # retry counts attempts after the first, httr2 counts attempts in total
  expect_equal(req$policies$retry_max_tries, 4)
  expect_equal(cansim:::statcan_request("https://example.com", retry=0)$policies$retry_max_tries, 1)
  # a status the package translates itself must not be thrown by httr2
  expect_false(req$policies$error_is_error(statcan_response(503)))

  # the budget is wall clock time from the start of the first attempt, so it has to be comfortably
  # longer than the backoff between the retries it is meant to allow, which is at most 2^n seconds
  # before the nth of them
  expect_equal(req$policies$retry_max_wait, 30)
  expect_gt(req$policies$retry_max_wait, sum(2^seq_len(req$policies$retry_max_tries - 1)))
})

test_that("the timeout bounds silence rather than the whole transfer", {
  req <- cansim:::statcan_request("https://www150.statcan.gc.ca/t1/wds/rest/getCodeSets", timeout=200)

  # a hard cap cannot tell a stalled connection from a large table still arriving, so there is none.
  # What is bounded is how long StatCan may send nothing useful for
  expect_null(req$options$timeout_ms)
  expect_equal(req$options$low_speed_time, 200)
  expect_equal(req$options$low_speed_limit, 100)
  # far below the megabytes per second a real table download runs at, so only a dead transfer trips it
  expect_lt(req$options$low_speed_limit, 1000)
  # connecting is quick whatever the request, and is bounded on its own so an unreachable host fails
  # fast rather than waiting out the much longer silence budget
  expect_equal(req$options$connecttimeout, 10)
  expect_lt(req$options$connecttimeout, req$options$low_speed_time)

  # StatCan works out a whole response before sending any of it, about 0.11s per vector, so a full
  # 300 item batch is silent for roughly 35 seconds before the first byte. The budget has to clear
  # that comfortably or the package would abort its own largest requests
  expect_gt(req$options$low_speed_time, 5 * 0.11 * 300)

  expect_equal(cansim:::statcan_request("https://example.com", timeout=45)$options$low_speed_time, 45)
})

test_that("erroring can be restored through an option", {
  old <- options(cansim.error_on_unavailable=TRUE)
  on.exit(options(old), add=TRUE)
  expect_error(cansim:::check_statcan_response(connection_failure()), "StatCan did not answer")
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
