test_that("a changed series list is turned into vectors, tables and coordinates", {
  series <- list(
    list(responseStatusCode=0, vectorId=32164132, productId=13100096,
         coordinate="1.3.1.1.1.0.0.0.0.0", releaseTime="2026-08-16T08:30"),
    list(responseStatusCode=0, vectorId=41690973, productId=18100004,
         coordinate="2.2.0.0.0.0.0.0.0.0", releaseTime="2026-08-16T08:30"))

  changed <- cansim:::changed_series_tibble(series)

  expect_identical(changed$VECTOR, c("v32164132","v41690973"))
  expect_identical(changed$cansimTableNumber, c("13-10-0096","18-10-0004"))
  # trailing zeros are stripped the way every other coordinate the package returns is
  expect_identical(changed$COORDINATE, c("1.3.1.1.1","2.2"))
  expect_identical(unique(changed$releaseTime), "2026-08-16T08:30")
})

test_that("a series with fields missing does not break the list", {
  changed <- cansim:::changed_series_tibble(list(list(vectorId=32164132)))

  expect_identical(nrow(changed), 1L)
  expect_identical(changed$VECTOR, "v32164132")
  expect_true(is.na(changed$cansimTableNumber))
  expect_true(is.na(changed$releaseTime))
})

test_that("an empty changed series list has the columns a full one has", {
  empty <- cansim:::empty_changed_series_list()
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), names(cansim:::changed_series_tibble(
    list(list(vectorId=1, productId=13100096, coordinate="1.1", releaseTime="t")))))
})

# StatCan wraps these records one level deeper on some methods than on others, so both shapes the
# API has been seen to answer with are covered
test_that("both record shapes of the changed series list are read", {
  series <- list(responseStatusCode=0, vectorId=32164132, productId=13100096,
                 coordinate="1.3.1.1.1.0.0.0.0.0", releaseTime="2026-08-16T08:30")

  nested <- list(status="SUCCESS", object=list(list(status="SUCCESS", object=series)))
  flat <- list(status="SUCCESS", object=list(series))

  read <- function(payload) {
    with_mocked_bindings(
      get_cansim_changed_series_list(),
      get_with_timeout_retry=function(...) structure(list(), class="httr2_response"),
      statcan_response_json=function(response) payload,
      .package="cansim")
  }

  expect_identical(suppressMessages(read(nested))$VECTOR, "v32164132")
  # the unwrapped shape carries no per-record status, which must not be read as every series in it
  # having failed
  expect_silent(expect_identical(read(flat)$VECTOR, "v32164132"))

  # a record that did fail in the wrapped shape is still dropped and reported
  failed <- list(status="SUCCESS", object=list(list(status="SUCCESS", object=series),
                                               list(status="FAILED", object="Vector is invalid")))
  reported <- capture_messages(kept <- read(failed))
  expect_identical(kept$VECTOR, "v32164132")
  expect_match(reported, "Vector is invalid", all=FALSE)
})

test_that("nothing having changed is an empty table rather than a failure", {
  # StatCan answers a request naming only unchanged series with an HTTP 404
  no_data <- function(...) cansim:::STATCAN_NO_DATA

  by_vector <- suppressMessages(with_mocked_bindings(
    get_cansim_changed_series_data_for_vectors("v41690973"),
    post_with_timeout_retry=no_data, .package="cansim"))
  by_coordinate <- suppressMessages(with_mocked_bindings(
    get_cansim_changed_series_data_for_coordinates("34-10-0013","1.1"),
    post_with_timeout_retry=no_data, .package="cansim"))

  for (result in list(by_vector, by_coordinate)) {
    expect_s3_class(result, "tbl_df")
    expect_identical(nrow(result), 0L)
  }

  # the list method says the same when StatCan answers with no series, which it reports in the body
  # rather than through a status
  listed <- with_mocked_bindings(
    get_cansim_changed_series_list(),
    get_with_timeout_retry=function(...) structure(list(), class="httr2_response"),
    statcan_response_json=function(response) list(status="SUCCESS", object=list()),
    .package="cansim")
  expect_identical(nrow(listed), 0L)
  expect_true("VECTOR" %in% names(listed))
})

test_that("the list method does not ask for a 404 to be read as nothing having changed", {
  # the data methods name the series they ask about, so a 404 from them means none of those changed.
  # The list method names none, so its 404 is StatCan not serving the route and has to stay a failure
  # rather than being passed off as a quiet day
  empty_status <- NULL
  with_mocked_bindings(
    get_cansim_changed_series_list(),
    get_with_timeout_retry=function(url, ...) { empty_status <<- list(...)$empty_status; NULL },
    .package="cansim")

  expect_null(empty_status)
})

test_that("StatCan being unavailable still yields NULL", {
  unavailable <- function(...) NULL

  expect_null(with_mocked_bindings(get_cansim_changed_series_list(),
                                   get_with_timeout_retry=unavailable, .package="cansim"))
  expect_null(suppressMessages(
    with_mocked_bindings(get_cansim_changed_series_data_for_vectors("v1"),
                         post_with_timeout_retry=unavailable, .package="cansim")))
  expect_null(suppressMessages(
    with_mocked_bindings(get_cansim_changed_series_data_for_coordinates("34-10-0013","1.1"),
                         post_with_timeout_retry=unavailable, .package="cansim")))
})

test_that("requests are batched and shaped the way the API expects", {
  bodies <- character()
  capture <- function(url, body, ...) {
    bodies <<- c(bodies, body)
    cansim:::STATCAN_NO_DATA
  }

  suppressMessages(with_mocked_bindings(
    get_cansim_changed_series_data_for_vectors(paste0("v", 990000001:990000301)),
    post_with_timeout_retry=capture, .package="cansim"))

  expect_length(bodies, 2)
  # the leading "v" is stripped and each vector is named the way the API documents
  expect_match(bodies[1], '^\\[\\{"vectorId":990000001\\}, \\{"vectorId":990000002\\}')
  expect_identical(lengths(regmatches(bodies, gregexpr("vectorId", bodies))), c(300L, 1L))

  bodies <- character()
  suppressMessages(with_mocked_bindings(
    get_cansim_changed_series_data_for_coordinates("34-10-0013", c("1.1","2.3")),
    post_with_timeout_retry=capture, .package="cansim"))

  # coordinates are padded out to the ten dimensions the API wants
  expect_match(bodies[1], '\\{"productId":34100013, "coordinate":"1\\.1\\.0\\.0\\.0\\.0\\.0\\.0\\.0\\.0"\\}')
  expect_match(bodies[1], '\\{"productId":34100013, "coordinate":"2\\.3\\.0\\.0\\.0\\.0\\.0\\.0\\.0\\.0"\\}')
})
