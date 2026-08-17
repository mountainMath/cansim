test_that("vector APIs combine cached batches once without changing row order", {
  vectors <- as.character(990000001:990000301)
  periods <- 2L
  batches <- split(vectors, ceiling(seq_along(vectors) / 300L))
  start_time <- as.Date("2020-01-01")
  end_time <- as.Date("2021-01-01")
  cache_paths <- character()

  batch_results <- lapply(batches, function(vecs) {
    vector_rows <- rep(vecs, each = periods)
    tibble::tibble(
      REF_DATE = rep(c("2020", "2021"), times = length(vecs)),
      VALUE = as.numeric(vector_rows),
      COORDINATE = paste0(as.numeric(vector_rows) %% 97L + 1L, ".1"),
      VECTOR = paste0("v", vector_rows),
      cansimTableNumber = "34-10-0001"
    )
  })

  for (batch_number in seq_along(batches)) {
    vecs <- batches[[batch_number]]
    latest_body <- paste0(
      "[",
      paste(purrr::map(as.character(vecs), function(x) {
        paste0('{"vectorId":', x, ',"latestN":', periods, "}")
      }), collapse = ", "),
      "]"
    )
    latest_path <- file.path(
      tempdir(),
      paste0("cansim_cache_", digest::digest(latest_body, algo = "md5"), ".rda")
    )

    vectors_string <- paste0(
      "vectorIds=",
      paste(lapply(as.character(vecs), function(x) paste0('"', x, '"')), collapse = ",")
    )
    time_string <- paste0(
      "startRefPeriod=", strftime(start_time, "%Y-%m-%d", tz = STATCAN_TIMEZONE),
      "&endReferencePeriod=", strftime(end_time, "%Y-%m-%d", tz = STATCAN_TIMEZONE)
    )
    range_path <- file.path(
      tempdir(),
      paste0("cansim_cache_", digest::digest(list(vectors_string, time_string), algo = "md5"), ".rda")
    )

    saveRDS(batch_results[[batch_number]], latest_path)
    saveRDS(batch_results[[batch_number]], range_path)
    cache_paths <- c(cache_paths, latest_path, range_path)
  }
  on.exit(unlink(cache_paths), add = TRUE)

  metadata_stub <- function(cansimTableNumber, coordinates, language) {
    tibble::tibble(cansimTableNumber = cansimTableNumber, COORDINATE = coordinates)
  }
  normalization_stub <- function(data, ...) data

  latest <- suppressMessages(with_mocked_bindings(
    get_cansim_vector_for_latest_periods(vectors, periods = periods, factors = FALSE),
    metadata_for_coordinates = metadata_stub,
    normalize_cansim_values = normalization_stub,
    .package = "cansim"
  ))
  ranged <- suppressMessages(with_mocked_bindings(
    get_cansim_vector(
      vectors,
      start_time = start_time,
      end_time = end_time,
      factors = FALSE
    ),
    metadata_for_coordinates = metadata_stub,
    normalize_cansim_values = normalization_stub,
    .package = "cansim"
  ))
  expected <- dplyr::bind_rows(batch_results)

  expect_s3_class(latest, "tbl_df")
  expect_identical(names(latest), names(expected))
  expect_identical(latest$VECTOR, expected$VECTOR)
  expect_identical(latest$VALUE, expected$VALUE)
  expect_identical(ranged, latest)
})
