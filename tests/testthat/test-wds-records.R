test_that("requests are split into batches StatCan accepts", {
  expect_length(batch_items(character(0)), 0)
  expect_length(batch_items(as.character(1:300)), 1)
  expect_identical(lengths(batch_items(as.character(1:301))), c(300L, 1L))
  expect_identical(lengths(batch_items(as.character(1:601))), c(300L, 300L, 1L))

  # the batches are put back together in order, so a caller sees its items in the order it asked
  expect_identical(unlist(batch_items(as.character(1:701)), use.names = FALSE), as.character(1:701))

  expect_identical(lengths(batch_items(as.character(1:5), size = 2)), c(2L, 2L, 1L))
})

# shapes taken from what the WDS methods answer with, see the note on wds_record_code()
wds_success <- list(status = "SUCCESS", object = list(
  responseStatusCode = 0, vectorId = 41690973, productId = 18100004,
  coordinate = "2.2.0.0.0.0.0.0.0.0"))
wds_invalid_vector <- list(status = "SUCCESS", object = list(
  responseStatusCode = 4, vectorId = 999999999))
wds_invalid_coordinate <- list(status = "SUCCESS", object = list(
  responseStatusCode = 2, vectorId = 0, productId = 35100003,
  coordinate = "1.99.0.0.0.0.0.0.0.0"))
wds_failed <- list(status = "FAILED", object = list(
  responseStatusCode = 3, vectorId = 999999999))
wds_failed_message <- list(status = "FAILED",
  object = "The cube product ID 99999999 does not exist. Error code = CUBE_NOT_AVAILABLE")

test_that("a record is only successful when both status and response code say so", {
  expect_true(wds_record_succeeded(wds_success))
  # the failure the package used to let through, SUCCESS carrying a non-zero response code
  expect_false(wds_record_succeeded(wds_invalid_vector))
  expect_false(wds_record_succeeded(wds_invalid_coordinate))
  expect_false(wds_record_succeeded(wds_failed))
  expect_false(wds_record_succeeded(wds_failed_message))

  # a method that reports no per-record code at all is taken at its word
  expect_true(wds_record_succeeded(list(status = "SUCCESS", object = list(vectorId = 1))))
})

test_that("failed records are split out and named", {
  records <- split_wds_records(list(wds_success, wds_invalid_vector, wds_failed_message))
  expect_length(records$success, 1)
  expect_length(records$failed, 2)
  expect_identical(records$success[[1]], wds_success)

  expect_identical(split_wds_records(list()), list(success = list(), failed = list()))
})

test_that("a failure names the item it answers for and says why", {
  expect_identical(wds_record_id(wds_invalid_vector), "v999999999")
  expect_identical(wds_record_id(wds_invalid_coordinate), "35-10-0003 1.99")
  # nothing to name when the record carries a sentence in place of an object
  expect_identical(wds_record_id(wds_failed_message), NA_character_)

  expect_identical(wds_record_reason(wds_invalid_vector), "Vector is invalid")
  expect_identical(wds_record_reason(wds_invalid_coordinate), "Invalid cube and series combination")
  expect_identical(wds_record_reason(wds_failed_message), wds_failed_message$object)
  # the API emits codes outside its own wdsResponseStatus code set
  expect_identical(wds_record_reason(list(status = "SUCCESS", object = list(responseStatusCode = 9))),
                   "StatCan response status code 9")
})

test_that("dropped records are reported, and grouped by reason", {
  records <- list(wds_success, wds_invalid_vector, wds_failed, wds_failed_message)

  reported <- capture_messages(kept <- successful_wds_records(records, "vector metadata"))
  expect_match(reported, "for 3 of the requested items", all = FALSE)
  expect_match(reported, "Vector is invalid \\(v999999999\\)", all = FALSE)
  expect_match(reported, "CUBE_NOT_AVAILABLE", all = FALSE)

  expect_length(kept, 1)
  expect_silent(successful_wds_records(list(wds_success), "vector metadata"))

  # a caller to which a given failure is the expected answer says so
  expect_silent(successful_wds_records(list(wds_success, wds_invalid_coordinate),
                                       "series information", ignore_codes = 2))
})

test_that("an invalid vector no longer reaches the metadata as a row of NAs", {
  metadata <- suppressMessages(
    extract_vector_metadata(successful_wds_records(list(wds_success, wds_invalid_vector), "metadata")))

  expect_identical(nrow(metadata), 1L)
  expect_identical(metadata$VECTOR, "v41690973")

  # every vector of a batch can be rejected, and the columns are built from the records
  all_dropped <- suppressMessages(
    extract_vector_metadata(successful_wds_records(list(wds_invalid_vector), "metadata")))
  expect_identical(nrow(all_dropped), 0L)
})

test_that("vector data with nothing in it warns rather than failing on a missing column", {
  # StatCan answers with an empty list both for a vector it has no data for and, for every vector at
  # once, during the nightly window. That used to travel on to the metadata join and surface there as
  # a missing `cansimTableNumber` column.
  vectors <- c("990000001", "990000002")
  body <- paste0("[", paste(paste0('{"vectorId":', vectors, ',"latestN":1}'), collapse = ", "), "]")
  cache_path <- file.path(tempdir(),
                          paste0("cansim_cache_", digest::digest(body, algo = "md5"), ".rda"))
  saveRDS(tibble::tibble(), cache_path)
  on.exit(unlink(cache_path), add = TRUE)

  expect_warning(result <- suppressMessages(get_cansim_vector_for_latest_periods(vectors, periods = 1)),
                 "no data for any of the requested vectors")
  expect_s3_class(result, "tbl_df")
  expect_identical(nrow(result), 0L)
})
