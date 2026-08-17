# get_cansim_vector() accepts vectors with or without the "v" prefix and with or without names, and
# the four spellings have to come out the same: the VECTOR column always carries the prefix, and a
# name given by the caller always becomes the label of its rows.
test_that("vectors come back standardized and named vectors keep their labels", {
  vector_for <- function(vectors) {
    suppressMessages(with_mocked_bindings(
      # refresh so a cache entry left by an earlier spelling, which strips to the same key, is not
      # read in place of the mocked answer
      get_cansim_vector(vectors, start_time="2020-01-01", end_time="2020-06-01",
                        factors=FALSE, refresh=TRUE),
      get_with_timeout_retry=function(...) structure(list(), class="httr2_response"),
      statcan_response_json=function(response) list(mock_vector_data_record(vectorId=990000778)),
      metadata_for_coordinates=mock_metadata_for_coordinates,
      .package="cansim"))
  }

  # the caller may spell a vector with or without the "v" prefix, the result always carries it
  expect_identical(vector_for("v990000778")$VECTOR, "v990000778")
  expect_identical(vector_for("990000778")$VECTOR, "v990000778")
  # and unnamed vectors get no label column
  expect_false("label" %in% names(vector_for("v990000778")))

  # rename_vectors() keys on the naked vector id, so both spellings of a named vector have to come
  # out with the caller's label
  expect_identical(vector_for(c(foo="v990000778"))$label, "foo")
  expect_identical(vector_for(c(foo="990000778"))$label, "foo")
})
