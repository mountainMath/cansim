test_that("disconnect_cansim_sqlite is deprecated in favour of disconnect_cansim_connection", {
  # neither one has anything to close here, what is under test is the warning and the forwarding
  connection <- dplyr::tibble(a=1)

  expect_warning(disconnect_cansim_sqlite(connection), "deprecated")
  expect_warning(disconnect_cansim_sqlite(connection), "disconnect_cansim_connection")

  # the replacement leaves anything that is not a sqlite connection alone, so that code holding a
  # parquet or feather connection can close it without knowing which format it was handed
  expect_no_warning(disconnect_cansim_connection(connection))
  expect_null(disconnect_cansim_connection(connection))
})
