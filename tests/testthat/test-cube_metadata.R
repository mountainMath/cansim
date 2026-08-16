test_that("cube metadata can be retrieved for several tables at once", {
  skip_on_cran()

  tables <- c("32-10-0098","18-10-0265")

  overview <- get_cansim_cube_metadata(tables)
  expect_equal(nrow(overview),2)
  expect_equal(overview$productId,tables)

  # types other than overview carry no table identifier of their own
  members <- get_cansim_cube_metadata(tables,type="members")
  expect_true("cansimTableNumber" %in% names(members))
  expect_equal(sort(unique(members$cansimTableNumber)),sort(tables))
})

test_that("cube metadata for a single table has the same shape as for several", {
  skip_on_cran()

  tables <- c("32-10-0098","18-10-0265")

  for (type in c("members","notes","corrections")) {
    combined <- get_cansim_cube_metadata(tables,type=type)
    singles <- tables %>%
      lapply(\(t)get_cansim_cube_metadata(t,type=type)) %>%
      dplyr::bind_rows()

    expect_true("cansimTableNumber" %in% names(singles))
    expect_equal(names(combined),names(singles))
    expect_equal(nrow(combined),nrow(singles))
  }
})

test_that("cube metadata only downloads tables that are not cached yet", {
  skip_on_cran()

  cached <- "32-10-0098"
  uncached <- "36-10-0434"

  get_cansim_cube_metadata(cached)
  cached_path <- cube_metadata_path(cached)
  uncached_path <- cube_metadata_path(uncached)
  unlink(uncached_path)
  expect_true(file.exists(cached_path))

  cached_time <- file.mtime(cached_path)

  result <- get_cansim_cube_metadata(c(cached,uncached))

  expect_equal(nrow(result),2)
  expect_true(file.exists(uncached_path))
  # the already cached table was served from cache rather than downloaded again
  expect_equal(file.mtime(cached_path),cached_time)
})

test_that("cube metadata fails informatively for unknown tables", {
  skip_on_cran()

  expect_error(suppressMessages(get_cansim_cube_metadata(c("32-10-0098","99-99-9999"))),
               "99-99-9999")
})

test_that("table templates can be built for several tables at once", {
  skip_on_cran()

  tables <- c("32-10-0098","18-10-0265")

  combined <- get_cansim_table_template(tables)
  singles <- tables %>%
    lapply(\(t)get_cansim_table_template(t))

  expect_equal(sort(unique(combined$cansimTableNumber)),sort(tables))
  expect_equal(nrow(combined),sum(sapply(singles,nrow)))
  expect_equal(attr(combined,"cansimTableNumber"),tables)

  # coordinates stay table specific
  expect_equal(combined %>% dplyr::filter(.data$cansimTableNumber==tables[1]) %>% dplyr::pull(.data$COORDINATE),
               singles[[1]]$COORDINATE)
})

test_that("functions that only handle a single table say so", {
  skip_on_cran()

  tables <- c("32-10-0098","18-10-0265")

  expect_error(get_cansim_table_url(tables),"single table number")
  expect_error(get_cansim_table_last_release_date(tables),"single table number")
  expect_error(get_cansim_table_info(tables),"single table number")
  expect_error(get_cansim_series_info_cube_coord(tables,"1.1.1.1.1.1"),"single table number")
})
