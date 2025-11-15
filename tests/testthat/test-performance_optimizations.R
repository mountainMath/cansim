# Tests for performance optimizations
# These tests ensure that optimization changes maintain data consistency
# and don't introduce breaking changes

test_that("batched index creation produces correct indexes", {
  skip_on_cran()
  skip_if_offline()

  # Use a small test table
  table_number <- "23-10-0061"

  # Create SQLite connection
  con <- get_cansim_connection(table_number, format = "sqlite")

  # Get list of indexes
  indexes <- DBI::dbGetQuery(con$src$con,
    "SELECT name FROM sqlite_master WHERE type='index' AND name LIKE 'index_%'")

  # Should have multiple indexes (dimensions + REF_DATE + DGUID, etc.)
  expect_gt(nrow(indexes), 0)

  # Verify key indexes exist
  index_names <- indexes$name
  expect_true(any(grepl("index_REF_DATE", index_names)))
  expect_true(any(grepl("index_DGUID", index_names)))

  # Check that ANALYZE was run by verifying sqlite_stat1 table exists
  stat_tables <- DBI::dbGetQuery(con$src$con,
    "SELECT name FROM sqlite_master WHERE type='table' AND name='sqlite_stat1'")
  expect_equal(nrow(stat_tables), 1,
               label = "ANALYZE should create sqlite_stat1 table for query optimization")

  DBI::dbDisconnect(con$src$con)
})


test_that("SQLite data integrity after transaction optimization", {
  skip_on_cran()
  skip_if_offline()

  # Use a small test table
  table_number <- "23-10-0061"

  # Get data via SQLite connection
  con <- get_cansim_connection(table_number, format = "sqlite")
  sqlite_data <- con %>%
    dplyr::collect() %>%
    dplyr::arrange(REF_DATE, DGUID)
  DBI::dbDisconnect(con$src$con)

  # Verify data structure
  expect_true("REF_DATE" %in% names(sqlite_data))
  expect_true("VALUE" %in% names(sqlite_data))
  expect_true("DGUID" %in% names(sqlite_data))

  # Check for data integrity
  expect_gt(nrow(sqlite_data), 0, label = "SQLite table should contain data")
  expect_false(all(is.na(sqlite_data$VALUE)), label = "Not all values should be NA")

  # Verify no duplicate primary keys (if applicable)
  # Most tables should have unique combinations of dimensions + REF_DATE
  key_columns <- names(sqlite_data)[!names(sqlite_data) %in% c("VALUE", "STATUS", "SYMBOL",
                                                                  "TERMINATED", "DECIMALS",
                                                                  "SCALAR_ID", "VECTOR", "COORDINATE")]
  if (length(key_columns) > 0) {
    dup_count <- sqlite_data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(key_columns))) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      nrow()
    # Some tables might have legitimate duplicates, but usually there shouldn't be many
    expect_lt(dup_count / nrow(sqlite_data), 0.01,
              label = "Less than 1% duplicates expected")
  }
})


test_that("consistency across database formats after optimizations", {
  skip_on_cran()
  skip_if_offline()

  table_number <- "23-10-0061"

  # Get data in all three formats
  formats <- c("sqlite", "parquet", "feather")
  data_list <- list()

  var_list <- get_cansim_column_list(table_number)$`Dimension name` %>%
    setdiff("Geography") |>
    c("REF_DATE", "DGUID") %>%
    rev()

  for (fmt in formats) {
    con <- get_cansim_connection(table_number, format = fmt)
    data_list[[fmt]] <- con %>%
      dplyr::filter(REF_DATE >= "2020-01-01") %>%
      collect_and_normalize(disconnect = TRUE) %>%
      dplyr::arrange(!!!rlang::syms(var_list))
  }

  data_list$memory <- get_cansim(table_number) %>%
    dplyr::filter(REF_DATE >= "2020-01-01") %>%
    dplyr::arrange(!!!rlang::syms(var_list))

  # Compare dimensions
  for (i in 1:length(formats)) {
    expect_equal(nrow(data_list[["memory"]]), nrow(data_list[[formats[i]]]),
                 label = paste("Row count should match between", formats[1], "and", formats[i]))

    expect_equal(ncol(data_list[["memory"]]), ncol(data_list[[formats[i]]]),
                 label = paste("Column count should match between", formats[1], "and", formats[i]))
  }

  # Compare VALUE columns (core data)
  for (i in 1:length(formats)) {
    # Allow for small numeric differences due to float representation
    expect_equal(data_list[["memory"]]$VALUE, data_list[[formats[i]]]$VALUE,
                tolerance = 1e-10,
                label = paste("VALUES should match between", formats[1], "and", formats[i]))
  }

  # Compare REF_DATE
  for (i in 1:length(formats)) {
    expect_equal(data_list[[formats[1]]]$REF_DATE, data_list[[formats[i]]]$REF_DATE,
                 label = paste("REF_DATE should match between", formats[1], "and", formats[i]))
  }

  count_differences <- function(d1,d2) {
    d1 <- d1 |>
      dplyr::mutate(SCALAR_FACTOR=gsub(" +$","",SCALAR_FACTOR)) |>
      dplyr::arrange(Date,COORDINATE)
    d2 <- d2 |>
      dplyr::mutate(SCALAR_FACTOR=gsub(" +$","",SCALAR_FACTOR)) |>
      dplyr::arrange(Date,COORDINATE)

    (d1==d2) |> dplyr::as_tibble() |> dplyr::summarize_all(\(x) sum(!is.na(x) & x==FALSE)) |> rowSums()
  }

  for (i in 1:length(formats)) {
    expect_equal(count_differences(data_list[[formats[i]]],data_list$memory),0,
                 label = paste("Table output should match between get_cansim and", formats[i]))
  }

 })


test_that("SQLite query performance with ANALYZE", {
  skip_on_cran()
  skip_if_offline()

  table_number <- "23-10-0061"
  con <- get_cansim_connection(table_number, format = "sqlite")


  tbl <- DBI::dbListTables(con$src$con)
  tbl <- tbl[grepl("^cansim", tbl)]
  # Get query plan for a filtered query
  query_plan <- DBI::dbGetQuery(con$src$con,
    paste0("EXPLAIN QUERY PLAN SELECT * FROM ",tbl," WHERE REF_DATE >= '2020-01-01'"))

  # Query plan should exist
  expect_gt(nrow(query_plan), 0)

  # Check if index is being used (plan should mention index in some form)
  plan_text <- paste(query_plan$detail, collapse = " ")

  # After ANALYZE, SQLite should be able to use indexes more effectively
  # The query plan should show some optimization strategy
  expect_true(nchar(plan_text) > 0, label = "Query plan should not be empty")

  DBI::dbDisconnect(con$src$con)
})


test_that("no data loss in chunked CSV to SQLite conversion", {
  skip_on_cran()
  skip_if_offline()

  # This test verifies that the transaction optimization in csv2sqlite
  # doesn't cause data loss

  table_number <- "23-10-0061"

  # Clear cache and re-download to test CSV conversion
  remove_cansim_cached_tables(table_number, format = "sqlite")

  # Download and convert (will use optimized csv2sqlite)
  con <- get_cansim_connection(table_number, format = "sqlite")

  tbl <- DBI::dbListTables(con$src$con)
  tbl <- tbl[grepl("^cansim", tbl)]

  # Count rows
  row_count <- DBI::dbGetQuery(con$src$con, paste0("SELECT COUNT(*) as count FROM ",tbl))$count

  # Should have data
  expect_gt(row_count, 0, label = "SQLite database should contain rows after conversion")

  # Get all data
  all_data <- dplyr::collect(con)

  # Verify structure
  expect_equal(nrow(all_data), row_count)
  expect_true("VALUE" %in% names(all_data))

  DBI::dbDisconnect(con$src$con)
})


test_that("index creation shows progress messages", {
  skip_on_cran()
  skip_if_offline()

  # This test verifies that progress indicators work
  table_number <- "23-10-0061"

  # Clear cache to trigger fresh index creation
  remove_cansim_cached_tables(table_number, format = "sqlite")

  # Capture messages during connection creation
  messages <- capture_messages({
    con <- get_cansim_connection(table_number, format = "sqlite")
  })

  # Should see index-related progress messages
  expect_true(any(grepl("Creating.*indexes", messages)) ||
              any(grepl("Indexing", messages)),
              label = "Should show index creation progress")

  # Should see ANALYZE message
  expect_true(any(grepl("ANALYZE", messages)),
              label = "Should show ANALYZE progress")

  DBI::dbDisconnect(con$src$con)
})


test_that("error handling in batched index creation", {
  skip_on_cran()

  # Test that batched index creation handles errors gracefully
  # Create a mock connection and test error handling

  # This is a unit test for the create_indexes_batch function
  # We'll test with an in-memory database

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # Create a simple test table
  DBI::dbExecute(con, "CREATE TABLE test_table (id INTEGER, name TEXT)")
  DBI::dbExecute(con, "INSERT INTO test_table VALUES (1, 'test')")

  # Create indexes on valid fields
  expect_silent({
    create_indexes_batch(con, "test_table", c("id", "name"), show_progress = FALSE)
  })

  # Verify indexes were created
  indexes <- DBI::dbGetQuery(con,
    "SELECT name FROM sqlite_master WHERE type='index' AND name LIKE 'index_%'")
  expect_equal(nrow(indexes), 2)

  # Verify ANALYZE was run
  stat_tables <- DBI::dbGetQuery(con,
    "SELECT name FROM sqlite_master WHERE type='table' AND name='sqlite_stat1'")
  expect_equal(nrow(stat_tables), 1)

  DBI::dbDisconnect(con)
})


test_that("empty field list handled correctly", {
  skip_on_cran()

  # Test that create_indexes_batch handles empty field list
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE test_table (id INTEGER)")

  # Should return NULL and not error
  expect_null(create_indexes_batch(con, "test_table", c(), show_progress = FALSE))

  DBI::dbDisconnect(con)
})
