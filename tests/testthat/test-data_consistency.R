test_that("consistent data output", {
  skip_on_cran()
  formats <- c("parquet","feather","sqlite")

  filter_function <- function(data) {
    data |>
      dplyr::filter(GEO=="Canada",
                    `Seasonal adjustment`=="Unadjusted",
                    Sales=="Units",
                    `Origin of manufacture`=="Total, country of manufacture",
                    `Vehicle type` %in% c("Passenger cars","Trucks"))
  }


  tables <- formats |>
    lapply(\(f) get_cansim_connection("20-10-0001", format=f, refres="auto") |>
    filter_function() |>
    collect_and_normalize(disconnect=TRUE)) |>
    setNames(formats)
  tables$memory <- get_cansim("20-10-0001") |>
    filter_function()

  count_differences <- function(d1,d2) {
    d1 <- d1 |>
      dplyr::mutate(SCALAR_FACTOR=gsub(" +$","",SCALAR_FACTOR)) |>
      dplyr::arrange(Date,COORDINATE)
    d2 <- d2 |>
      dplyr::mutate(SCALAR_FACTOR=gsub(" +$","",SCALAR_FACTOR)) |>
      dplyr::arrange(Date,COORDINATE)

    (d1==d2) |> dplyr::as_tibble() |> dplyr::summarize_all(\(x) sum(!is.na(x) & x==FALSE)) |> rowSums()
  }

  expect_equal(count_differences(tables$parquet,tables$memory),0)
  expect_equal(count_differences(tables$feather,tables$memory),0)
  expect_equal(count_differences(tables$sqlite,tables$memory),0)
})


test_that("consistent vector data output", {
  skip_on_cran()

  filter_function <- function(data) {
    data |>
      dplyr::filter(GEO=="Canada",
                    `Seasonal adjustment`=="Unadjusted",
                    Sales=="Units",
                    `Origin of manufacture`=="Total, country of manufacture",
                    `Vehicle type` %in% c("Passenger cars","Trucks"))
  }


  table <- get_cansim_connection("20-10-0001", format="parquet", refres="auto") |>
             filter_function() |>
             collect_and_normalize()

  vectors <- get_cansim_vector(c("v42169920", "v42169933"))

  common_names <- intersect(names(table),names(vectors)) |>
    setdiff("REF_DATE")


  count_differences <- function(d1,d2) {
    d1 <- d1 |>
      dplyr::arrange(Date,COORDINATE)
    d2 <- d2 |>
      dplyr::arrange(Date,COORDINATE)

    (d1==d2) |> dplyr::as_tibble() |> dplyr::summarize_all(\(x) sum(!is.na(x) & x==FALSE)) |> rowSums()
  }

  expect_equal(count_differences(dplyr::select(table,any_of(common_names)),dplyr::select(vectors,any_of(common_names))),0)
})

test_that("consistent vector languages", {
  skip_on_cran()

  vectors_en <- get_cansim_vector(c("v42169920", "v42169933"),language="eng")
  vectors_fr <- get_cansim_vector(c("v42169920", "v42169933"),language="fra")

  count_differences <- function(d1,d2) {
    common_names <- intersect(names(d1),names(d2)) |> setdiff("GEO")
    d1 <- d1 |>
      select(all_of(common_names)) |>
      dplyr::arrange(Date,COORDINATE)
    d2 <- d2 |>
      select(all_of(common_names)) |>
      dplyr::arrange(Date,COORDINATE)


    (d1==d2) |> dplyr::as_tibble() |> dplyr::summarize_all(\(x) sum(!is.na(x) & x==FALSE)) |> rowSums()
  }

  expect_equal(dim(vectors_en),dim(vectors_fr))
  new_names <- paste0("X",seq(1,ncol(vectors_en)))
  expect_equal(count_differences(vectors_en,vectors_fr |> rename_columns_for_language("fra","eng")),0)
})


test_that("consistent coordinate languages", {
  skip_on_cran()

  vectors_en <- get_cansim_data_for_table_coord_periods("35-10-0003",coordinate="1.12",periods=3,language="eng")
  vectors_fr <- get_cansim_data_for_table_coord_periods("35-10-0003",coordinate="1.12",periods=3,language="fra")

  count_differences <- function(d1,d2) {
    common_names <- intersect(names(d1),names(d2)) |> setdiff("GEO")
    d1 <- d1 |>
      select(all_of(common_names)) |>
      dplyr::arrange(Date,COORDINATE)
    d2 <- d2 |>
      select(all_of(common_names)) |>
      dplyr::arrange(Date,COORDINATE)


    (d1==d2) |> dplyr::as_tibble() |> dplyr::summarize_all(\(x) sum(!is.na(x) & x==FALSE)) |> rowSums()
  }

  expect_equal(dim(vectors_en),dim(vectors_fr))
  new_names <- paste0("X",seq(1,ncol(vectors_en)))
  expect_equal(count_differences(vectors_en,vectors_fr |> rename_columns_for_language("fra","eng")),0)
})

test_that("consistent census tables", {
  skip_on_cran()
  formats <- c("parquet","feather","sqlite")

  table <- "98-10-0036-01"

  tables <- formats |>
    lapply(\(f) get_cansim_connection(table, format=f, refres="auto") |>
             collect_and_normalize(disconnect=TRUE)) |>
    setNames(formats)
  tables$memory <- get_cansim(table)

  count_differences <- function(d1,d2) {
    d1 <- d1 |>
      dplyr::arrange(REF_DATE,COORDINATE)
    d2 <- d2 |>
      dplyr::arrange(REF_DATE,COORDINATE)

    (d1==d2) |> dplyr::as_tibble() |> dplyr::summarize_all(\(x) sum(!is.na(x) & x==FALSE)) |> rowSums()
  }

  expect_equal(count_differences(tables$parquet,tables$memory),0)
  expect_equal(count_differences(tables$feather,tables$memory),0)
  expect_equal(count_differences(tables$sqlite,tables$memory),0)
})

test_that("consistent cache", {
  skip_on_cran()

  table <- "98-10-0036-01"

  tables.statcan <- get_cansim(table)
  tables.cache <- get_cansim(table)

  count_differences <- function(d1,d2) {
    d1 <- d1 |>
      dplyr::arrange(REF_DATE,COORDINATE)
    d2 <- d2 |>
      dplyr::arrange(REF_DATE,COORDINATE)

    (d1==d2) |> dplyr::as_tibble() |> dplyr::summarize_all(\(x) sum(!is.na(x) & x==FALSE)) |> rowSums()
  }

  expect_equal(count_differences(tables.statcan,tables.cache),0)
})
