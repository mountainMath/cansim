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
    # GEO and UOM hold translated names, only their language-independent codes are comparable
    common_names <- intersect(names(d1),names(d2)) |> setdiff(c("GEO","UOM"))
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

  vectors_en <- get_cansim_data_for_table_coord_periods(list("35-10-0003"=c("1.1","1.12")),periods=3,language="eng")
  vectors_fr <- get_cansim_data_for_table_coord_periods(list("35-10-0003"=c("1.1","1.12")),periods=3,language="fra")

  count_differences <- function(d1,d2) {
    # GEO and UOM hold translated names, only their language-independent codes are comparable
    common_names <- intersect(names(d1),names(d2)) |> setdiff(c("GEO","UOM"))
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

  table <- "98-10-0036"

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

  remove_cansim_cached_tables(table)
})

test_that("consistent cache", {
  skip_on_cran()

  table <- "98-10-0036"

  tables.statcan <- get_cansim(table, refresh=TRUE)
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

test_that("unit of measure for coordinate and vector data matches the full table", {
  skip_on_cran()

  # the unit of measure varies by member of the dimension StatCan flags with hasUom, so the
  # check covers several members of a table that uses a range of different units
  members <- get_cansim_cube_metadata("18-10-0004", type="members") |>
    dplyr::filter(.data$hasUom) |>
    dplyr::slice_head(n=1, by="memberUomCode")

  coordinates <- paste0("2.", members$memberId)

  by_coordinate <- get_cansim_data_for_table_coord_periods(list("18-10-0004"=coordinates), periods=1)

  expect_true(all(c("UOM","UOM_ID") %in% names(by_coordinate)))
  expect_gt(dplyr::n_distinct(by_coordinate$UOM_ID), 1)

  full_table <- get_cansim("18-10-0004") |>
    dplyr::select("COORDINATE","UOM","UOM_ID") |>
    unique() |>
    dplyr::mutate(COORDINATE=gsub("(\\.0)+$","",.data$COORDINATE))

  comparison <- by_coordinate |>
    dplyr::select("COORDINATE","UOM","UOM_ID") |>
    unique() |>
    dplyr::inner_join(full_table, by="COORDINATE", suffix=c("",".expected"))

  expect_equal(nrow(comparison), length(coordinates))
  expect_equal(comparison$UOM, comparison$UOM.expected)
  expect_equal(comparison$UOM_ID, comparison$UOM_ID.expected)
})

test_that("tables without a unit of measure get no unit columns", {
  skip_on_cran()

  # census tables carry uom code 0, StatCan's marker for "no unit", and their full table
  # download has no unit columns either
  by_coordinate <- get_cansim_data_for_table_coord_periods(list("98-10-0036"=c("1.1.1","1.2.1")), periods=1)

  expect_false(any(c("UOM","UOM_ID") %in% names(by_coordinate)))
  expect_false(any(c("UOM","UOM_ID") %in% names(get_cansim("98-10-0036"))))
})
