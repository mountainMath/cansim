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
