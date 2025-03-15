test_that("classification codes play nice with factor levels", {
  skip_on_cran()

  d <- cansim::get_cansim("10-10-0016") |>
    distinct(`Statement of operations and balance sheet`)

  expect_equal(sum(is.na(d)),0)
})
