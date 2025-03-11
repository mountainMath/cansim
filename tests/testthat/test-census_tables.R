test_that("Basic parsing check for census tables", {
  d <- get_cansim("98-10-0036-01")
  expect_equal(nrow(d),336)
})
