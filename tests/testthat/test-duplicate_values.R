test_that("duplicate base values", {
  skip_on_cran()
  skip_if(!nzchar(Sys.getenv("COMPILE_VIG")))

  d <- get_cansim("36-10-0108")
  s1 <- levels(d$Estimates)
  s2 <- unique(dplyr::arrange(d,Estimates)$Estimates)
  expect_equal(length(s1),length(s2))
  expect_equal(sum(s1!=s2),0)
})

test_that("duplicate child values", {
  skip_on_cran()
  skip_if(!nzchar(Sys.getenv("COMPILE_VIG")))

  d <- get_cansim("36-10-0580")
  s1 <- levels(d$Categories)
  s2 <- unique(dplyr::arrange(d,Categories)$Categories)
  expect_equal(length(s1),length(s2))
  expect_equal(sum(s1!=s2),0)
})
