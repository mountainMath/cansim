test_that("the default period count is the largest value the latestN methods accept", {
  # StatCan rejects a latestN above the signed 32-bit maximum with a JSON syntax error, so the
  # default has to sit exactly on that bound rather than at an arbitrary large round number.
  expect_identical(cansim:::MAX_PERIODS, .Machine$integer.max)
  expect_identical(cansim:::MAX_PERIODS, as.integer(cansim:::MAX_PERIODS))
})

test_that("clean_periods treats an absent count as every period", {
  expect_identical(cansim:::clean_periods(NULL), cansim:::MAX_PERIODS)
  expect_identical(cansim:::clean_periods(NA), cansim:::MAX_PERIODS)
  expect_identical(cansim:::clean_periods(NA_integer_), cansim:::MAX_PERIODS)
  expect_identical(cansim:::clean_periods(integer(0)), cansim:::MAX_PERIODS)
})

test_that("clean_periods caps counts that the API could not encode", {
  expect_identical(cansim:::clean_periods(Inf), cansim:::MAX_PERIODS)
  expect_identical(cansim:::clean_periods(1e10), cansim:::MAX_PERIODS)
  # as.integer() alone would silently turn these into NA and send "latestN":NA to StatCan
  expect_false(is.na(cansim:::clean_periods(1e10)))
})

test_that("clean_periods keeps an explicit count and stays vectorized", {
  expect_identical(cansim:::clean_periods(10), 10L)
  expect_identical(cansim:::clean_periods(c(5, NA, 1e10, 2)),
                   c(5L, cansim:::MAX_PERIODS, cansim:::MAX_PERIODS, 2L))
})

test_that("clean_periods refuses a count the API would reject outright", {
  expect_error(cansim:::clean_periods(0), "at least 1")
  expect_error(cansim:::clean_periods(-4), "at least 1")
  expect_error(cansim:::clean_periods(c(3, 0)), "at least 1")
})

test_that("the default period count serializes into the request body as a plain integer", {
  # a double default would paste as 2.147484e+09 and break the JSON body
  expect_identical(paste0('"latestN":', cansim:::clean_periods(NULL)),
                   '"latestN":2147483647')
})
