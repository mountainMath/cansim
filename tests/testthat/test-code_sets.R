failed_code_sets <- list(status="FAILED",
                         object="getCodeSets threw unhandled exception. Error code = GENERIC_REQUEST_FAILED")
good_code_sets <- list(status="SUCCESS",
                       object=list(uom=list(list(memberUomCode=0, memberUomEn="units", memberUomFr="unités"))))

code_set_with_answers <- function(answers, ...) {
  calls <- 0
  result <- with_mocked_bindings(
    get_cansim_code_set("uom", ...),
    get_with_timeout_retry=function(...) structure(list(), class="httr2_response"),
    statcan_response_json=function(response) { calls <<- calls + 1; answers[[min(calls, length(answers))]] },
    .package="cansim")
  list(result=result, calls=calls)
}

test_that("a FAILED code set answer is retried and never cached", {
  path <- file.path(tempdir(), "cansim_code_sets.Rmd")
  withr::defer(unlink(path))
  local_mocked_bindings(Sys.sleep=function(...) NULL, .package="base")

  unlink(path)
  expect_warning(out <- code_set_with_answers(list(failed_code_sets)), "GENERIC_REQUEST_FAILED")
  expect_null(out$result)
  expect_identical(out$calls, 3)
  expect_false(file.exists(path))

  out <- code_set_with_answers(list(failed_code_sets, good_code_sets))
  expect_identical(out$result$memberUomEn, "units")
  expect_identical(out$calls, 2)
  expect_true(file.exists(path))
})

test_that("a FAILED answer left in the cache is refetched", {
  path <- file.path(tempdir(), "cansim_code_sets.Rmd")
  withr::defer(unlink(path))
  saveRDS(failed_code_sets, path)
  out <- code_set_with_answers(list(good_code_sets))
  expect_identical(out$result$memberUomEn, "units")
  expect_identical(out$calls, 1)
})
