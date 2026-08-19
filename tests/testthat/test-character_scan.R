# Tests for the #169 instrumentation, to be deleted together with R/cansim_character_scan.R once
# StatCan stops publishing these characters and the scan has nothing left to find.

NON_BREAKING_SPACE <- intToUtf8(0x00A0)

test_that("a cube is scanned as StatCan sent it", {
  # the shape the API returns, one dimension with a clean member and one with a bad name in each
  # language, the whole point being that no repair happens between the API and the count
  object <- list(
    productId="27100123",
    cubeTitleEn="Clean title",
    cubeTitleFr=paste0("Titre au cours des 12",NON_BREAKING_SPACE,"derniers mois"),
    dimension=list(
      list(dimensionNameEn=paste0("Performance",NON_BREAKING_SPACE," strategy"),
           dimensionNameFr="Strategie",
           member=list(list(memberNameEn="Total",memberNameFr="Total"),
                       list(memberNameEn="Other",memberNameFr=paste0("Autre",NON_BREAKING_SPACE)))),
      list(dimensionNameEn="Geography",dimensionNameFr="Geographie",
           member=list(list(memberNameEn="Canada",memberNameFr="Canada")))))

  scan <- cansim:::scan_cube_object(object)
  hits <- dplyr::bind_rows(scan$hits)

  expect_equal(scan$scanned$cansimTableNumber, "27-10-0123")
  expect_equal(scan$scanned$dimensions, 2)
  expect_equal(scan$scanned$members, 3)

  expect_equal(nrow(hits), 3)
  expect_equal(sort(hits$level), c("dimension","member","title"))
  expect_equal(hits$language[hits$level=="dimension"], "eng")
  expect_equal(hits$language[hits$level=="title"], "fra")
  # a member hit is labelled with the English name of its dimension whichever language it is in
  expect_equal(hits$dimension[hits$level=="member"], paste0("Performance",NON_BREAKING_SPACE," strategy"))
  # the value is unrepaired, that is the only reason to read the raw response
  expect_true(grepl(NON_BREAKING_SPACE, hits$value[hits$level=="member"]))
})

test_that("a clean cube produces no hits", {
  object <- list(productId="36100580",cubeTitleEn="Clean",cubeTitleFr="Propre",
                 dimension=list(list(dimensionNameEn="Geography",dimensionNameFr="Geographie",
                                     member=list(list(memberNameEn="Canada",memberNameFr="Canada")))))

  scan <- cansim:::scan_cube_object(object)
  expect_length(scan$hits, 0)
  expect_equal(scan$scanned$members, 1)
})

test_that("the summary counts by level, language and survey", {
  # assembled by hand rather than downloaded, so the arithmetic can be checked against numbers that
  # are known rather than against whatever StatCan happens to be publishing today
  scan <- list(
    hits=dplyr::tibble(
      cansimTableNumber=c("11-10-0001","11-10-0001","11-10-0002","11-10-0003"),
      level=c("title","member","member","dimension"),
      language=c("eng","fra","fra","eng"),
      dimension=c(NA,"Geography","Geography","Age"),
      value=c(paste0("a",NON_BREAKING_SPACE,"b"), paste0("c",NON_BREAKING_SPACE),
              "d\ne", paste0("f",NON_BREAKING_SPACE,"g",NON_BREAKING_SPACE))) %>%
      dplyr::mutate(escaped=cansim:::escape_statcan_characters(.data$value)),
    scanned=dplyr::tibble(
      cansimTableNumber=c("11-10-0001","11-10-0002","11-10-0003","11-10-0004"),
      dimensions=c(2L,2L,1L,1L),
      members=c(10L,10L,5L,5L),
      surveyCode=c("1001","1001, 1002","1002",""),
      surveyEn=c("First survey","First survey, Second survey","Second survey","")),
    failed=character(0))

  summary <- cansim:::summarize_statcan_character_problems(scan)

  expect_equal(summary$overall$scanned, c(4,6,30))
  expect_equal(summary$overall$english, c(1,1,0))
  expect_equal(summary$overall$french, c(0,0,2))
  expect_equal(summary$overall$total, c(1,1,2))

  expect_equal(summary$tables$tables, 4)
  expect_equal(summary$tables$affected, 3)
  expect_equal(summary$tables$english_only, 1)
  expect_equal(summary$tables$french_only, 1)
  expect_equal(summary$tables$both, 1)

  # a name holding two non-breaking spaces counts twice here and once everywhere else
  expect_equal(summary$characters$occurrences[summary$characters$code_point=="U+00A0"], 4)
  expect_equal(summary$characters$occurrences[summary$characters$code_point=="U+000A"], 1)

  # 11-10-0002 lists both surveys and is counted under each, so the survey rows overlap
  surveys <- summary$surveys
  expect_equal(surveys$tables[surveys$surveyCode=="1001"], 2)
  expect_equal(surveys$tables[surveys$surveyCode=="1002"], 2)
  expect_equal(surveys$total[surveys$surveyCode=="1001"], 3)
  expect_equal(surveys$total[surveys$surveyCode=="1002"], 2)
  # the name comes from the cube that lists the survey on its own, survey names have commas of
  # their own so a cube listing several cannot be taken apart again
  expect_equal(surveys$survey[surveys$surveyCode=="1001"], "First survey")
  expect_equal(surveys$survey[surveys$surveyCode=="1002"], "Second survey")
  # the table listing no survey at all is left out rather than given an empty survey
  expect_false("" %in% surveys$surveyCode)
})

test_that("scanning finds the characters StatCan is currently publishing", {
  skip_on_cran()

  # 27-10-0123 has a non-breaking space inside a dimension name, 37-10-0295 a line feed, and
  # 46-10-0072 leading non-breaking spaces on its French member names
  scan <- cansim:::scan_statcan_character_problems(c("27-10-0123","37-10-0295","46-10-0072"),
                                                  quiet=TRUE)
  skip_if(is.null(scan), "StatCan unavailable")

  expect_equal(scan$failed, character(0))
  expect_equal(nrow(scan$scanned), 3)
  expect_true(all(scan$scanned$members > 0))

  # if this test starts failing, StatCan may have fixed these tables, which is the point of #169
  expect_true("Performance<U+00A0> strategy" %in%
                scan$hits$escaped[scan$hits$cansimTableNumber=="27-10-0123"])
  expect_true("Geographic region<U+000A>" %in%
                scan$hits$escaped[scan$hits$cansimTableNumber=="37-10-0295"])
  expect_true(any(scan$hits$cansimTableNumber=="46-10-0072" & scan$hits$language=="fra"))

  summary <- cansim:::summarize_statcan_character_problems(scan)
  expect_equal(sum(summary$overall$total), nrow(scan$hits))
  # nothing was fetched through the cube list, so there is nothing to attribute to a survey
  expect_null(summary$surveys)
})
