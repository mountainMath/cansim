NON_BREAKING_SPACE <- intToUtf8(0x00A0)
ZERO_WIDTH_SPACE <- intToUtf8(0x200B)
PROBLEM_CHARACTERS <- paste0("[",intToUtf8(c(0x00A0,0x0009,0x000A,0x000D,0x200B,0xFEFF)),"]")

has_problem_characters <- function(x) any(grepl(PROBLEM_CHARACTERS,x),na.rm=TRUE)

test_that("non-standard whitespace is repaired", {
  repair <- cansim:::repair_statcan_strings

  expect_equal(repair(paste0("Characteristics",NON_BREAKING_SPACE)), "Characteristics")
  expect_equal(repair(paste0("Factors for job promotion ",NON_BREAKING_SPACE)), "Factors for job promotion")
  expect_equal(repair(paste0("Performance",NON_BREAKING_SPACE,"strategy")), "Performance strategy")
  expect_equal(repair(paste0("Performance",NON_BREAKING_SPACE," strategy")), "Performance strategy")
  expect_equal(repair("Geographic region\n"), "Geographic region")
  expect_equal(repair("a\tb"), "a b")

  # zero width characters are dropped rather than turned into a space
  expect_equal(repair(paste0("ab",ZERO_WIDTH_SPACE,"cd")), "abcd")
})

test_that("repair leaves ordinary strings untouched", {
  repair <- cansim:::repair_statcan_strings

  # squishing and trimming must not reach strings that hold none of the problem characters
  expect_equal(repair("Already clean"), "Already clean")
  expect_equal(repair("Double  space kept"), "Double  space kept")
  expect_equal(repair(" leading and trailing kept "), " leading and trailing kept ")
  expect_equal(repair(c(NA_character_,"")), c(NA_character_,""))
  expect_equal(repair(character(0)), character(0))
  expect_equal(repair(1:3), 1:3)

  # applying the repair twice changes nothing further
  once <- repair(paste0("a",NON_BREAKING_SPACE,"b"))
  expect_equal(repair(once), once)
})

test_that("values are repaired through their distinct values", {
  repair <- cansim:::repair_statcan_values

  x <- c(paste0("Total",NON_BREAKING_SPACE),"Total","Other",paste0("Total",NON_BREAKING_SPACE))
  expect_equal(repair(x), c("Total","Total","Other","Total"))

  # scanning distinct values must give the same answer as scanning every one of them
  y <- rep(c(paste0("a",NON_BREAKING_SPACE,"b"),"c",NA_character_), 50)
  expect_equal(repair(y), cansim:::repair_statcan_strings(y))

  expect_equal(repair(c("clean","values")), c("clean","values"))
  expect_equal(repair(character(0)), character(0))
  expect_equal(repair(1:3), 1:3)
})

test_that("the dimension columns of the data are the ones that get repaired", {
  columns <- cansim:::dimension_columns_in_data

  # StatCan names the geography dimension in the metadata but calls the column GEO in the data
  expect_equal(columns(c("REF_DATE","GEO","Age group","VALUE"),
                       c("Geography","Age group"),"eng"),
               c("GEO","Age group"))
  expect_equal(columns(c("PERIODE DE REFERENCE",paste0("G",intToUtf8(0x00C9),"O"),"Sexe"),
                       c(paste0("G",intToUtf8(0x00E9),"ographie"),"Sexe"),"fra"),
               c(paste0("G",intToUtf8(0x00C9),"O"),"Sexe"))

  # the coordinate column holds one distinct value per series, scanning it is what this avoids
  expect_false("COORDINATE" %in% columns(c("COORDINATE","GEO"),c("Geography"),"eng"))
  # a dimension that has no column in the data is skipped rather than erroring
  expect_equal(columns(c("REF_DATE","VALUE"),c("Geography","Age group"),"eng"), character(0))
})

test_that("member labels in the data match the repaired metadata", {
  skip_on_cran()

  # 13-10-0920 has a member label ending in a non-breaking space. The metadata is repaired, so a
  # data value that is not repaired the same way falls outside the factor levels and becomes NA.
  table <- "13-10-0920"
  column <- "Indicators"
  member <- "Dental insurance coverage, none"

  check <- function(data,label) {
    expect_false(has_problem_characters(levels(data[[column]])), label=paste0(label," levels"))
    expect_true(member %in% levels(data[[column]]), label=paste0(label," member"))
    expect_equal(sum(is.na(data[[column]])), 0, label=paste0(label," NA count"))
  }

  # each of the three readers parses the csv on its own and needs the repair separately
  check(suppressWarnings(get_cansim(table, refresh=TRUE)), "get_cansim")

  for (format in c("sqlite","parquet")) {
    connection <- suppressWarnings(get_cansim_connection(table, format=format, refresh=TRUE))
    check(suppressWarnings(collect_and_normalize(connection)), format)
    disconnect_cansim_sqlite(connection)
  }

  # the label is the same whichever way it is retrieved, so the two can be joined
  template <- suppressWarnings(get_cansim_table_template(table))
  expect_true(member %in% template[[column]])
})

test_that("cube metadata and table templates have usable names", {
  skip_on_cran()

  # 46-10-0101 has non-breaking spaces inside a dimension name, 13-10-0397 has one trailing,
  # 37-10-0295 has a line feed
  for (table in c("46-10-0101","13-10-0397","37-10-0295")) {
    members <- suppressWarnings(get_cansim_cube_metadata(table, type="members", refresh=TRUE))
    expect_false(has_problem_characters(members$dimensionNameEn))
    expect_false(has_problem_characters(members$dimensionNameFr))
    expect_false(has_problem_characters(members$memberNameEn))
    expect_false(has_problem_characters(members$memberNameFr))

    template <- suppressWarnings(get_cansim_table_template(table))
    expect_false(has_problem_characters(names(template)))
  }
})

test_that("repaired columns are reachable and still carry their metadata", {
  skip_on_cran()

  table <- "27-10-0123"
  data <- suppressWarnings(get_cansim(table, refresh=TRUE))

  expect_false(has_problem_characters(names(data)))
  # the dimension StatCan spells with a non-breaking space can be addressed by its plain name
  expect_true("Performance strategy" %in% names(data))
  expect_equal(nrow(dplyr::select(data,"Performance strategy")), nrow(data))

  # metadata still folds in, which only works when the data columns and the metadata dimension
  # names are repaired the same way
  expect_true("Hierarchy for Performance strategy" %in% names(data))
  expect_true("Classification Code for Performance strategy" %in% names(data))

  expect_false(any(vapply(data[vapply(data,is.factor,logical(1))],
                          \(f) has_problem_characters(levels(f)), logical(1))))
})

test_that("the offending characters are shown by code point", {
  escape <- cansim:::escape_statcan_characters
  abbreviate <- cansim:::abbreviate_around_escape

  expect_equal(escape(paste0("Performance",NON_BREAKING_SPACE," strategy")), "Performance<U+00A0> strategy")
  expect_equal(escape("Geographic region\n"), "Geographic region<U+000A>")
  expect_equal(escape(paste0("ab",ZERO_WIDTH_SPACE,"cd")), "ab<U+200B>cd")
  expect_equal(escape("nothing to escape"), "nothing to escape")

  # a short name is shown whole, a long one is windowed so the marker stays visible
  expect_equal(abbreviate("a<U+00A0>b"), "a<U+00A0>b")
  long <- abbreviate(escape(paste0(strrep("x",90),NON_BREAKING_SPACE,"31")))
  expect_true(nchar(long) < 90)
  expect_true(endsWith(long, "<U+00A0>31"))
})

test_that("repairing warns about what was changed", {
  skip_on_cran()

  # the warning has to show the name as StatCan sent it, a repaired name would hide the problem
  expect_warning(get_cansim_cube_metadata("13-10-0397", type="members", refresh=TRUE),
                 "Characteristics<U+00A0>", fixed=TRUE)
  expect_warning(get_cansim_cube_metadata("13-10-0397", type="members", refresh=TRUE),
                 "non-breaking spaces or control characters")

  old <- options(cansim.suppress_repair_warnings=TRUE)
  on.exit(options(old), add=TRUE)
  expect_no_warning(get_cansim_cube_metadata("13-10-0397", type="members", refresh=TRUE))
})
