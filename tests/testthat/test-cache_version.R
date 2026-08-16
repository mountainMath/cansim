NBSP <- intToUtf8(0x00A0)

new_cache_dir <- function() {
  dir <- tempfile("cansim_cache_version_test_")
  dir.create(dir)
  dir
}

test_that("the cache records the package version it was parsed under", {
  dir <- new_cache_dir()
  base <- file.path(dir,"13100920-eng.Rda")

  # a cache with no marker was written before the marker existed, which is to say before 0.4.5
  expect_null(cansim:::read_cache_version(dir))
  expect_true(cansim:::cache_predates_value_repair(dir))

  cansim:::write_cache_version(base)
  expect_true(file.exists(paste0(base,"_version")))
  expect_equal(cansim:::read_cache_version(dir), utils::packageVersion("cansim"))
  expect_false(cansim:::cache_predates_value_repair(dir))

  # an unreadable or nonsensical marker is treated the same as a missing one rather than erroring
  saveRDS("not a version",paste0(base,"_version"))
  expect_null(cansim:::read_cache_version(dir))
  expect_true(cansim:::cache_predates_value_repair(dir))

  saveRDS("0.4.4",paste0(base,"_version"))
  expect_true(cansim:::cache_predates_value_repair(dir))
})

test_that("an old cache is checked against the metadata cached with it", {
  dir <- new_cache_dir()
  base <- file.path(dir,"13100920-eng.Rda")

  saveRDS(dplyr::tibble(`Dimension ID`=1L,`Dimension name`="Indicators"),paste0(base,"2"))
  saveRDS(dplyr::tibble(`Member ID`=1:2,
                        `Member Name`=c("Private insurance",paste0("Dental coverage, none",NBSP))),
          paste0(base,"_column_1"))
  # footnotes are not looked at, a line feed inside footnote text is part of the text
  saveRDS(dplyr::tibble(Footnote="A footnote\nover two lines"),paste0(base,"5"))

  expect_equal(cansim:::stale_cached_labels(dir), paste0("Dental coverage, none",NBSP))

  # a cache whose labels hold none of these characters is not worth reporting on
  saveRDS(dplyr::tibble(`Member ID`=1:2,`Member Name`=c("Private insurance","Public insurance only")),
          paste0(base,"_column_1"))
  expect_equal(cansim:::stale_cached_labels(dir), character(0))

  # dimension names are covered as well as member labels
  saveRDS(dplyr::tibble(`Dimension ID`=1L,`Dimension name`=paste0("Performance",NBSP,"strategy")),
          paste0(base,"2"))
  expect_equal(cansim:::stale_cached_labels(dir), paste0("Performance",NBSP,"strategy"))
})

test_that("a cached table carries its version and reports on stale labels", {
  skip_on_cran()

  cache_path <- new_cache_dir()
  old_cache_path <- Sys.getenv("CANSIM_CACHE_PATH")
  Sys.setenv(CANSIM_CACHE_PATH=cache_path)
  on.exit(Sys.setenv(CANSIM_CACHE_PATH=old_cache_path), add=TRUE)

  table <- "13-10-0920"
  connection <- suppressWarnings(get_cansim_connection(table, format="parquet", refresh=TRUE))
  disconnect_cansim_sqlite(connection)

  cached <- list_cansim_cached_tables(cache_path=cache_path)
  expect_equal(cached$cansimVersion, as.character(utils::packageVersion("cansim")))

  # a table cached under the current version has nothing to report
  expect_no_warning(get_cansim_connection(table, format="parquet"))

  # a cache written before 0.4.5 has no marker and holds the characters StatCan sent in the metadata
  # cached with it, which is what puts them in the data next to it
  table_dir <- file.path(cache_path,dir(cache_path,"parquet"))
  file.remove(file.path(table_dir,dir(table_dir,"\\.Rda_version$")))
  member_file <- file.path(table_dir,dir(table_dir,"\\.Rda_column_1$"))
  members <- readRDS(member_file)
  member_name_column <- grep("Member Name",names(members),value=TRUE)
  members[[member_name_column]][1] <- paste0(members[[member_name_column]][1],NBSP)
  saveRDS(members,member_file)
  expect_warning(get_cansim_connection(table, format="parquet"), "built by cansim 0.4.4 or earlier")
  expect_warning(get_cansim_connection(table, format="parquet"), "refresh=TRUE", fixed=TRUE)

  old <- options(cansim.suppress_repair_warnings=TRUE)
  on.exit(options(old), add=TRUE)
  expect_no_warning(get_cansim_connection(table, format="parquet"))
})
