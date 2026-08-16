NBSP <- intToUtf8(0x00A0)

new_cache_dir <- function() {
  dir <- tempfile("cansim_cache_version_test_")
  dir.create(dir)
  dir
}

test_that("the cache records when it was downloaded and what parsed it", {
  dir <- new_cache_dir()
  base <- file.path(dir,"13100920-eng.Rda")
  downloaded <- as.POSIXct("2026-08-16 11:06:30")

  # nothing on disk at all
  expect_equal(cansim:::read_cache_info(dir), list(timeCached=NA_character_,cansimVersion=NA_character_))
  expect_null(cansim:::read_cache_version(dir))
  expect_true(cansim:::cache_predates_value_repair(dir))

  cansim:::write_cache_info(base,downloaded)
  expect_true(file.exists(paste0(base,"_info")))
  info <- cansim:::read_cache_info(dir)
  expect_equal(info$timeCached, "2026-08-16 11:06:30")
  expect_equal(info$cansimVersion, as.character(utils::packageVersion("cansim")))
  expect_equal(cansim:::read_cache_version(dir), utils::packageVersion("cansim"))
  expect_false(cansim:::cache_predates_value_repair(dir))

  # a nonsensical version is treated the same as a missing one rather than erroring
  saveRDS(list(timeCached="2026-08-16 11:06:30",cansimVersion="not a version"),paste0(base,"_info"))
  expect_null(cansim:::read_cache_version(dir))
  expect_true(cansim:::cache_predates_value_repair(dir))
  expect_equal(cansim:::read_cache_info(dir)$timeCached, "2026-08-16 11:06:30")
})

test_that("a cache written before 0.4.5 still gives up its timestamp", {
  dir <- new_cache_dir()
  base <- file.path(dir,"13100920-eng.Rda")

  # up to 0.4.4 the timestamp lived on its own and there was no version
  saveRDS("2025-08-16 14:16:06",paste0(base,"_time"))
  expect_equal(cansim:::read_cache_info(dir),
               list(timeCached="2025-08-16 14:16:06",cansimVersion=NA_character_))
  expect_true(cansim:::cache_predates_value_repair(dir))

  # refreshing such a cache replaces the old file rather than leaving it behind to go stale
  cansim:::write_cache_info(base,as.POSIXct("2026-08-16 11:06:30"))
  expect_false(file.exists(paste0(base,"_time")))
  expect_equal(cansim:::read_cache_info(dir)$timeCached, "2026-08-16 11:06:30")
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
  info_file <- file.path(table_dir,dir(table_dir,"\\.Rda_info$"))
  saveRDS(readRDS(info_file)["timeCached"],info_file)
  member_file <- file.path(table_dir,dir(table_dir,"\\.Rda_column_1$"))
  members <- readRDS(member_file)
  member_name_column <- grep("Member Name",names(members),value=TRUE)
  members[[member_name_column]][1] <- paste0(members[[member_name_column]][1],NBSP)
  saveRDS(members,member_file)
  expect_warning(get_cansim_connection(table, format="parquet"))
  # the warning is wrapped to the console width, so it is matched on the collapsed text
  warning <- warning_text(get_cansim_connection(table, format="parquet"))
  expect_match(warning, "built by cansim 0.4.4 or earlier", fixed=TRUE)
  expect_match(warning, "refresh=TRUE", fixed=TRUE)

  old <- options(cansim.suppress_repair_warnings=TRUE)
  on.exit(options(old), add=TRUE)
  expect_no_warning(get_cansim_connection(table, format="parquet"))
})
