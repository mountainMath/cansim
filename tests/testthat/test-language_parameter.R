NBSP <- intToUtf8(0x00A0)
E_ACUTE <- intToUtf8(0x00E9)
C_CEDILLA <- intToUtf8(0x00E7)

test_that("both languages are named in either language", {
  english <- c("english","English","ENGLISH","en","EN","eng","engl",
               "anglais","Anglais","anglaise","ang","angl"," english ")
  for (name in english) expect_equal(cansim:::cleaned_ndm_language(name), "eng", info=name)

  french <- c("french","French","FRENCH","fr","FR","fra","fren",
              "francais","Francais","francaise","franc","fran"," french ")
  for (name in french) expect_equal(cansim:::cleaned_ndm_language(name), "fra", info=name)

  # the accented spellings are the ones a French speaker would actually type
  accented <- c(paste0("fran",C_CEDILLA,"ais"),
                paste0("Fran",C_CEDILLA,"ais"),
                paste0("fran",C_CEDILLA,"aise"))
  for (name in accented) expect_equal(cansim:::cleaned_ndm_language(name), "fra")

  # the codes the package stores in cache paths have to survive a round trip
  expect_equal(cansim:::cleaned_ndm_language("eng"), "eng")
  expect_equal(cansim:::cleaned_ndm_language("fra"), "fra")
})

test_that("an unrecognized language is an error rather than an NA", {
  expect_error(cansim:::cleaned_ndm_language("de"), 'Unknown language "de"')
  expect_error(cansim:::cleaned_ndm_language("de"), "case and accents are ignored")
  # the message names what was passed, not what it was normalized to
  expect_error(cansim:::cleaned_ndm_language("Deutsch"), 'Unknown language "Deutsch"')
  expect_error(cansim:::cleaned_ndm_language(""), "Unknown language")
  expect_error(cansim:::cleaned_ndm_language(NA), "Unknown language")

  # a language that is only a fragment of a name is not a language
  expect_error(cansim:::cleaned_ndm_language("e"), "Unknown language")
  expect_error(cansim:::cleaned_ndm_language("f"), "Unknown language")
  expect_error(cansim:::cleaned_ndm_language("germanic"), "Unknown language")

  # a name carrying the characters StatCan likes to send is not silently accepted either
  expect_error(cansim:::cleaned_ndm_language(paste0("english",NBSP)), "Unknown language")
})

test_that("several languages can be asked for at once", {
  # remove_cansim_cached_tables() asks for both languages in one call
  expect_equal(cansim:::cleaned_ndm_language(c("eng","fra")), c("eng","fra"))
  expect_equal(cansim:::cleaned_ndm_language(c("English","francais")), c("eng","fra"))
  expect_equal(cansim:::cleaned_ndm_language(character(0)), logical(0))

  # every offending entry is named, not just the first
  expect_error(cansim:::cleaned_ndm_language(c("en","de","es")), '"de", "es"')
  expect_error(cansim:::cleaned_ndm_language(c("en","de")), "Unknown language")
})

test_that("exported functions default to english", {
  language_default <- function(f) {
    default <- formals(get(f, envir=asNamespace("cansim")))$language
    if (is.null(default)) NA_character_ else as.character(default)
  }
  functions <- Filter(function(f) {
    fn <- get(f, envir=asNamespace("cansim"))
    is.function(fn) && "language" %in% names(formals(fn))
  }, getNamespaceExports("cansim"))

  defaults <- vapply(functions, language_default, character(1))
  # NULL is a deliberate default on the removal functions, it means every language
  expect_setequal(unique(defaults[!is.na(defaults)]), "english")
  expect_setequal(names(defaults)[is.na(defaults)],
                  c("remove_cansim_cached_tables","remove_cansim_sqlite_cached_table"))
})

test_that("an unrecognized language stops before anything is downloaded", {
  # these used to travel as far as a StatCan URL or a cache path and fail there instead
  expect_error(get_cansim_table_url("34-10-0013", language="de"), "Unknown language")
  expect_error(get_cansim_table_overview("34-10-0013", language="de"), "Unknown language")
  expect_error(get_cansim("34-10-0013", language="de"), "Unknown language")
  expect_error(get_cansim_connection("34-10-0013", language="de"), "Unknown language")
  expect_error(get_cansim_vector("v41690973", language="de"), "Unknown language")
})
