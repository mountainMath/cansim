# Precompile vignettes


library(knitr)
library(tools)
library(purrr)

# Convert *.orig to *.Rmd -------------------------------------------------

orig_files <- file.path(list.files(path = "vignettes/", pattern = "*\\.Rmd\\.orig", full.names = TRUE))

walk(orig_files, ~knit(.x, file_path_sans_ext(.x)))


# Move .png files into correct directory so they render -------------------

images <- file.path(list.files(".", pattern = 'vignette-fig.*\\.png$'))

success <- file.copy(from = images,
                     to = file.path("vignettes", images),
                     overwrite = TRUE)


# Clean up if successful --------------------------------------------------

if (!all(success)) {
  stop("Image files were not successfully transferred to vignettes directory")
} else {
  unlink(images)
}
