# Precompile vignettes


library(knitr)
library(tools)
library(purrr)

# Convert *.orig to *.Rmd -------------------------------------------------

orig_files <- file.path(list.files(path = here::here("vignettes/"), pattern = "*\\.Rmd\\.orig", full.names = TRUE))

walk(orig_files, ~knit(.x, file_path_sans_ext(.x)))


# Move .png files into correct directory so they render -------------------

source_image_path <- here::here("figure")
target_image_path <- here::here("vignettes/figure")

if (dir.exists(source_image_path)) {
  if (dir.exists(target_image_path)) unlink(target_image_path,recursive=TRUE)
  success <- file.rename(source_image_path,target_image_path)
} else success <- FALSE

# images <- file.path(list.files(here::here("figure"), pattern = 'vignette-fig.*\\.png$'))
#
# success <- file.copy(from = images,
#                      to = file.path("vignettes", images),
#                      overwrite = TRUE)


# Clean up if successful --------------------------------------------------

if (!all(success)) {
  stop("Image files were not successfully transferred to vignettes directory")
} else {
  #unlink(images)
  message("Image files were successfully transferred to vignettes directory")
}
