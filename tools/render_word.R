library(rprojroot)
library(fs)
library(magrittr)
library(purrr)
library(rmarkdown)

dir_check <-
  rprojroot::find_root(criterion = rprojroot::has_file(".git/index")) %>%
  fs::path_file()  # Extract end of file path to match against project name.
if (dir_check != "Thesis") {
  stop("Verify working directory is set to 'Thesis/' root project directory.")
}

if (!fs::dir_exists("inst/word")) {
  fs::dir_create("inst/word")
}

c(
  "01-INTRODUCTION",
  "02-METHODS",
  "03-RESULTS",
  "04-DISCUSSION",
  "06-ALIGNMENTS"
) %>%
  purrr::walk(.x = ., function(filename) {
    rmarkdown::render(
      input = fs::path("inst/manuscript/", filename, ext = "Rmd"),
      output_file = fs::path(filename, ext = "docx"),
      output_format = "bookdown::word_document2",
      output_dir = "inst/word"
    )
  })

