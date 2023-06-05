#!/usr/local/bin Rscript

suppressPackageStartupMessages({
  library(fs)
  library(purrr)
  library(rmarkdown)
})

purrr::walk(
  .x = c("./", "data-raw"),
  .f = function(directory) {
    readme <- fs::path(directory, "README", ext = "Rmd")
    if (fs::file_exists(readme)) {
      rmarkdown::render(
        input = readme,
		output_dir = directory,
		output_file = fs::path_ext_set(readme, ext = "md"),
		output_format = "github_document"
      )
    }
  }
)
