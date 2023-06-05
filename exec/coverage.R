#!/usr/local/bin Rscript

suppressPackageStartupMessages({
  library(covr)
})

covr::report(
  x = covr::package_coverage(),
  file = "_site/coverage.html",
  browse = interactive()
)
