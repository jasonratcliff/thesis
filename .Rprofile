options(repos = c(CRAN = 'https://cloud.r-project.org'))

source("renv/activate.R")

# Ensure tigris uses cache directory.
#> Sys.getenv('TIGRIS_CACHE_DIR')  # Check path set in .Renviron
# - Cache directory is set from users' home as "~/tigris"
#> tigris::tigris_cache_dir(path = '~/tigris')
options(tigris_class = "sf", tigris_use_cache = TRUE)

if (interactive()) {
  suppressMessages({
    require(thesis)
    require(devtools)
    require(pkgdown)
  })
}
