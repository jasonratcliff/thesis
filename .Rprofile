source("renv/activate.R")

# Ensure tigris uses cache directory.
#> Sys.getenv('TIGRIS_CACHE_DIR')  # Check path set in .Renviron
# - Cache directory is set from users' home as "~/tigris"
#> tigris::tigris_cache_dir(path = '~/tigris')
options(tigris_class = "sf", tigris_use_cache = TRUE)

# Fix xcolor warning from TEXshade causing segfault in kableExtra / msa:
# - https://github.com/rstudio/rmarkdown/issues/1384#issuecomment-400092146
options(kableExtra.latex.load_packages = FALSE)
