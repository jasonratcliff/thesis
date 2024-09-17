source("renv/activate.R")

# Ensure `tigris` uses cache directory for retrieved shapefiles.
if (is.null(getOption("tigris_use_cache"))) {
  packageStartupMessage("Setting option `tigris_use_cache = TRUE`")
  options(tigris_use_cache = TRUE)
}

# Derive package path to user directory for caching persistent data.
if (is.null(getOption("TIGRIS_CACHE_DIR"))) {
  options(
    TIGRIS_CACHE_DIR = tools::R_user_dir(package = "thesis", which = "data")
  )
  packageStartupMessage(
    "Cache directory: `TIGRIS_CACHE_DIR`\n* ", getOption("TIGRIS_CACHE_DIR")
  )
}

# Load development packages for interactive sessions.
if (interactive()) {
  suppressMessages({
    require(thesis)
    require(devtools)
    require(pkgdown)
    require(testthat)
  })
}
