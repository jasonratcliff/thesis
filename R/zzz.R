.onLoad <- function(libname, pkgname) {
  # Set `thesis.data` global option with path to persistent package data
  persistent_data <- getOption("thesis.data", NULL)
  if (is.null(persistent_data)) {
    options(
      "thesis.data" = tools::R_user_dir(package = "thesis", which = "data")
    )
  }
  persist_data()
  invisible()
}

persist_data <- function() {
  # Create persistent package data directory tree from path set by global option
  tigris_data <- path(getOption("thesis.data", NULL), "tigris")
  if (!is.null(tigris_data)) {
    if (!dir_exists(tigris_data)) {
      ui_info(
        c(
          "Creating persistent package data directory for `tigris` shapefiles:",
          "{ui_path(tigris_data)}",
          .sep = "\n"
        )
      )
      dir_create(path(tigris_data, c("states", "counties")))
      options("thesis.persist" = TRUE)
    }
  }
  invisible()
}

.onUnload <- function(libname, pkgname) {
  options(
    "thesis.data" = NULL,
    "thesis.persist" = NULL
  )
  invisible()
}
