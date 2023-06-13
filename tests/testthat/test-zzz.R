test_that("Initialize persistent data via `.onLoad()`", {
  tmp <- path_temp("zzz")

  # For each scope of border layer, verify subdirectory existence
  with_options(
    new = list("thesis.data" = tmp),
    code = {
      # Reload namespace to simulated `.onLoad()` data directory initialization
      expect_message(
        object = thesis:::.onLoad(libname = "thesis"),
        regexp = "zzz/tigris"
      )
      walk(
        .x = c("states", "counties"),
        .f = function(layer) {
          directory <- path(getOption("thesis.data"), "tigris", layer)
          expect_true(dir_exists(directory))
        }
      )
      dir_delete(tmp)
      ui_done("Deleted test directory:\n{ui_path(tmp)}")
    }
  )

  # Check unload operation sets global option to NULL
  thesis:::.onUnload(libname = "thesis")
  expect_identical(object = getOption('thesis.data'), expected = NULL)

  # Check load operation sets persistent data option path for user package
  thesis:::.onLoad(libname = "thesis")
  expect_equal(
    object = getOption('thesis.data'),
    expected = tools::R_user_dir(package = "thesis", which = "data")
  )
})
