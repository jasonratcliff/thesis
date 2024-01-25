tmp <- path_temp()
tmp_tigris <- path(tmp, "tigris")
if (!dir_exists(tmp_tigris)) dir_create(tmp_tigris)

## ---- SpecimenMap$states() ---------------------------------------------------
test_that("Persistent state shapefile package data.", {
  specimens <- build_cartography()$clone()
  with_options(
    new = list("thesis.data" = tmp),
    code = {
      state_rda <- path(
        getOption("thesis.data"), "tigris", "states",
        ext = "rda"
      )

      # Check time for data initialization against reading perstistent files
      sys_time_init <-
        system.time(expr = {
          tigris_states <- specimens$tigris_states()
        })

      # Orthogonal path check to verify relative path from package option
      expect_true(file_exists(state_rda))
      expect_true(file_exists(path(tmp, "tigris/states", ext = "rda")))

      # Compare time from reading persistent data against download operation
      sys_time_persist <-
        system.time(expr = {
          tigris_states <- specimens$tigris_states()
        })
      expect_gt(sys_time_init[["elapsed"]], sys_time_persist[["elapsed"]])

      # Check message for path to persistent data from package option
      if (file_exists(state_rda)) file_delete(state_rda)
      expect_message(
        object = specimens$tigris_states(),
        regexp = path(options("thesis.data"), "tigris/states.rda")
      )

      # Check return class matches simple features data frame
      expect_null(tigris_states)
      expect_s3_class(specimens$sf_states, class = c("sf", "data.frame"))
    }
  )
})

## ---- SpecimenMap$counties() -------------------------------------------------
test_that("Persistent county shapefile package data.", {
  specimens <- SpecimenMap$new(thesis::vouchers, "scientificName")
  # specimens <- build_cartography()$clone()
  with_options(
    new = list("thesis.data" = tmp),
    code = {
      # Check time for data initialization against reading perstistent files
      sys_time_init <-
        system.time(expr = {
          tigris_counties <- specimens$tigris_counties(.states = NULL)
        })
      sys_time_persist <-
        system.time(expr = {
          tigris_counties <- specimens$tigris_counties(.states = NULL)
        })
      sys_time_field <-
        system.time(expr = {
          tigris_counties <- specimens$sf_counties
        })
      expect_gt(sys_time_init[["elapsed"]], sys_time_persist[["elapsed"]])
      expect_gt(sys_time_persist[["elapsed"]], sys_time_field[["elapsed"]])

      # Check expect output from aggregated county border simple features data
      expect_s3_class(object = tigris_counties, class = c("sf", "data.frame"))

      # Verify persistent file is written for all states in specimen records
      unique(specimens$records$stateProvince) %>%
        keep(.x = ., .p = ~ !is.na(.x) & (.x %in% datasets::state.name)) %>%
        walk(
          # Orthogonal path check to verify relative path from local option
          .f = function(state) {
            county_rda <- path(
              getOption("thesis.data"), "tigris/counties", state,
              ext = "rda"
            )
            expect_true(file_exists(county_rda))
          }
        )

      # Expect simple features for states external from `self$records`
      purrr::walk(
        .x = c("Nevada", "Oregon"),
        .f = function(include) {
          specimens$tigris_counties(.states = include)
          expect_true(
            file_exists(
              fs::path(tmp_tigris, "counties", include, ext = "rda")
            )
          )
        }
      )

      # Ensure check against non-character states
      expect_error(
        object = specimens$tigris_counties(.states = 3),
        regexp = "must be a character vector"
      )
    }
  )
})

if (dir_exists(tmp_tigris)) dir_delete(tmp_tigris)

test_that("SpecimenMap R6 Subclass", {
  vouchers <- build_cartography()$clone()
  expect_type(vouchers, type = "environment")
  expect_identical(class(vouchers), c("SpecimenMap", "Specimen", "R6"))
  expect_s3_class(vouchers$records, c("tbl_df"))
  expect_identical(vouchers$identifier, expected = "scientificName")

  # Border Features ------------------------------------------------------------
  expect_type(vouchers$features, type = "closure")
  expect_error(vouchers$features(.borders = "black", .states = list()))

  # County / State layer simple features
  voucher_features <- vouchers$features(.borders = "black")
  purrr::walk(
    .x = list(
      voucher_features[[1]][[1]],
      voucher_features[[2]][[1]]
    ),
    .f = function(layer) {
      expect_type(layer, type = "environment")
      expect_identical(
        class(layer),
        c("LayerInstance", "LayerSf", "Layer", "ggproto", "gg")
      )
    }
  )

  # Simple feature coordinate systems
  purrr::walk(
    .x = list(
      voucher_features[[1]][[2]],
      voucher_features[[2]][[2]],
      voucher_features[[3]]
    ),
    .f = function(layer) {
      expect_type(layer, type = "environment")
      expect_identical(
        class(layer),
        c("CoordSf", "CoordCartesian", "Coord", "ggproto", "gg")
      )
    }
  )

  # Verify limit subsetting
  expect_identical(
    voucher_features[[3]]$limits,
    list(x = c(-110, -109), y = c(44, 45))
  )

  # Specimen Layer -------------------------------------------------------------
  expect_type(vouchers$specimens, type = "closure")
  voucher_specimens <- vouchers$specimens()

  # Verify record sorting
  expect_identical(
    voucher_specimens[[1]]$data %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::pull(scientificName),
    expected = "Medicari iugerum"
  )

  # Check layer geom aesthetics
  expect_identical(
    class(voucher_specimens[[1]]),
    expected = c("LayerInstance", "Layer", "ggproto", "gg")
  )
  expect_snapshot(voucher_specimens[[1]]$mapping)

  # Manual Scales --------------------------------------------------------------
  expect_type(vouchers$scales, type = "closure")
  voucher_scales <- vouchers$scales()
  expect_identical(voucher_scales[[1]]$aesthetics, expected = "colour")
  expect_identical(voucher_scales[[2]]$aesthetics, expected = "shape")
  purrr::walk(
    .x = voucher_scales,
    .f = function(scale) {
      expect_equal(
        names(scale$labels),
        expected = unique(vouchers$records[["scientificName"]]),
        ignore_attr = TRUE
      )
    }
  )

  # Plot Theme -----------------------------------------------------------------
  expect_type(vouchers$theme, type = "closure")
  voucher_theme <- vouchers$theme(.legend = vouchers$identifier)
  expect_identical(class(voucher_theme[[1]]), expected = c("theme", "gg"))
  expect_identical(voucher_theme[[1]]$legend.text, ggtext::element_markdown())
  expect_snapshot(voucher_theme[[2]])

  # Collector Tags -------------------------------------------------------------
  expect_type(vouchers$repel, type = "closure")
  voucher_repel <-
    vouchers$repel(
      "Hooker" = 1:4,
      repel.params = list(segment.color = "#d0ff00")
    )
  expect_identical(
    voucher_repel$data[["label"]],
    expected = paste0("Hooker\n", 1:4)
  )
  expect_identical(
    voucher_repel$aes_params[["segment.colour"]],
    expected = "#d0ff00"
  )
  expect_identical(
    class(voucher_repel),
    expected = c("LayerInstance", "Layer", "ggproto", "gg")
  )

  # Optionally set specific specimen records in addition to collection search.
  voucher_labels <-
    tibble::tribble(
      ~"recordedBy", ~"recordNumber", ~"decimalLongitude", ~"decimalLatitude",
      "A. Gray", 5, 46, -111
    )
  expect_identical(
    dplyr::select(vouchers$repel(vouchers = voucher_labels)$data, -"label"),
    voucher_labels
  )
  expect_identical(
    vouchers$repel(vouchers = voucher_labels)$data$label,
    expected = "Gray\n5"
  )
  expect_equal(
    nrow(
      vouchers$repel(
        "Hooker" = 1:4,
        vouchers = voucher_labels
      )$data
    ),
    expected = 5
  )

  # Map Plots ------------------------------------------------------------------
  expect_type(vouchers$map, type = "closure")
  map_vouchers <- thesis::vouchers %>%
    dplyr::filter(.data$stateProvince == "Wyoming") %>%
    SpecimenMap$new(records = ., identifier = "scientificName")

  expect_error(
    withr::with_envvar(
      new = list(GGMAP_GOOGLE_API_KEY = ""),
      vouchers$map(baselayer = "ggmap")
    ),
    regexp = "Register an API key with Google."
  )
  expect_error(
    vouchers$map(baselayer = "ggmap", center = c(1, 2, 3)),
    regexp = "Map centroid requires numeric vector of length 2."
  )

  expect_s3_class(vouchers$map(baselayer = "base"), class = c("gg", "ggplot"))
  expect_s3_class(vouchers$map(baselayer = "ggmap"), class = c("gg", "ggplot"))
  expect_s3_class(vouchers$map(baselayer = "elevatr"), class = c("gg", "ggplot"))

  if (dir.exists(".mustashe")) {
    unlink(".mustashe", recursive = TRUE)
  }
})
