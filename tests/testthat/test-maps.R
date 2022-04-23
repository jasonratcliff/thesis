test_that("SpecimenMap R6 Subclass", {
  vouchers <- tibble::tibble(
    State = "Wyoming",
    Species = c("Medicari iugerum", rep("Medicari profundus", 3)),
    Collector = "J. Hooker",
    Collection_Number = 1:4
  ) %>%
    dplyr::bind_cols(
      x = .,
      y = tibble::tibble(
        Latitude = c(45, 44, 44, 45),
        Longitude = c(-110, -110, -109, -109),
      )
    ) %>%
    SpecimenMap$new(
      records = .,
      identifier = "Species"
    )

  expect_type(vouchers, type = "environment")
  expect_identical(class(vouchers), c("SpecimenMap", "Specimen", "R6"))
  expect_s3_class(vouchers$records, c("tbl_df"))
  expect_identical(vouchers$identifier, expected = "Species")

  # Border Features ------------------------------------------------------------
  expect_type(vouchers$features, type = "closure")
  voucher_features <- vouchers$features(sf_border = "black")

  # County / State layer simple features
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
      dplyr::pull(Species),
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
        expected = unique(vouchers$records[["Species"]]),
        ignore_attr = TRUE
      )
    }
  )

  # Plot Theme -----------------------------------------------------------------
  expect_type(vouchers$theme, type = "closure")
  voucher_theme <- vouchers$theme(legend = vouchers$identifier)
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

  # Map Plots ------------------------------------------------------------------
  expect_type(vouchers$map, type = "closure")
  map_vouchers <- Thesis::herbarium_specimens %>%
    dplyr::filter(State == "Wyoming") %>%
    SpecimenMap$new(records = ., identifier = "Taxon_a_posteriori")

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

  if (dir.exists("tests/testthat/.mustashe")) {
    unlink("tests/testthat/.mustashe", recursive = TRUE)
  }
})
