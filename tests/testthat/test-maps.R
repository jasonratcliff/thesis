specimen_map <- SpecimenMap$new(records = thesis::vouchers)

test_that("Specimen superclass inheritance", {
  expect_identical(SpecimenMap$inherit, as.symbol("Specimen"))
  expect_s3_class(specimen_map$records, "tbl_df")
  expect_identical(specimen_map$identifier, expected = "scientificName")
})

test_that("SpecimenMap R6 subclass", {
  expect_s3_class(SpecimenMap, "R6ClassGenerator")
  expect_type(specimen_map, type = "environment")
  expect_identical(class(specimen_map), c("SpecimenMap", "Specimen", "R6"))
})

test_that("Shapefile simple features active fields", {

  purrr::walk(
    .x = list(
      specimen_map$states[[1]],
      specimen_map$counties[[1]]
    ),
    .f = function(features) {
      expect_s3_class(
        features,
        class = c('LayerInstance', 'LayerSf', 'Layer', 'ggproto', 'gg'),
        exact = TRUE
      )
    }
  )

  purrr::walk(
    .x = list(
      specimen_map$states[[2]],
      specimen_map$counties[[2]],
      specimen_map$coordinates
    ),
    .f = function(features) {
      expect_s3_class(
        features,
        class = c('CoordSf', 'CoordCartesian', 'Coord', 'ggproto', 'gg'),
        exact = TRUE
      )
    }
  )

})

test_that("Update deprecated tests", {
  skip()

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
