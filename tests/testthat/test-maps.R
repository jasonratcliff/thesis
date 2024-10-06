specimens <- SpecimenMap$new(records = thesis::vouchers)

test_that("SpecimenMap subclass inheritance", {
  expect_s3_class(SpecimenMap, "R6ClassGenerator")
  expect_identical(SpecimenMap$inherit, as.symbol("Specimen"))
  expect_type(specimens, type = "environment")
  expect_s3_class(
    object = specimens,
    class = c("SpecimenMap", "Specimen", "Extent", "R6"),
    exact = TRUE
  )
})

test_that("Private method $arranged() sorts $sf by decreasing counts", {
  TestArranged <- R6::R6Class(
    inherit = SpecimenMap,
    public = list(
      test_arranged = function() {
        arranged <- private$arranged()
        expect_false(is.unsorted(rev(arranged$n)))
      }
    )
  )
  TestArranged$new(records = thesis::vouchers)$test_arranged()
})

test_that("Private method $geoms() returns list of simple features layers", {
  TestGeoms <- R6::R6Class(
    inherit = SpecimenMap,
    public = list(
      test_geoms = function() {
        expect_type(private$geoms, type = "closure")
        expect_type(private$geoms(), type = "list")
        purrr::walk(
          .x = seq_along(private$geoms()),
          .f = \(i) {
            geom <- private$geoms()[[i]]
            expect_type(geom[[1]], type = "environment")
            expect_type(geom[[2]], type = "environment")
            expect_s3_class(
              object = geom[[1]],
              class = c('LayerInstance', 'LayerSf', 'Layer', 'ggproto', 'gg'),
              exact = TRUE
            )
            expect_s3_class(
              object = geom[[2]],
              class = c("CoordSf", "CoordCartesian", "Coord", "ggproto", "gg"),
              exact = TRUE
            )
          }
        )
        expect_snapshot(private$geoms()[[3]][[1]]$mapping)
      }
    )
  )
  TestGeoms$new(records = thesis::vouchers)$test_geoms()
})

test_that("Private method $coords() sets coordinates from $bbox() limit", {
  TestCoords <- R6::R6Class(
    inherit = SpecimenMap,
    public = list(
      test_coords = function() {
        expect_type(private$coords, type = "closure")
        expect_type(private$coords(), type = "list")
        expect_s3_class(
          object = private$coords()[[1]],
          class = c("CoordSf", "CoordCartesian", "Coord", "ggproto", "gg"),
          exact = TRUE
        )
        bbox <- self$bbox()
        expect_equal(
          private$coords()[[1]]$limits,
          list(
            x = c(bbox[1], bbox[3]),
            y = c(bbox[2], bbox[4])
          )
        )
      }
    )
  )
  TestCoords$new(records = thesis::vouchers)$test_coords()
})

test_that("Private method $scales() returns list of aesthetic scales", {
  TestScales <- R6::R6Class(
    inherit = SpecimenMap,
    public = list(
      test_scales = function() {
        expect_type(private$scales, type = "closure")
        purrr::walk2(
          .x = private$scales(),
          .y = c("colour", "shape"),
          .f = \(x, y) {
            expect_identical(x$aesthetics, expected = y)
            expect_identical(names(x$labels), unique(self$sf[[self$identifier]]))
          }
        )
      }
    )
  )
  TestScales$new(records = thesis::vouchers)$test_scales()
})

test_that("Private method $theme() returns theme options", {
  TestTheme <- R6::R6Class(
    inherit = SpecimenMap,
    public = list(
      test_theme = function() {
        expect_type(private$theme, type = "closure")
        expect_s3_class(
          object = private$theme()[[1]],
          class = c("theme", "gg"),
          exact = TRUE
        )
        expect_s3_class(
          object = private$theme()[[1]]$legend.text,
          class = c("element_markdown", "element_text", "element"),
          exact = TRUE
        )
        expect_snapshot(private$theme()[[2]])
      }
    )
  )
  TestTheme$new(records = thesis::vouchers)$test_theme()
})

test_that("SpecimenMap R6 Subclass", {
  vouchers <- build_cartography()$clone()
  # Verify limit subsetting
  expect_identical(
    voucher_features[[3]]$limits,
    list(x = c(xmin = -110, xmax = -109), y = c(ymin = 44, ymax = 45))
  )


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
