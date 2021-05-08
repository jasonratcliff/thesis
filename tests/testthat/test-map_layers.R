context("map_layers.R")

test_that("Specimen layer works", {

  # Test color and shape aesthetics
  aes_color <-
    layer_specimens(specimen_tbl = spp_co_front_range, shape_aes = FALSE,
                    id_column = "Taxon_a_posteriori")
  expect_null(aes_color[[1]]$mapping$shape)
  expect_s3_class(object = aes_color[[1]],
                  class = c("LayerInstance", "Layer", "ggproto", "gg"))
  aes_shape <-
    layer_specimens(specimen_tbl = spp_co_front_range, shape_aes = TRUE,
                    id_column = "Taxon_a_posteriori")
  shape_name <- rlang::get_expr(aes_shape[[1]]$mapping$shape)
  expect_identical(rlang::as_string(shape_name), "Taxon_a_posteriori")

  # Test ggplot builds
  specimen_ggplot <- function(shape_opt) {
    ggplot(data = ThesisPackage::spp_co_front_range) +
      layer_specimens(specimen_tbl = ThesisPackage::spp_co_front_range,
                      id_column = "Taxon_a_posteriori", shape_aes = shape_opt)
  }
  specimen_ggplots <- lapply(X = c(TRUE, FALSE), specimen_ggplot)
  expect_warning(object = ggplot2::ggplot_build(specimen_ggplots[[1]]),
                 regexp = "shape palette")
  expect_s3_class(object = specimen_ggplots[[2]], class =  c("gg", "ggplot"))
})

test_that("Border layer works", {
  border_layer <- ggplot() +
    layer_borders(spl_extent = spl_bbox(ThesisPackage::spp_co_front_range),
                  sf_crs = 3857)
  expect_s3_class(object = border_layer$layers[[1]]$geom,
                  class = c("GeomSf", "Geom", "ggproto", "gg"))
  expect_identical(sf::st_crs(border_layer$layers[[1]]$data)$input, "EPSG:3857")
})

test_that("Build map works", {

  build_base <-
    build_map(specimen_tbl = spp_co_front_range, id_column = "prior_id",
              legend_title = "Prior Annotations", map_base = "base",
              sf_county_color = "black", geom_size = 4)

  build_ggmap <-
    build_map(specimen_tbl = spp_co_front_range, id_column = "prior_id",
              legend_title = "Prior Annotations", map_base = "ggmap",
              zoom_lvl = 8, gg_map_type = "satellite")

  build_elev <-
    build_map(specimen_tbl = spp_co_front_range, id_column = "prior_id",
              legend_title = "Prior Annotations", map_base = "elevation",
              sf_county_color = "black", raster_zoom = 8)
})

