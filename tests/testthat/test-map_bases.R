test_that(desc = "Layer ggmap functions", {

  # Test missing API key
  api_key <- Sys.getenv(x = "GGMAP_GOOGLE_API_KEY")
  Sys.setenv("GGMAP_GOOGLE_API_KEY" = "")
  expect_error(
    layer_ggmap(
      specimen_tbl = spp_co_front_range,
      gg_map_type = "satellite", zoom_lvl = 8
    )
  )
  Sys.setenv("GGMAP_GOOGLE_API_KEY" = api_key) # Reset API key

  # Test Longitude & Latitude location
  expect_s3_class(
    layer_ggmap(
      specimen_tbl = spp_co_front_range,
      gg_map_type = "satellite", zoom_lvl = 8,
      gg_longitude = -111, gg_latitude = 42
    ),
    class = "ggplot"
  )
  expect_error(
    layer_ggmap(
      specimen_tbl = spp_co_front_range,
      gg_map_type = "satellite", zoom_lvl = 8,
      gg_longitude = -111
    )
  )

  # Test Longitude & Latitude midrange
  expect_s3_class(
    layer_ggmap(
      specimen_tbl = Thesis::spp_co_front_range,
      gg_map_type = "satellite"
    ),
    class = "ggplot"
  )
})

test_that(desc = "Layer elevation functions", {
  elev_ggplot <-
    layer_elevation(
      specimen_tbl = Thesis::spp_co_front_range,
      raster_zoom = 7, raster_factor = 1
    )
  expect_s3_class(elev_ggplot, "ggplot")
  expect_s3_class(elev_ggplot$layers[[1]]$geom, "GeomTile")
  expect_s3_class(elev_ggplot$scales$scales[[1]], "ScaleContinuous")
})
