context("map_helpers.R")

# spl_order() ----
test_that("Arrange ggplot specimens by count", {
  specimens_ordered <- spl_order(specimen_tbl = spp_co_front_range,
                                 id_column = "prior_id")
  top_spp <- spp_co_front_range %>%
    dplyr::group_by(.data$prior_id) %>% dplyr::tally(x = .) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::slice(1) %>% dplyr::pull("prior_id")
  expect_identical(specimens_ordered$prior_id[1], top_spp)
})

# spl_bbox() ----
test_that("Bounding box extraction", {
  specimen_bbox <- spl_bbox(herbarium_specimens)
  expect_equal(specimen_bbox[1, 1],
               min(herbarium_specimens$Longitude, na.rm = TRUE))
  expect_equal(specimen_bbox[2, 1],
               max(herbarium_specimens$Longitude, na.rm = TRUE))
  expect_equal(specimen_bbox[1, 2],
               min(herbarium_specimens$Latitude, na.rm = TRUE))
  expect_equal(specimen_bbox[3, 2],
               max(herbarium_specimens$Latitude, na.rm = TRUE))
  specimen_ggmap <-
    layer_ggmap(specimen_tbl = ThesisPackage::spp_co_front_range,
                gg_map_type = "satellite")
  expect_s3_class(spl_bbox(specimen_ggmap), class = "data.frame")
})

# spl_states() ----
test_that("Bounding box intersection", {
  states_sfs <- tigris::states() %>% rmapshaper::ms_simplify(input = .)
  states_spp <- herbarium_specimens %>%
    dplyr::filter(Longitude > -114 & Longitude < -104.5 &
                    Latitude > 42 & Latitude < 48.75) %>% spl_bbox() %>%
    spl_states(spl_extent = ., sf_states = states_sfs)
  expect_equal(states_spp, c("Idaho", "Utah", "Wyoming", "Montana"))
})

# spl_id() ----

# Inlcudes check of `find_spp()`
test_that("Specimen identification(s) geoms", {

  # Sample specimen collector / collection pairs and aesthetic ID.
  spl_ids <- dplyr::sample_n(
    tbl = dplyr::select(herbarium_specimens,
                        Collector, Collection_Number),
    size = 50
  )
  id_aes <- sample(c("prior_id", "Taxon_a_posteriori"), 1)

  # List of evaluated calls to `spl_id()` with sampled collections.
  spl_calls <-
    purrr::pmap(spl_ids, function(Collector, Collection_Number) {
      rlang::call2(.fn = rlang::expr(ThesisPackage::spl_id),
          specimen_tbl = herbarium_specimens,
          id_column = id_aes, shape_aes = id_aes,
          collector = Collector, collection = Collection_Number
        ) %>% rlang::eval_tidy()
    })

  # Walk specimen ID layer calls to check geom slot.
  purrr::walk(spl_calls, function(spl_call) {
    expect_identical(
      c("GeomPoint", "GeomPoint", "GeomLabel"),
      purrr::map_chr(spl_call, function(spl_geom) {
        class(spl_geom$geom)[[1]]
        })
      )
    })

  # Check layering over base ggplot.
  expect_s3_class(object = ggplot2::ggplot() +
                    spl_calls, class = c("gg", "ggplot"))

})


