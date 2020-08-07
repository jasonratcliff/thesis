context("map_helpers.R")

test_that("Arrange ggplot specimens by count", {
  specimens_ordered <- spl_order(specimen_tbl = spp_co_front_range,
                                 id_column = "prior_id")
  top_spp <- spp_co_front_range %>%
    dplyr::group_by(.data$prior_id) %>% dplyr::tally(x = .) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::slice(1) %>% dplyr::pull("prior_id")
  expect_identical(specimens_ordered$prior_id[1], top_spp)
})

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

test_that("Bounding box intersection", {
  states_sfs <- tigris::states() %>% rmapshaper::ms_simplify(input = .)
  states_spp <- herbarium_specimens %>%
    dplyr::filter(Longitude > -114 & Longitude < -104.5 &
                    Latitude > 42 & Latitude < 48.75) %>% spl_bbox() %>%
    spl_states(spl_extent = ., sf_states = states_sfs)
  expect_equal(states_spp, c("Idaho", "Utah", "Wyoming", "Montana"))
})


