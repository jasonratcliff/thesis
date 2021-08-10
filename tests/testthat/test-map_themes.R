context("map_themes.R")

test_that("HTML markdown vector", {
  html_vctr <-
    spl_labels(
      specimen_tbl = Thesis::herbarium_specimens,
      id_column = "prior_id"
    )
  expect_identical(
    sort(names(html_vctr)),
    sort(unique(Thesis::herbarium_specimens$prior_id))
  )
  expect_true(
    unique(
      grepl(
        pattern = "<br><span>&nbsp;&nbsp;&nbsp;</span>",
        x = html_vctr[grep(pattern = "subsp\\.", x = names(html_vctr))]
      )
    )
  )
})

test_that("Map themes", {
  theme_layer <-
    layer_themes(
      specimen_tbl = spp_co_front_range,
      id_column = "Taxon_a_posteriori",
      legend_title = "Reivewed Annotations"
    )
  # Test return classes - ccales, theme, and labels:
  expect_identical(
    lapply(theme_layer, class),
    list(
      c("ScaleDiscrete", "Scale", "ggproto", "gg"),
      c("ScaleDiscrete", "Scale", "ggproto", "gg"),
      c("theme", "gg"), "labels"
    )
  )
  theme_ggplot <- ggplot() +
    layer_specimens(
      specimen_tbl = spp_co_front_range, shape_aes = TRUE,
      id_column = "Taxon_a_posteriori"
    ) +
    theme_layer
  expect_s3_class(theme_ggplot, "ggplot")
})

test_that("Legend extraction", {
  plot_legend <- spl_legend(
    specimen_tbl = spp_co_front_range,
    id_column = "Taxon_a_posteriori",
    legend_title = "Review Annotations",
    shape_aes = TRUE, geom_size = 3
  )
  expect_s3_class(plot_legend, c("gtable", "gTree", "grob", "gDesc"))
  expect_s3_class(cowplot::plot_grid(plot_legend), "ggplot")
})
