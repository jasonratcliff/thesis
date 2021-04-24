library(ThesisPackage)
library(cowplot)
library(dplyr)
library(fs)
library(ggplot2)
library(purrr)

set.seed(20210311)

# Introduction: Build ggplot distribution map with prior annotations.
priors <- list()
priors$specimens <- ThesisPackage::herbarium_specimens %>%
  select("prior_id", "Latitude", "Longitude") %>%
  filter(prior_id %in%
    paste("Physaria",
      c("acutifolia", "brassicoides", "condensata", "dornii",
        "eburniflora", "integrifolia", "vitulifera",
        "medicinae", "chambersii", "rollinsii",
        paste("didymocarpa ssp.", c("didymocarpa", "lanata", "lyrata")),
        paste("saximontana ssp.", c("saximontana", "dentata")),
        paste("floribunda ssp.", c("floribunda", "osterhoutii"))
      )
    ) | grepl("^Physaria$", .data$prior_id)
  ) %>%
  subset_coords(
    specimen_tbl = .,
    Latitude = c(37, 49.1),
    Longitude = c(-114.5, -102)
  )

priors$ggplot <- ggplot() +
  layer_borders(
    spl_extent = ThesisPackage::spl_bbox(priors$specimens),
    sf_county_color = "black"
  ) +
  layer_specimens(
    specimen_tbl = priors$specimens,
    shape_aes = TRUE,
    id_column = "prior_id",
    jitter_width = 0.1,
    jitter_height = 0.1,
    jitter_alpha = 0.75
  ) +
  layer_themes(
    specimen_tbl = priors$specimens,
    id_column = "prior_id",
    legend_title = "Prior Annotations"
  ) +
  coord_sf(
    xlim = range(ThesisPackage::spl_bbox(priors$specimens)[["Longitude"]]),
    ylim = range(ThesisPackage::spl_bbox(priors$specimens)[["Latitude"]])
  ) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE))

priors$map <-
  priors$ggplot +
  theme(
    panel.background = element_rect(fill = "grey99"),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0.55, 0, 0, "in")
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

priors$legend <-
  get_legend(
    priors$ggplot +
      theme(
        legend.background = element_rect(fill = "grey99"),
        legend.position = "bottom",
        legend.direction = "vertical"
      )
  )

priors$figure <-
  plot_grid(
    priors$map,
    priors$legend,
    ncol = 1,
    rel_heights = c(0.75, 0.25)
  )

purrr::walk(.x = c("png", "pdf"), .f = function(ext) {
  cowplot::save_plot(
    filename = fs::path("Figs/FigIntroPriors", ext = ext),
    plot = priors$figure,
    base_width = 6,
    base_height = 4,
    base_asp = 2.5,
    nrow = 2
  )
})
