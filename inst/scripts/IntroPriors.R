library(Thesis)
library(cowplot)
library(dplyr)
library(fs)
library(ggplot2)
library(purrr)

set.seed(20210311)

# Introduction: Build ggplot distribution map with prior annotations.
priors <- list()
priors$specimens <- Thesis::herbarium_specimens %>%
  select("prior_id", "Latitude", "Longitude") %>%
  filter(prior_id %in%
    paste("Physaria",
      c("acutifolia", "brassicoides", "condensata", "dornii",
        "eburniflora", "integrifolia", "vitulifera",
        "medicinae", "chambersii", "rollinsii",
        paste("didymocarpa subsp.", c("didymocarpa", "lanata", "lyrata")),
        paste("saximontana subsp.", c("saximontana", "dentata")),
        paste("floribunda subsp.", c("floribunda", "osterhoutii"))
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
    spl_extent = Thesis::spl_bbox(priors$specimens),
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
    xlim = range(Thesis::spl_bbox(priors$specimens)[["Longitude"]]),
    ylim = range(Thesis::spl_bbox(priors$specimens)[["Latitude"]])
  )

# Map ----
priors$map <-
  priors$ggplot +
  theme(
    panel.background = element_rect(fill = "grey99"),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0.55, 0.2, 0, "in")
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# .pdf ----

priors$map_pdf <- priors$map +
  theme(plot.margin = margin(0, -2.5, 0, -3, "in"))

priors$legend_pdf <-
  get_legend(
    priors$ggplot +
      guides(color = guide_legend(ncol = 3, byrow = TRUE)) +
      theme(
        legend.background = element_rect(fill = "grey99"),
        legend.position = "bottom",
        legend.direction = "vertical"
      )
  )

priors$figure_pdf <-
  plot_grid(
    priors$map_pdf,
    priors$legend_pdf,
    ncol = 1,
    rel_heights = c(0.75, 0.25)
  )

# .png ----

priors$map_png <- priors$map +
  theme(plot.margin = margin(0.25, -2.5, 0.25, -2.5, "in"))

priors$legend_png <-
  get_legend(
    priors$ggplot +
      guides(color = guide_legend(ncol = 1)) +
      theme(
        legend.background = element_rect(fill = "grey99"),
        legend.position = "right",
        legend.direction = "vertical"
      )
  )

priors$figure_png <-
  plot_grid(
    priors$map_png,
    priors$legend_png,
    nrow = 1,
    rel_widths = c(0.8, 0.2)
  )

# cowplot Grid ----

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(priors$figure_png, priors$figure_pdf),
    width = c(5, 6),
    height = c(7, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/IntroPriors", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
})

