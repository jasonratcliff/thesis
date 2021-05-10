library(Thesis)
library(dplyr)
library(ggplot2)
library(cowplot)

set.seed(20210311)

# Discussion: Build ggplot distribution map with reviewed annotations.
reviewed <- list()
reviewed$specimens <- Thesis::herbarium_specimens %>%
  select("Taxon_a_posteriori", "Latitude", "Longitude") %>%
  filter(Taxon_a_posteriori %in%
    paste("Physaria",
      c("acutifolia", "brassicoides", "condensata", "dornii",
        "eburniflora", "integrifolia", "vitulifera",
        "medicinae", "chambersii", "rollinsii",
        paste("didymocarpa subsp.", c("didymocarpa", "lanata", "lyrata")),
        paste("saximontana subsp.", c("saximontana", "dentata")),
        paste("floribunda subsp.", c("floribunda", "osterhoutii"))
      )
    ) | grepl("^Physaria$", .data$Taxon_a_posteriori)
  ) %>%
  subset_coords(
    specimen_tbl = .,
    Latitude = c(37, 49.1),
    Longitude = c(-114.5, -102)
  )

reviewed$ggplot <- ggplot() +
  layer_borders(
    spl_extent = spl_bbox(reviewed$specimens),
    sf_county_color = "black"
  ) +
  layer_specimens(
    specimen_tbl = reviewed$specimens,
    shape_aes = TRUE,
    id_column = "Taxon_a_posteriori",
    jitter_width = 0.1,
    jitter_height = 0.1,
    jitter_alpha = 0.75
  ) +
  layer_themes(
    specimen_tbl = reviewed$specimens,
    id_column = "Taxon_a_posteriori",
    legend_title = "Reviewed Annotations"
  ) +
  coord_sf(
    xlim = range(spl_bbox(reviewed$specimens)[["Longitude"]]),
    ylim = range(spl_bbox(reviewed$specimens)[["Latitude"]])
  )


reviewed$map <-
  reviewed$ggplot +
  theme(
    panel.background = element_rect(fill = "grey99"),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0.55, 0.2, 0, "in")
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

reviewed$legend_pdf <-
  get_legend(
    reviewed$ggplot +
      guides(color = guide_legend(ncol = 3, byrow = TRUE)) +
      theme(
        legend.background = element_rect(fill = "grey99"),
        legend.position = "bottom",
        legend.direction = "vertical"
      )
  )

reviewed$legend_png <-
  get_legend(
    reviewed$ggplot +
      guides(color = guide_legend(ncol = 1)) +
      theme(
        legend.background = element_rect(fill = "grey99"),
        legend.position = "right",
        legend.direction = "vertical"
      )
  )

reviewed$figure_pdf <-
  plot_grid(
    reviewed$map,
    reviewed$legend_pdf,
    ncol = 1,
    rel_heights = c(0.75, 0.25)
  )

reviewed$figure_png <-
  plot_grid(
    reviewed$map,
    reviewed$legend_png,
    nrow = 1,
    rel_widths = c(0.8, 0.2)
  )

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(reviewed$figure_png, reviewed$figure_pdf),
    width = c(6, 6),
    height = c(8, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/DiscussionPosteriori", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })
