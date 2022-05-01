library(thesis)
library(dplyr)
library(ggplot2)
library(cowplot)

set.seed(20210312)

# Subset Specimens ----

spp_lanata <- thesis::herbarium_specimens %>%
  subset_coords(
    specimen_tbl = .,
    Latitude = c(41.5, 45.5),
    Longitude = c(-109.5, -104.5)
  )

specimen_labels <-
  spl_labels(
    specimen_tbl = spp_lanata,
    id_column = "Taxon_a_posteriori"
  )

# Elevation Profile ----

ggplot_elevation <-
  layer_elevation(
    specimen_tbl = spp_lanata,
    raster_zoom = 7
  ) +
  layer_borders(
    spl_extent = spl_bbox(spp_lanata)
  ) +
  coord_sf(
    xlim = range(spl_bbox(spp_lanata)[["Longitude"]]),
    ylim = range(spl_bbox(spp_lanata)[["Latitude"]]),
    expand = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(breaks = -109:-105)

# Specimen Layers ----

ggplot_specimens <- function() {
  list(
    geom_jitter(
      data = spp_lanata, size = 3,
      aes(x = Longitude, y = Latitude,
          color = Taxon_a_posteriori, shape = Taxon_a_posteriori)
    ),
    ggplot2::scale_color_manual(
      name = "Annotations", labels = specimen_labels,
      values = thesis::spp_color, na.value = "black"
    ),
    ggplot2::scale_shape_manual(
      name = "Annotations", labels = specimen_labels,
      values = thesis::spp_shape, na.value = 17
    ),
    theme(
      legend.box = "horizontal",
      legend.text = ggtext::element_markdown(),
      legend.key = element_blank()
    )
  )
}

# Legends ----

legends <- list()

legends$elevation <- get_legend(ggplot_elevation)

legends$specimens_pdf <-
  get_legend(
    ggplot() + 
      ggplot_specimens() +
      guides(col = guide_legend(ncol = 2)) +
      theme(legend.title.align = 0.5)
  )

legends$specimens_png <-
  get_legend(
    ggplot() + 
      ggplot_specimens() +
      guides(col = guide_legend(ncol = 1)) +
      theme(legend.title.align = 0.5)
  )

# cowplot Grid ----

grids <- list()

grids$pdf <-
  plot_grid(
    ggplot_elevation +
      theme(legend.position = "none") +
      ggplot_specimens(),
    plot_grid(
      NULL, legends$elevation, legends$specimens_pdf, NULL,
      nrow = 1, rel_widths = c(1, 1, 3, 0.5)
    ),
    nrow = 2, rel_heights = c(3, 1)
  )

grids$png <-
  plot_grid(
    ggplot_elevation +
      theme(legend.position = "none") +
      ggplot_specimens(),
    plot_grid(
      legends$specimens_png, NULL, legends$elevation,
      ncol = 1, rel_heights = c(0.6, -0.325, 0.4)
    ),
    ncol = 2, rel_widths = c(3, 1)
  )

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(grids$png, grids$pdf),
    width = c(6, 6),
    height = c(8, 4),
    aspect = c(.167, .167),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/DiscussionElevationBigHorn", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })
