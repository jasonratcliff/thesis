library(ThesisPackage)
library(dplyr)
library(ggplot2)
library(cowplot)

set.seed(20210312)

# Subset Specimens ----

spp_lanata <- ThesisPackage::herbarium_specimens %>%
  subset_coords(
    specimen_tbl = .,
    Latitude = c(42.25, 46.25),
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
      values = ThesisPackage::spp_color, na.value = "black"
    ),
    ggplot2::scale_shape_manual(
      name = "Annotations", labels = specimen_labels,
      values = ThesisPackage::spp_shape, na.value = 17
    ),
    theme(
      legend.box = "horizontal",
      legend.text = ggtext::element_markdown(),
      legend.key = element_blank()
    )
  )
}

# Extract Legends ----

legend_elevation <- get_legend(ggplot_elevation)

legend_specimens <-
  get_legend(
    ggplot() + 
      ggplot_specimens() +
      guides(col = guide_legend(ncol = 2)) +
      theme(legend.title.align = 0.5)
  )

legend_grid <-
  plot_grid(
    NULL, legend_elevation, legend_specimens, NULL,
    nrow = 1, rel_widths = c(1, 1, 3, 0.5)
  )

# cowplot Grid ----

FigDiscussionElevationBigHorn <-
  plot_grid(
    ggplot_elevation +
      theme(legend.position = "none") +
      ggplot_specimens(),
    legend_grid,
    nrow = 2, rel_heights = c(3, 1)
  )

ThesisPackage::save_plot(
  gg_plot = FigDiscussionElevationBigHorn,
  height = 6, width = 6
)
