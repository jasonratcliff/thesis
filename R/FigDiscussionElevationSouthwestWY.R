library(ThesisPackage)
library(dplyr)
library(ggplot2)
library(cowplot)

set.seed(20210312)

# Subset Specimens ----

spp_integrifolia <- ThesisPackage::herbarium_specimens %>%
  subset_coords(
    specimen_tbl = .,
    Latitude = c(41, 44),
    Longitude = c(-112, -108)
  )

specimen_labels <-
  spl_labels(
    specimen_tbl = spp_integrifolia,
    id_column = "Taxon_a_posteriori"
  )

# Elevation Profile ----

ggplot_elevation <-
  layer_elevation(
    specimen_tbl = spp_integrifolia,
    raster_zoom = 7
  ) +
  layer_borders(
    spl_extent = spl_bbox(spp_integrifolia)
  ) +
  coord_sf(
    xlim = range(spl_bbox(spp_integrifolia)[["Longitude"]]),
    ylim = range(spl_bbox(spp_integrifolia)[["Latitude"]]),
    expand = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(breaks = -111:-108)

# Specimen Layers ----

ggplot_specimens <- function() {
  list(
    stat_ellipse(
      data = spp_integrifolia %>%
        select(Longitude, Latitude, Taxon_a_posteriori) %>%
        filter(grepl("dornii|condensata", Taxon_a_posteriori)),
      mapping = aes(x = Longitude, y = Latitude, group = Taxon_a_posteriori),
      geom = "polygon", type = "norm", level = 0.995,
      fill = "black", alpha = 0.25
    ),
    stat_ellipse(
      data = spp_integrifolia %>%
        select(Longitude, Latitude, Taxon_a_posteriori) %>%
        filter(grepl("chambersii", Taxon_a_posteriori)),
      mapping = aes(x = Longitude, y = Latitude, group = Taxon_a_posteriori),
      geom = "polygon", type = "euclid", level = 0.2,
      fill = "black", alpha = 0.25
    ),
    geom_jitter(
      data = spp_integrifolia, size = 3,
      height = 0.05, width = 0.05,
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

ggplot() +
  ggplot_specimens()

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

FigDiscussionElevationSouthwestWY <-
  plot_grid(
    ggplot_elevation +
      theme(legend.position = "none") +
      ggplot_specimens(),
    legend_grid,
    nrow = 2, rel_heights = c(3, 1)
  )

ThesisPackage::save_plot(
  gg_plot = FigDiscussionElevationSouthwestWY,
  height = 6, width = 6
)
