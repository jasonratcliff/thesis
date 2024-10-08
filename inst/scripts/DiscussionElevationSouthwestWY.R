library(thesis)
library(dplyr)
library(ggplot2)
library(cowplot)
library(sf)

set.seed(20210312)

# Subset Specimens ----

spp_integrifolia <- thesis::herbarium_specimens |>
  dplyr::rename(
    decimalLongitude = Longitude,
    decimalLatitude = Latitude
  ) |>
  Extent$new()

spp_integrifolia$bbox(xmin = -112, ymin = 41, xmax = -108, ymax = 44)

spp_integrifolia <-
  dplyr::bind_cols(
    tibble::as_tibble(spp_integrifolia$sf),
    sf::st_coordinates(spp_integrifolia$sf)
  ) |>
  dplyr::rename(Longitude = X, Latitude = Y)

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
      aes(
        x = Longitude, y = Latitude,
        color = Taxon_a_posteriori, shape = Taxon_a_posteriori
      )
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

ggplot() +
  ggplot_specimens()

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
      ncol = 1, rel_heights = c(0.6, -0.3, 0.4)
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
      filename = fs::path("inst/figures/DiscussionElevationSouthwestWY", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  }
)
