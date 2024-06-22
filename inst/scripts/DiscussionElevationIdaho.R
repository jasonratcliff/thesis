library(thesis)
library(dplyr)
library(ggplot2)
library(cowplot)
library(sf)

set.seed(20210312)

# Subset Specimens ----

spp_lyrata <- thesis::herbarium_specimens |>
  filter(!grepl("nelsonii", .data$Taxon_a_posteriori)) |>
  dplyr::rename(
    decimalLongitude = Longitude,
    decimalLatitude = Latitude
  ) |>
  Extent$new()

spp_lyrata$bbox(xmin = -115.5, ymin = 43, xmax = -110.5, ymax = 47)

spp_lyrata <-
  dplyr::bind_cols(
    tibble::as_tibble(spp_lyrata$sf),
    sf::st_coordinates(spp_lyrata$sf)
  ) |>
  dplyr::rename(Longitude = X, Latitude = Y)

specimen_labels <-
  spl_labels(
    specimen_tbl = spp_lyrata,
    id_column = "Taxon_a_posteriori"
  )

# Elevation Profile ----

ggplot_elevation <-
  layer_elevation(
    specimen_tbl = spp_lyrata,
    raster_zoom = 7
  ) +
  layer_borders(
    spl_extent = spl_bbox(spp_lyrata)
  ) +
  coord_sf(
    xlim = range(spl_bbox(spp_lyrata)[["Longitude"]]),
    ylim = range(spl_bbox(spp_lyrata)[["Latitude"]]),
    expand = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(breaks = -114:-111)

# Specimen Layers ----

ggplot_specimens <- function() {
  list(
    stat_ellipse(
      data = spp_lyrata %>%
        select(Longitude, Latitude, Taxon_a_posteriori) %>%
        filter(grepl("lyrata", Taxon_a_posteriori)),
      mapping = aes(x = Longitude, y = Latitude),
      geom = "polygon",
      type = "t", level = 0.99,
      fill = "black", alpha = 0.25
    ),
    geom_jitter(
      data = spp_lyrata, size = 3,
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
      ncol = 1, rel_heights = c(0.6, -0.35, 0.4)
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
      filename = fs::path("inst/figures/DiscussionElevationIdaho", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  }
)
