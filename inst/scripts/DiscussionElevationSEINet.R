library(thesis)
library(dplyr)
library(ggplot2)
library(cowplot)

# Specimens ----

# Results: Combined SEINet / herbarium specimen CO floribunda
spp_colorado <- thesis::herbarium_specimens |>
  dplyr::rename(
    decimalLongitude = Longitude,
    decimalLatitude = Latitude
  ) |>
  Extent$new()

spp_colorado$bbox(xmin = -110.4, ymin = 36.8, xmax = -103.5, ymax = 42.2)

spp_colorado <-
  dplyr::bind_cols(
    tibble::as_tibble(spp_colorado$sf),
    sf::st_coordinates(spp_colorado$sf)
  ) |>
  dplyr::rename(
    Longitude = X, Latitude = Y,
    scientificName = Taxon_a_posteriori
  )

# Filter SEINet data, bind herbarium vouchers, and sort by occurrence tally.
spp_seinet <- seinet %>%
  dplyr::rename(
    Longitude = "decimalLongitude",
    Latitude = "decimalLatitude"
  ) %>%
  select("scientificName", "Longitude", "Latitude") %>%
  bind_rows(., spp_colorado) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  group_by(scientificName) %>%
  add_tally() %>%
  arrange(desc(n)) %>%
  ungroup()

specimen_labels <-
  spl_labels(
    specimen_tbl = spp_seinet,
    id_column = "scientificName"
  )

ggplot_specimens <- function() {
  list(
    stat_ellipse(
      data = spp_colorado %>%
        select(Longitude, Latitude, scientificName) %>%
        filter(grepl("vitulifera|bellii|'medicinae'", scientificName)),
      mapping = aes(x = Longitude, y = Latitude, group = scientificName),
      geom = "polygon", type = "norm", level = 0.999,
      fill = "black", alpha = 0.25
    ),
    geom_jitter(
      data = spp_seinet, size = 3,
      aes(
        x = Longitude, y = Latitude,
        color = scientificName, shape = scientificName
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

# Elevation Profile ----

# Build elevation and border base layer subset to extent.
ggplot_extent <-
  tibble::tribble(
    ~"Longitude", ~"Latitude",
    -110.3819, 36.79328,
    -103.3507, 36.79328,
    -110.3819, 42.21372,
    -103.3507, 42.21372
  )

ggplot_elevation <-
  layer_elevation(
    specimen_tbl = ggplot_extent,
    raster_zoom = 7
  ) +
  layer_borders(spl_extent = ggplot_extent) +
  coord_sf(
    xlim = range(ggplot_extent[["Longitude"]]),
    ylim = range(ggplot_extent[["Latitude"]]),
    expand = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude")


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
      ncol = 1, rel_heights = c(0.6, -0.275, 0.4)
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
      filename = fs::path("inst/figures/DiscussionElevationSEINet", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  }
)
