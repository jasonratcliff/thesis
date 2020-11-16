library(ThesisPackage)
library(ggplot2)
library(cowplot)

# Specimens ----

# Results: Combined SEINet / herbarium specimen CO floribunda
spp_colorado <-
  subset_coords(
    specimen_tbl = ThesisPackage::herbarium_specimens,
    Latitude = c(36.79328, 42.21372),
    Longitude = c(-110.3819, -103.3507)
  ) %>%
  dplyr::group_by(Taxon_a_posteriori) %>% dplyr::add_tally() %>%
  dplyr::filter(n > 3, !is.na(Taxon_a_posteriori),
                !grepl(pattern = "\\?", x = Taxon_a_posteriori)) %>%
  dplyr::ungroup() %>%
  dplyr::select("Taxon_a_posteriori", "Longitude", "Latitude") %>%
  dplyr::rename("scientificName" = "Taxon_a_posteriori")

# Filter SEINet data, bind herbarium vouchers, and sort by occurrence tally.
spp_seinet <- seinet_coords %>%
  dplyr::select("scientificName", "Longitude", "Latitude") %>%
  dplyr::bind_rows(., spp_colorado) %>%
  dplyr::filter(!is.na(Latitude), !is.na(Longitude)) %>%
  dplyr::group_by(scientificName) %>%
  dplyr::add_tally() %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::ungroup()

specimen_labels <-
  spl_labels(specimen_tbl = spp_seinet, id_column = "scientificName")

ggplot_specimens <- function() {
  list(
    geom_jitter(
      data = spp_seinet, size = 3,
      aes(x = Longitude, y = Latitude,
          color = scientificName, shape = scientificName)
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
  layer_elevation(specimen_tbl = ggplot_extent, raster_zoom = 7) +
  layer_borders(spl_extent = ggplot_extent) +
  coord_sf(
    xlim = range(ggplot_extent[["Longitude"]]),
    ylim = range(ggplot_extent[["Latitude"]]),
    expand = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude")


# Legends ----

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

FigResultsSEINet <-
  plot_grid(
    ggplot_elevation +
      theme(legend.position = "none") +
      ggplot_specimens(),
    legend_grid,
    nrow = 2, rel_heights = c(3, 1)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsSEINet,
  height = 6, width = 6
)
