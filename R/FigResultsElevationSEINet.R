library(ThesisPackage)
library(dplyr)
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
  group_by(Taxon_a_posteriori) %>%
  add_tally() %>%
  filter(
    n > 3,
    !is.na(Taxon_a_posteriori),
    !grepl(pattern = "\\?", x = Taxon_a_posteriori)
  ) %>%
  ungroup() %>%
  select("Taxon_a_posteriori", "Longitude", "Latitude") %>%
  rename("scientificName" = "Taxon_a_posteriori")

# Filter SEINet data, bind herbarium vouchers, and sort by occurrence tally.
spp_seinet <- seinet_coords %>%
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
        filter(grepl("vitulifera|bellii|medicinae", scientificName)),
      mapping = aes(x = Longitude, y = Latitude, group = scientificName),
      geom = "polygon", type = 'norm', level = 0.999,
      fill = "black", alpha = 0.25
    ),
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

FigResultsElevationSEINet <-
  plot_grid(
    ggplot_elevation +
      theme(legend.position = "none") +
      ggplot_specimens(),
    legend_grid,
    nrow = 2, rel_heights = c(3, 1)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsElevationSEINet,
  height = 6, width = 6
)
