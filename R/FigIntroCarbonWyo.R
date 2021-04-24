library(ThesisPackage)
library(ggplot2)
# options(tigris_use_cache = TRUE)

# Introduction: Build ggmap of putative *P. vitulifera* in Carbon County, WY.
carbon <- list()

# Row tibble for text annotation of Bears Ears Range area.
carbon$bears_ear <-
  tibble::tribble(
    ~"Feature", ~"Longitude", ~"Latitude",
    "Bears Ears Range", -106.05, 40.275,
    "North\nPark", -106.25, 40.75,
    "Middle Park", -105.94, 40.11
  )

carbon$specimens <-
  subset_coords(specimen_tbl = ThesisPackage::herbarium_specimens,
                Latitude = c(39.1, 41.9), Longitude = c(-107.9, -105.1)) %>%
  dplyr::mutate(
    Taxon_a_posteriori = dplyr::case_when(
      grepl("vitulifera", x = .data$Taxon) & .data$Latitude > 40.5 ~
        "Physaria vitulifera - Carbon",
      grepl("acutifolia", x = .data$prior_id) &
        grepl("medicinae", x = .data$Taxon_a_posteriori) ~
        "Physaria acutifolia - vitulifera-like",
      TRUE ~ as.character(.data$Taxon_a_posteriori)
    )
  )

# Subset specimens to add map labels.
carbon$labels <- carbon$specimens %>%
  dplyr::filter(grepl("Kastning|Nelson", Collector)) %>%
  dplyr::filter(grepl("1462|1725|49286|49478", Collection_Number))

# Satellite map of Carbon, WY specimens.
carbon$ggplot <-
  layer_ggmap(
    specimen_tbl = medicine$specimens,
    gg_map_type = "satellite", zoom_lvl = 8,
    gg_longitude = -106.5, gg_latitude = 40.4
  ) + 
  layer_borders(
    spl_extent = spl_bbox(medicine$specimens),
    sf_county_color = "black"
  ) + 
  layer_specimens(
    specimen_tbl = medicine$specimens,
    shape_aes = TRUE, 
    id_column = "Taxon_a_posteriori",
    legend_status = TRUE
  ) +
  layer_themes(
    specimen_tbl = medicine$specimens,
    id_column = "Taxon_a_posteriori",
    legend_title = "Reviewed Annotations"
  ) +
  geom_text(
    data = medicine$bears_ear,
    aes(Longitude, Latitude, label = Feature, group = NULL),
    color = "white",
    size = 3.5
  ) +
  spl_id(
    specimen_tbl = medicine$labels,
    id_column = "Taxon_a_posteriori", shape_aes = "Taxon_a_posteriori", 
    collector = "Kastning", collection = 1462,
    h_adjust = -0.425, v_adjust = -0.15
  ) +
  spl_id(
    specimen_tbl = medicine$labels,
    id_column = "Taxon_a_posteriori", shape_aes = "Taxon_a_posteriori",
    collector = "Kastning", collection = 1725,
    h_adjust = -0.4, v_adjust = -0.15
  ) + 
  spl_id(
    specimen_tbl = medicine$labels,
    id_column = "Taxon_a_posteriori", shape_aes = "Taxon_a_posteriori",
    collector = "Nelson", collection = 49286,
    h_adjust = 0.275, v_adjust = -0.1
  ) + 
  spl_id(
    specimen_tbl = carbon$labels,
    id_column = "Taxon_a_posteriori", shape_aes = "Taxon_a_posteriori",
    collector = "Nelson", collection = 49478,
    h_adjust = -0.225, v_adjust = -0.12
  ) +
  
  theme(legend.direction = "vertical", legend.position = "bottom") +
  guides(colour = guide_legend(ncol = 2, byrow = TRUE))

purrr::walk(.x = c("png", "pdf"), .f = function(ext) {
  cowplot::save_plot(
    filename = fs::path("Figs/FigIntroCarbonWyo", ext = ext),
    plot = carbon$ggplot,
    base_width = 6,
    base_height = 4,
    base_asp = 2.5,
    nrow = 2
  )
})


# ThesisPackage::save_plot(
#   gg_plot = FigIntroCarbonWyo,
#   width = 6, height = 6
# )
