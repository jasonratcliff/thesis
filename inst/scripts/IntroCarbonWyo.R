library(Thesis)
library(ggplot2)
library(cowplot)

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
  subset_coords(specimen_tbl = Thesis::herbarium_specimens,
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

carbon$aesthetics <-
  c(
    Thesis::spl_labels(
      specimen_tbl = carbon$specimens,
      id_column = "Taxon_a_posteriori"
    ),
    Thesis::spl_labels(
      specimen_tbl = seinet_coords,
      id_column = "scientificName"
    )
  )

# Subset specimens to add map labels.
carbon$labels <- carbon$specimens %>%
  dplyr::filter(grepl("Kastning|Nelson", Collector)) %>%
  dplyr::filter(grepl("1462|1725|49286|49478", Collection_Number))

# Satellite map of Carbon, WY specimens.
carbon$ggplot <-
  layer_ggmap(
    specimen_tbl = carbon$specimens,
    gg_map_type = "satellite", zoom_lvl = 8,
    gg_longitude = -106.5, gg_latitude = 40.4
  ) + 
  layer_borders(
    spl_extent = spl_bbox(carbon$specimens),
    sf_county_color = "black"
  ) + 
  layer_specimens(
    specimen_tbl = carbon$specimens,
    shape_aes = TRUE, 
    id_column = "Taxon_a_posteriori",
    legend_status = TRUE
  ) +
  geom_point(
    data = seinet_coords,
    mapping = aes(
      x = Longitude, y = Latitude,
      color = scientificName, shape = scientificName
    ),
    show.legend = TRUE, na.rm = TRUE, inherit.aes = TRUE
  ) +
  scale_color_manual(
    name = "Annotation", labels = carbon$aesthetics,
    values = Thesis::spp_color, na.value = "black"
  ) +
  scale_shape_manual(
    name = "Annotation", labels = carbon$aesthetics,
    values = Thesis::spp_shape, na.value = 17
  ) +
  theme(
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(fill = NA, color = "black"), 
    legend.text.align = 0,
    legend.title.align = 0.5,
    legend.direction = "vertical",
    legend.key = ggplot2::element_blank(),
    legend.background = ggplot2::element_rect(
      fill = "grey90",
      color = "black"
    ),
    legend.text = ggtext::element_markdown()
  ) +
  labs(x = "Longitude", y = "Latitude")

carbon$map <- carbon$ggplot +
  geom_text(
    data = carbon$bears_ear,
    aes(Longitude, Latitude, label = Feature, group = NULL),
    color = "white",
    size = 3.5
  ) +
  spl_id(
    specimen_tbl = carbon$labels,
    id_column = "Taxon_a_posteriori", shape_aes = "Taxon_a_posteriori", 
    collector = "Kastning", collection = 1462,
    h_adjust = -0.425, v_adjust = -0.15
  ) +
  spl_id(
    specimen_tbl = carbon$labels,
    id_column = "Taxon_a_posteriori", shape_aes = "Taxon_a_posteriori",
    collector = "Kastning", collection = 1725,
    h_adjust = -0.4, v_adjust = -0.15
  ) + 
  spl_id(
    specimen_tbl = carbon$labels,
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
  theme(
    panel.background = element_rect(fill = "grey99"),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0.5, -1, 0, "in")
  )

carbon$legend_pdf <-
  get_legend(
    carbon$ggplot +
      guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
      theme(
        legend.background = element_rect(fill = "grey99"),
        legend.position = "bottom",
        legend.direction = "vertical"
      )
  )

carbon$legend_png <-
  get_legend(
    carbon$ggplot +
      guides(color = guide_legend(ncol = 1)) +
      theme(
        legend.background = element_rect(fill = "grey99"),
        legend.position = "right",
        legend.direction = "vertical"
      )
  )

carbon$figure_pdf <-
  plot_grid(
    plotlist = list(
      NULL,
      carbon$map,
      NULL,
      carbon$legend_pdf
    ),
    ncol = 1,
    rel_heights = c(-0.1, 0.8, -0.1, 0.2)
  )

carbon$figure_png <-
  plot_grid(
    carbon$map,
    carbon$legend_png,
    nrow = 1,
    rel_widths = c(0.8, 0.2)
  )

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(carbon$figure_png, carbon$figure_pdf),
    width = c(6, 6),
    height = c(7.5, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/IntroCarbonWyo", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })

