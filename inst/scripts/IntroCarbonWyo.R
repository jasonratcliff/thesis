library(thesis)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tibble)

# Introduction: Build ggmap of putative *P. vitulifera* in Carbon County, WY.
carbon <- list()

# Specimens ----
carbon$specimens <-
  thesis::subset_coords(
    specimen_tbl = thesis::herbarium_specimens,
    Longitude = c(-109, -105),
    Latitude = c(39.1, 41.9)
  ) %>%
  dplyr::mutate(
    Taxon_a_posteriori = dplyr::case_when(
      grepl("vitulifera", x = .data$Taxon) & .data$Latitude > 40.5 ~
        "Physaria vitulifera - Carbon",
      grepl("acutifolia", x = .data$prior_id) &
        grepl("'medicinae'", x = .data$Taxon_a_posteriori) ~
        "Physaria acutifolia - vitulifera-like",
      TRUE ~ as.character(.data$Taxon_a_posteriori)
    )
  )

# Aesthetic values from combined herbarium and SEINet records.
carbon$aesthetics <-
  c(
    thesis::spl_labels(
      specimen_tbl = carbon$specimens,
      id_column = "Taxon_a_posteriori"
    ),
    thesis::spl_labels(
      specimen_tbl = thesis::seinet,
      id_column = "scientificName"
    )
  )

carbon$aesthetics <-  # Drop duplicate aesthetics values from named vector
  carbon$aesthetics[!duplicated(names(carbon$aesthetics))]

# Subset of DNA specimens from Medicine Bow National Forest and CO Front Range.
carbon$dna <-
  thesis::subset_coords(
    specimen_tbl = thesis::dna_specimens,
    Longitude = c(-108.5, -105),
    Latitude = c(39, 42)
  ) %>%
  dplyr::select(Collector, Collection_Number, Longitude, Latitude) %>%
  dplyr::mutate(
    Label = stringr::str_remove_all(
      string = Collector,
      pattern = "[A-Z]\\. ?"
    ) %>%
    gsub("with|and", "&", x = .) %>%
    gsub("&", "&\n", x =.) %>%
    paste(., Collection_Number, sep = "\n")
  )

carbon$jackson <-
  thesis::find_spp(
    specimen_tbl = thesis::herbarium_specimens,
    collector = "Kastning|Nelson",
    collection = "1462|1725|49286|49478"
  ) %>%
  dplyr::select(Collector, Collection_Number, Longitude, Latitude) %>%
  dplyr::mutate(
    Label = stringr::str_remove_all(
      string = Collector,
      pattern = "[A-Z]\\. ?"
    ) %>%
      gsub("with|and", "&", x = .) %>%
      paste(., Collection_Number, sep = "\n"),
    Collection_Number = as.numeric(.data$Collection_Number)
  )

carbon$labels <-
  dplyr::bind_rows(
    carbon$dna,
    carbon$jackson
  ) %>%
  dplyr::mutate(
    Key = stringr::str_replace_all(
      string = .data$Label,
      pattern = "[^A-z0-9]+",
      replacement = "_"
    )
  )

# Map ----

# Satellite map of Carbon, WY specimens.
carbon$ggplot <-
  thesis::layer_ggmap(
    specimen_tbl = carbon$specimens,
    gg_map_type = "satellite", zoom_lvl = 8,
    gg_longitude = -106.75, gg_latitude = 40.4
  ) +
  thesis::layer_borders(
    spl_extent = thesis::spl_bbox(carbon$specimens),
    sf_county_color = "black"
  ) +
  thesis::layer_specimens(
    specimen_tbl = carbon$specimens,
    shape_aes = TRUE,
    id_column = "Taxon_a_posteriori",
    legend_status = TRUE
  ) +
  geom_text(
    data = tibble::tribble(
      ~"Feature", ~"Longitude", ~"Latitude",
      "North\nPark", -106.25, 40.75,
      "Middle\nPark", -105.94, 40.11
    ),
    mapping = aes(x = Longitude, y = Latitude, label = Feature),
    color = "white", alpha = 0.75, size = 3
  ) +
  geom_text(
    data = tibble::tribble(
      ~"Feature", ~"Longitude", ~"Latitude",
      "Bears Ears\nRange", -106.1285, 40.475,
    ),
    mapping = aes(x = Longitude, y = Latitude, label = Feature),
    color = "white", alpha = 0.5, size = 3
  ) +
  geom_point(
    data = thesis::seinet,
    mapping = aes(
      x = decimalLongitude, y = decimalLatitude,
      color = scientificName, shape = scientificName
    ),
    show.legend = TRUE, na.rm = TRUE, inherit.aes = TRUE
  ) +
  geom_point(
    data = carbon$dna, inherit.aes = FALSE, show.legend = FALSE,
    mapping = aes(x = .data$Longitude, y = .data$Latitude),
    size = 5, shape = 5, fill = NA, color = "white"
  ) +
  geom_point(
    data = carbon$jackson, inherit.aes = FALSE, show.legend = FALSE,
    mapping = aes(x = .data$Longitude, y = .data$Latitude),
    size = 5, shape = 0, fill = NA, color = "gold"
  ) +
  scale_color_manual(
    name = "Annotation", labels = carbon$aesthetics,
    values = thesis::spp_color, na.value = "black"
  ) +
  scale_shape_manual(
    name = "Annotation", labels = carbon$aesthetics,
    values = thesis::spp_shape, na.value = 17
  ) +
  theme(
    legend.background = element_rect(color = "black"),
    legend.direction = "vertical",
    legend.key = element_blank(),
    legend.text = ggtext::element_markdown(),
    legend.text.align = 0,
    legend.title.align = 0.5,
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"),
    panel.grid = element_blank()
  ) +
  labs(x = "Longitude", y = "Latitude")

# .png ----

carbon$map_png <-
  tibble::tribble(
    ~"nudge_x", ~"nudge_y", ~"segment.curvature", ~"Key",
    0.4, -0.05, 0.1,  "Nelson_49286",
    -1, -0.25, 0.1, "Kastning_Kastning_1462",
    -1, -0.25, 0.1, "Kastning_Culp_1725",
    -0.4, -0.1, -0.1, "Nelson_49478",
    0.5, 0.1, 0.1,  "Rollins_Rollins_8621",
    -0.8, -0.125, 0.2,  "Dorn_10105",
    -0.33, 0.1, -0.1,  "Fertig_16713",
    -0.4, 0.3, -0.1,  "Fertig_Welp_19075",
    0.7, 0.3, -0.75,  "Ratcliff_O_Kane_Jr_34",
    -0.275, -0.25, 0.1,  "O_Kane_Jr_3754",
    -0.3, -0.225, 0.1,  "Dorn_9837"
  ) %>%
  thesis::repel_map_labels(
    map_nudges = .,
    map_labels = carbon$labels,
    initial_ggplot = carbon$ggplot
  ) %>%
  rlang::eval_tidy(expr = .)

carbon$legend_png <-
  get_legend(
    carbon$ggplot +
      guides(color = guide_legend(ncol = 1)) +
      theme(
        legend.background = element_rect(fill = "grey99"),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = margin(
          t = 0,
          r = -0.25,
          b = 0,
          l = 0.1,
          unit = "in"
        )
      )
  )

carbon$figure_png <-
  plot_grid(
    carbon$map_png +
      theme(
        plot.margin = margin(
          t = 0.25,
          r = -2.5,
          b = 0.25,
          l = -2.5,
          unit = "in"
        ),
        legend.position = "none"
      ),
    carbon$legend_png,
    nrow = 1,
    rel_widths = c(0.75, 0.25)
  )

# .pdf ----

carbon$map_pdf <-
  tibble::tribble(
    ~"nudge_x", ~"nudge_y", ~"segment.curvature", ~"Key",
    0.6, -0.05, 0.1,  "Nelson_49286",
    -1.4, -0.25, 0.1, "Kastning_Kastning_1462",
    -0.925, -0.25, 0.1, "Kastning_Culp_1725",
    -0.65, -0.1, -0.1, "Nelson_49478",
    0.75, 0.1, 0.1,  "Rollins_Rollins_8621",
    -1.25, -0.125, 0.2,  "Dorn_10105",
    -0.33, 0.1, -0.1,  "Fertig_16713",
    -0.4, 0.5, -0.1,  "Fertig_Welp_19075",
    0.775, 0.3, -0.75,  "Ratcliff_O_Kane_Jr_34",
    -0.275, -0.25, 0.1,  "O_Kane_Jr_3754",
    -0.3, -0.225, 0.1,  "Dorn_9837"
  ) %>%
  thesis::repel_map_labels(
    map_nudges = .,
    map_labels = carbon$labels,
    initial_ggplot = carbon$ggplot
  ) %>%
  rlang::eval_tidy(expr = .)

carbon$legend_pdf <-
  get_legend(
    carbon$ggplot +
      guides(color = guide_legend(ncol = 2)) +
      theme(
        legend.position = "bottom",
        legend.direction = "vertical"
      )
  )

carbon$figure_pdf <-
  plot_grid(
    carbon$map_pdf +
      theme(
        plot.margin = margin(
          t = -0.5,
          r = 0.5,
          b = -0.5,
          l = 0,
          unit = "in"
        ),
        legend.position = "none"
      ), NULL,
    carbon$legend_pdf,
    ncol = 1,
    rel_heights = c(0.75, -0.01, 0.25)
  )

# cowplot Grid ----

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(carbon$figure_png, carbon$figure_pdf),
    width = c(6, 6),
    height = c(8, 3.75),
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
