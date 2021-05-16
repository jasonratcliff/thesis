library(Thesis)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tibble)

# Introduction: Build ggmap of putative *P. vitulifera* in Carbon County, WY.
carbon <- list()

# Specimens ----
carbon$specimens <-
  Thesis::subset_coords(
    specimen_tbl = Thesis::herbarium_specimens,
    Longitude = c(-107.9, -105.1),
    Latitude = c(39.1, 41.9)
  ) %>%
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

# Labels ----

# Row tibble for text annotation of Bears Ears Range area.
carbon$bears_ear <-
  tibble::tribble(
    ~"Feature", ~"Longitude", ~"Latitude",
    "Bears Ears\nRange", -106.025, 40.45,
    "North\nPark", -106.25, 40.75,
    "Middle\nPark", -105.94, 40.11
  )

# Aesthetic values from combined herbarium and SEINet records.
carbon$aesthetics <-
  c(
    Thesis::spl_labels(
      specimen_tbl = carbon$specimens,
      id_column = "Taxon_a_posteriori"
    ),
    Thesis::spl_labels(
      specimen_tbl = Thesis::seinet_coords,
      id_column = "scientificName"
    )
  )

carbon$aesthetics <-  # Drop duplicate aesthetics values from named vector
  carbon$aesthetics[!duplicated(names(carbon$aesthetics))]

# Subset of DNA specimens from Medicine Bow National Forest and CO Front Range.
carbon$dna <-
  Thesis::subset_coords(
    specimen_tbl = Thesis::dna_specimens,
    Longitude = c(-108, -105),
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
  Thesis::find_spp(
    specimen_tbl = Thesis::herbarium_specimens,
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
  Thesis::layer_ggmap(
    specimen_tbl = carbon$specimens,
    gg_map_type = "satellite", zoom_lvl = 8,
    gg_longitude = -106.5, gg_latitude = 40.4
  ) +
  Thesis::layer_borders(
    spl_extent = Thesis::spl_bbox(carbon$specimens),
    sf_county_color = "black"
  ) +
  Thesis::layer_specimens(
    specimen_tbl = carbon$specimens,
    shape_aes = TRUE,
    id_column = "Taxon_a_posteriori",
    legend_status = TRUE
  ) +
  geom_text(
    data = carbon$bears_ear,
    mapping = aes(x = Longitude, y = Latitude, label = Feature),
    color = "white", alpha = 0.5, size = 2
  ) +
  geom_point(
    data = Thesis::seinet_coords,
    mapping = aes(
      x = Longitude, y = Latitude,
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
    size = 5, shape = 5, fill = NA, color = "gold"
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

# Call Expression ----

carbon$map <-
  tibble::tribble(
    ~"nudge_x", ~"nudge_y", ~"segment.curvature", ~"Key",
    0.4, -0.05, 0.1,  "Nelson_49286",
    -0.6, -0.25, 0.1, "Kastning_Kastning_1462",
    -0.5, -0.25, 0.1, "Kastning_Culp_1725",
    -0.4, -0.1, -0.1, "Nelson_49478",
    0.5, 0.1, 0.1,  "Rollins_Rollins_8621",
    -0.4, -0.125, 0.2,  "Dorn_10105",
    -0.33, 0.1, -0.1,  "Fertig_16713",
    -0.4, 0.3, -0.1,  "Fertig_Welp_19075",
    0.7, 0.3, -0.75,  "Ratcliff_O_Kane_Jr_34",
    -0.275, -0.25, 0.1,  "O_Kane_Jr_3754",
    -0.3, -0.225, 0.1,  "Dorn_9837"
  ) %>%
  dplyr::left_join(
    x = .,
    y = carbon$labels,
    by = "Key"
  ) %>%
  purrr::pmap(
    .l = .,
    .f = function(
      Collector, Collection_Number,
      Longitude, Latitude, Label,
      nudge_x, nudge_y, segment.curvature, ...
    ) {
      rlang::call2(
        .fn = ggrepel::geom_label_repel,
        data = tibble::tibble(
          Longitude = Longitude,
          Latitude = Latitude,
          Label = Label
        ),
        mapping = ggplot2::aes(
          x = Longitude,
          y = Latitude,
          label = Label
        ),
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        segment.curvature = segment.curvature,
        box.padding = 0.5,
        alpha = 0.66,
        segment.color = "white"
      )
    }) %>%
  purrr::reduce(
    .x = .,
    .f = ~ rlang::expr(!!.x + !!.y),
    .init = rlang::expr(carbon$ggplot)
  )

carbon$map <- rlang::eval_tidy(carbon$map)

# .png ----

carbon$legend_png <-
  get_legend(
    carbon$ggplot +
      guides(color = guide_legend(ncol = 1)) +
      theme(
        legend.background = element_rect(fill = "grey99"),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = margin(0, -0.25, 0, 0.1, "in")
      )
  )

carbon$figure_png <-
  plot_grid(
    carbon$map +
      theme(
        plot.margin = margin(0.25, -2.5, 0.25, -2.5, "in"),
        legend.position = "none"
      ),
    carbon$legend_png,
    nrow = 1,
    rel_widths = c(0.75, 0.25)
  )

# .pdf ----

carbon$legend_pdf <-
  get_legend(
    carbon$ggplot +
      guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
      theme(
        legend.position = "bottom",
        legend.direction = "vertical"
      )
  )

carbon$figure_pdf <-
  plot_grid(
    carbon$map +
      theme(
        plot.margin = margin(-0.33, 0.2, -0.66, 0, "in"),
        legend.position = "none"
      ),
    carbon$legend_pdf,
    ncol = 1,
    rel_heights = c(0.8, 0.2)
  )

# cowplot Grid ----

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(carbon$figure_png, carbon$figure_pdf),
    width = c(6, 6),
    height = c(8, 4),
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

