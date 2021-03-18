library(ThesisPackage)
library(ggplot2)
library(cowplot)

set.seed(20210317)

# Specimens ----

spp_didymocarpa <- herbarium_specimens %>%
  
  subset_coords(
    specimen_tbl = .,
    Longitude = c(-115, 107),
    Latitude = c(42, 49)
  ) %>%
  
  dplyr::filter(
    !(Taxon_a_posteriori %in% paste(
      "Physaria", c("acutifolia", "brassicoides",
                    "condensata", "dornii", "integrifolia")
    )) &
      # Remove specimens with one-off prior identifications.
      !grepl("geyeri|macrantha|cordiformis|nelsonii", .data$prior_id)
  ) %>%
  
  dplyr::select(
    "prior_id", "Taxon_a_posteriori", "Latitude", "Longitude") %>%

  # Sort by uncommon identifications for overplotting.
  dplyr::group_by(.data$Taxon_a_posteriori, .data$prior_id) %>%
  dplyr::add_count() %>%
  dplyr::arrange(dplyr::desc(.data$n)) %>%
  dplyr::ungroup()

# Mapping ----

# Function wrapper to plot P. didymocarpa prior and reviewed distributions.
map_didymocarpa <- function(specimen_tbl, id_column, legend_title, ...) {
  markdown_labels <-
    spl_labels(specimen_tbl = specimen_tbl, id_column = id_column)
  base_map <- list(
    layer_borders(
      spl_extent = spl_bbox(specimen_tbl),
      sf_county_color = "black"
    ),
    layer_specimens(
      specimen_tbl = specimen_tbl,
      id_column = id_column, shape_aes = TRUE
    ),
    coord_sf(
      xlim = range(spl_bbox(specimen_tbl)[["Longitude"]]),
      ylim = range(spl_bbox(specimen_tbl)[["Latitude"]])
    ),
    scale_color_manual(
      name = legend_title, labels = markdown_labels,
      values = ThesisPackage::spp_color, na.value = "black"
    ),
    scale_shape_manual(
      name = legend_title, labels = markdown_labels,
      values = ThesisPackage::spp_shape, na.value = 17
    ),
    theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color =  "black"),
      legend.key = ggplot2::element_blank(),
      legend.text = ggtext::element_markdown()
    )
  )
  ggplot() +
    base_map
}

map_priors <- 
  map_didymocarpa(
    specimen_tbl = spp_didymocarpa,
    id_column = "prior_id",
    legend_title = "Priors"
  ) +
  ggtitle("Prior Annotations")

map_reviewed <- 
  map_didymocarpa(
    specimen_tbl = spp_didymocarpa,
    id_column = "Taxon_a_posteriori",
    legend_title = "Annotations"
  ) +
  ggtitle("Reviewed Annotations")

# Build joint legend with all ID combinations.
spp_legend <- spp_didymocarpa %>%
  dplyr::select(
    "prior_id", "Taxon_a_posteriori",
    "Latitude", "Longitude"
  ) %>%
  tidyr::pivot_longer(
    cols = c("prior_id", "Taxon_a_posteriori"),
    values_to = "Taxon"
  ) %>%
  map_didymocarpa(
    specimen_tbl = .,
    id_column = "Taxon",
    legend_title = "Annotations"
  )

FigResultsPdidymocarpaMap <-
  plot_grid(
    plot_grid(
      map_priors + theme(legend.position = "none"),
      map_reviewed + theme(legend.position = "none"),
      nrow = 1, labels = c("A", "B")
    ),
    get_legend(
      spp_legend +
        theme(legend.title.align = 0.5) + 
        guides(col = guide_legend(ncol = 2))
    ),
    nrow = 2, rel_heights = c(3, 1)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsPdidymocarpaMap,
  height = 4.75, width = 6
)