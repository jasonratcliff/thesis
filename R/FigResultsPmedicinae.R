library(ThesisPackage)
library(ggplot2)
library(cowplot)
library(gridExtra)

# Specimens ----

# Results: *P. medicinae* Carbon County, WY elevation profile ANOVA.
spp_carbon_wyo <-
  subset_coords(
    specimen_tbl = ThesisPackage::herbarium_specimens,
    Latitude = c(39.1, 41.9), Longitude = c(-107.9, -105.1)
  ) %>%
  dplyr::filter(!grepl(pattern = "^Physaria$|floribunda",
                       x = .data$Taxon_a_posteriori))

# Text markdown label replacements to legend.
label_aes <- 
  spl_labels(
    specimen_tbl = spp_carbon_wyo,
    id_column = "Taxon_a_posteriori"
  )

label_x_axis <- label_aes %>%
  purrr::map_chr(.x = ., function(taxon) {
    gsub("Physaria", "P\\.", taxon)
  })

# Elevation ggplot ----

ggplotCarbonElevation <-
  ggplot(data = spp_carbon_wyo) +
  geom_violin(
    aes(x = Taxon_a_posteriori, y = Elev_raw_max), na.rm = TRUE
  ) +
  geom_jitter(
    aes(x = Taxon_a_posteriori, y = Elev_raw_max,
        color = Taxon_a_posteriori, shape = Taxon_a_posteriori),
    width = 0.1, na.rm = TRUE, size = 3
  ) +
  scale_x_discrete(name = NULL, labels = label_x_axis) +
  scale_y_continuous(name = "Elevation (ft.)") +
  scale_color_manual(
    "Reviewed Annotations", values = spp_color, labels = label_aes
  ) +
  scale_shape_manual(
    "Reviewed Annotations", values = spp_shape, labels = label_aes
  ) +
  theme_classic() +
  theme(
    axis.text.x = ggtext::element_markdown(),
    legend.text = ggtext::element_markdown()
  ) +
  guides(ncol = 1)

# Fruit Length ggplot ----

trait_carbon_fruits <-
  dplyr::bind_cols(
    dplyr::select(spp_carbon_wyo, "Taxon_a_posteriori", "Date_md"),
    range_split(trait_tbl = spp_carbon_wyo,
                split_var = "Mature_fruit_length_mm")
  ) %>%
  dplyr::filter(!is.na(Mature_fruit_length_mm_max) & 
                  !is.na(Date_md)) %>%
  dplyr::rename(Trait_max = Mature_fruit_length_mm_max)

ggplotCarbonFruits <-
  ggplot(trait_carbon_fruits,
         aes(x = Date_md, y = Trait_max,
             colour = Taxon_a_posteriori, shape = Taxon_a_posteriori)) +
  geom_point(na.rm = TRUE, size = 3) +
  geom_smooth(method = 'lm', na.rm = TRUE, formula = y~x,) +
  scale_y_continuous(name = "Mature Fruit Length (mm)") +
  scale_x_date(name = "Collection Date", date_labels = "%m-%d") +
  scale_color_manual("Reviewed Annotations", values = spp_color) +
  scale_shape_manual("Reviewed Annotations", values = spp_shape) +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(ncol = 1))

# cowplot Grid ----

grid_title <- ggdraw() +
  draw_label(
    expression(italic("Physaria medicinae")),
    x = 0, hjust = 0
  )

grid_traits <- 
  plot_grid(
    ggplotCarbonElevation + theme(legend.position = "none"),
    ggplotCarbonFruits + theme(legend.position = "none"),
    nrow = 2, align = "v", rel_heights = c(1, 1),
    labels = c("A", "B"), label_size = 12
  )

FigResultsPmedicinae <-
  plot_grid(
    grid_traits,
    get_legend(ggplotCarbonElevation),
    ncol = 2, rel_widths = c(2, 1)
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsPmedicinae,
  height = 6, width = 6
)

