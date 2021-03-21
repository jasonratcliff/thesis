library(ThesisPackage)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggExtra)
library(cowplot)

set.seed(20210319)

# Specimens ----
specimens <- list()

# Set of reviewed annotations for species of interest.
specimens$filtered <- ThesisPackage::herbarium_specimens %>%
  filter_reviewed(specimen_tbl = .) %>%
  bind_cols(range_split(trait_tbl = ., split_var = "Mature_fruit_length_mm"))

# Fruits ----

traits <- list()

traits$labels <-
  spl_labels(
    specimen_tbl = specimens$filtered,
    id_column = "Taxon_a_posteriori"
  ) %>%
  gsub("Physaria", "P.", x = .)

# Extract ggplot legend for cowplot grid.
traits$legend <-
  ThesisPackage::annotation_legend(
    specimen_tbl = specimens$filtered,
    aesthetic_id = "Taxon_a_posteriori",
    legend_title = "Reviewed Annotations",
    ncol = 1
  )

traits$fruits <-
  jitter_violin(
    specimen_tbl = specimens$filtered,
    trait = "Mature_fruit_length_mm_max",
    aesthetic_id = "Taxon_a_posteriori",
    aesthetic_labels = traits$labels,
    legend_title = "Reviewed Annotations",
    violin.params = list(scale = "width"),
    jitter.params = list(width = 0.25),
    theme.params = list(
      axis.text.x = ggtext::element_markdown(
        angle = 45,
        hjust = 1
      )
    )
  ) +
  scale_x_discrete(labels = traits$labels) +
  labs(y = "Fruit Length (mm)")

traits$fruits <-
  ggMarginal(
    p = traits$fruits,
    margins = "y",
    type = "violin"
  )

traits$phenology <-
  trait_phenology(
    specimen_tbl = specimens$filtered,
    trait = "Mature_fruit_length_mm_max",
    aesthetic_id = "Taxon_a_posteriori",
    aesthetic_labels = traits$labels,
    legend_title = "Reviewed Annotations",
    theme.params = list(
      legend.position = "none",
      strip.text = ggtext::element_markdown(),  # Facet strip text
      axis.text.x = element_text(size = 6)
    )
  ) +
  scale_y_continuous(limits = c(0, 20)) +
  facet_wrap(
    facets = ~ Taxon_a_posteriori, ncol = 3,
    labeller = labeller(Taxon_a_posteriori = traits$labels)
  ) +
  labs(y = "Fruit Length (mm)", x = "Collection Date")

# Grid Plot ----

FigResultsPhysariaFruits <-
  plot_grid(
    traits$fruits,
    plot_grid(
      traits$phenology,
      traits$legend,
      nrow = 1, rel_widths = c(2, 1)
    ),
    ncol = 1, rel_heights = c(1, 2),
    labels = c("A", "B")
  )

ThesisPackage::save_plot(
  gg_plot = FigResultsPhysariaFruits,
  height = 10, width = 8
)

