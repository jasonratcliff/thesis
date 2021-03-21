library(ThesisPackage)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggExtra)
library(cowplot)

set.seed(20210320)

# Specimens ----
specimens <- list()

# Set of reviewed annotations for species of interest.
specimens$filtered <- ThesisPackage::herbarium_specimens %>%
  filter_reviewed(specimen_tbl = .)

# Elevation ----

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

traits$elevation <-
  jitter_violin(
    specimen_tbl = specimens$filtered,
    trait = "Elev_raw_max",
    aesthetic_id = "Taxon_a_posteriori",
    aesthetic_labels = traits$labels,
    violin.params = list(scale = "width"),
    jitter.params = list(size = 3, height = 0),
    theme.params = list(
      axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1)
    )
  ) +
  scale_x_discrete(labels = traits$labels) +
  labs(y = "Maximum Elevation (ft.)")

traits$elevation <-
  ggMarginal(
    p = traits$elevation,
    margins = "y",
    type = "violin"
  )

# Grid Plot ----

FigResultsPhysariaElevation <-
  plot_grid(traits$elevation)

ThesisPackage::save_plot(
  gg_plot = FigResultsPhysariaElevation,
  height = 4, width = 8
)

